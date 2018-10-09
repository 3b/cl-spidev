#|
 This file is a part of cl-spidev
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.spidev.lli)

;; According to https://www.kernel.org/doc/Documentation/spi/spidev

(defvar *spidev-root* #p"/dev/")

(declaim (inline spi-ioc-message))
(defun spi-ioc-message (n)
  (assert (< 0 n #.(floor (expt 2 ioc-sizebits)
                          spi-ioc-transfer-size)))
  (let ((s (* n spi-ioc-transfer-size)))
    (dpb s (byte ioc-sizebits ioc-sizeshift) spi-ioc-message-1)))

(defun file-name (pathname)
  (format NIL "~a~@[.~a~]"
          (pathname-name pathname) (pathname-type pathname)))

(defun spidev-file (id)
  (merge-pathnames (format NIL "spidev~a" id) *spidev-root*))

(defun devices ()
  (loop for device in (directory (make-pathname :name :wild :type :wild :defaults *spidev-root*))
        when (eql 0 (search "spidev" (file-name device)))
        collect (subseq (file-name device) (length "spidev"))))

(defun open-spi (id &key (direction :io))
  (open (spidev-file id) :direction direction
                         :element-type '(unsigned-byte 8)
                         :if-exists :overwrite))

(defun close-spi (handle)
  (finish-output handle)
  (close handle))

(defmacro with-open-spi ((handle id &key (direction :io)) &body body)
  (let ((handleg (gensym "HANDLE")))
    `(let* ((,handleg (open-spi ,id :direction ,direction))
            (,handle ,handleg))
       (unwind-protect
            (progn ,@body)
         (close-spi ,handleg)))))

(defun mode (handle)
  (let ((mode (ioctl handle SPI-IOC-RD-MODE32)))
    (case mode
      (#.SPI-MODE-0 :mode-0)
      (#.SPI-MODE-1 :mode-1)
      (#.SPI-MODE-2 :mode-2)
      (#.SPI-MODE-3 :mode-3)
      (T mode))))

(defun (setf mode) (mode handle)
  (case mode
    (:mode-0 (setf (ioctl handle SPI-IOC-WR-MODE32) SPI-MODE-0))
    (:mode-1 (setf (ioctl handle SPI-IOC-WR-MODE32) SPI-MODE-1))
    (:mode-2 (setf (ioctl handle SPI-IOC-WR-MODE32) SPI-MODE-2))
    (:mode-3 (setf (ioctl handle SPI-IOC-WR-MODE32) SPI-MODE-3))
    (T (setf (ioctl handle SPI-IOC-WR-MODE32) mode))))

(defun lsb-first (handle)
  (< 0 (ioctl handle SPI-IOC-RD-LSB-FIRST)))

(defun (setf lsb-first) (value handle)
  (setf (ioctl handle SPI-IOC-WR-LSB-FIRST)
        (if value 1 0)))

(defun bits/word (handle)
  (let ((value (ioctl handle SPI-IOC-RD-BITS-PER-WORD)))
    (if (= 0 value)
        8
        value)))

(defun (setf bits/word) (bits handle)
  (setf (ioctl handle SPI-IOC-WR-BITS-PER-WORD) bits))

(defun max-speed (handle)
  (ioctl handle SPI-IOC-RD-MAX-SPEED-HZ))

(defun (setf max-speed) (value handle)
  (setf (ioctl handle SPI-IOC-WR-MAX-SPEED-HZ) value)
  (let ((actual (max-speed handle)))
    (unless (= actual value)
      (warn "Failed to set max speed to ~a, reset to ~a."
            value actual))
    actual))

(defun write-bytes (bytes handle &key (start 0) end)
  (write-sequence bytes handle :start start :end end))

(defun read-bytes (bytes handle &key (start 0) end)
  (read-sequence bytes handle :start start :end end))

(defun transmit (handle bytes speed-hz delay-usecs bits/word)
  (cffi:with-pointer-to-vector-data (tx-buf bytes)
    (let ((read (cffi:make-shareable-byte-vector (length bytes))))
      (cffi:with-pointer-to-vector-data (rx-buf read)
        (cffi:with-foreign-object (xfer '(:struct xfer))
          (setf (xfer-tx-buf xfer) (cffi:pointer-address tx-buf))
          (setf (xfer-rx-buf xfer) (cffi:pointer-address rx-buf))
          (setf (xfer-len xfer) (length bytes))
          (setf (xfer-speed-hz xfer) speed-hz)
          (setf (xfer-delay-usecs xfer) delay-usecs)
          (setf (xfer-bits/word xfer) bits/word)
          (setf (xfer-cs-change xfer) 0)
          (setf (xfer-tx-nbits xfer) 0)
          (setf (xfer-rx-nbits xfer) 0)
          (setf (xfer-pad xfer) 0)
          (setf (ioctl handle spi-ioc-message-1) (cffi:pointer-address xfer))
          read)))))

(defun read-chunked (handle chunk-size count speed-hz delay-usecs bits/word)
  (let* ((total-size (* chunk-size count))
         (read (cffi:make-shareable-byte-vector total-size)))
    (cffi:with-pointer-to-vector-data (rx-buf read)
      (cffi:with-foreign-object (xfers '(:struct xfer) count)
        (flet ((fill-1 (xfer buf)
                 (setf (xfer-tx-buf xfer) 0)
                 (setf (xfer-rx-buf xfer) (cffi:pointer-address buf))
                 (setf (xfer-len xfer) chunk-size)
                 (setf (xfer-speed-hz xfer) speed-hz)
                 (setf (xfer-delay-usecs xfer) delay-usecs)
                 (setf (xfer-bits/word xfer) bits/word)
                 (setf (xfer-cs-change xfer) 0)
                 (setf (xfer-tx-nbits xfer) 0)
                 (setf (xfer-rx-nbits xfer) 0)
                 (setf (xfer-pad xfer) 0)))
          (loop for i below count
             do (fill-1 (cffi:mem-aptr xfers '(:struct xfer) i)
                        (cffi:inc-pointer rx-buf (* i chunk-size)))))
        (setf (ioctl handle (spi-ioc-message count))
              xfers)
        read))))

(defstruct (chunked-transfer-buffer (:conc-name ctb-))
  (xfers (cffi:null-pointer) :type cffi:foreign-pointer)
  (buffer nil :type (or null (simple-array (unsigned-byte 8) 1)))
  (buffer-base 0 :type fixnum)
  ;; as far as i can tell, spidev support 4k by default, expandable to
  ;; 64k by module params
  (stride 0 :type (unsigned-byte 16))
  (count 0 :type (unsigned-byte 16)))

(declaim (inline ctb-offset))
(defun ctb-offset (ctb index)
  (+ (ctb-buffer-base ctb) (* index (ctb-stride ctb))))

(defmacro with-transfer-buffers ((ctb
                                  buffer chunk-size count
                                  speed-hz delay-usecs bits/word)
                                 &body body)
  (alexandria:with-gensyms (rx-buf xfer n i o xfers)
    (alexandria:once-only (chunk-size count speed-hz delay-usecs bits/word)
      `(cffi:with-pointer-to-vector-data (,rx-buf ,buffer)
         (cffi:with-foreign-object (,xfers '(:struct xfer) ,count)
           (flet ((fill1 (,xfer ,n)
                    (setf (xfer-tx-buf ,xfer) 0)
                    (setf (xfer-rx-buf ,xfer)
                          (cffi:pointer-address
                           (cffi:inc-pointer ,rx-buf ,n)))
                    (setf (xfer-len ,xfer) ,chunk-size)
                    (setf (xfer-speed-hz ,xfer) ,speed-hz)
                    (setf (xfer-delay-usecs ,xfer) ,delay-usecs)
                    (setf (xfer-bits/word ,xfer) ,bits/word)
                    (setf (xfer-cs-change ,xfer) 0)
                    (setf (xfer-tx-nbits ,xfer) 0)
                    (setf (xfer-rx-nbits ,xfer) 0)
                    (setf (xfer-pad ,xfer) 0)))
             (loop
                for ,i below ,count
                for ,o from 0 by ,chunk-size
                do (fill1 (cffi:mem-aptr ,xfers '(:struct xfer) ,i) ,o)))
           (let ((,ctb (make-chunked-transfer-buffer
                        :xfers ,xfers
                        :buffer ,buffer
                        :buffer-base 0
                        :stride ,chunk-size
                        :count ,count)))
             (progn
               ,@body)))))))

(defun update-ctb-offsets* (ctb new-byte-offset)
  (unless (= new-byte-offset (ctb-buffer-base ctb))
    (cffi:with-pointer-to-vector-data (rx-buf (ctb-buffer ctb))
      (setf (ctb-buffer-base ctb) new-byte-offset)
      (loop for i below (ctb-count ctb)
         for o from new-byte-offset by (ctb-stride ctb)
       for rx = (cffi:mem-aptr (ctb-xfers ctb) '(:struct xfer) i)
       do (assert (< (+ o (ctb-stride ctb))
                     (length (ctb-buffer ctb))))
         (setf (xfer-rx-buf rx) (cffi:pointer-address
                                 (cffi:inc-pointer rx-buf o)))))))

(defun update-ctb-offsets (ctb new-chunk-offset)
  (update-ctb-offsets* ctb (* new-chunk-offset (ctb-stride ctb))))

(declaim (inline read-chunked*))
(defun read-chunked* (handle ctb start count)
  (declare (type (unsigned-byte 16) start count)
           (type chunked-transfer-buffer ctb)
           (optimize speed))
  (assert (< (+ start count) (ctb-count ctb)))
  (let* ((xfers (ctb-xfers ctb))
         (ret (%ioctl (stream-fd handle) (spi-ioc-message count)
                      (cffi:mem-aptr xfers '(:struct xfer) start))))
    (declare (type (signed-byte 32) ret))
    (when (minusp ret)
      (error "IOCTL SPI-IOC-MESSAGE(~a) failed: ~a"
             count (strerror *errno*)))))

