#|
 This file is a part of cl-spidev
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.spidev.lli)

;; According to https://www.kernel.org/doc/Documentation/spi/spidev

(defvar *spidev-root* #p"/dev/")

;; Read out using constants.c
(defconstant SPI-CPHA                 #x1)
(defconstant SPI-CPOL                 #x2)
(defconstant SPI-MODE-0               #x0)
(defconstant SPI-MODE-1               #x1)
(defconstant SPI-MODE-2               #x2)
(defconstant SPI-MODE-3               #x3)
(defconstant SPI-CS-HIGH              #x4)
(defconstant SPI-LSB-FIRST            #x8)
(defconstant SPI-3WIRE                #x10)
(defconstant SPI-LOOP                 #x20)
(defconstant SPI-NO-CS                #x40)
(defconstant SPI-READY                #x80)
(defconstant SPI-TX-DUAL              #x100)
(defconstant SPI-TX-QUAD              #x200)
(defconstant SPI-RX-DUAL              #x400)
(defconstant SPI-RX-QUAD              #x800)
(defconstant SPI-IOC-RD-MODE          #x80016B01)
(defconstant SPI-IOC-WR-MODE          #x40016B01)
(defconstant SPI-IOC-RD-LSB-FIRST     #x80016B02)
(defconstant SPI-IOC-WR-LSB-FIRST     #x40016B02)
(defconstant SPI-IOC-RD-BITS-PER-WORD #x80016B03)
(defconstant SPI-IOC-WR-BITS-PER-WORD #x40016B03)
(defconstant SPI-IOC-RD-MAX-SPEED-HZ  #x80046B04)
(defconstant SPI-IOC-WR-MAX-SPEED-HZ  #x40046B04)
(defconstant SPI-IOC-RD-MODE32        #x80046B05)
(defconstant SPI-IOC-WR-MODE32        #x40046B05)

(defun file-name (pathname)
  (format NIL "~a~@[.~a~]"
          (pathname-name pathname) (pathname-type pathname)))

(defun spidev-file (id)
  (merge-pathnames (format NIL "spidev~a" id) *spidev-root*))

(defun devices ()
  (loop for device in (directory (make-pathname :name "*" :type "*" :defaults *spidev-root*))
        when (eql 0 (search "spidev" (file-name device)))
        collect (subseq (file-name device) (length "spidev"))))

(defun open-spi (id &key (direction :io))
  (open (spidev-file id) :direction direction
                         :element-type '(unsigned-byte 8)))

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
