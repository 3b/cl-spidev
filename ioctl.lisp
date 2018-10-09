#|
 This file is a part of cl-spidev
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.spidev.lli)

(declaim (inline %ioctl))
(cffi:defcfun (%ioctl "ioctl") :int
  (fd :int)
  (request :ulong)
  (data :pointer))

(cffi:defcvar "errno" :int)

(cffi:defcfun strerror :string
  (error :int))

(declaim (inline stream-fd))
(defun stream-fd (fd)
  #+sbcl (sb-sys:fd-stream-fd fd)
  #+ccl (ccl:stream-device fd :io)
  #-(or sbcl ccl) fd)

(defun ioctl (fd cmd)
  (cffi:with-foreign-object (arg :int)
    (let ((ret (%ioctl (stream-fd fd) cmd arg)))
      (if (<= 0 ret)
          (cffi:mem-ref arg :int)
          (error "IOCTL ~a failed: ~a" cmd (strerror *errno*))))))

(defun (setf ioctl) (value fd cmd)
  (if (cffi:pointerp value)
      (let ((ret (%ioctl (stream-fd fd) cmd value)))
	(if (<= 0 ret)
	    value
	    (error "IOCTL ~a failed: ~a" cmd (strerror *errno*))))
      (cffi:with-foreign-object (arg :int)
	(setf (cffi:mem-ref arg :int) value)
	(let ((ret (%ioctl (stream-fd fd) cmd arg)))
	  (if (<= 0 ret)
	      value
	      (error "IOCTL ~a failed: ~a" cmd (strerror *errno*)))))))

