;;;; cl-crc.lisp

(in-package #:cl-crc)

;; TODO: input-reflected-p, result-reflected-p
;; TODO Check value [Optional]: This value is not required but often specified to help to validate the implementation.
;; This is the CRC value of input string "123456789"
(defmacro make-crc (name width polynomial &key (initial-crc 0) (final-xor 0))
  (let ((defconstant-name (intern (format nil "+~a-TABLE+" name))))
    `(progn
       (let ((generator ,polynomial)
             (crc-table (make-array 256 :element-type '(unsigned-byte ,width))))
         ;; iterate over all byte values 0 - 255
         (dotimes (divident 256)
           ;; move divident byte into MSB of width Bit CRC
           (let ((byte (ash divident ,(- width 8))))
             ;; calculate the CRC-32 value for current byte
             (dotimes (bit 8)
               (if (zerop (ldb (byte 1 ,(1- width)) byte))
                   (setf byte (ash byte 1))
                   (setf byte (logxor (ash byte 1) generator))))
             ;; store CRC value in lookup table
             (setf (aref crc-table divident) (ldb (byte ,width 0) byte))))
         (defconstant ,defconstant-name crc-table))
       (defun ,(intern (format nil "~a" name)) (bufer &key (start 0) (end nil))
         (loop :with crc := ,initial-crc
               :for idx :from start :below (or end (length bufer))
               :for byte := (aref bufer idx)
               ;; XOR-in next input byte into MSB of crc, that's our new intermediate divident
               :for pos := (logxor (ash crc ,(- 8 width)) byte)
               ;; Shift out the MSB used for division per lookuptable and XOR with the remainder
               :do (setf crc (ldb (byte ,width 0) (logxor (ash crc 8) (aref ,defconstant-name pos))))
               :finally (return ,(if (zerop final-xor)
                                     'crc
                                     `(logxor ,final-xor crc))))))))
