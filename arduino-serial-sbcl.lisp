(defpackage :arduino-serial-sbcl
  (:shadowing-import-from :cl close open ftruncate truncate time abort
			  read write)
  (:use :cl :sb-posix)
  (:export #:open-serial
	   #:close-serial
	   #:fd-type
	   #:serial-recv-length
	   #:read-response
	   #:talk-arduino))

(in-package :arduino-serial-sbcl)

(defconstant FIONREAD #x541B)
(defconstant IXANY #o4000)
(defconstant CRTSCTS #o20000000000)

(deftype fd-type ()
  `(unsigned-byte 31))

#+nil
(defparameter *com* (open-serial "/dev/ttyUSB0" :rate B9600))

#+nil
(sb-sys:fd-stream-fd *com*)

#+nil
(close-serial *com*)

(defun open-serial (tty &key (element-type 'character) (rate B115200))
  (declare (type (or pathname string) tty)
	   (values stream &optional))
  (let* ((fd (sb-posix:open
	      tty (logior O-RDWR
			  O-NOCTTY #+nil (this terminal can't control this program)
			  O-NDELAY #+nil (we don't wait until dcd is space)
			  )))
	 (term (tcgetattr fd))
	 (baud-rate rate))

    (fcntl fd F-SETFL (logior O-RDWR O-NOCTTY)) #+nil (reset file status flags, clearing e.g. O-NDELAY)

    (cfsetispeed baud-rate term)
    (cfsetospeed baud-rate term)

    (macrolet ((set-flag (flag &key (on ()) (off ()))
		 `(setf ,flag (logior ,@on (logand ,flag ,@off)))))

    (setf
     (aref (termios-cc term) VMIN) 1 #+nil (wake up after 32 chars are read)
     (aref (termios-cc term) VTIME) 5 #+nil (wake up when no char arrived for .1 s))

     ;; check and strip parity, handshake off
     (set-flag (termios-iflag term)
	       :on ()
	       :off (IXON IXOFF IXANY
		     IGNBRK BRKINT PARMRK ISTRIP
		     INLCR IGNCR ICRNL))

     ;; process output
     (set-flag (termios-oflag term)
	       :off (OPOST))

     ;; canonical input but no echo
     (set-flag (termios-lflag term)
	       :on ()
	       :off (ICANON ECHO ECHONL IEXTEN ISIG))

     ;; enable receiver, local mode, 8N1 (no parity)
     (set-flag (termios-cflag term)
	       :on (CLOCAL CREAD
			   CS8 CRTSCTS)
	       :off (CSTOPB CSIZE PARENB)))

    (tcflush fd TCIFLUSH) #+nil (throw away any input data)

    (tcsetattr fd TCSANOW term) #+nil (set terminal port attributes)
    (sb-sys:make-fd-stream fd :input t :output t :element-type element-type
			   :buffering :full)))

(defun close-serial (s)
  (declare (type stream s)
	   (values null &optional))
  (let ((fd (sb-sys:fd-stream-fd s)))
   (fcntl fd F-SETFL 0) #+nil (reset file status flags, clearing e.g. O-NONBLOCK)
   (sb-posix:close fd) #+nil (this will set DTR low))
  nil)

(defun serial-recv-length (s)
  (declare (type stream s)
	   (values (signed-byte 32) &optional))
  (sb-alien:with-alien ((bytes sb-alien:int))
    (ioctl (sb-sys:fd-stream-fd s) FIONREAD (sb-alien:addr bytes))
    bytes))

(defun read-response (tty-stream)
  (declare (type stream tty-stream)
	   (values string &optional))
  (let ((n (serial-recv-length tty-stream)))
    (if (eq 0 n)
	""
	(let ((ret (make-string n)))
	  (dotimes (i n)
	    (setf (char ret i) (read-char tty-stream)))
	  ret))))

;; the arduino can be restarted by resetting dtr
;; c code is in the end of tty_ioctl(4)
;; more information on resetting arduino:
;; http://playground.arduino.cc/Main/DisablingAutoResetOnSerialConnection

(defun set-dtr (s val)
  (declare (type stream s)
           (type boolean val)
           (values (signed-byte 32) &optional))
  (let ((TIOCM-DTR 2)
        (TIOCMSET #x5418))
    (sb-alien:with-alien ((serial sb-alien:int))
      (setf (ldb (byte 1 TIOCM-DTR) serial) (if val 1 0))
      (arduino-serial-sbcl::ioctl (sb-sys:fd-stream-fd s)
				  TIOCMSET (sb-alien:addr serial))
      serial)))


(defun write-arduino (tty-stream command)
  (declare (type stream tty-stream)
	   (type string command))
  (format tty-stream "~a~a" command #\Return)
  (finish-output tty-stream))

(defun ensure-response-buffer-clear (s)
 (unless (= 0 (serial-recv-length s))
   (read-response s)))

(defun talk-arduino (tty-stream command &key (time .009d0))
  (declare (type stream tty-stream)
	   (type string command)
	   (values string &optional))
  (ensure-response-buffer-clear tty-stream)
  (write-arduino tty-stream command)
  ;(sleep .1)
  (let ((n (do ((i 0 (1+ i))
		(n 0 (serial-recv-length tty-stream)))
	       ((or (< 0 n) (<= 30 i)) n)
	     (sleep time))))
    (if (eq 0 n)
	""
	(read-response tty-stream))))

