;;; -*- Mode LISP; Syntax: COMMON-LISP; Package: AUTOLISP; Base: 10 -*-
;;;
;;; AutoLisp
;;;
;;; Copyright (c) 2011
;;;  William R. Fraser
;;;

(ql:quickload "hunchentoot")
(ql:quickload "cl-who")
;(ql:quickload "hunchentoot-dir-lister")

(defpackage :autolisp
    (:use :cl :cl-who :hunchentoot)
    (:export :getcwd :reload :start-server))

(in-package :autolisp)

(defmacro dbg (fmt &rest args)
;    `(format t ,fmt ,@args))
    `(list ,fmt ,@args))

(setf *show-lisp-errors-p* t)
;(setf *show-lisp-backtraces-p* t)
;(setf hunchentoot:*catch-errors-p* nil)

(defparameter *autolisp-server-port* 3259)

(defparameter *autolisp-basedir* "/home/wfraser/lisp/")
(defparameter *autolisp-mapping* (list
    ; url prefix     => filesystem prefix
    (cons "/autolisp/"  "autolisp/")
    (cons "/srv/"       "/srv/www/")
    (cons "/"           "")))
(defparameter *autolisp-indexes* (list
    (cons "index.html"  `autolisp-plain-dispatcher)
    (cons "index.lisp"  `autolisp-dispatcher)))
(defparameter *custom-mime-types* (list
    (cons "php"         "text/plain")
    (cons "lisp"        "text/plain")))

;; this requires modifying Hunchentoot to make *mime-type-hash* exported
(loop for pair in *custom-mime-types* do
    (setf (gethash (car pair) hunchentoot:*mime-type-hash*) (cdr pair)))

(defun getcwd ()
    (block nil
#+sbcl  (return (sb-posix:getcwd))
#+clisp (return (ext:cd))
        (error "no getcwd available!")))

(defun map-uri-to-file (uri)
    (dbg "request: ~A~%" uri)
    (let ((base *autolisp-basedir*))
        (cond ((null base)
            (setq base (getcwd))))
        
        (loop for mapping in *autolisp-mapping* do
            (cond ((eql (search (car mapping) uri) 0)
                ; found it
                (dbg "mapping found: ~A~%" mapping)
                (let* ((uri-prefix (car mapping))
                      (file-prefix (cdr mapping))
                      (uri-right (subseq uri (length uri-prefix))))
                    
                    (cond ((or (eql :relative (car (pathname-directory file-prefix)))
                                (null (pathname-directory file-prefix)))
                            ; relative path prefix
                            (setq file-prefix (concatenate 'string base file-prefix))))
                    
                    (dbg "returning: ~A~A~%" file-prefix uri-right)
                    (return-from map-uri-to-file (concatenate 'string file-prefix uri-right))))))
        
        ; return a bogus path to force a 404
        "/___force_404"))

(defun autolisp-index-dispatcher ()
    (let ((dir (probe-file (map-uri-to-file (script-name*)))))
        ; make sure a dir was requested
        (cond ((null dir)
                (setf (return-code*) +http-not-found+))
            ((not (null (pathname-name dir)))
                (setf (return-code*) +http-bad-request+))
            (t (loop for indexpair in *autolisp-indexes* do
                    ; find which index to use
                    (let ((ifile (concatenate 'string (namestring dir) (car indexpair))))
                        (cond ((probe-file ifile)
                                (let ((iuri (concatenate 'string 
                                        (script-name*) (car indexpair))))
                                    (dbg "calling ~A with ~A~%"
                                        (cdr indexpair) iuri)
                                    (return-from autolisp-index-dispatcher 
                                        (funcall (cdr indexpair) iuri)))))))
                (setf (return-code*) +http-forbidden+)))))

(defun eval-file (file)
    (let ((canonical-file (probe-file file)))
        (cond ((not canonical-file)
                (setf (return-code*) +http-not-found+))
            ((not (pathname-name canonical-file))
                (setf (return-code*) +http-forbidden+))
            (t (eval (read (open file)))))))

(defun autolisp-dispatcher (&optional file)
    (cond ((null file) (setq file (map-uri-to-file (script-name*))))
        (t (setq file (map-uri-to-file file))))
    (dbg "autolisp-dispatcher running ~A~%" file)
    (eval-file file))

(defun autolisp-source-dispatcher (&optional file)
    (cond ((null file)
        (let ((uri (script-name*)))
            (setq file (map-uri-to-file
                (subseq uri 0 (- (length uri) (length ".source")))))))
        (t (setq file (map-uri-to-file file))))
    (dbg "autolisp-source-dispatcher sending ~A~%" file)
    (handle-static-file file))

(defun autolisp-plain-dispatcher (&optional file)
    (cond ((null file) (setq file (map-uri-to-file (script-name*))))
        (t (setq file (map-uri-to-file file))))
    (dbg "autolisp-plain-dispatcher sending ~A~%" file)
    (handle-static-file file))

(setq *dispatch-table*
    `(
        ,(create-regex-dispatcher "/$" `autolisp-index-dispatcher)
        ,(create-regex-dispatcher "/.*\.lisp$" `autolisp-dispatcher)
        ,(create-regex-dispatcher "/.*\.lisp\.source$" `autolisp-source-dispatcher)
        ,(create-prefix-dispatcher "/" `autolisp-plain-dispatcher)))

;; note: won't redefine on reloading
(defvar *acceptor* nil)

(defun start-server ()
    (cond ((null *acceptor*)
        (setq *acceptor*
            (hunchentoot:start (make-instance 'hunchentoot:acceptor :port *autolisp-server-port*))))))

(defun reload ()
    (format t "reloading autolisp~%")
    (load "serv.lisp")
    (cond ((not (eql (acceptor-port *acceptor*) *autolisp-server-port*))
        (let ()
            (format t "~A~%" hunchentoot:*acceptor*)
            (hunchentoot:stop *acceptor*)
            (setq *acceptor* nil)
            (start-server)))))

;(sb-ext:save-lisp-and-die "serv.bin" :executable t :toplevel 'autolisp:start-server)

(start-server)
