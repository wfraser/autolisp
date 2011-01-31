;;; -*- Mode LISP; Syntax: COMMON-LISP; Package: AUTOLISP; Base: 10 -*-
;;;
;;; AutoLisp
;;;
;;; Copyright (c) 2011
;;;  William R. Fraser
;;;

(ql:quickload "hunchentoot")
(ql:quickload "cl-who")
(ql:quickload "cl-ppcre")
;(ql:quickload "hunchentoot-dir-lister")

(defpackage :autolisp
    (:use :cl :cl-who :hunchentoot)
    (:export :getcwd :reload :start-server :start-server-and-wait :*script-name* :*script-filename* :*path-info*))

(in-package :autolisp)

(defmacro dbg (fmt &rest args)
    `(format t ,fmt ,@args))       ; swap these two lines for debug output
;    `(list ,fmt ,@args))            ; 

(setf *show-lisp-errors-p* t)
;(setf *show-lisp-backtraces-p* t)
;(setf hunchentoot:*catch-errors-p* nil)

(defparameter *autolisp-server-port* 3259)

(defparameter *autolisp-basedir* nil)   ; nil means to use the current working directory
(defparameter *autolisp-mapping* (list
    ; url prefix   =>  filesystem prefix
    (cons "/autolisp/" "test/")
    (cons "/"          "")))
(defparameter *autolisp-indexes* (list
    (cons "index.html"  `autolisp-plain-dispatcher)
    (cons "index.lisp"  `autolisp-dispatcher)))
(defparameter *custom-mime-types* (list
    (cons "php"         "text/plain")
    (cons "lisp"        "text/plain")))

(defvar *script-name* nil)
(defvar *script-filename* nil)
(defvar *path-info* nil)

;; this requires modifying Hunchentoot to make *mime-type-hash* exported
(loop for pair in *custom-mime-types* do
    (setf (gethash (car pair) hunchentoot:*mime-type-hash*) (cdr pair)))

(defun getcwd ()
    (block nil
#+sbcl  (return (concatenate 'string (sb-posix:getcwd) "/"))
#+clisp (return (ext:cd))
        (error "no getcwd available!")))

(defmacro is-relative-path (path)
    `(or (eql :relative (car (pathname-directory ,path)))
        (null (pathname-directory ,path))))

(defmacro file-exists (path)
    `(not (null (probe-file ,path))))

(defun find-real-file-prefix (path)
    (let ((prefix path)
          (suffix "")
          (name ""))

        ; strip directory components off the end until the path exists.
        (loop while (null (probe-file prefix)) do
            (format nil "prefix(~A) suffix(~A)~%" prefix suffix)
            (setq suffix (concatenate 'string "/" (file-namestring prefix) suffix))
            (setq prefix (directory-namestring prefix))
            (setq prefix (subseq prefix 0 (- (length prefix) 1))))

        (when (and (null (pathname-name (probe-file prefix))) (not (null suffix)))
            ; no suffix allowed if it's a directory
            (dbg "doing 404 from find-real-file-prefix")
            (setf (hunchentoot:return-code*) 404)
            (hunchentoot:abort-request-handler))

        ; if it's a directory and missing a '/', append one and redirect
        (when (and (not (string= (subseq prefix (- (length prefix) 1) (length prefix)) "/"))  ; prefix[last] != '/'
                (null (pathname-name (probe-file prefix))))
            (let ((new-path (concatenate 'string (script-name*) "/")))
                (dbg "doing 301 to ~A~%" new-path)
                (setf (hunchentoot:return-code*) 301)
                (setf (hunchentoot:header-out "Location") new-path)
                (hunchentoot:abort-request-handler)))

        (setq name (subseq (script-name*) 0 (- (length (script-name*)) (length suffix))))
        (dbg "prefix(~A) name(~A) suffix(~A)~%" prefix name suffix)
        (values prefix name suffix)))

(defun map-uri-to-file (uri)
    (dbg "request: ~A~%" uri)
    (let ((base *autolisp-basedir*))
        (cond ((null base)
            (setq base (getcwd))))
        
        (loop for mapping in *autolisp-mapping* do
            (when (eql (search (car mapping) uri) 0)
                ; found it
                (dbg "mapping found: ~A~%" mapping)
                (let* ((uri-prefix (car mapping))
                      (file-prefix (cdr mapping))
                      (uri-right (subseq uri (length uri-prefix))))
                    
                    (if (is-relative-path file-prefix)
                            (setq file-prefix (concatenate 'string base file-prefix)))
                    
                    (dbg "returning: ~A~A~%" file-prefix uri-right)
                    (multiple-value-bind (path name path-info)
                        (find-real-file-prefix (concatenate 'string file-prefix uri-right))
                        (setf *script-filename* path)
                        (setf *script-name* name)
                        (setf *path-info* path-info)
                        (return-from map-uri-to-file path)))))
        
        ; return a bogus path to force a 404
        "/___force_404"))

(defun autolisp-index-dispatcher (&optional dir)
    (if (null dir) (setq dir (map-uri-to-file (script-name*))))
    ; make sure a dir was requested
    (cond ((null dir)
            (setf (return-code*) +http-not-found+))
        ((not (null (pathname-name dir)))
            (setf (return-code*) +http-bad-request+))
        (t (loop for indexpair in *autolisp-indexes* do
                ; find which index to use
                (let ((ifile (concatenate 'string dir (car indexpair))))
                    (if (probe-file ifile)
                            (let ((iuri (concatenate 'string 
                                    dir (car indexpair))))
                                (dbg "calling ~A with ~A~%"
                                    (cdr indexpair) iuri)
                                (return-from autolisp-index-dispatcher 
                                    (funcall (cdr indexpair) iuri))))))
            (setf (return-code*) +http-forbidden+))))

(defun eval-file (file)
    (let ((canonical-file (probe-file file)))
        (cond ((not canonical-file)
                (setf (return-code*) +http-not-found+))
            ((not (pathname-name canonical-file))
                (setf (return-code*) +http-forbidden+))
            (t (eval (read (open file)))))))

(defun autolisp-dispatcher (&optional file)
    (if (null file) (setq file (map-uri-to-file (script-name*))))
    (dbg "autolisp-dispatcher running ~A~%" file)
    (eval-file file))

(defun autolisp-source-dispatcher (&optional file)
    (if (null file) (setq file (map-uri-to-file (script-name*))))
    (dbg "autolisp-source-dispatcher sending ~A~%" file)
    (handle-static-file file "text/plain"))

(defun autolisp-plain-dispatcher (&optional file)
    (if (null file) (setq file (map-uri-to-file (script-name*))))
    (dbg "autolisp-plain-dispatcher sending ~A~%" file)
    (handle-static-file file))

(defun autolisp-master-dispatcher ()
    (let ((file (map-uri-to-file (script-name*))))
        (cond ((cl-ppcre:scan "\.lisp$" file)
                (block nil
                    (format t "file(~A) dispatching for lisp: ~A~%" file (cl-ppcre:scan "\.lisp$" file))
                    (autolisp-dispatcher file)))
            ((cl-ppcre:scan "\.lisp\.source$" file)
                (autolisp-source-dispatcher file))
            ((cl-ppcre:scan "/$" file)
                (autolisp-index-dispatcher file))
            (t
                (autolisp-plain-dispatcher file)))))

(setq *dispatch-table*
    `(
        ,(create-prefix-dispatcher "/" `autolisp-master-dispatcher)))

;; note: won't redefine on reloading
(defvar *acceptor* nil)

(defun start-server ()
    (cond ((null *acceptor*)
        (setq *acceptor*
            (hunchentoot:start (make-instance 'hunchentoot:acceptor :port *autolisp-server-port*))))))

(defun start-server-and-wait ()
    (start-server)
    (loop (sleep 1000)))

(defun reload ()
    (format t "reloading autolisp~%")
    (load "serv.lisp")
    (cond ((not (eql (acceptor-port *acceptor*) *autolisp-server-port*))
        (let ()
            (format t "~A~%" hunchentoot:*acceptor*)
            (hunchentoot:stop *acceptor*)
            (setq *acceptor* nil)
            (start-server)))))

;(sb-ext:save-lisp-and-die "serv.bin" :executable t :toplevel 'autolisp:start-server-and-wait)

(start-server)
