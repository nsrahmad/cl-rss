;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          main.lisp
;;;; Purpose:       Main RSS functions
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Sep 2003
;;;;
;;;; $Id$
;;;; *************************************************************************

(in-package #:rss)

(defclass rss-0.9x-channel ()
  ((title :accessor title :initform nil)
   (link :accessor link :initform nil)
   (description :accessor description)
   (items :accessor items :initform nil)))

(defclass rss-0.9x-item ()
  ((title :accessor title :initform nil )
   (link :accessor link :initform nil)))

(defvar *sites*
    '("http://www.cliki.net/recent-changes.rdf"))

(defun show-sites (&optional (sites *sites*))
  (dolist (site (mklist sites))
    (awhen (rss-site site)
	   (display-site it))))

(defun display-site (site &key (stream *standard-output*))
  (format stream "Site: ~A~%" (title site))
  (dolist (item (items site))
    (format stream "  ~A~%" (title item))))

(defun rss-site (uri)
  (multiple-value-bind (body response headers true-uri)
      (net.aserve.client:do-http-request uri)
    (declare (ignore true-uri headers))
    (when (eql 200 response)
      (with-input-from-string (strm body)
	(parse-rss-0.9x-stream strm)))))
  
(defun parse-rss-0.9x-file (file)
  (with-open-file (stream file :direction :input)
    (parse-rss-0.9x-stream stream)))

(defun is-rss-version-supported (attributes)
  (awhen (position "version" attributes :key #'car :test #'string=)
	 (let ((version (second (nth it attributes))))
	   (= 4 (length version))
	   (string= "0.9" (subseq version 0 3)))))

(defun parse-rss-0.9x-stream (stream)
  (let* ((*package* (find-package 'kmrcl))
	 (tree (remove-from-tree-if 
		(lambda (x) (and (stringp x) (is-string-whitespace x)))
		(xmls:parse stream :compress-whitespace t))))
    (unless (and (string= "rss" (first tree))
		 (is-rss-version-supported (second tree)))
      (return-from parse-rss-0.9x-stream nil))
    (let* ((content (third tree))
	   (pos 0)
	   (len (length content))
	   (rss (make-instance 'rss-0.9x-channel)))
      (when (string= "channel" (nth pos content))
	(incf pos)
	(while (and (< pos len) 
		    (or (string= "title" (car (nth pos content)))
			(string= "link" (car (nth pos content)))
			(string= "description" (car (nth pos content)))))
	  (let ((slot (nth pos content)))
	    (cond
	     ((string= "title" (car slot))
	      (setf (title rss) (second slot)))
	     ((string= "link" (car slot))
	      (setf (link rss) (second slot)))
	     ((string= "description" (car slot))
	      (setf (description rss) (second slot)))))
	  (incf pos)))
      (while (< pos len)
	(when (string= "item" (car (nth pos content)))
	  (let ((item (make-instance 'rss-0.9x-item)))
	    (dolist (pair (cdr (nth pos content)))
	      (cond
	       ((string= "title" (car pair))
		(setf (title item) (second pair)))
	       ((string= "link" (car pair))
		(setf (link item) (second pair)))))
	    (push item (items rss))))
	(incf pos))
      (setf (items rss) (nreverse (items rss)))
      rss)))



	    
      
      
