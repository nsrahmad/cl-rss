;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          rss.asd
;;;; Purpose:       ASDF definition file for Rss
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: rss.asd 7061 2003-09-07 06:34:45Z kevin $
;;;; *************************************************************************

(defpackage #:rss-system (:use #:asdf #:cl))
(in-package #:rss-system)

#+allegro (require 'aserve)

(defsystem rss
  :name "rss"
  :author "Kevin Rosenberg based on work by Craig Brozensky"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "BSD"
  :description "Remote Site Summary"

  :properties ((#:author-email . "kevin@rosenberg.net")
	       ((#:albert #:output-dir) . "albert-docs/")
	       ((#:albert #:formats) . ("docbook"))
	       ((#:albert #:docbook #:template) . "book")
	       ((#:albert #:docbook #:bgcolor) . "white")
	       ((#:albert #:docbook #:textcolor) . "black"))
  
  :serial t
  :depends-on (kmrcl xmls #-allegro aserve)
  :components
  ((:file "package")
   (:file "main")
   ))

(defmethod perform ((o test-op) (c (eql (find-system 'rss))))
  (operate 'load-op 'rss-tests)
  (operate 'test-op 'rss-tests :force t))

(defsystem rss-tests
    :depends-on (rss ptester)
    :components ((:file "tests")))

(defmethod perform ((o test-op) (c (eql (find-system 'rss-tests))))
  (operate 'load-op 'rss-tests)
  (or (funcall (intern (symbol-name '#:do-tests)
		       (find-package '#:rss-tests)))
      (error "test-op failed")))

