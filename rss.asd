;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          rss.asd
;;;; Purpose:       ASDF definition file for Rss
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Aug 2003
;;;;
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
  :version "0.9.1.1"

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
   (:file "main")))
