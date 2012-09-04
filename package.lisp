;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition for RSS
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Sep 2003
;;;;
;;;; $Id$
;;;; *************************************************************************

(in-package cl-user)
(defpackage rss
  (:use #:cl #:kmrcl)
  (:export
   #:rss-channel #:rss-item #:rss-image
   #:show-sites #:rss-site
   #:parse-rss-file #:parse-rss-stream

   ;; Accessors to class slots
   #:title #:link #:description #:language #:pub-date #:image #:items
   #:url #:width #:height #:last-build-date))


