;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition for RSS
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Sep 2003
;;;;
;;;; $Id: rss.asd 7061 2003-09-07 06:34:45Z kevin $
;;;; *************************************************************************

(in-package cl-user)
(defpackage rss
  (:use #:cl #:kmrcl)
  (:export
   #:show-sites
   ))

