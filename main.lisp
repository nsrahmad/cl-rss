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

(defclass rss-channel ()
  ((title :reader title)
   (link :reader link)
   (description :reader description :initform nil)
   (language :reader language :initform nil)
   (image :reader image :initform nil)
   (pub-date :reader pub-date :initform nil)
   (last-build-date :reader last-build-date :initform nil)

   (items :accessor items :initform nil)))

(defclass rss-item ()
  ((title :reader title)
   (link :reader link)
   (description :reader description :initform nil)
   (pub-date :reader pub-date :initform nil)))

(defclass rss-image ()
  ((url :reader url)
   (title :reader title)
   (link :reader link)
   (width :reader width :initform nil)
   (height :reader height :initform nil)
   (description :reader description :initform nil)))

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
        (parse-rss-stream strm)))))

(defun parse-rss-file (file)
  (with-open-file (stream file :direction :input)
    (parse-rss-stream stream)))

(defun is-rss-version-supported (version-string)
  (and (member version-string '("0.91" "0.92" "2.0") :test #'string=) t))

(define-condition rss-parse-error (error)
  ((msg :initarg :msg :reader msg))
  (:documentation "Thrown when PARSE-RSS-STREAM encounters invalid RSS data.")
  (:report
   (lambda (condition stream)
     (format stream "Parse error reading RSS~@[. ~A~]" (msg condition)))))

(define-condition rss-version-unsupported (rss-parse-error)
  ((version :initarg :version :reader version))
  (:documentation
   "Thrown when PARSE-RSS-STREAM encounters RSS of a version it doesn't
recognise.")
  (:report
   (lambda (condition stream)
     (format stream "Unexpected RSS version: ~S" (version condition)))))

(defmacro string=-case (keyform (&rest cases) &optional &body otherwise)
  "A version of CASE that tests using string=."
  (let ((key (gensym)) (expected))
    `(let ((,key ,keyform))
       (cond
         ,@(mapcar
            (lambda (form)
              (destructuring-bind (string &body body) form
                (unless (stringp string)
                  (error "Can only deal with strings as keys."))
                (push string expected)
                `((string= ,string ,key) ,@body)))
            cases)
         (t
          ,@otherwise)))))

(defun setf-unique-slot (object name value)
  "Set the slot with the given NAME in OBJECT to VALUE, throwing an error if it
was already set."
  (when (and (slot-boundp object name) (slot-value object name))
    (error 'rss-parse-error
           :msg (format nil "<~A> should only be specified once in the node."
                        name)))
  (setf (slot-value object name) value))

(defun setf-unique-string (object name node)
  "Set the slot with the given NAME in OBJECT to the string contents of NODE,
throwing an error if it was already set, or if they aren't a string. Used for
elements like <title> and <link>, which shouldn't crop up twice."
  (let ((string (car (xmls:xmlrep-children node))))
    (unless (stringp string)
      (error 'rss-parse-error
             :msg (format nil "Got ~A when expecting string for contents of <~A>"
                          string name)))
    (setf-unique-slot object name string)))

(defun ensure-string-slots-filled (object required-slots strict?)
  "For each slot in REQUIRED-SLOTS, if it is unbound in OBJECT, set it to the
empty string if STRICT? is NIL, or throw an error if true."
  (dolist (slot required-slots)
    (unless (and (slot-boundp object slot) (slot-value object slot))
      (if strict?
          (error 'rss-parse-error
                 :msg (format nil "Required field ~A not set for object." slot))
          (setf (slot-value object slot) "")))
    (when (and strict? (not (stringp (slot-value object slot))))
      (error 'rss-parse-error
             :msg (format nil "Slot ~A is set to ~A, which is not a string."
                          slot (slot-value object slot)))))
  (values))

(defun parse-type (class child-parser node strict? required-string-slots)
  (let ((object (make-instance class)))
    (map nil
         (lambda (subnode) (funcall child-parser subnode object strict?))
         (xmls:xmlrep-children node))
    (ensure-string-slots-filled object required-string-slots strict?)
    object))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symbol-to-name (sym)
    "Convert symbols in the form SOME-SYM to RSS-style camelCase (a lopsided
camel, it seems)."
    (let ((parts (split-alphanumeric-string (symbol-name sym))))
      (format nil "~A~{~A~}"
              (string-downcase (first parts))
              (mapcar #'string-capitalize (cdr parts))))))

(defmacro def-child-parser (name (&rest unique-strings)
                            &rest complicated-forms)
  "Define a parser that sets UNIQUE-STRINGS in the obvious
way. COMPLICATED-FORMS should be lists (KEY &body BODY) where KEY is a string
and BODY is performed with ITEM set to the item we're modifying and NODE set to
the XML node we just got."
  `(defun ,name (node object strict?)
     (declare (ignorable strict?))
     (string=-case (xmls:xmlrep-tag node)
         (,@(mapcar
             (lambda (sym) `(,(symbol-to-name sym)
                              (setf-unique-string object ',sym node)))
             unique-strings)
          ,@complicated-forms))))

(def-child-parser parse-item-child (title link description pub-date))
(defun parse-item (node strict?)
  (parse-type 'rss-item 'parse-item-child node strict? '(title link)))

(def-child-parser parse-image-child (url title link width height description))
(defun parse-image (node strict?)
  (parse-type 'rss-image 'parse-image-child node strict? '(url title link)))

(def-child-parser parse-channel-child
    (title link description language pub-date last-build-date)
  ("item" (push (parse-item node strict?) (items object)))
  ("image" (setf-unique-slot object 'image (parse-image node strict?))))

(defun parse-rss-stream (str &key err strict?)
  "Parse RSS data from STR, which can be either a stream or a string. If ERR,
then throw an error when something goes wrong, otherwise just return NIL. If
STRICT?, check more carefully for whether the document fails to follow the RSS
2.0 specification (which incorporates 0.9x)"
  (handler-case
      (let* ((*package* (find-package 'kmrcl))
             (tree (xmls:parse str :compress-whitespace t))
             (children (xmls:xmlrep-children tree))
             (version (xmls:xmlrep-attrib-value "version" tree nil)))

        (unless (string= "rss" (xmls:xmlrep-tag tree))
          (error 'rss-parse-error :msg "Data doesn't claim to be RSS."))
        (unless (is-rss-version-supported version)
          (error 'rss-version-unsupported :version version))
        (unless (and (= 1 (length children))
                     (string= "channel" (xmls:xmlrep-tag (first children))))
          (error 'rss-parse-error
                 :msg "<rss> should have one child, a <channel>."))

        (let ((channel (first children))
              (rss (make-instance 'rss-channel)))

          (map nil (lambda (child) (parse-channel-child child rss strict?))
               (xmls:xmlrep-children channel))

          (ensure-string-slots-filled
           rss
           `(title link description
                   ,@(when (string= version "0.91") '(language)))
           strict?)

          (unless (or (not strict?) (string= version "2.0") (image rss))
            (error 'rss-parse-error
                   :msg "<rss> should have <image> specified for version <2.0"))

          (setf (items rss) (nreverse (items rss)))
          rss))

    (rss-parse-error (e)
      (when err (error e))
      nil)))
