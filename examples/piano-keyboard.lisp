;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright (C) 2024  Bruno Emond 
;;; bruno.emond@icloud.com
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301
;;; USA
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : piano-keyboard.lisp
;;; Version     : 1.0 (2024-11-29)
;;; 
;;; Description : A set of classes and methods to use with the 
;;;             : visicon-object class. 
;;; 
;;; Bugs        : 
;;;
;;; To do       : - Hands actions on the keyboard.
;;; 
;;; ----- History -----
;;;
;;; 2024.11.29 Bruno  
;;;             : Version 1.0
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; utilities
;;;
(defun make-instances-from-midi (class-name midi-start mdid-end)
  (do* ((midi midi-start (1+ midi))
        (lst (list (apply #'make-instance (list class-name :midi midi)))
             (append lst (list (apply #'make-instance (list class-name :midi midi))))))
       ((= midi mdid-end) lst)))

(defmethod make-str ((object string)) (string-upcase object))
(defmethod make-str ((object number)) (format nil "~S" object))
(defmethod make-str ((object symbol)) (symbol-name object))

(defun intern-symbol (&rest elements)
  (when elements
    (intern (apply #'concatenate 
                   (append '(string) 
                           (mapcar #'make-str elements))))))
      
;;;
;;; tone-octave
;;;
(defclass tone-octave ()
  ((midi :initarg :midi :initform 60 :reader midi)
   tone 
   octave)
  (:documentation ""))

(defmethod print-object ((object tone-octave) (stream stream))
  (print-unreadable-object (object stream :type t)
    (with-slots (midi tone octave) object
      (format stream "~S ~S ~S" midi tone octave))))

(defun midi->tone (midi)
  (1+ (mod midi 12)))

(defun midi->octave (midi)
  (- (floor midi 12) 1))

(defmethod initialize-instance :after ((object tone-octave) &rest initargs &key &allow-other-keys)
  (with-slots (midi tone octave) object
    (setf tone (midi->tone midi)
          octave (midi->octave midi))))

(describe (make-instances-from-midi 'tone-octave 48 71))

;;;
;;; visual-key
;;; 
(defclass visual-key (tone-octave)
  (color
   group
   group-pos
   visual-class
   visual-key)
  (:documentation ""))

(defmethod print-object ((object visual-key) (stream stream))
  (print-unreadable-object (object stream :type t)
    (with-slots (midi visual-key) object
      (format stream "~S ~S" midi visual-key))))

(defun tone->color (tone)
  (cond ((member tone '(1 3 5 6 8 10 12)) 'w)
        ((member tone '(2 4 7 9 11)) 'b)))

(defun tone->group (tone)
  (cond ((member tone '(1 2 3 4 5)) 'b2)
        ((member tone '(6 7 8 9 10 11 12)) 'b3)))

(defun tone->group-pos (tone)
  (1+ (or (position tone '(1 3 5))
          (position tone '(2 4))
          (position tone '(6 8 10 12))
          (position tone '(7 9 11)))))

(defmethod initialize-instance :after ((object visual-key) &rest initargs &key &allow-other-keys)
  (with-slots (tone octave color group group-pos visual-class visual-key) object
    (setf color (tone->color tone)
          group (tone->group tone)
          group-pos (tone->group-pos tone)
          visual-class (intern-symbol group '- color group-pos)
          visual-key (intern-symbol visual-class '- octave))))

(describe (make-instances-from-midi 'visual-key 48 71))

;;;
;;; pitch-sets
;;;
(defclass pitch-sets (tone-octave)
  (pitch-class-set
   pitch-set
   (pitch-classes-set :initarg :pitch-classes-set))
  (:documentation ""))

(defconstant *sharp* (code-char #x266F))
(defconstant *flat* (code-char #x266D))
(defun sharp (str) (format nil "~A~A" str *sharp*))
(defun flat (str) (format nil "~A~A" str *flat*))

(defclass scientific-pitch-sets (pitch-sets)
  ()
  (:documentation "")
  (:default-initargs
   :pitch-classes-set
   `(("C") (,(sharp "C") ,(flat "D")) ("D") (,(sharp "D") ,(flat "E")) ("E")
     ("F") (,(sharp "F") ,(flat "G")) ("G") (,(sharp "G") ,(flat "A")) ("A") 
     (,(sharp "A") ,(flat "B")) ("B"))))

(defclass lilypond-pitch-sets (pitch-sets)
  ()
  (:documentation "")
  (:default-initargs
   :pitch-classes-set
   '(("c") ("cis" "des") ("d") ("dis" "ees") ("e")
     ("f") ("fis" "ges") ("g") ("gis" "aes") ("a") ("ais" "bes") ("b"))))

(defclass symbol-pitch-sets (lilypond-pitch-sets)
  ()
  (:documentation ""))

(defmethod print-object ((object pitch-sets) (stream stream))
  (print-unreadable-object (object stream :type t)
    (with-slots (midi pitch-class-set pitch-set) object
      (format stream "~S ~S ~S" midi pitch-class-set pitch-set))))

(defun tone->pitch-class-set (tone pitch-classes-set)
  (nth (- tone 1) pitch-classes-set))

(defmethod make-pitch-class-set ((objet pitch-sets))
  (with-slots (tone pitch-classes-set) objet
    (tone->pitch-class-set tone pitch-classes-set)))

(defmethod make-pitch-class-set ((objet symbol-pitch-sets))
  (with-slots (tone pitch-classes-set) objet
    (tone->pitch-class-set 
     tone
     (mapcar 
      (lambda (set) 
        (mapcar #'intern-symbol set))
      pitch-classes-set))))

(defmethod make-pitch-set ((object scientific-pitch-sets))
  (with-slots (octave pitch-class-set) object
    (mapcar (lambda (pitch-class)
              (format nil "~A~A" pitch-class octave))
            pitch-class-set)))

(defmethod make-pitch-set ((object symbol-pitch-sets))
  (with-slots (octave pitch-class-set) object
    (mapcar (lambda (pitch-class)
              (intern-symbol pitch-class octave))
            pitch-class-set)))

(defmethod make-pitch-set ((object lilypond-pitch-sets))
  (with-slots (octave pitch-class-set) object
    (mapcar (lambda (pitch-class)
              (concatenate 'string pitch-class
                 (cond ((eq octave 4) "")
                       ((< octave 4)
                        (make-string (- 4 octave) :initial-element #\,))
                       ((> octave 4)
                        (make-string (- octave 4) :initial-element #\')))))
            pitch-class-set)))

(defmethod initialize-instance :after ((object pitch-sets) &rest initargs &key &allow-other-keys)
  (with-slots (pitch-class-set pitch-set) object
    (setf pitch-class-set (make-pitch-class-set object)
          pitch-set (make-pitch-set object))))

(make-instances-from-midi 'scientific-pitch-sets 48 83)
(make-instances-from-midi 'lilypond-pitch-sets 48 83)
(make-instances-from-midi 'symbol-pitch-sets 48 83)


;;;
;;; piano-key
;;;
(defconstant *white-width* (cm->pixels 2.35))
(defconstant *white-height* (cm->pixels 15.0))
(defconstant *black-width* (cm->pixels 1.4))
(defconstant *black-height* (cm->pixels 10.0))

(defclass piano-key (visicon-object visual-key)
  ()
  (:default-initargs
   :kind 'piano-key
   :visloc-type 'piano-key-features
   :visobj-type 'piano-key
   :visual-features '(octave group group-pos)
   :visual-object-features '(visual-class visual-key))
  (:documentation "")) 

(defmethod initialize-instance :after ((object piano-key) &rest initargs &key &allow-other-keys)
  (with-slots (color width height) object
    (case color
      (w (setf width *white-width*
               height *white-height*))
      (b (setf width *black-width*
               height *black-height*)))))
  
(defun width-1/2 (key)
  (floor (/ (width key) 2)))

(defun height-3/4 (key)
  (floor (* (height key) 3/4)))

(defun adjust-x (key current-x)
  (case (color key)
    (w (setf (x key) (+ current-x (width-1/2 key))))
    (b (setf (x key) current-x))))

(defun adjust-y (key current-y)
  (setf (y key) (+ current-y (height-3/4 key))))

(defun adjust-x-y (current-x current-y keys)
  (dolist (key keys keys)
    (adjust-x key current-x)
    (adjust-y key current-y)
    (when (equal 'w (color key))
      (setf current-x 
            (+ current-x (width key))))))

(defun make-piano-key (key-class midi)
  (apply #'make-instance (list key-class :midi midi)))

(defun make-piano-keys (key-class current-x current-y start end)
  (do* ((midi start (1+ midi))
        (keys (list (make-piano-key key-class midi))
              (append keys (list (make-piano-key key-class midi)))))
       ((= midi end) (adjust-x-y current-x current-y keys))))

(describe (make-piano-keys 'piano-key 0 0 60 71))


(defun demo-visicon ()

  (echo-act-r-output)

  ;; define a model
  (clear-all)
  (define-model visual-piano
    ;; visual-location and object chunk-types definition
    (visual-location-chunk-type 'piano-key)
    (visual-object-chunk-type 'piano-key)
      ;(define-chunks square true)
    )

  ;; example for visicon methods
  (format t "ADD TO VISICON~%")

  (dolist (piano-key (make-piano-keys 'piano-key 0 0 21 108))
    (add-to-visicon piano-key))

  (run-n-events 3)
  (print-visicon)

  )


(defun run-example ()

  (echo-act-r-output)

  (reset)

  (dolist (piano-key (make-piano-keys 'piano-key 0 0 21 108))
    (add-to-visicon piano-key))

  (run-n-events 5)

  (define-chunks-fct '((goal isa find-key step find-a-key octave 4 group b2 group-pos 1 color w)))

  ;; find middle-c
  (goal-focus-fct 'goal)
  (run 3)

  ;; find next white to the right
  (mod-focus-fct '(step find-next-key-same-color-right))
  (run 3)

  ;; find next white to the left
  (mod-focus-fct '(step find-next-key-same-color-left))
  (run 3)

  ;; find same key in next octave
  (mod-focus-fct '(step find-same-key-in-next-octave))
  (run 3)

  )


(load-act-r-model 
 (make-pathname 
  :directory (pathname-directory (or *load-truename* *compile-file-pathname*))
  :type "lisp"
  :name "piano-keyboard-model"))



:eof

