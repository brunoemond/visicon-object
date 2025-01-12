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
;;; Filename    : visicon-objet.lisp
;;; Version     : 1.3.2 (2025-01-11)
;;; 
;;; Description : The **visicon-object class and methods** is a software module
;;;             : designed to work with the ACT-R cognitive architecture, 
;;;             : providing an interface for managing visual feature 
;;;             : representations of CLOS objects in the ACT-R visicon.
;;; 
;;; Bugs        : 
;;;
;;; To do       : A function that would define chunks for symbols created as slot 
;;;             ; values of visicon-object slots.
;;; 
;;; ----- History (reversed time order) -----
;;;
;;; 2025.01.11 Bruno  
;;;             : Version 1.3.2. Combining the calls visual-location-chunk-type and
;;;               visual-object-chunk-type into one: visicon-object-chunk-types (class-name)
;;;
;;; 2025.01.09 Bruno  
;;;             : Version 1.3.1. Solving issues with modification features list
;;;               having nil feature value and duplicate features.
;;;
;;; 2025.01.08 Bruno  
;;;             : Version 1.3. New device-objet class for independent device 
;;;               modelling and model visual perception through the visicon.
;;;
;;; 2024.12.04 Bruno  
;;;             : Version 1.2. Replaced when-let macro for better compatibility.
;;;
;;; 2024.12.02 Bruno  
;;;             : Version 1.1. Added a hash table to support motor interactions
;;;             : with visicon-objects using visual-location chunk names as a
;;;             ; reference (hash table key).
;;;
;;; 2024.11.29 Bruno  
;;;             : Version 1.0
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The software includes the visicon-object class which support the definition
;;; of custom objects that will be represented in the visicon. 
;;; 
;;; *** visicon methods ***
;;; (add-to-visicon visicon-object)
;;; (add-to-visicon list-of-visicon-objects)
;;; (modify-visicon visicon-object)
;;; (delete-from-visicon visicon-object)
;;; (delete-from-visicon list-of-visicon-objects)
;;;
;;; *** chunk-type definition methods ***
;;; (visual-location-chunk-type class-name)
;;; (visual-object-chunk-type class-name)
;;;
;;; (run-demo)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *actr-pixels/inch* 72
  "Value for the :pixels/inch parameter.")

(defparameter *actr-viewing-distance* 15
  "Value in inches for the :viewing-distance parameter.")

(defparameter *cm/inch* 2.45
  "There are 2.45 centimeters per inch.")

(defun cm->pixels (centimeters)
  "Convert cm to pixels."
  (floor (* (/ centimeters *cm/inch*) 
            *actr-pixels/inch*)))

(defclass visicon-object ()
  ((uid :initarg :uid :reader uid)
   (screen-x :initform 0 :initarg :screen-x)
   (screen-y :initform 0 :initarg :screen-y)
   (distance :initform nil :initarg :distance)
   (width :initform 1 :initarg :width :accessor width)
   (height :initform 1 :initarg :height :accessor height)
   (color :initform nil :initarg :color :accessor color)
   (value :initform nil :initarg :value :accessor value)
   (visloc-type :initform 'visual-location :initarg :visloc-type
                :documentation "The visual-location type used in the add-visicon-features command.")
   (visobj-type :initform 'visual-object :initarg :visobj-type
                :documentation "The visual-object type used in the add-visicon-features command.")
   (visual-features :initform nil :initarg :visual-features
                    :documentation "List of slots shared by visual-location and visual-object features.")
   (visual-location-features :initform nil :initarg :visual-location-features
                    :documentation "List of slots for visual-location features.")
   (visual-object-features :initform nil :initarg :visual-object-features
                    :documentation "List of slots for visual-object features.")
   (feature-id :initform nil :reader feature-id
               :documentation "The feature id returned by the add-visicon-features command.")
   (visual-location :initform nil :reader visual-location
                    :documentation "The name of the visual-location chunk for the visicon feature.")
   (screen-pos :initform nil)
   (kind :initform nil)
   (status :initform nil)
   (size :initform nil))
  (:documentation 
   "This class encapsulates information about visual locations and the attributes of visual objects.
It includes slots used by the follwoing ACT-R chunk types:
visual-location: (screen-x, screen-y, distance, kind, color, value, height, width, size)
visual-object: (screen-pos, value, status, color, height, width)
The distance attribute defaults to the value specified by the ACT-R :viewing-distance parameter if not explicitly provided when creating an object instance. This parameter is converted into pixels by ACT-R (e.g. 15×72=1080).
The attribute 'kind' is included in a visicon entry for an instance only if the 'value' attribute is not NIL. 
Attributes such as 'size' and 'status' are computed automatically by ACT-R at runtime."))

(defmethod initialize-instance :after ((object visicon-object) &rest initargs &key &allow-other-keys)
  (with-slots (screen-x screen-y distance) object
    (let ((x (getf initargs :x))
          (y (getf initargs :y))
          (z (getf initargs :z)))
      (when x (setf screen-x x))
      (when y (setf screen-y y))
      (when z (setf distance z)))))

(defmethod x ((object visicon-object))
  (with-slots (screen-x) object
    screen-x))

(defmethod (setf x) (value (object visicon-object))
  (with-slots (screen-x) object
    (setf screen-x value)))

(defmethod y ((object visicon-object))
  (with-slots (screen-y) object
    screen-y))

(defmethod (setf y) (value (object visicon-object))
  (with-slots (screen-y) object
    (setf screen-y value)))

(defmethod z ((object visicon-object))
  (with-slots (distance) object
    distance))

(defmethod (setf z) (value (object visicon-object))
  (with-slots (distance) object
    (setf distance value)))

(defun features-chunk-types (visicon-object)
  "The types to used for the add-visicon-features command."
  (with-slots (visloc-type visobj-type) visicon-object
    (if (equal 'visual-object visobj-type)
        `(isa ,visloc-type)
      `(isa (,visloc-type ,visobj-type)))))

(let ((vo1 (make-instance 'visicon-object))
      (vo2 (make-instance 'visicon-object :visloc-type 'vis-loc :visobj-type 'vis-obj)))
  (assert (equal '(ISA VISUAL-LOCATION)
                 (features-chunk-types vo1)))
  (assert (equal '(isa (vis-loc vis-obj))
                 (features-chunk-types vo2))))

;;;
;;; act-r feature functions interface
;;;
(defun append-features (features other-features)
  (let ((result features))
    (do* ((key-values other-features (cddr key-values))
          (key (first key-values) (first key-values)))
         ((null key) result)
      (setf result
            (if (member key result)
                result
              (append result (list key (second key-values))))))))

(let ((f1 '(a 1 b 2 c 3))
      (f2 '(b 5 d 6)))
  (assert (equal 
           '(A 1 B 2 C 3 D 6)
           (append-features f1 f2))))

(defun default-visual-location-features (visicon-object)
  "Default visual-location features."
  (with-slots (screen-x screen-y width height color value distance visobj-type) visicon-object
    (append `(screen-x ,screen-x)
            `(screen-y ,screen-y)
            `(width ,width)
            `(height ,height)
            (when value `(value (,visobj-type ,value)))
            (when color `(color ,color))          
            (when distance `(distance ,distance)))))

(let ((vo (make-instance 'visicon-object)))
  (assert (equal '(SCREEN-X 0 SCREEN-Y 0 WIDTH 1 HEIGHT 1)
                 (default-visual-location-features vo))))

(defun vo-visual-features (visicon-object)
  (with-slots (visual-features) visicon-object
    (let (features)
      (dolist (slot visual-features features)
        (setf features 
              (append features 
                      (when (slot-value visicon-object slot)
                        (list slot 
                              (slot-value visicon-object slot)))))))))

(defun visual-location-features (visicon-object)
  (with-slots (visual-location-features) visicon-object
    (let (features)
      (dolist (slot visual-location-features features)
        (setf features 
              (append features 
                      (when (slot-value visicon-object slot)
                        (list slot 
                            (list (slot-value visicon-object slot) nil)))))))))

(defun visual-object-features (visicon-object)
  (with-slots (visual-object-features) visicon-object
    (let (features)
      (dolist (slot visual-object-features features)
        (setf features 
              (append features 
                      (when (slot-value visicon-object slot)
                        (list slot 
                              (list nil (slot-value visicon-object slot))))))))))

(defun combined-features (visicon-object)
  (let ((combined-features (default-visual-location-features visicon-object)))
    (dolist (features (list (vo-visual-features visicon-object)
                            (visual-location-features visicon-object)
                            (visual-object-features visicon-object)) 
                      combined-features)
      (setf combined-features (append-features combined-features features)))))

(defun isa-features (visicon-object)
  (append (features-chunk-types visicon-object)
          (combined-features visicon-object)))

(defun modification-features-list (visicon-object)
  (append (list (feature-id visicon-object))
          (combined-features visicon-object)))

(let ((vo (make-instance 'visicon-object)))
  (assert (equal '(ISA VISUAL-LOCATION SCREEN-X 0 SCREEN-Y 0 WIDTH 1 HEIGHT 1)
                 (isa-features vo))))
;;;
;;; act-r chunk-type and chunk functions interface
;;;
(defun visual-location-type (class-name)
  (let ((class (find-class class-name)))
    (or 
     (cadadr (assoc :visloc-type 
                    (class-default-initargs class)))
     (cadr (slot-definition-initform
            (find 'visloc-type 
                  (class-slots (find-class class-name))
                  :key #'slot-definition-name))))))

(defun visual-object-type (class-name)
  (let ((class (find-class class-name nil)))
    (or 
     (cadadr (assoc :visobj-type 
                    (class-default-initargs class)))
     (cadr (slot-definition-initform
         (find 'visobj-type 
               (class-slots (find-class class-name))
               :key #'slot-definition-name))))))

(defun visual-chunk-type-slots (class-name)
  (let ((class (find-class class-name)))
    (when class
      (append (cadr (slot-definition-initform
                     (find 'visual-features (class-slots class)
                           :key #'slot-definition-name)))
              (cadadr (assoc :visual-features 
                             (class-default-initargs class)))))))

(defun visual-location-chunk-type-slots (class-name)
  (let ((class (find-class class-name)))
    (when class
      (append (cadr (slot-definition-initform
                     (find 'visual-location-features (class-slots class)
                           :key #'slot-definition-name)))
              (cadadr (assoc :visual-location-features 
                             (class-default-initargs class)))))))

(defun visual-object-chunk-type-slots (class-name)
  (let ((class (find-class class-name)))
    (when class
      (append (cadr (slot-definition-initform
                     (find 'visual-object-features (class-slots class)
                           :key #'slot-definition-name)))
              (cadadr (assoc :visual-object-features 
                             (class-default-initargs class)))))))

(defun visual-location-chunk-type-spec (class-name)
  (when (visual-location-type class-name)
    (append `((,(visual-location-type class-name)
               (:include visual-location)))
            (visual-chunk-type-slots class-name)
            (visual-location-chunk-type-slots class-name))))

(defun visual-object-chunk-type-spec (class-name)
  (when (visual-object-type class-name)
    (append `((,(visual-object-type class-name)
               (:include visual-object)))
            (visual-chunk-type-slots class-name)
            (visual-object-chunk-type-slots class-name))))

(progn 
  (defclass square (visicon-object)
    ((sides :initform 4 :reader sides)
     (regular :initform 'true :reader regular))
    (:default-initargs
     :visloc-type 'square-features
     :visobj-type 'square
     :visual-location-features '(regular)
     :visual-object-features '(sides)))
  (assert (equal 'visual-location (visual-location-type 'visicon-object)))
  (assert (equal 'visual-object (visual-object-type 'visicon-object)))
  (assert (equal 'square (visual-object-type 'square)))
  (assert (equal '((SQUARE-FEATURES (:INCLUDE VISUAL-LOCATION)) REGULAR)
                 (visual-location-chunk-type-spec 'square)))
  (assert (equal '((SQUARE (:INCLUDE VISUAL-OBJECT)) SIDES)
                 (visual-object-chunk-type-spec 'square)))
  (unintern 'square))

;;;
;;; visicon-object chunks
;;;
(defmethod has-slot-p ((object visicon-object) slot-name)
  (find slot-name
        (mapcar #'slot-definition-name
                (class-slots (class-of object)))
        :test #'eq))

(defmethod collect-symbol-values ((object visicon-object) (slot-name symbol))
  (when (and (has-slot-p object slot-name)
             (slot-boundp object slot-name))
    (let ((slot-value (slot-value object slot-name)))
      (when (and slot-value 
                 (symbolp slot-value))
        (list slot-value)))))

(defmethod collect-symbol-values ((object visicon-object) (slot-names list))
  (let (chunk-names)
    (dolist (slot-name slot-names chunk-names)
      (setf chunk-names
            (append chunk-names (collect-symbol-values object slot-name))))))

(defun visicon-object-chunks (visicon-object slot-names)
  (define-chunks-fct (collect-symbol-values visicon-object slot-names)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods to add, modify and delete a visicon entry.
;;; The methods provide an interface to ACT-R visual features functions. 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *visicon-objects* (make-hash-table)
  "A hash table to map visual-location names to visicon-object instances.")

(defun clear-visicon-objects ()
  (clrhash *visicon-objects*))

(defmethod get-visicon-object ((visual-location symbol))
  (gethash visual-location *visicon-objects*))

(defmethod (setf get-visicon-object) ((visicon-object visicon-object) (visual-location symbol))
  (setf (gethash visual-location *visicon-objects*)
        visicon-object))

(defmethod add-to-visicon ((anything t)) nil)

(defmethod add-to-visicon ((object visicon-object))
  (with-slots (feature-id visual-location) object
    (setf feature-id (car (add-visicon-features (isa-features object)))
          visual-location (chunk-visual-loc feature-id))
    (when visual-location
      (setf (get-visicon-object visual-location) object))))

(defmethod modify-visicon ((object visicon-object))
  (modify-visicon-features 
   (modification-features-list object)))

(defmethod delete-from-visicon ((object visicon-object))
  (with-slots (feature-id visual-location) object 
    (delete-visicon-features feature-id)
    (remhash visual-location *visicon-objects*)
    (setf feature-id nil
          visual-location nil)))

(defclass device-objects ()
  ((name :initarg :name)
   (device-objects :initarg :htable :initform (make-hash-table) :reader device-objects)
   (visicon-objects :initarg :index :initform *visicon-objects* :reader visicon-objects))
  (:documentation 
   "The class holds hash tables for device objects (not necessary in the visicon) and the same objects that
are in the visicon. This distinction allows to access device objects independly of the fact that they are
accessible from a visual-location chunk. This functionality separates device modeling from the cognitive model
interaction with a device."))

(defmethod print-object ((object device-objects) (stream stream))
  (with-slots (name) object
    (print-unreadable-object (object stream :type t)
      (format stream "~S" name))))

(defmethod get-device-object ((uid symbol) (object device-objects))
  (gethash uid (device-objects object)))

(defmethod (setf get-device-object) (value (uid symbol) (object device-objects))
  (setf (gethash uid (device-objects object)) value))

(defmethod add-to-visicon ((objects list))
  (dolist (object objects t)
    (add-to-visicon object)))

(defmethod delete-from-visicon ((objects list))
  (dolist (object objects t)
    (delete-from-visicon object)))

(defmethod add-to-visicon ((object device-objects))
  (maphash 
   (lambda (key value)
     (declare (ignore key))
     (add-to-visicon value))
   (device-objects object))
  object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods to define sub-types of visual-location and visual-object types.
;;; The methods provide an interface to ACT-R chunk-type function. 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun visual-location-chunk-type (class-name)
  (when (visual-location-type class-name)
    (apply #'chunk-type-fct
           (list (visual-location-chunk-type-spec class-name)))))

(defun visual-object-chunk-type (class-name)
  (when (visual-object-type class-name)
    (apply #'chunk-type-fct
           (list (visual-object-chunk-type-spec class-name)))))

(defun visicon-object-chunk-types (class-name)
  (visual-location-chunk-type class-name)
  (visual-object-chunk-type class-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;              
;; Define a class that inherits from visicon-object
(defclass square (visicon-object)
  ((sides :initform 4 :reader sides)
   (regular :initform 'true :reader regular))
  (:default-initargs
   :visloc-type 'square-features
   :visobj-type 'square
   :height 100
   :width 100
   :visual-location-features '(regular)
   :visual-object-features '(sides)))

(defmethod say-square ((object square))
    (format t "SQUARE~%"))

(defmethod say-square ((object symbol))
  (say-square (get-visicon-object object)))


(defun run-demo ()

  (clear-visicon-objects)

  ;; create instances
  (let ((vo1 (make-instance 'visicon-object))
        (vo2 (make-instance 'visicon-object :x 20 :y 20 :z (cm->pixels 40)))
        (square (make-instance 'square :x 30 :y 30)))

    (echo-act-r-output)

    ;; define a model
    (clear-all)
    (define-model test
      ;; visual-location and object chunk-types definition
      (visual-location-chunk-type 'square)
      (visual-object-chunk-type 'square)
      (define-chunks square true)
      ;(visicon-object-chunks square '(kind sides))
      )

    ;; example for visicon methods
    (format t "ADD TO VISICON~%")
    (add-to-visicon (list vo1 vo2 square))
    (run-n-events 3)
    (print-visicon)

    (format t "MODIFY VISICON~%")
    (setf (x vo1) 10)
    (modify-visicon vo1)
    (run-n-events 3)
    (setf (x vo1) 30)
    (modify-visicon vo1)
    (print-visicon)

    (format t "DELETE FROM VISICON~%")
    (delete-from-visicon vo2)
    (run-n-events 3)
    (print-visicon)

    ;; Action on a visicon-object using the 
    ;; visual-location as a key
    (say-square (visual-location square))


    (setf (x square) 10)
    (modify-visicon square)
    (run-n-events 3)

    (values vo1 vo2 square)

    ))

:eof