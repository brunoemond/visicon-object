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
;;; Version     : 1.4 (2025-01-15)
;;; 
;;; Description : The **visicon-object class and methods** is a software module
;;;             : designed to work with the ACT-R cognitive architecture, 
;;;             : providing an interface for managing visual feature 
;;;             : representations of CLOS objects in the ACT-R visicon.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History (reversed time order) -----
;;;
;;; 2025.01.15 Bruno  
;;;             : Version 1.4. Automatic chunk definitions for visicon-object slot values
;;;               when added to the visicon to avoid warnings about chunk creation when
;;;               running a model. Integration of a device instance to visicon-object instances.
;;;               Visicon-object that are added to the visicon are now stored in the device
;;;               instead of a the general parameter *visicon-objects*. The creation of a 
;;;               visicon object now requires a device object. 
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
;;; The software includes the device and visicon-object classes. The latter supports
;;; the definition of custom objects that will be represented in the visicon.
;;; 
;;; *** device methods ***
;;; (make-device (&optional name))
;;; 
;;; *** device-object methods ***
;;; (make-visicon-object (class device &rest initargs))
;;; (get-device-object uid device)
;;; (setf (get-device-object uid device) device-object)
;;;
;;; *** visicon methods ***
;;; (get-visicon-object visual-location device)
;;; (add-to-visicon visicon-object)
;;; (add-to-visicon list-of-visicon-objects)
;;; (modify-visicon visicon-object)
;;; (delete-from-visicon visicon-object)
;;; (delete-from-visicon list-of-visicon-objects)
;;;
;;; *** chunk-type definition methods ***
;;; (visicon-object-chunk-types class-name)
;;;
;;; *** demo ***
;;;
;;; (device-demo)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *visicon-objects* (make-hash-table)
  "A hash table to map visual-location names to visicon-object instances for all devices.")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; device 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass device ()
  ((name :initarg :name :initform 'some-device :type symbol :reader name)
   (device-objects :initform (make-hash-table) :reader device-objects)
   (objects-in-visicon :initform (make-hash-table) :reader objects-in-visicon))
  (:documentation 
   "The device class manages hash tables for device objects, whether or not they are included 
in the ACT-R visicon. It also maintains an index to these objects by their visual-location chunk, 
if they have been added to the visicon. By separating device modeling from the cognitive model's 
interaction with a device, this design streamlines the integration of device objects into 
cognitive simulations."))

(defmethod print-object ((device device) (stream stream))
  (with-slots (name device-objects objects-in-visicon) device
    (print-unreadable-object (device stream :type t)
      (format stream "~S :device-objects ~S :visicon ~S" name 
              (hash-table-count device-objects) 
              (hash-table-count objects-in-visicon)))))

(defun make-device (&optional (name 'some-device))
  (make-instance 'device :name name))

;;;
;;; device-objects
;;;
(defmethod get-device-object ((uid symbol) (device device))
  (gethash uid (device-objects device)))

(defun isa-visicon-object (object)
  (if (typep object 'visicon-object)
      t (error "Object ~S isa not a visicon-object." object)))

(defmethod (setf get-device-object) (visicon-object (uid symbol) (device device))
  (if (and (isa-visicon-object visicon-object) (equal (uid visicon-object) uid))
      (setf (gethash uid (device-objects device)) visicon-object)
    (error "uid value ~S and uid slot value ~S of object ~S are not equal." 
           uid (uid visicon-object) visicon-object)))

(defmethod (setf get-device-object) ((visicon-objects list) uid (device device))
  (declare (ignore uid))
  (dolist (visicon-object visicon-objects (device-objects device))
    (setf (get-device-object (uid visicon-object) device) visicon-object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; visicon-object 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass visicon-object ()
  ((uid :initarg :uid :type symbol :reader uid
        :documentation "A unique identifier for the visicon-object, used as the hash key.")
   (device :initarg :device :type devive :reader device
           :documentation "The device containing the visicon-object.")
   (device-name :type symbol :reader device-name
           :documentation "The device name computed wne initializing the object.")
   (visloc-type :initform 'visual-location :initarg :visloc-type
                :documentation "The visual-location type used in the act-r add-visicon-features command.")
   (visobj-type :initform 'visual-object :initarg :visobj-type
                :documentation "The visual-object type used in the act-r add-visicon-features command.")
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
   ;; Act-r visual-location and visual-object slots
   (screen-x :initform 0 :initarg :screen-x)
   (screen-y :initform 0 :initarg :screen-y)
   (distance :initform nil :initarg :distance)
   (width :initform 1 :initarg :width :accessor width)
   (height :initform 1 :initarg :height :accessor height)
   (color :initform nil :initarg :color :accessor color)
   (value :initform nil :initarg :value :accessor value)
   (screen-pos :initform nil)
   (kind :initform nil)
   (status :initform nil)
   (size :initform nil))
  (:documentation 
   "This class encapsulates information about visual locations and the attributes of visual objects.
It includes slots used by the follwoing ACT-R chunk types:
visual-location: (screen-x, screen-y, distance, kind, color, value, height, width, size)
visual-object: (screen-pos, value, status, color, height, width)
The distance attribute defaults to the value specified by the ACT-R :viewing-distance parameter if not
explicitly provided when creating an object instance. This parameter is converted into pixels by 
ACT-R (e.g. 15×72=1080).  Attributes such as 'screen-pos, 'size', 'status', and 'kind' are computed 
by ACT-R at runtime."))

(defun make-visicon-object (class device &rest initargs)
  (if (typep device 'device)
      (apply #'make-instance (append (list class :device device) initargs))
    (error "~S is not a device." device)))

(defmethod initialize-instance :after ((object visicon-object) &rest initargs &key &allow-other-keys)
  (with-slots (device device-name screen-x screen-y distance) object
    (let ((x (getf initargs :x))
          (y (getf initargs :y))
          (z (getf initargs :z)))
      (setf device-name (name device))
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

(let* ((device (make-device))
      (vo1 (make-visicon-object 'visicon-object device))
      (vo2 (make-visicon-object 'visicon-object device :visloc-type 'vis-loc :visobj-type 'vis-obj)))
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

(let ((vo (make-instance 'visicon-object :device (make-device))))
  (assert (equal '(SCREEN-X 0 SCREEN-Y 0 WIDTH 1 HEIGHT 1)
                 (default-visual-location-features vo))))

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

(defun visual-location-object-features (visicon-object)
  (with-slots (visual-features) visicon-object
    (let (features)
      (dolist (slot visual-features 
                    (append `(device ,(name (device visicon-object)))
                            features))
        (setf features 
              (append features 
                      (when (slot-value visicon-object slot)
                        (list slot 
                              (slot-value visicon-object slot)))))))))

(defun combined-features (visicon-object)
  (let ((combined-features (default-visual-location-features visicon-object)))
    (dolist (features (list (visual-location-object-features visicon-object)
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

(let ((vo (make-instance 'visicon-object :device (make-device))))
  (assert (equal '(ISA VISUAL-LOCATION SCREEN-X 0 SCREEN-Y 0 WIDTH 1 HEIGHT 1 DEVICE SOME-DEVICE)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The methods provide an interface to ACT-R chunk-type function. 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; chunk-types
;;;
(defun visual-location-chunk-type (class-name)
  (when (visual-location-type class-name)
    (apply #'chunk-type-fct
           (list (visual-location-chunk-type-spec class-name)))))

(defun visual-object-chunk-type (class-name)
  (when (visual-object-type class-name)
    (apply #'chunk-type-fct
           (list (visual-object-chunk-type-spec class-name)))))

(defun visicon-object-chunk-types-fct (class-name)
  (visual-location-chunk-type class-name)
  (visual-object-chunk-type class-name))

(defmacro visicon-object-chunk-types (class-name)
  `(visicon-object-chunk-types-fct ',class-name))

;;;
;;; chunks
;;;
(defun chunk-slots (visicon-object)
  (with-slots (visual-features visual-location-features 
                         visual-object-features) visicon-object
    (append '(device-name uid visloc-type visobj-type) visual-features visual-location-features 
            visual-object-features)))

(defun chunk-names (visicon-object)
  (let ((slots (chunk-slots visicon-object))
        chunk-names)
    (dolist (slot slots chunk-names)
      (when (and (slot-boundp visicon-object slot)
                 (symbolp (slot-value visicon-object slot)))
        (setf chunk-names
              (adjoin (slot-value visicon-object slot) chunk-names))))))
        
(defun define-visicon-object-chunks (visicon-object)
  (let (chunks)
    (dolist (chunk-name (chunk-names visicon-object) chunks)
      (unless (chunk-p-fct chunk-name)
        (setf chunks (append chunks (define-chunks-fct (list chunk-name))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; device: objects-in-visicon 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod get-visicon-object ((visual-location symbol) (device device))
  (gethash visual-location (objects-in-visicon device)))

(defmethod (setf get-visicon-object) ((visicon-object visicon-object) (visual-location symbol) (device device))
  (setf (gethash visual-location (objects-in-visicon device))
        visicon-object))

(defmethod add-to-visicon ((anything t)) nil)

(defmethod add-to-visicon ((visicon-object visicon-object))
  (define-visicon-object-chunks visicon-object)
  (with-slots (feature-id visual-location) visicon-object
    (setf feature-id (car (add-visicon-features (isa-features visicon-object)))
          visual-location (chunk-visual-loc feature-id))
    (when visual-location
      (setf (get-visicon-object visual-location (device visicon-object)) visicon-object))))

(defmethod add-to-visicon ((device device))
  (maphash 
   (lambda (uid visicon-object)
     (declare (ignore uid))
     (add-to-visicon visicon-object))
   (device-objects device))
  device)

(defmethod modify-visicon ((visicon-object visicon-object))
  (define-visicon-object-chunks visicon-object)
  (modify-visicon-features 
   (modification-features-list visicon-object)))

(defmethod delete-from-visicon ((visicon-object visicon-object))
  (with-slots (feature-id visual-location) visicon-object 
    (delete-visicon-features feature-id)
    (remhash visual-location (objects-in-visicon (device visicon-object)))
    (setf feature-id nil
          visual-location nil)))

(defmethod delete-from-visicon ((visicon-objects list))
  (dolist (visicon-object visicon-objects t)
    (delete-from-visicon visicon-object)))

(defmethod delete-from-visicon ((device device))
  (maphash 
   (lambda (visual-location visicon-object)
     (declare (ignore visual-location))
     (delete-from-visicon visicon-object))
   (objects-in-visicon device))
  device)

(defun clear-visicon-objects (device)
  (delete-from-visicon device))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;              
;; Define a class that inherits from visicon-object
(defparameter *device-demo*
  (make-device 'device-demo))

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

(defun add-objects-to-device-example (objects)
  (setf (get-device-object t *device-demo*) objects))

(add-objects-to-device-example
 (list (make-instance 'visicon-object :device *device-demo* :uid 'uid1) 
       (make-instance 'visicon-object :device *device-demo* :uid 'uid2)
       (make-instance 'square :device *device-demo* :uid 'square :x 30 :y 30)))

(defun device-demo ()

  (echo-act-r-output)

  (clear-visicon-objects *device-demo*)

  (clear-all)

  (define-model device-demo
    (visicon-object-chunk-types square))

  (let ((vo1 (get-device-object 'uid1 *device-demo*))
        (vo2 (get-device-object 'uid2 *device-demo*)))

    ;; example for visicon methods
    (format t "EMPTY VISICON~%")

    (print-visicon)

    (format t "ADD TO VISICON~%")
    (add-to-visicon *device-demo*)
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
    ))

:eof