;;; ORIGINAL TEXT
;;; This example demonstrates how visual features created using
;;; the add-visicon-features command can be modified and removed from
;;; the visicon, and that those changes automatically trigger an update
;;; by the vision module.


;;; MODIFIED CODE
;;; Definition of a device instance to hold visicon-objects. 
(defparameter *figures* 
  (make-instance 'device :name 'figures))

;;; A visicon-object class (see class :documentation)
(defclass polygon (visicon-object)
  ((regular :initarg :regular :accessor regular)
   (sides :initarg :sides :accessor sides))
  (:default-initargs
   :visloc-type 'polygon-feature
   :visobj-type 'polygon
   :visual-location-features '(regular)
   :visual-object-features '(sides))
  (:documentation 
   "The polygon class inherits from the visicon-object class. 
Additional slots (regular and sides) are added to match the original example.
The slots are specified as being either visual-location or visual-object features.
Also, the visual-location (:visloc-type) and object (:visobj-type) types will be used for the creation of visicon chunks."))

;;; Adding vision-objects to the *figures* device  
(setf (get-device-object t *figures*)
      (list (make-instance
             'polygon :x 0 :y 50
             :uid 'polygon1
             :device *figures*
             :value '(polygon "poly1")
             :height 20 :width 40
             :color 'blue :regular 'false
             :sides 7)
            (make-instance
             'polygon :x 50 :y 70
             :uid 'polygon2
             :device *figures*
             :value '(polygon "square")
             :height 20 :width 40
             :color 'red :regular 'true
             :sides 4)))


(defun run-example ()

  ;; MODIFIRED TEXT
  (echo-act-r-output)
  
  (reset)
  
  ;; ORIGINAL TEXT
  ;; Add-visicon-features returns a list of names which can be
  ;; used to update the features which were added to the visicon.
  ;; The modify-visicon-features command can be used to adjust
  ;; the location and object representation of an item that was
  ;; added to the visicon.  The delete-visicon-features and
  ;; delete-all-visicon-features commands can be used to remove
  ;; items which were added to the visicon.
  
  ;; ORIGINAL TEXT
  ;; For this example these chunk-types are defined in the model
  ;; which subclass the standard visual-location and visual-object
  ;; chunk-types as were used for the new-visicon-features example.
  
  ;; ORIGINAL TEXT
  ;; (chunk-type (polygon-feature (:include visual-location)) regular)
  ;; (chunk-type (polygon (:include visual-object)) sides)

  (let (#|
         ;; ORIGINAL CODE
         (features (add-visicon-features 
                        '(isa (polygon-feature polygon) screen-x 0 screen-y 50
                          value (polygon "poly1") height 20 width 40 
                          color blue regular (false nil) 
                          sides (nil 7))
                        '(isa (polygon-feature polygon) screen-x 50 screen-y 70 
                          value (polygon "square") height 30 width 30 
                          color red regular (true nil) 
                          sides (nil 4))))
         |#
        ;; MODIFIED CODE
        ;; Instead of creating object features that are independent of lisp objects.
        ;; Instances that inherit from the visicon-object class are can be modified 
        ;; as lisp objects. These objects are associated with a device, in the current
        ;; case the *figures* device.
        (polygon1 (get-device-object 'polygon1 *figures*))
        (polygon2 (get-device-object 'polygon1 *figures*)))

    ;; MODIFIED CODE
    ;; Adding all device-objects to the act-r visicon
    (add-to-visicon *figures*)

    ;; ORIGINAL TEXT AND CODE
    ;; Give the vision module a chance to process the display
    ;; before printing the visicon.
    (run-n-events 3)
    (print-visicon)

    ;; ORIGINAL TEXT AND CODE
    ;; run the model to show the current chunks
    (run 10)
 
    ;; ORIGINAL TEXT
    ;; Modify the first of those features to change the color to green,
    ;; adjust the x position, and change the number of sides to 8.
    ;; The parameters for modify-visicon-features are very similar to
    ;; add except that the list of features must start with the name
    ;; of the feature to update which was returned from add-visicon-features.
    ;; One thing to note is that because nil is used in the list of values to
    ;; indicate that a slot should not be applied to the location or object 
    ;; means that you can't use nil to 'remove' a slot from a feature through
    ;; a modification.

    ;; ORIGINAL CODE
    ;; (modify-visicon-features (list (first features) 'color 'green 'screen-x 5 'sides '(nil 8)))
   
    ;; MODIFIED CODE
    ;; The source lisp object is modified, then its representation in the visicon is updated.
    (setf (color polygon1) 'green
          (x polygon1) 5
          (sides polygon1) 8)

    (modify-visicon polygon1)

    ;; ORIGINAL TEXT AND CODE
    ;; Give the vision module a chance to process the display
    ;; before printing the visicon.
    (run-n-events 3)
    (print-visicon)
    
    ;; ORIGINAL TEXT AND CODE
    ;; run the model to show the updated
    (run 10)
        
    ;; ORIGINAL TEXT
    ;; Modify the second of those features to change the regular value to false and the
    ;; height to 25
    
    ;; ORIGINAL CODE
    ;; (modify-visicon-features (list (second features) 'height 25 'regular '(false nil)))

    ;; MODIFIED CODE
    ;; The source lisp object is modified, then its representation in the visicon is updated.
    (setf (height polygon2) 25
          (regular polygon2) 'false)

    (modify-visicon polygon2)
    
    ;; ORIGINAL TEXT AND CODE
    ;; Give the vision module a chance to process the display
    ;; before printing the visicon.
    (run-n-events 3)
    (print-visicon)
    
    ;; ORIGINAL TEXT AND CODE
    ;; run the model to show the updated
    (run 10)
    
    ;; ORIGINAL TEXT
    ;; delete the second feature.  delete-visicon-features takes any number
    ;; of parameters each of which should be the name of a feature that was
    ;; returned by add-visicon-feature and those features are removed from
    ;; the visicon.
    
    ;; ORIGINAL CODE
    ;; (delete-visicon-features (second features))

    ;; MODIFIED CODE
    ;; The deletion refers to the source lisp object. The deletion removes
    ;; the feature from the visicon, and set the feature-id slot of the 
    ;; lisp object to nil.
    (delete-from-visicon polygon2)
    
    ;; ORIGINAL TEXT AND CODE
    ;; Give the vision module a chance to process the display
    ;; before printing the visicon.
    (run-n-events 3)
    (print-visicon)
    
    ;; ORIGINAL TEXT AND CODE
    ;; run the model to show the updated
    (run 10)
    
    ;; ORIGINAL TEXT
    ;; delete all of the visicon features.  delete-all-visicon-features
    ;; removes all of the features from the visicon.
    ;; (delete-all-visicon-features)

    ;; MODIFIED CODE
    (delete-from-visicon *figures*)
    
    ;; ORIGINAL TEXT AND CODE
    ;; Give the vision module a chance to process the display
    ;; before printing the visicon.
    (run-n-events 3)
    (print-visicon)
    
    ;; ORIGINAL TEXT AND CODE
    ;; run the model to show the updated
    (run 10)))
  
  

;;; The model is very simple in that it just repeatedly finds
;;; a location and then attends to the item there printing
;;; out the chunks in the visual-location and visual buffers
;;; after the corresponding requet completes.
;;; The ordering of the modifications are such that it will
;;; not automatically updated the attended object chunks 
;;; since the currently attended item is unchanged in each
;;; case.

;; (load-act-r-model "ACT-R:examples;vision-module;adjust-visicon-features-model.lisp")

(load-act-r-model 
 (make-pathname 
  :directory (pathname-directory (or *load-pathname* *compile-file-pathname*))
  :type "lisp"
  :name "adjust-visicon-features-modified-model"))

