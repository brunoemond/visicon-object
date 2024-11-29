# visicon-object class and methods 
- Version 1.0 (2024/11/29)

The **visicon-object class and methods** is a software module designed to work with the ACT-R cognitive architecture, providing an interface for managing visual feature representations of CLOS objects in the ACT-R visicon. 

## Overview

The core component of the module is the `visicon-object` class, which encapsulates the properties and behaviors of individual visual objects for the visicon.

## Features

- **Add Visual Objects**
  - Create new instances in the visicon with specified attributes, such as shape, color, and position.
- **Modify Visual Objects**
  - Update the attributes of existing visual objects dynamically, supporting real-time adjustments.
- **Delete Visual Objects**
  - Remove specific objects from the visicon, reflecting environmental changes in the simulation.

## Purpose

The **visicon-object class and methods** simplifies interaction with ACT-R’s visual feature creation functions.

The software includes the visicon-object class which support the definition of custom objects that will be represented in the visicon. 
 
```lisp
   ;;; visicon methods
   (add-to-visicon visicon-object)
   (add-to-visicon list-of-visicon-objects)
   (modify-visicon visicon-object)
   (delete-from-visicon visicon-object)
   (delete-from-visicon list-of-visicon-objects)

   ;;; chunk-type definition methods ***
   (visual-location-chunk-type class-name)
   (visual-object-chunk-type class-name)
```

## Examples

- The visicon-object.lisp file defines a function to demonstrate the core functionality of the software.

```lisp
(defun run-demo ()

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
      (define-chunks square true))

    ;; example for visicon methods
    (format t "ADD TO VISICON~%")
    (add-to-visicon (list vo1 vo2 square))
    (run-n-events 3)
    (print-visicon)

    (format t "MODIFY VISICON~%")
    (setf (x vo1) 10)
    (modify-visicon vo1)
    (run-n-events 3)
    (print-visicon)

    (format t "DELETE FROM VISICON~%")
    (delete-from-visicon vo1)
    (run-n-events 3)
    (print-visicon)
    (unintern 'square)))
```

- The examples/adjust-visicon-features-modified.lisp file contains annotated and adapted code for the visicon-object class and methods. The original code can be found at this logical pathname location: "ACT-R:examples;vision-module;adjust-visicon-features.lisp". 