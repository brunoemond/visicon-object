# visicon-object class and methods 
- Version 1.4.1 (2025/01/16)

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
;;; *** device methods ***
;;; (make-device (&key name class-name))
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
```

## Examples

- The visicon-object.lisp file defines a function to demonstrate the core functionality of the software.

```lisp
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
    (run-n-events 3)
    (print-visicon)

    (format t "DELETE FROM VISICON~%")
    (delete-from-visicon vo2)
    (run-n-events 3)
    (print-visicon)

    ))
```

- The examples/adjust-visicon-features-modified.lisp file contains annotated and adapted code for the visicon-object class and methods. The original code can be found at this logical pathname location: "ACT-R:examples;vision-module;adjust-visicon-features.lisp". 

- The examples folder has two files for a model and visicon-objects.