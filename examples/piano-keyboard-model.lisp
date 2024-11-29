;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;
;;  54 keys
;;

(clear-all)

(define-model look-at-keys

  (sgp :auto-attend t)

  (visual-location-chunk-type 'piano-key)
  (visual-object-chunk-type 'piano-key)

  (chunk-type find-key step octave group group-pos color)
  
  (define-chunks 
   find-a-key
   find-next-key-same-color-right 
   find-next-key-same-color-left 
   find-same-key-in-next-octave
   say
   stop)
  
  (set-visloc-default screen-x lowest)

  (p find-a-key
     =goal>
     step find-a-key
     octave =octave
     group =group
     group-pos =position
     color =color

     ?visual>
     state free

     =visual-location>
     screen-x =x

     ==>
     =goal>
     step say

     +visual-location>
     octave =octave
     group =group
     group-pos =position
     color =color

     !output! (current-x =x)
     )

  (p find-next-key-same-color-right
     =goal>
     step find-next-key-same-color-right

     ?visual>
     state free

     =visual-location>
     screen-x =x
     color =color

     ==>
     =goal>
     step say


     +visual-location>
     color =color
     > screen-x =x
     :nearest =visual-location

     !output! (current-x =x)
     )

  (p find-next-key-same-color-left
     =goal>
     step find-next-key-same-color-left

     ?visual>
     state free

     =visual-location>
     screen-x =x
     color =color

     ==>
     =goal>
     step say


     +visual-location>
     color =color
     < screen-x =x
     :nearest =visual-location

     !output! (current-x =x)
     )

  (p find-same-key-in-next-octave
     =goal>
     step find-same-key-in-next-octave

     ?visual>
     state free

     =visual-location>
     screen-x =x
     color =color
     group =group
     group-pos =group-pos
     octave =octave

     ==>
     =goal>
     step say


     +visual-location>
     color =color
     group =group
     group-pos =group-pos
     > octave =octave
     :nearest =visual-location

     !output! (current-x =x)
     )


  (p say-key
     =goal>
     step say
 
     ?visual>
     state free
   
     =visual-location>
     screen-x =x

     =visual>
     visual-key =visual-key

     ==>
     =goal>
     step stop

     =visual-location>

     !output! (current-x =x)
     !output! (key =visual-key)

     )
  
  )

:eof