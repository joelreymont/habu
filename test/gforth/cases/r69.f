\ A local declared in one MATCH arm is no name in the next (checked).
ENUM opt 0
   VARIANT none ;VARIANT
   VARIANT some FIELD x n ;VARIANT
;ENUM
: TAKE ( opt -- n )
   MATCH opt
      some OF {: v :} v 1 + ENDOF
      none OF v ENDOF
   ;MATCH ;
