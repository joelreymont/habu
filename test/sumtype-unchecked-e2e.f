\ A declaration that generates constructor words is refused while checking is
\ suspended (`0 set-check`): the checker certifies those words, so a SUMTYPE,
\ PRODUCT or STRUCTURE met there exits 70 with the standard declaration line
\ before anything registers. Unrefused, a product died 76 ("checker: incomplete
\ generated constructor plan") and a sum's constructors carried rows without
\ authority, which a later checked caller refused. A NEWTYPE generates no word
\ and is admitted inside the window; a closed `check@ 0 set-check … set-check`
\ window leaves the declarations after it ordinary. A multi-error load counts
\ each refusal and leaves no state behind for the next declaration.
\ bin/hb --load test/sumtype-unchecked-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/process.f
require src/lib/test/outcome.f
require src/lib/test/subject.f

package SUMTYPE-UNCHECKED-TEST

$1000 constant CAP
$2000 constant PROG-CAP
20000 constant CHILD-MS
create PROG PROG-CAP allot
variable PROG-U
create OUT CAP allot
create ERR CAP allot

: PROG+ ( ptr u8 n -- )
   PROG PROG-CAP PROG-U BUF-APPEND
   10 PROG PROG-CAP PROG-U BUF-APPEND-C ;

: PROG$ ( -- ptr u8 n ) PROG PROG-U @ ;

: SUM-OFF$ ( -- ptr u8 n )
   0 PROG-U !
   s" 0 set-check" PROG+
   s" SUMTYPE suoff VARIANT one ;VARIANT ;SUMTYPE" PROG+
   PROG$ ;

: SUM-REFUSAL$ ( -- ptr u8 n )
   s" habu: bad sumtype declaration 'suoff': checking is suspended; declare it where checking is on at 'suoff'" ;

: PRODUCT-OFF$ ( -- ptr u8 n )
   0 PROG-U !
   s" 0 set-check" PROG+
   s" PRODUCT suprod FIELD x n FIELD y n ;PRODUCT" PROG+
   PROG$ ;

: PRODUCT-REFUSAL$ ( -- ptr u8 n )
   s" habu: bad product declaration 'suprod': checking is suspended; declare it where checking is on at 'suprod'" ;

: STRUCTURE-OFF$ ( -- ptr u8 n )
   0 PROG-U !
   s" 0 set-check" PROG+
   s" STRUCTURE surow FIELD first n ;STRUCTURE" PROG+
   PROG$ ;

: STRUCTURE-REFUSAL$ ( -- ptr u8 n )
   s" habu: bad structure declaration 'surow': checking is suspended; declare it where checking is on at 'surow'" ;

\ A NEWTYPE inside the window, then a sum declared after it closes; checked
\ words construct, match and cast their values.
: AFTER-WINDOW$ ( -- ptr u8 n )
   0 PROG-U !
   s" package SU-NEW" PROG+
   s" check@ 0 set-check" PROG+
   s" variable SU-RAW" PROG+
   s" NEWTYPE sunew" PROG+
   s" set-check" PROG+
   s" private" PROG+
   s" CAST: >SUNEW ( n -- sunew )" PROG+
   s" CAST: SUNEW> ( sunew -- n )" PROG+
   s" public" PROG+
   s" : SU-NEW-RUN ( -- ) 5 >SUNEW SUNEW> . ;" PROG+
   s" ;package" PROG+
   s" SUMTYPE sushape VARIANT dot ;VARIANT VARIANT box n ;VARIANT ;SUMTYPE" PROG+
   s" : SU-VALUE ( sushape -- n ) MATCH sushape dot OF 0 ENDOF box OF ENDOF ;MATCH ;" PROG+
   s" : SU-RUN ( -- ) 7 SUSHAPE:BOX SU-VALUE . SUSHAPE:DOT SU-VALUE . ;" PROG+
   s" SU-RUN SU-NEW:SU-NEW-RUN" PROG+
   PROG$ ;

: WANT$ ( -- ptr u8 n ) s\" 7\n0\n5\n" ;


\ Under a multi-error load a PRODUCT, a STRUCTURE and a sum whose body passes
\ the 4096-byte declaration buffer are each refused inside the window, then the
\ same names and a replayed sum declare with checking on. The over-long body
\ caps its collection buffer and flags it; the oversize sum comes last and the
\ replay right after the window, because only a replay entry meets that flag
\ without a collector clearing it first. Left set, the flag refused the replay
\ as `declaration too long`, rc 70.
: MULTI$ ( -- ptr u8 n )
   0 PROG-U !
   s" -1 MULTI-ERR !  0 MULTI-ERR-N !" PROG+
   s" check@ 0 set-check" PROG+
   s" PRODUCT suprod FIELD x n FIELD y n ;PRODUCT" PROG+
   s" STRUCTURE surow FIELD first n ;STRUCTURE" PROG+
   s" SUMTYPE subig " PROG PROG-CAP PROG-U BUF-APPEND
   200 0 ?do s" VARIANT vbig ;VARIANT " PROG PROG-CAP PROG-U BUF-APPEND loop
   s" ;SUMTYPE" PROG+
   s" set-check" PROG+
   s" MULTI-ERR-N @ .  0 MULTI-ERR !" PROG+
   s\" s\" oksum\" s\" VARIANT one ;VARIANT\" CHECKER-DEFSUM" PROG+
   s" SUMTYPE subig VARIANT tiny n ;VARIANT ;SUMTYPE" PROG+
   s" PRODUCT suprod FIELD x n FIELD y n ;PRODUCT" PROG+
   s" STRUCTURE surow FIELD first n ;STRUCTURE" PROG+
   s" : SU-BIG ( subig -- n ) MATCH subig tiny OF ENDOF ;MATCH ;" PROG+
   s" : SU-MULTI-RUN ( -- ) 4 SUBIG:TINY SU-BIG . 3 9 SUPROD:MAKE SUPROD:UNMAKE + . 6 SUROW:MAKE SUROW:UNMAKE . ;" PROG+
   s" SU-MULTI-RUN" PROG+
   PROG$ ;

: MULTI-WANT$ ( -- ptr u8 n ) s\" 3\n4\n12\n6\n" ;

: SUBJECT ( ptr u8 n -- len len outcome ) {: src:ptr srcu:n :}
   src srcu OUT CAP >LEN ERR CAP >LEN CHILD-MS >MS SUBJECT:RUN ;

: SUBJECT-REFUSED ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu:n wa:ptr wu:n :}
   src srcu SUBJECT {: outu:len erru:len oc :}
   src srcu OUT outu LEN>N ERR erru LEN>N oc 70 T-OUTCOME-EXITED=
   ERR erru LEN>N wa wu CONTAINS? TTRUE ;

: SUBJECT-PRINTS ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu:n wa:ptr wu:n :}
   src srcu SUBJECT {: outu:len erru:len oc :}
   src srcu OUT outu LEN>N ERR erru LEN>N oc 0 T-OUTCOME-EXITED=
   OUT outu LEN>N wa wu T$= ;

public

: RUN ( -- )
   T-RESET
   s" a SUMTYPE under 0 set-check is refused" T-LABEL
   SUM-OFF$ SUM-REFUSAL$ SUBJECT-REFUSED
   s" a PRODUCT under 0 set-check is refused" T-LABEL
   PRODUCT-OFF$ PRODUCT-REFUSAL$ SUBJECT-REFUSED
   s" a STRUCTURE under 0 set-check is refused" T-LABEL
   STRUCTURE-OFF$ STRUCTURE-REFUSAL$ SUBJECT-REFUSED
   s" a NEWTYPE under 0 set-check and a sum after the window certify and run" T-LABEL
   AFTER-WINDOW$ WANT$ SUBJECT-PRINTS
   s" a multi-error load refuses each declaration and leaves no state behind" T-LABEL
   MULTI$ MULTI-WANT$ SUBJECT-PRINTS
   T-REPORT ;

;package

SUMTYPE-UNCHECKED-TEST:RUN
