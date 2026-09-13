\ Loaded after native-window-owner-child.f replaces the checker from source.
package LOOP-OBLIGATION-TEST

\ This reduced prefix has the family registry, before the declaration driver.
s" choice" s" 0 VARIANT first ;VARIANT VARIANT second ;VARIANT" CHECKER-DEFSUM

: FIRST ( -- choice ) CONSTRUCT choice first ;
: SECOND ( -- choice ) CONSTRUCT choice second ;

: CHOICE-ID ( choice -- n )
   MATCH choice first OF 1 ENDOF second OF 2 ENDOF ;MATCH ;

: EQ! ( n n -- ) <> if 79 throw then ;

PTR-VARIABLE SOURCE-A
variable SOURCE-U

\ The generated definition has a dynamic effect; failures re-enter catch.
TRUSTED: LOAD-SOURCE ( -- )
   SOURCE-A @ SOURCE-U @ evaluate ;

: REJECT ( ptr u8 n -- )
   SOURCE-U ! SOURCE-A !
   [: LOAD-SOURCE ;] catch 70 EQ! ;

: DIRECT ( -- )
   3 0 do unloop exit loop ;

: GUARDED ( n -- n )
   3 0 ?do dup i = if unloop exit then loop ;

: NESTED ( n -- n )
   3 0 do
      4 0 do
         dup i = if unloop unloop exit then
      loop
   loop ;

: OUTER-INDEX ( -- n )
   0 4 0 do
      5 2 do unloop i + unloop exit loop
   loop ;

: OUTER-SECOND ( -- n )
   0 5 1 do
      4 2 do
         3 0 do unloop j + unloop unloop exit loop
      loop
   loop ;

: BOTH-ARMS ( bool -- bool )
   3 0 do dup if unloop else unloop then exit loop ;

: ELSE-EXIT ( n -- n )
   3 0 do dup i = if else unloop exit then loop ;

: BEGIN-EXIT ( n -- n )
   3 0 do begin unloop exit again loop ;

: REPEAT-EXIT ( n -- n )
   3 0 do begin dup 0 > while unloop exit repeat loop ;

: DIRECT-LEAVE ( n -- n )
   3 0 do leave loop ;

: EXIT-OR-LEAVE ( n -- n )
   3 0 do dup 0= if unloop exit else leave then loop ;

: CASE-DISCHARGE ( n -- n )
   5 1 do
      4 2 do
         case unloop 1 of endof endcase
         i unloop exit
      loop
   loop ;

: RETURN-ROW ( n -- n )
   >r 3 0 do r> unloop exit loop ;

: RETURN-PRESERVE ( | R -- | R )
   3 0 do unloop exit loop ;

: RETURN-CALL ( n -- n )
   >r RETURN-PRESERVE r> ;

: ZERO-TRIP ( n -- n )
   0 0 ?do unloop exit loop ;

: RETURN-SKIP ( n n -- n )
   swap >r 0 ?do r> unloop exit loop r> ;

: QUOT-EXIT ( n -- n )
   3 0 do [: dup 0 < if exit then 1+ ;] execute loop ;

: QUOT-LOOP ( -- n )
   [: 0 ;] 3 0 do drop [: 0 4 0 do i + loop ;] loop execute ;

: CASE-EXIT ( n -- n )
   3 0 do
      dup case
         1 of unloop exit endof
         2 of unloop exit endof
      endcase
   loop ;

: MATCH-EXIT ( choice -- choice )
   3 0 do
      dup MATCH choice
         first OF unloop exit ENDOF
         second OF ENDOF
      ;MATCH
   loop ;

: MATCH-BOTH ( choice -- choice )
   3 0 do
      dup MATCH choice
         first OF unloop ENDOF
         second OF unloop ENDOF
      ;MATCH
      exit
   loop ;

: MATCH-DEAD ( choice -- choice )
   3 0 do
      dup MATCH choice
         first OF unloop exit ENDOF
         second OF unloop exit ENDOF
      ;MATCH
   loop ;

variable ROUND

: RUNTIME ( -- )
   0 ROUND !
   128 0 do
      i ROUND @ EQ!
      DIRECT i ROUND @ EQ!
      i GUARDED i EQ!
      i NESTED i EQ!
      OUTER-INDEX 0 EQ!
      OUTER-SECOND 1 EQ!
      0 0= BOTH-ARMS 0= if 79 throw then
      0 1 = BOTH-ARMS if 79 throw then
      i ELSE-EXIT i EQ!
      i BEGIN-EXIT i EQ!
      i REPEAT-EXIT i EQ!
      i DIRECT-LEAVE i EQ!
      i EXIT-OR-LEAVE i EQ!
      1 CASE-DISCHARGE 1 EQ!
      2 CASE-DISCHARGE 1 EQ!
      i RETURN-ROW i EQ!
      i RETURN-CALL i EQ!
      i ZERO-TRIP i EQ!
      i 0 RETURN-SKIP i EQ!
      i 3 RETURN-SKIP i EQ!
      -1 QUOT-EXIT -1 EQ!
      5 QUOT-EXIT 8 EQ!
      QUOT-LOOP 6 EQ!
      1 CASE-EXIT 1 EQ!
      2 CASE-EXIT 2 EQ!
      3 CASE-EXIT 3 EQ!
      FIRST MATCH-EXIT CHOICE-ID 1 EQ!
      SECOND MATCH-EXIT CHOICE-ID 2 EQ!
      FIRST MATCH-BOTH CHOICE-ID 1 EQ!
      SECOND MATCH-BOTH CHOICE-ID 2 EQ!
      FIRST MATCH-DEAD CHOICE-ID 1 EQ!
      SECOND MATCH-DEAD CHOICE-ID 2 EQ!
      1 ROUND +!
   loop
   ROUND @ 128 EQ! ;

: REFUSALS ( -- )
   s" : LOB-BARE ( -- ) 3 0 do exit loop ;" REJECT
   s" : LOB-NONE ( -- ) unloop ;" REJECT
   s" : LOB-TWICE ( -- ) 3 0 do unloop unloop exit loop ;" REJECT
   s" : LOB-INNER ( -- ) 3 0 do 3 0 do unloop exit loop loop ;" REJECT
   s" : LOB-BACK ( -- ) 3 0 do unloop loop ;" REJECT
   s" : LOB-LEAVE ( -- ) 3 0 do unloop leave loop ;" REJECT
   s" : LOB-I ( -- ) 3 0 do unloop i drop exit loop ;" REJECT
   s" : LOB-J ( -- ) 3 0 do 3 0 do unloop j drop unloop exit loop loop ;" REJECT
   s" : LOB-QI ( -- n ) [: 0 ;] 3 0 do drop [: i ;] loop execute ;" REJECT
   s" : LOB-QJ ( -- ) 3 0 do 3 0 do [: j drop ;] drop loop loop ;" REJECT
   s" : LOB-QUNLOOP ( -- ) 3 0 do [: unloop ;] drop loop ;" REJECT
   s" : LOB-JOIN ( bool -- ) 3 0 do dup if unloop then loop drop ;" REJECT
   s" : LOB-ELSE ( bool -- ) 3 0 do dup if unloop else then loop drop ;" REJECT
   s" : LOB-BEGIN ( -- ) 3 0 do begin unloop 0 0= until loop ;" REJECT
   s" : LOB-AGAIN ( -- ) 3 0 do begin unloop again loop ;" REJECT
   s" : LOB-REPEAT ( -- ) 3 0 do begin 0 0= while unloop repeat loop ;" REJECT
   s" : LOB-CASE ( n -- ) 3 0 do dup case 1 of unloop endof endcase loop drop ;" REJECT
   s" : LOB-MATCH ( choice -- ) 3 0 do dup MATCH choice first OF unloop ENDOF second OF ENDOF ;MATCH loop drop ;" REJECT
   s" : LOB-RET ( | n -- | n ) 3 0 do r> drop unloop exit loop ;" REJECT ;

RUNTIME
s" loop-runtime: ok" type cr
REFUSALS
: AFTER-REFUSALS ( -- ) 3 0 do i drop loop ;
AFTER-REFUSALS
RUNTIME
;package
