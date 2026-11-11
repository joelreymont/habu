\ A scope whose cleanup quotation reads the scope word's local, on return, on
\ throw, nested, recursive and in two tasks at once.
require src/lib/test.f
require src/lib/task.f

package QUOTATION-LOCALS-TEST
private

$100000007 constant E-BODY
-$100000009 constant E-CLEANUP
8 TYPED-BUFFER OBJECTS n
16 TYPED-BUFFER CLOSED n
variable CLOSES
variable SEEN
variable ARRIVED
TYPED-VARIABLE SEEN-A n
TYPED-VARIABLE SEEN-B n
TASK:MIN-STACK TASK:TASK WORKER-A
TASK:MIN-STACK TASK:TASK WORKER-B
defer PLAIN ( -- n )


\ An object is a cell of OBJECTS: 1 while entered, 0 once closed.
: OBJ ( n -- ptr n ) OBJECTS ;
: OBJ# ( ptr n -- n ) NULL-PTR - 0 OBJECTS NULL-PTR - - 8 / ;
: ENTER ( ptr n -- ) 1 swap ! ;


\ Appends under an atomic index, so two tasks closing at once both land.
: CLOSE ( ptr n -- )
   {: o:ptr :}
   0 o !
   o OBJ# 1 CLOSES atomic-add CLOSED ! ;


\ The closed objects' numbers as decimal digits behind a leading 9.
: ORDER ( -- n )
   9 CLOSES @ 0 ?do 10 * i CLOSED @ + loop ;


: SCOPE ( ptr n [ -- ] -- )
   {: o:ptr body :}
   o ENTER body [: o CLOSE ;] finally ;


: SCOPE-CLEANUP-THROWS ( ptr n [ -- ] -- )
   {: o:ptr body :}
   o ENTER body [: o CLOSE E-CLEANUP throw ;] finally ;


: ON-RETURN ( -- ) 1 OBJ [: 1 OBJ @ SEEN ! ;] SCOPE ;
: ON-THROW ( -- ) 2 OBJ [: E-BODY throw ;] SCOPE ;
: CLEANUP-THROWS ( -- ) 3 OBJ [: E-BODY throw ;] SCOPE-CLEANUP-THROWS ;
: NESTED ( -- ) 4 OBJ [: 5 OBJ [: 4 OBJ @ 5 OBJ @ + SEEN ! ;] SCOPE ;] SCOPE ;
: NESTED-THROWS ( -- ) 4 OBJ [: 5 OBJ [: E-BODY throw ;] SCOPE ;] SCOPE ;


\ Object k's scope holds object k-1's, so the innermost closes first.
: DESCEND ( n -- )
   {: k:n :}
   k OBJ [: k 0 > if k 1- RECURSE then ;] SCOPE ;


\ Each body waits inside its scope until both are inside, then reads its locals.
: MEET ( -- )
   1 ARRIVED atomic-add drop
   begin ARRIVED atomic@ 2 < while TASK:PAUSE repeat ;


: MET-SCOPE ( ptr n ptr n -- )
   {: o:ptr seen:ptr :}
   o [: MEET o OBJ# seen ! ;] SCOPE ;


: WORK-A ( -- ) 6 OBJ SEEN-A MET-SCOPE ;
: WORK-B ( -- ) 7 OBJ SEEN-B MET-SCOPE ;


: WAIT ( ptr n -- )
   {: t:ptr :}
   begin t TASK:DONE? 0= while TASK:PAUSE repeat
   t TASK:KILL ;


: TWO-TASKS ( -- )
   0 ARRIVED atomic!
   ['] WORK-A WORKER-A TASK:ACTIVATE
   ['] WORK-B WORKER-B TASK:ACTIVATE
   WORKER-A WAIT WORKER-B WAIT ;


\ A plain quotation kept in a defer, beside one that reads a local.
: BESIDE ( n -- n )
   {: a:n :}
   [: 123 ;] is PLAIN [: a 1+ ;] execute ;


: RUN ( -- )
   T-RESET
   0 CLOSES !
   ON-RETURN
   SEEN @ 1 T= 1 OBJ @ 0 T= ORDER 91 T=
   0 CLOSES !
   [: ON-THROW ;] E-BODY TTHROWSQ
   2 OBJ @ 0 T= ORDER 92 T=
   0 CLOSES !
   [: CLEANUP-THROWS ;] E-CLEANUP TTHROWSQ
   3 OBJ @ 0 T= ORDER 93 T=
   0 CLOSES !
   NESTED
   SEEN @ 2 T= 4 OBJ @ 0 T= 5 OBJ @ 0 T= ORDER 954 T=
   0 CLOSES !
   [: NESTED-THROWS ;] E-BODY TTHROWSQ
   4 OBJ @ 0 T= 5 OBJ @ 0 T= ORDER 954 T=
   0 CLOSES !
   3 DESCEND
   0 OBJ @ 0 T= 1 OBJ @ 0 T= 2 OBJ @ 0 T= 3 OBJ @ 0 T= ORDER 90123 T=
   0 CLOSES !
   TWO-TASKS
   6 OBJ @ 0 T= 7 OBJ @ 0 T= SEEN-A @ 6 T= SEEN-B @ 7 T= CLOSES @ 2 T=
   41 BESIDE 42 T= PLAIN 123 T=
   T-REPORT ;

RUN
;package
