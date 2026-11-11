\ Child of test/arena.f: checked code can neither call nor tick the arena
\ runtime words, at top level, in a body or through a re-export, and a
\ package's own `field-project` is refused at its definition.
require src/lib/string.f

1024 constant CAP-ROOM
CAP-ROOM BUFFER: CAP-SRC
variable CAP-U
: CAP, ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 ?do a i + c@ CAP-SRC CAP-U @ + c! CAP-U @ 1 + CAP-U ! loop ;


TYPED-VARIABLE ROW-A ptr u8
variable ROW-U
: ROW-RUN ( -- ) ROW-A @ ROW-U @ INCLUDE-EVALUATE ;
: OWNER-ROW ( ptr u8 n ptr u8 n -- ) ROW-U ! ROW-A ! type [: ROW-RUN ;] catch . ;

\ A head the user declares is inert: it may be named, but no call or tick of
\ ARENA-NEW or ARENA-RELEASE, directly or through a re-export, certifies in
\ checked code. Z2, if it certified, would let an 8-byte record stand for any
\ type; TOP-CALL would run on a NULL head. Each row prints its refusal code;
\ SIG prints 0.
TYPED-VARIABLE HH DYNAMIC-STORAGE:arena-head
: CALL-NAME ( -- ptr u8 n ) s" x" ;
package ARENA-PX
public
EXPORT DYNAMIC-STORAGE:ARENA-RELEASE
;package

: CALL-ROWS ( -- )
   s" arena-sig: " s" : Y3 ( ptr DYNAMIC-STORAGE:arena-head -- ) drop ;" OWNER-ROW
   s" arena-new: " s" : Z2 ( -- ptr u8 ) HH 8 CALL-NAME DYNAMIC-STORAGE:ARENA-NEW ;" OWNER-ROW
   s" arena-release: " s" : Z3 ( -- ) HH DYNAMIC-STORAGE:ARENA-RELEASE ;" OWNER-ROW
   s" arena-null-new: " s" : Y1 ( -- ptr u8 ) NULL-PTR 8 CALL-NAME DYNAMIC-STORAGE:ARENA-NEW ;" OWNER-ROW
   s" arena-null-release: " s" : Y2 ( -- ) NULL-PTR DYNAMIC-STORAGE:ARENA-RELEASE ;" OWNER-ROW
   s" arena-tick: " s" : T1 ( -- ptr u8 ) NULL-PTR 8 CALL-NAME ['] DYNAMIC-STORAGE:ARENA-NEW execute ;" OWNER-ROW
   s" arena-quot: " s" : T3 ( -- ptr u8 ) NULL-PTR 8 CALL-NAME [: DYNAMIC-STORAGE:ARENA-NEW ;] execute ;" OWNER-ROW
   s" arena-top-tick: " s" ' DYNAMIC-STORAGE:ARENA-RELEASE drop" OWNER-ROW
   s" arena-top-call: " s" NULL-PTR DYNAMIC-STORAGE:ARENA-RELEASE" OWNER-ROW
   s" arena-export: " s" : E1 ( -- ) NULL-PTR ARENA-PX:ARENA-RELEASE ;" OWNER-ROW ;
CALL-ROWS

\ STRUCTURE's accessors spell `field-project` unqualified, so a sealed engine
\ refuses it as a definition name. Were STRUCT-CAPTURE's admitted, S2-Y's store
\ would land in V, printing 77 twice. The row prints its refusal code. The
\ package opens in this file, so the refused row leaves none open.
: LINE, ( ptr u8 n -- ) CAP, s"  " CAP, ;


: STRUCT-CAPTURE-SRC$ ( -- ptr u8 n )
   0 CAP-U !
   s" variable V" LINE,
   s" : field-project ( a n -- ptr n ) drop drop V ;" LINE,
   s" STRUCTURE s2 DERIVE addr FIELD x n FIELD y n ;STRUCTURE" LINE,
   s" TYPED-VARIABLE R s2" LINE,
   s" : RUN ( -- ) 77 R S2-Y !  V @ .  R S2-Y @ . ;  RUN" LINE,
   CAP-SRC CAP-U @ ;


: STRUCT-CAPTURE-ROW ( -- ) s" struct-capture: " STRUCT-CAPTURE-SRC$ OWNER-ROW ;
package STRUCT-CAPTURE
STRUCT-CAPTURE-ROW
;package
