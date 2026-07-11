\ task.f - checked CPU tasking over pthread.

s" lib/errors.f" required
s" lib/memory.f" required
s" lib/ffi.f" required

package TASK

$8 constant TASK-CELL
$10000 constant TASK-MIN-STACK
$10000 constant TASK-REGION-BYTES
$80 constant TASK-MUTEX-BYTES
0 constant TASK-FACILITY-OWNER-OFF
$8 constant TASK-FACILITY-MUTEX-OFF
TASK-FACILITY-MUTEX-OFF TASK-MUTEX-BYTES + constant TASK-FACILITY-BYTES

0 constant TASK-EMPTY
1 constant TASK-CONSTRUCTED
2 constant TASK-RUNNING
3 constant TASK-DONE
4 constant TASK-HALT-REQ

0 constant TCB.SIZE-OFF
$8 constant TCB.XT-OFF
$10 constant TCB.THREAD-OFF
$18 constant TCB.STACK-OFF
$20 constant TCB.STACK-U-OFF
$28 constant TCB.REGION-OFF
$30 constant TCB.REGION-U-OFF
$38 constant TCB.DBASE-OFF
$40 constant TCB.NDICT-OFF
$48 constant TCB.CP-OFF
$50 constant TCB.STATUS-OFF
$58 constant TCB.STOP-OFF
$60 constant TCB.RET-OFF
$68 constant TCB.USER-XT-OFF
$70 constant TASK-TCB-BYTES

BEGIN-STRUCTURE TASK-TCB-SIZE
   CELL +FIELD TCB.SIZE
   CELL +FIELD TCB.XT
   CELL +FIELD TCB.THREAD
   PTR-FIELD: TCB.STACK
   CELL +FIELD TCB.STACK-U
   PTR-FIELD: TCB.REGION
   CELL +FIELD TCB.REGION-U
   CELL +FIELD TCB.DBASE
   CELL +FIELD TCB.NDICT
   CELL +FIELD TCB.CP
   CELL +FIELD TCB.STATUS
   CELL +FIELD TCB.STOP
   CELL +FIELD TCB.RET
   CELL +FIELD TCB.USER-XT
END-STRUCTURE

: TASK-TCB-LAYOUT-CHECK ( -- )
   TASK-TCB-SIZE TASK-TCB-BYTES <> if s" task: tcb layout" E-TASK-STATE die then ;

TASK-TCB-LAYOUT-CHECK

create TASK-LIBC
   108 c, 105 c, 98 c, 99 c, 46 c, 115 c, 111 c, 46 c, 54 c, 0 c,
create TASK-LIBSYSTEM
   47 c, 117 c, 115 c, 114 c, 47 c, 108 c, 105 c, 98 c, 47 c,
   108 c, 105 c, 98 c, 83 c, 121 c, 115 c, 116 c, 101 c, 109 c,
   46 c, 66 c, 46 c, 100 c, 121 c, 108 c, 105 c, 98 c, 0 c,
create TASK-SYM-PTHREAD-CREATE
   112 c, 116 c, 104 c, 114 c, 101 c, 97 c, 100 c, 95 c, 99 c,
   114 c, 101 c, 97 c, 116 c, 101 c, 0 c,
create TASK-SYM-PTHREAD-JOIN
   112 c, 116 c, 104 c, 114 c, 101 c, 97 c, 100 c, 95 c, 106 c,
   111 c, 105 c, 110 c, 0 c,
create TASK-SYM-PTHREAD-EXIT
   112 c, 116 c, 104 c, 114 c, 101 c, 97 c, 100 c, 95 c, 101 c,
   120 c, 105 c, 116 c, 0 c,
create TASK-SYM-SCHED-YIELD
   115 c, 99 c, 104 c, 101 c, 100 c, 95 c, 121 c, 105 c, 101 c,
   108 c, 100 c, 0 c,
create TASK-SYM-PTHREAD-MUTEX-INIT
   112 c, 116 c, 104 c, 114 c, 101 c, 97 c, 100 c, 95 c, 109 c,
   117 c, 116 c, 101 c, 120 c, 95 c, 105 c, 110 c, 105 c, 116 c, 0 c,
create TASK-SYM-PTHREAD-MUTEX-LOCK
   112 c, 116 c, 104 c, 114 c, 101 c, 97 c, 100 c, 95 c, 109 c,
   117 c, 116 c, 101 c, 120 c, 95 c, 108 c, 111 c, 99 c, 107 c, 0 c,
create TASK-SYM-PTHREAD-MUTEX-UNLOCK
   112 c, 116 c, 104 c, 114 c, 101 c, 97 c, 100 c, 95 c, 109 c,
   117 c, 116 c, 101 c, 120 c, 95 c, 117 c, 110 c, 108 c, 111 c,
   99 c, 107 c, 0 c,
create TASK-SYM-MUNMAP
   109 c, 117 c, 110 c, 109 c, 97 c, 112 c, 0 c,

variable TASK-H
variable TASK-PTHREAD-CREATE
variable TASK-PTHREAD-JOIN
variable TASK-PTHREAD-EXIT
variable TASK-SCHED-YIELD
variable TASK-PTHREAD-MUTEX-INIT
variable TASK-PTHREAD-MUTEX-LOCK
variable TASK-PTHREAD-MUTEX-UNLOCK
variable TASK-MUNMAP
variable TASK-ENTRY
variable TASK-USER-NEXT

$40C8 constant TASK-USER-BASE   \ above UNCGH-CELL ($40C0), which sits above the grown protected-WID table ($3CC0..$40C0, src/habu/layout.f); bounded by DATA-START ($43C0)
TASK-USER-BASE TASK-USER-NEXT !

: TASK-NULL ( -- ptr a )
   NULL$ drop ;

TRUSTED: TASK-N>PTR ( n -- ptr a ) ;

TRUSTED: TASK-CELL>PTR-SLOT ( ptr a -- ptr ptr a ) ;

: TASK-ALIGN8 ( -- )
   here P>N 7 and dup 0= if drop exit then
   8 swap - allot ;

: TASK-LIB-PATH ( -- ptr u8 )
   HB-TARGET-MACOS? if TASK-LIBSYSTEM exit then
   TASK-LIBC ;

: TASK-OPEN ( -- )
   TASK-H @ 0 <> if exit then
   TASK-LIB-PATH RTLD-NOW DLOPEN dup 0= if E-TASK-DLOPEN throw then
   TASK-H ! ;

: TASK-SYM ( ptr u8 -- n ) {: name:ptr :}
   TASK-OPEN
   TASK-H @ name DLSYM dup 0= if E-TASK-DLSYM throw then ;

: TASK-CFUNS ( -- )
   TASK-PTHREAD-CREATE @ 0 <> if exit then
   TASK-SYM-PTHREAD-CREATE TASK-SYM TASK-PTHREAD-CREATE !
   TASK-SYM-PTHREAD-JOIN TASK-SYM TASK-PTHREAD-JOIN !
   TASK-SYM-PTHREAD-EXIT TASK-SYM TASK-PTHREAD-EXIT !
   TASK-SYM-SCHED-YIELD TASK-SYM TASK-SCHED-YIELD !
   TASK-SYM-PTHREAD-MUTEX-INIT TASK-SYM TASK-PTHREAD-MUTEX-INIT !
   TASK-SYM-PTHREAD-MUTEX-LOCK TASK-SYM TASK-PTHREAD-MUTEX-LOCK !
   TASK-SYM-PTHREAD-MUTEX-UNLOCK TASK-SYM TASK-PTHREAD-MUTEX-UNLOCK !
   TASK-SYM-MUNMAP TASK-SYM TASK-MUNMAP ! ;

: TASK-RC0 ( n -- )
   dup 0 <> if E-TASK-THREAD throw then
   drop ;

: TASK-CHECK-SIZE ( n -- )
   dup TASK-MIN-STACK < if E-TASK-SIZE throw then
   drop ;

: TASK-STATE@ ( ptr a -- n )
   TCB.STATUS @ ;

: TASK-STATE! ( n ptr a -- )
   TCB.STATUS ! ;

: TASK-LIVE+ ( -- )
   data-base TASKS-LIVE-CELL + dup @ 1 + swap ! ;

: TASK-LIVE- ( -- )
   data-base TASKS-LIVE-CELL + dup @ 1 - dup 0 < if drop 0 then swap ! ;

: TASK-MUNMAP-SPAN ( ptr a n -- )
   {: a:ptr cap:n :}
   a P>N cap TASK-MUNMAP @ CALL2 TASK-RC0 ;

: TASK-RELEASE-MEM ( ptr a -- ) {: tcb:ptr :}
   tcb TCB.STACK-U @ 0 <> if
      tcb TCB.STACK @ tcb TCB.STACK-U @ TASK-MUNMAP-SPAN
      TASK-NULL tcb TCB.STACK !
      0 tcb TCB.STACK-U !
   then
   tcb TCB.REGION-U @ 0 <> if
      tcb TCB.REGION @ tcb TCB.REGION-U @ TASK-MUNMAP-SPAN
      TASK-NULL tcb TCB.REGION !
      0 tcb TCB.REGION-U !
   then ;

: TASK-COPY-CELL ( ptr a ptr a n -- )
   {: src:ptr dst:ptr off:n :}
   src off + @ dst off + ! ;

: TASK-PTR-SLOT ( ptr a n -- ptr ptr a )
   + TASK-CELL>PTR-SLOT ;

: TASK-PTR! ( ptr a ptr a n -- )
   TASK-PTR-SLOT ! ;

: TASK-PTR@ ( ptr a n -- ptr a )
   TASK-PTR-SLOT @ ;

: TASK-REGION-INIT ( ptr a -- ) {: tcb:ptr :}
   tcb TCB.REGION @ {: reg:ptr :}
   data-base reg ARGC-CELL TASK-COPY-CELL
   data-base reg ARGV-CELL TASK-COPY-CELL
   data-base reg ENVP-CELL TASK-COPY-CELL
   rbase reg RBASE-CELL + !
   tcb TCB.STACK @ reg S0-CELL TASK-PTR!
   0 reg RSP-CELL + !
   0 reg LOOPSP-CELL + !
   0 reg LVD-CELL + !
   0 reg HND-CELL + !
   0 reg EVALD-CELL + !
   0 reg EVALERR-CELL + !
   tcb reg TASK-TCB-CELL TASK-PTR!
   1 reg TASKS-LIVE-CELL + ! ;

: TASK-CONSTRUCTED? ( ptr a -- bool )
   TASK-STATE@ TASK-EMPTY <> ;

: PREPARE ( ptr a -- ) {: tcb:ptr :}
   TASK-CFUNS
   tcb TASK-CONSTRUCTED? if exit then
   tcb TCB.SIZE @ TASK-CHECK-SIZE
   tcb TCB.SIZE @ MEM-ALLOC-64K-SPAN tcb TCB.STACK-U ! tcb TCB.STACK !
   TASK-REGION-BYTES 8 / >COUNT MEM-ALLOC-CELLS
   TASK-REGION-BYTES tcb TCB.REGION-U !
   tcb TCB.REGION !
   dbase@ tcb TCB.DBASE !
   ndict@ tcb TCB.NDICT !
   cp@ tcb TCB.CP !
   0 tcb TCB.STOP !
   tcb TASK-REGION-INIT
   TASK-CONSTRUCTED tcb TASK-STATE! ;

: TASK-PTHREAD-CREATE-RC ( ptr a -- n ) {: tcb:ptr :}
   tcb TCB.THREAD P>N
   0
   TASK-ENTRY @
   tcb P>N
   TASK-PTHREAD-CREATE @ CALL4 ;

: TASK-PTHREAD-JOIN-CALL ( ptr a -- ) {: tcb:ptr :}
   tcb TCB.THREAD @ tcb TCB.RET P>N TASK-PTHREAD-JOIN @ CALL2 TASK-RC0 ;

: TASK-ENTRY-NEEDED? ( -- bool )
   TASK-ENTRY @ 0= ;

: A64-LDRX ( n n n -- n ) {: rt:n rn:n off:n :}
   $F9400000 off 8 / 10 lshift or rn 5 lshift or rt or ;

: A64-STRX ( n n n -- n ) {: rt:n rn:n off:n :}
   $F9000000 off 8 / 10 lshift or rn 5 lshift or rt or ;

: A64-MOVZ ( n n -- n ) {: rd:n imm:n :}
   $D2800000 imm 5 lshift or rd or ;

: A64-BLR ( n -- n )
   5 lshift $D63F0000 or ;

TRUSTED: TASK-PATCH ( n n -- )           \ code-emission boundary: patch32 is a
   patch32 ;                             \ TRUSTED-ONLY capability prim (F3 gate)

: TASK-ENTRY-BUILD ( -- )
   TASK-ENTRY-NEEDED? 0= if exit then
   cp@ {: fn:n :}
   $A9BF7BFD                            fn       TASK-PATCH
   $A9BF53F3                            fn $4 +  TASK-PATCH
   $A9BF6FFA                            fn $8 +  TASK-PATCH
   $F81F0FFC                            fn $C +  TASK-PATCH
   9 0 TCB.XT-OFF A64-LDRX              fn $10 + TASK-PATCH
   19 0 TCB.STACK-OFF A64-LDRX          fn $14 + TASK-PATCH
   20 0 TCB.REGION-OFF A64-LDRX         fn $18 + TASK-PATCH
   26 0 TCB.DBASE-OFF A64-LDRX          fn $1C + TASK-PATCH
   27 0 TCB.NDICT-OFF A64-LDRX          fn $20 + TASK-PATCH
   28 0 TCB.CP-OFF A64-LDRX             fn $24 + TASK-PATCH
   0 20 TASK-TCB-CELL A64-STRX          fn $28 + TASK-PATCH
   10 TASK-RUNNING A64-MOVZ             fn $2C + TASK-PATCH
   10 0 TCB.STATUS-OFF A64-STRX         fn $30 + TASK-PATCH
   9 A64-BLR                            fn $34 + TASK-PATCH
   10 20 TASK-TCB-CELL A64-LDRX         fn $38 + TASK-PATCH
   11 TASK-DONE A64-MOVZ                fn $3C + TASK-PATCH
   11 10 TCB.STATUS-OFF A64-STRX        fn $40 + TASK-PATCH
   0 0 A64-MOVZ                         fn $44 + TASK-PATCH
   $F84107FC                            fn $48 + TASK-PATCH
   $A8C16FFA                            fn $4C + TASK-PATCH
   $A8C153F3                            fn $50 + TASK-PATCH
   $A8C17BFD                            fn $54 + TASK-PATCH
   $D65F03C0                            fn $58 + TASK-PATCH
   fn TASK-ENTRY !
   fn $5C + cp! ;

: TASK-READY ( -- )
   TASK-CFUNS
   TASK-ENTRY-BUILD ;

: TASK-JOIN-RELEASE ( ptr a -- ) {: tcb:ptr :}
   tcb TASK-PTHREAD-JOIN-CALL
   TASK-LIVE-
   tcb TASK-RELEASE-MEM
   TASK-EMPTY tcb TASK-STATE! ;

: TASK-SELF ( -- ptr a )
   data-base TASK-TCB-CELL + @ TASK-N>PTR ;

: TASK-SELF-N ( -- n )
   data-base TASK-TCB-CELL + @ ;

: TASK-RC>EXIT ( n -- n )
   $FF and ;

: TASK-RUNNER ( -- )
   TASK-SELF TCB.USER-XT @ catch dup 0= if drop exit then
   TASK-RC>EXIT s" task: unhandled throw" rot die ;

: ACTIVATE ( n ptr a -- ) {: xt:n tcb:ptr :}
   TASK-READY
   tcb TASK-STATE@ TASK-RUNNING = if E-TASK-STATE throw then
   tcb TASK-STATE@ TASK-HALT-REQ = if E-TASK-STATE throw then
   tcb TASK-STATE@ TASK-DONE = if tcb TASK-JOIN-RELEASE then
   tcb PREPARE
   xt tcb TCB.USER-XT !
   ['] TASK-RUNNER tcb TCB.XT !
   0 tcb TCB.STOP !
   TASK-RUNNING tcb TASK-STATE!
   TASK-LIVE+
   tcb TASK-PTHREAD-CREATE-RC dup 0 <> if
      TASK-LIVE-
      TASK-CONSTRUCTED tcb TASK-STATE!
      E-TASK-THREAD throw
   then
   drop ;

: TASK-STOP@ ( ptr a -- n )
   TCB.STOP @ ;

: TASK-STOP! ( n ptr a -- )
   TCB.STOP ! ;

: PAUSE ( -- )
   TASK-CFUNS
   TASK-SELF-N dup 0= if drop TASK-SCHED-YIELD @ CALL0 TASK-RC0 exit then
   TASK-N>PTR dup TASK-STOP@ 0 <> if
         TASK-DONE over TASK-STATE!
         0 TASK-PTHREAD-EXIT @ CALL1 drop
   then
   drop
   TASK-SCHED-YIELD @ CALL0 TASK-RC0 ;

: HALT ( ptr a -- )
   TASK-HALT-REQ over TASK-STATE!
   1 swap TASK-STOP! ;

: TASK-KILL ( ptr a -- ) {: tcb:ptr :}
   tcb TASK-STATE@ TASK-EMPTY = if exit then
   tcb TASK-STATE@ TASK-CONSTRUCTED = if
      tcb TASK-RELEASE-MEM
      TASK-EMPTY tcb TASK-STATE!
      exit
   then
   tcb TASK-STATE@ TASK-DONE = if
      tcb TASK-JOIN-RELEASE
      exit
   then
   tcb TASK-STATE@ TASK-RUNNING = tcb TASK-STATE@ TASK-HALT-REQ = or if
      tcb HALT
      tcb TASK-JOIN-RELEASE
   then ;

: TASK-DONE? ( ptr a -- bool )
   TASK-STATE@ TASK-DONE = ;

TRUSTED: TASK ( n -- )
   dup TASK-CHECK-SIZE
   TASK-ALIGN8
   create
      , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,
   does> ( -- ptr a ) ;

: #USER ( -- n )
   TASK-USER-NEXT @ dup 0= if drop TASK-USER-BASE then ;

: USER-ROOM ( n -- )
   DATA-START > if E-TASK-USER throw then ;

TRUSTED: +USER ( n n -- n )
   over over + dup USER-ROOM
   dup TASK-USER-NEXT !
   >r drop create , r>
   does> ( -- ptr a ) @ data-base + ;

: HIS ( ptr a ptr a -- ptr a ) {: tcb:ptr cur:ptr :}
   cur data-base - tcb TCB.REGION @ + ;

TRUSTED: FACILITY ( -- )
   TASK-ALIGN8
   create TASK-FACILITY-BYTES allot
   does> ( -- ptr a ) ;

: FACILITY-OWNER ( ptr a -- ptr a )
   TASK-FACILITY-OWNER-OFF + ;

: FACILITY-MUTEX ( ptr a -- ptr a )
   TASK-FACILITY-MUTEX-OFF + ;

: TASK-OWNER ( -- n )
   TASK-SELF-N dup 0= if drop data-base P>N then ;

: FACILITY-OWNER@ ( ptr a -- n )
   FACILITY-OWNER atomic@ ;

: FACILITY-OWNER! ( n ptr a -- )
   FACILITY-OWNER atomic! ;

: FACILITY-INIT ( ptr a -- )
   TASK-CFUNS
   0 over FACILITY-OWNER!
   FACILITY-MUTEX P>N 0 TASK-PTHREAD-MUTEX-INIT @ CALL2 TASK-RC0 ;

: GET ( ptr a -- ) {: f:ptr :}
   TASK-CFUNS
   TASK-OWNER {: owner:n :}
   f FACILITY-OWNER@ owner = if exit then
   f FACILITY-MUTEX P>N TASK-PTHREAD-MUTEX-LOCK @ CALL1 TASK-RC0
   owner f FACILITY-OWNER! ;

: RELEASE ( ptr a -- ) {: f:ptr :}
   TASK-CFUNS
   TASK-OWNER {: owner:n :}
   f FACILITY-OWNER@ owner <> if exit then
   0 f FACILITY-OWNER!
   f FACILITY-MUTEX P>N TASK-PTHREAD-MUTEX-UNLOCK @ CALL1 TASK-RC0 ;

public

TASK-MIN-STACK constant MIN-STACK

: TASK ( n -- )
   TASK ;

: PREPARE ( ptr a -- )
   PREPARE ;

: ACTIVATE ( n ptr a -- )
   ACTIVATE ;

: SELF ( -- ptr a )
   TASK-SELF ;

: SELF-N ( -- n )
   TASK-SELF-N ;

: PAUSE ( -- )
   PAUSE ;

: HALT ( ptr a -- )
   HALT ;

: KILL ( ptr a -- )
   TASK-KILL ;

: DONE? ( ptr a -- bool )
   TASK-DONE? ;

: #USER ( -- n )
   #USER ;

: +USER ( n n -- n )
   +USER ;

: HIS ( ptr a ptr a -- ptr a )
   HIS ;

: FACILITY ( -- )
   FACILITY ;

: FACILITY-INIT ( ptr a -- )
   FACILITY-INIT ;

: GET ( ptr a -- )
   GET ;

: RELEASE ( ptr a -- )
   RELEASE ;

end-package
