\ task.f - checked CPU tasking over pthread.

s" lib/errors.f" required
s" lib/memory.f" required
s" lib/ffi-abi.f" required
s" lib/image-lifecycle.f" required
s" lib/codegen.f" required        \ +USER builds its generated accessor with CODEGEN's buffer

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
   CELL +FIELD TCB.XT-CELL
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
   CELL +FIELD TCB.USER-XT-CELL
END-STRUCTURE

: TASK-TCB-LAYOUT-CHECK ( -- )
   TASK-TCB-SIZE TASK-TCB-BYTES <> if s" task: tcb layout" E-TASK-STATE die then ;

TASK-TCB-LAYOUT-CHECK

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

variable TASK-ENTRY
variable TASK-USER-NEXT

FFI:SCRATCH-END constant TASK-USER-BASE
TASK-USER-BASE TASK-USER-NEXT !

: TASK-NULL ( -- ptr n )
   NULL$ drop CELL-VIEW ;

\ TCB raw-cell pointer refinement and pointer-slot reinterpretation are outside
\ checker inference. Retirement owner: habu-typed-defining-words-aa224eb5.
TRUSTED: TASK-N>PTR ( n -- ptr n ) ;

TRUSTED: TASK-CELL>PTR-SLOT ( ptr n -- ptr ptr n ) ;

\ These two cells are quotation slots in the fixed pthread entry ABI. The
\ low-level structure definer describes offsets; every callback store and load
\ below uses its declared empty stack effect.
TRUSTED: TASK-CELL>XT-SLOT ( ptr n -- ptr [ -- ] ) ;

: TCB.XT ( ptr n -- ptr [ -- ] )
   TCB.XT-CELL TASK-CELL>XT-SLOT ;

: TCB.USER-XT ( ptr n -- ptr [ -- ] )
   TCB.USER-XT-CELL TASK-CELL>XT-SLOT ;

: TASK-ALIGN8 ( -- )
   here FFI:>CELL 7 and dup 0= if drop exit then
   8 swap - allot ;

variable MUNMAP-XT
variable PTHREAD-CREATE-XT
variable PTHREAD-JOIN-XT
variable PTHREAD-EXIT-XT
variable SCHED-YIELD-XT
variable MUTEX-INIT-XT
variable MUTEX-LOCK-XT
variable MUTEX-UNLOCK-XT
variable SYMBOLS-REGISTERED
TASK-ALIGN8
variable SYMBOLS-READY

\ These symbols are borrowed from the process's libc/libSystem dependency.
\ RTLD_DEFAULT is zero on Linux and -2 on macOS; no dlopen reference is owned.
: TASK-SYM ( ptr u8 -- n ) {: name:ptr :}
   HB-TARGET-MACOS? if -2 else 0 then
   name FFI:DLSYM dup 0= if E-TASK-DLSYM throw then ;

\ Capture is quiescent. Both foreign addresses and the generated pthread entry
\ belong to this process; the next task operation constructs them afresh.
: RESET-SYMBOLS ( -- )
   0 MUNMAP-XT ! 0 PTHREAD-CREATE-XT ! 0 PTHREAD-JOIN-XT !
   0 PTHREAD-EXIT-XT ! 0 SCHED-YIELD-XT ! 0 MUTEX-INIT-XT !
   0 MUTEX-LOCK-XT ! 0 MUTEX-UNLOCK-XT ! 0 TASK-ENTRY !
   0 SYMBOLS-REGISTERED ! 0 SYMBOLS-READY atomic! ;

: LOAD-SYMBOLS ( -- )
   SYMBOLS-REGISTERED @ 0= if
      [: RESET-SYMBOLS ;] IMAGE-LIFECYCLE:REGISTER
      1 SYMBOLS-REGISTERED !
   then
   TASK-SYM-MUNMAP TASK-SYM MUNMAP-XT !
   TASK-SYM-PTHREAD-CREATE TASK-SYM PTHREAD-CREATE-XT !
   TASK-SYM-PTHREAD-JOIN TASK-SYM PTHREAD-JOIN-XT !
   TASK-SYM-PTHREAD-EXIT TASK-SYM PTHREAD-EXIT-XT !
   TASK-SYM-SCHED-YIELD TASK-SYM SCHED-YIELD-XT !
   TASK-SYM-PTHREAD-MUTEX-INIT TASK-SYM MUTEX-INIT-XT !
   TASK-SYM-PTHREAD-MUTEX-LOCK TASK-SYM MUTEX-LOCK-XT !
   TASK-SYM-PTHREAD-MUTEX-UNLOCK TASK-SYM MUTEX-UNLOCK-XT ! ;

: TASK-SYMBOLS ( -- )
   begin
      SYMBOLS-READY atomic@ 2 = if exit then
      0 1 SYMBOLS-READY atomic-cas 0= if
         [: LOAD-SYMBOLS ;] catch dup 0 <> if
            0 SYMBOLS-READY atomic! throw
         then drop
         2 SYMBOLS-READY atomic! exit
      then
   again ;

\ Exact task-internal C bindings fix every pointer extent and scalar role before
\ entering the bounded FFI trampoline. Retirement owner: habu-ptx-m1-c-1df1d6e7.
TRUSTED: MUNMAP-CALL ( ptr n n -- n ) {: a:ptr len:n :}
   TASK-SYMBOLS FFI:RESET
   a 0 FFI:READABLE!
   len 1 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 2 MUNMAP-XT @ ffi-call-bounded ;

TRUSTED: PTHREAD-CREATE-CALL ( ptr n n n ptr n -- n )
   {: thread:ptr attr:n entry:n arg:ptr :}
   TASK-SYMBOLS FFI:RESET
   thread 8 0 FFI:WRITABLE!
   attr 1 FFI:VALUE!
   entry 2 FFI:VALUE!
   arg 3 FFI:READABLE!
   FFI:ARGS FFI:REG-LENS 4 PTHREAD-CREATE-XT @ ffi-call-bounded ;

TRUSTED: PTHREAD-JOIN-CALL ( n ptr n -- n ) {: thread:n out:ptr :}
   TASK-SYMBOLS FFI:RESET
   thread 0 FFI:VALUE!
   out 8 1 FFI:WRITABLE!
   FFI:ARGS FFI:REG-LENS 2 PTHREAD-JOIN-XT @ ffi-call-bounded ;

TRUSTED: PTHREAD-EXIT-CALL ( n -- ) {: value:n :}
   TASK-SYMBOLS FFI:RESET
   value 0 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 1 PTHREAD-EXIT-XT @ ffi-call-bounded drop ;

TRUSTED: SCHED-YIELD-CALL ( -- n )
   TASK-SYMBOLS FFI:RESET
   FFI:ARGS FFI:REG-LENS 0 SCHED-YIELD-XT @ ffi-call-bounded ;

TRUSTED: MUTEX-INIT-CALL ( ptr n n -- n ) {: mutex:ptr attr:n :}
   TASK-SYMBOLS FFI:RESET
   mutex TASK-MUTEX-BYTES 0 FFI:WRITABLE!
   attr 1 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 2 MUTEX-INIT-XT @ ffi-call-bounded ;

TRUSTED: MUTEX-LOCK-CALL ( ptr n -- n ) {: mutex:ptr :}
   TASK-SYMBOLS FFI:RESET
   mutex TASK-MUTEX-BYTES 0 FFI:WRITABLE!
   FFI:ARGS FFI:REG-LENS 1 MUTEX-LOCK-XT @ ffi-call-bounded ;

TRUSTED: MUTEX-UNLOCK-CALL ( ptr n -- n ) {: mutex:ptr :}
   TASK-SYMBOLS FFI:RESET
   mutex TASK-MUTEX-BYTES 0 FFI:WRITABLE!
   FFI:ARGS FFI:REG-LENS 1 MUTEX-UNLOCK-XT @ ffi-call-bounded ;

: TASK-RC0 ( n -- )
   dup 0 <> if E-TASK-THREAD throw then
   drop ;

: TASK-CHECK-SIZE ( n -- )
   dup TASK-MIN-STACK < if E-TASK-SIZE throw then
   drop ;

: TASK-STATE@ ( ptr n -- n )
   TCB.STATUS @ ;

: TASK-STATE! ( n ptr n -- )
   TCB.STATUS ! ;

: TASK-LIVE+ ( -- )
   data-base TASKS-LIVE-CELL + dup @ 1 + swap ! ;

: TASK-LIVE- ( -- )
   data-base TASKS-LIVE-CELL + dup @ 1 - dup 0 < if drop 0 then swap ! ;

: TASK-MUNMAP-SPAN ( ptr n n -- )
   MUNMAP-CALL TASK-RC0 ;

: TASK-RELEASE-MEM ( ptr n -- ) {: tcb:ptr :}
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

: TASK-COPY-CELL ( ptr n ptr n n -- )
   {: src:ptr dst:ptr off:n :}
   src off + @ dst off + ! ;

: TASK-PTR-SLOT ( ptr n n -- ptr ptr n )
   + TASK-CELL>PTR-SLOT ;

: TASK-PTR! ( ptr n ptr n n -- )
   TASK-PTR-SLOT ! ;

: TASK-PTR@ ( ptr n n -- ptr n )
   TASK-PTR-SLOT @ ;

: TASK-REGION-INIT ( ptr n -- ) {: tcb:ptr :}
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

: TASK-CONSTRUCTED? ( ptr n -- bool )
   TASK-STATE@ TASK-EMPTY <> ;

: PREPARE ( ptr n -- ) {: tcb:ptr :}
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

: TASK-PTHREAD-CREATE-RC ( ptr n -- n ) {: tcb:ptr :}
   tcb TCB.THREAD 0 TASK-ENTRY @ tcb PTHREAD-CREATE-CALL ;

: TASK-PTHREAD-JOIN-CALL ( ptr n -- ) {: tcb:ptr :}
   tcb TCB.THREAD @ tcb TCB.RET PTHREAD-JOIN-CALL TASK-RC0 ;

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
\ Retirement owner: habu-checker-capability-gate-14022ba9.

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
   TASK-SYMBOLS TASK-ENTRY-BUILD ;

: TASK-JOIN-RELEASE ( ptr n -- ) {: tcb:ptr :}
   tcb TASK-PTHREAD-JOIN-CALL
   TASK-LIVE-
   tcb TASK-RELEASE-MEM
   TASK-EMPTY tcb TASK-STATE! ;

: TASK-SELF ( -- ptr n )
   data-base TASK-TCB-CELL + @ TASK-N>PTR ;

: TASK-SELF-N ( -- n )
   data-base TASK-TCB-CELL + @ ;

: TASK-RC>EXIT ( n -- n )
   $FF and ;

: TASK-RUN-USER ( -- n )
   TASK-SELF TCB.USER-XT @ catch ;

: TASK-RUNNER ( -- )
   TASK-RUN-USER dup 0= if drop exit then
   TASK-RC>EXIT s" task: unhandled throw" rot die ;

: ACTIVATE ( [ -- ] ptr n -- ) {: xt tcb:ptr :}
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

: TASK-STOP@ ( ptr n -- n )
   TCB.STOP @ ;

: TASK-STOP! ( n ptr n -- )
   TCB.STOP ! ;

: PAUSE ( -- )
   TASK-SELF-N dup 0= if drop SCHED-YIELD-CALL TASK-RC0 exit then
   TASK-N>PTR dup TASK-STOP@ 0 <> if
         TASK-DONE over TASK-STATE!
         0 PTHREAD-EXIT-CALL
   then
   drop
   SCHED-YIELD-CALL TASK-RC0 ;

: HALT ( ptr n -- )
   TASK-HALT-REQ over TASK-STATE!
   1 swap TASK-STOP! ;

: TASK-KILL ( ptr n -- ) {: tcb:ptr :}
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

: TASK-DONE? ( ptr n -- bool )
   TASK-STATE@ TASK-DONE = ;

\ ---- the three storage definers, and why only one converted -------------------
\ TWO ADDRESS KINDS LIVE HERE, and only one of them is expressible as generated
\ source today. A `create ... does> ( -- ptr n ) ;` word pushes the ABSOLUTE
\ address of its dictionary storage, which is the same address in every thread.
\ A generated `data-base <off> +` word resolves against the CALLING thread's data
\ region - register 20, which the thread entry below points at the task's own
\ 64 KiB region (TCB.REGION) - so inside a task it names a different object.
\
\ TASK and FACILITY publish SHARED storage: one TCB per task and one pthread
\ mutex every task locks. They keep `create ... does>` because the tree has no
\ checked source expression for a thread-invariant address (`dbase@`, register 26,
\ holds it but is typed `-- n`, so writing it would trade a does> for a new
\ pointer cast). Measured, not assumed: converting FACILITY makes each task lock
\ its own uninitialised copy - lib/task-test.f dies E-TASK-THREAD (rc 237) - and
\ converting TASK faults reading a TCB at region-base + dictionary-offset. The
\ missing piece is one thread-invariant `( -- ptr n )` origin; until it exists,
\ generated `data-base` accessors are for region-local storage only.
\
\ +USER is the other kind and converts cleanly: its storage IS a task-local slot,
\ its does> body already read `@ data-base +`, and the offsets it hands out are
\ bounded by TXN-STATE-OFF, inside every region.

\ CREATE/DOES> publishes a typed TCB address, outside checker inference.
\ Retirement owner: habu-typed-defining-words-aa224eb5.
TRUSTED: TASK ( n -- )
   dup TASK-CHECK-SIZE
   TASK-ALIGN8
   create
      , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,
   does> ( -- ptr n ) ;

: #USER ( -- n )
   TASK-USER-NEXT @ dup 0= if drop TASK-USER-BASE then ;

: USER-NEXT ( n n -- n ) {: off:n size:n :}
   off TASK-USER-BASE < if E-TASK-USER throw then
   off TXN-STATE-OFF > if E-TASK-USER throw then
   size 0 < if E-TASK-USER throw then
   size TXN-STATE-OFF off - > if E-TASK-USER throw then
   off size + ;

\ The slot's offset is fixed when the slot is declared, so it is baked into the
\ generated body; `data-base` is read at CALL time, which is what makes the word
\ answer the RUNNING task's own region - the whole point of a user slot, and what
\ the retired does> body (`@ data-base +`) did with an extra load.
\
\ The text is built with package CODEGEN's append machinery rather than a fifth
\ private copy of it. The calls are qualified rather than imported: a bare RESET
\ under `using CODEGEN` collides with the global RESET (E-USING-SHADOW-GLOBAL),
\ which is the collision docs/forth.md names as the case for qualifying.
$60 constant SLOT-GEN-CAP
SLOT-GEN-CAP CODEGEN:BUFFER SLOT-GEN

: SLOT-NAME ( -- ptr u8 n )
   parse-name dup 0= if E-TASK-USER throw then ;

: +USER ( n n -- n ) {: off:n size:n :}
   off size USER-NEXT {: next:n :}
   next TASK-USER-NEXT !
   SLOT-NAME {: name:ptr nameu:n :}
   SLOT-GEN CODEGEN:RESET
   s" : " SLOT-GEN CODEGEN:APPEND-STRING
   name nameu SLOT-GEN CODEGEN:APPEND-STRING
   s"  ( -- ptr n ) data-base " SLOT-GEN CODEGEN:APPEND-STRING
   off SLOT-GEN CODEGEN:APPEND-DECIMAL
   s"  + ;" SLOT-GEN CODEGEN:APPEND-STRING
   SLOT-GEN CODEGEN:CONTENTS INCLUDE-EVALUATE
   next ;

: HIS ( ptr n ptr n -- ptr n ) {: tcb:ptr cur:ptr :}
   cur data-base - tcb TCB.REGION @ + ;

\ CREATE/DOES> publishes owner-tracked pthread mutex storage, shared by every
\ task (see the address-kind note above).
\ Retirement owner: habu-typed-defining-words-aa224eb5.
TRUSTED: FACILITY ( -- )
   TASK-ALIGN8
   create TASK-FACILITY-BYTES allot
   does> ( -- ptr n ) ;

: FACILITY-OWNER ( ptr n -- ptr n )
   TASK-FACILITY-OWNER-OFF + ;

: FACILITY-MUTEX ( ptr n -- ptr n )
   TASK-FACILITY-MUTEX-OFF + ;

: TASK-OWNER ( -- n )
   TASK-SELF-N dup 0= if drop data-base FFI:>CELL then ;

: FACILITY-OWNER@ ( ptr n -- n )
   FACILITY-OWNER atomic@ ;

: FACILITY-OWNER! ( n ptr n -- )
   FACILITY-OWNER atomic! ;

: FACILITY-INIT ( ptr n -- )
   0 over FACILITY-OWNER!
   FACILITY-MUTEX 0 MUTEX-INIT-CALL TASK-RC0 ;

: GET ( ptr n -- ) {: f:ptr :}
   TASK-OWNER {: owner:n :}
   f FACILITY-OWNER@ owner = if exit then
   f FACILITY-MUTEX MUTEX-LOCK-CALL TASK-RC0
   owner f FACILITY-OWNER! ;

: RELEASE ( ptr n -- ) {: f:ptr :}
   TASK-OWNER {: owner:n :}
   f FACILITY-OWNER@ owner <> if exit then
   0 f FACILITY-OWNER!
   f FACILITY-MUTEX MUTEX-UNLOCK-CALL TASK-RC0 ;

public

TASK-MIN-STACK constant MIN-STACK

: TASK ( n -- )
   TASK ;

: PREPARE ( ptr n -- )
   PREPARE ;

: ACTIVATE ( [ -- ] ptr n -- )
   ACTIVATE ;

: SELF ( -- ptr n )
   TASK-SELF ;

: SELF-N ( -- n )
   TASK-SELF-N ;

: PAUSE ( -- )
   PAUSE ;

: HALT ( ptr n -- )
   HALT ;

: KILL ( ptr n -- )
   TASK-KILL ;

: DONE? ( ptr n -- bool )
   TASK-DONE? ;

: #USER ( -- n )
   #USER ;

: +USER ( n n -- n )
   +USER ;

: HIS ( ptr n ptr n -- ptr n )
   HIS ;

: FACILITY ( -- )
   FACILITY ;

: FACILITY-INIT ( ptr n -- )
   FACILITY-INIT ;

: GET ( ptr n -- )
   GET ;

: RELEASE ( ptr n -- )
   RELEASE ;

private

get-current prot-wid-add

public

get-current prot-wid-add

;package
