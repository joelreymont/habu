\ ffi-callback.f - C callbacks into checked Habu (docs/ffi-callback.md).
\
\ CALLBACK: NAME ( in -- out ) clauses ;CALLBACK declares one of the engine's
\ CB-POOL fixed callback stubs (src/habu/habu1.f BCALLBACK-ENTRY) and generates
\   NAME           ( -- FFI-CB:callback )  the descriptor;
\   NAME-BODY      a TYPED-VARIABLE holding the checked body, typed from the
\                  declaration (i32/u32 read as n, ptr u8 as ptr u8);
\   NAME-DISPATCH  ( -- ) the slot's dispatch: it reads the arguments from the
\                  marshal frame, runs the body under catch and leaves the
\                  result for C, or the declared fallback after a throw. A body
\                  never stored is such a throw, E-FFI-CALLBACK-STATE.
\ Arguments are register arguments only: n, i32, u32, r and ptr u8, at most
\ INT-REGS integer and FLOAT-REGS float ones. A result is n, i32, u32, r or
\ none; a value result needs `n FALLBACK` (r: `r FFALLBACK`), the answer C gets
\ when the body throws. The throw code is kept for FAULT@ until CLEAR.
\
\ ENTRY binds the slot to a context (lib/task.f CONTEXT-BIND) and answers the C
\ function pointer; UNBIND releases it. A call arriving on an unbound slot, on
\ a context busy on another thread or on one with no outbound call in flight
\ fails closed: the thunk exits 106 (ENGINE-ERROR:CALLBACK).
require lib/errors.f
require lib/codegen.f
require lib/ffi-abi.f
require lib/task.f

package FFI-CB
public

NEWTYPE callback 0

private

CAST: >CALLBACK ( n -- callback )
CAST: CALLBACK>N ( callback -- n )

8 constant FLOAT-REGS
$400 constant GEN-CAP
$200 constant PART-CAP            \ sixteen `7 FFI-CB:I32-ARG@ ` reads need $108
$40 constant TOK-CAP

0 constant R-NONE
1 constant R-VALUE
2 constant R-FLOAT

\ Integer argument registers: eight on AAPCS64, six on SysV.
: INT-REGS ( -- n )
   HB-TARGET-LINUX-X86-64? if 6 else 8 then ;

\ The marshal frame is C's register file as the engine's thunk parked it, and a
\ pointer argument is whatever address C passed.
TRUSTED: N>CELLS ( n -- ptr n ) ;
TRUSTED: N>FLOATS ( n -- ptr r ) ;
TRUSTED: N>BYTES ( n -- ptr u8 ) ;

\ The C entry of engine stub n: an immutable code address, never a Habu xt.
TRUSTED: STUB ( n -- n ) callback-entry ;

\ The dispatch table the engine's thunk indexes by slot. Its cells are
\ quotations, which only typed storage holds and capture relocates, and the
\ count of a TYPED-BUFFER is a decimal literal (docs/forth.md "Rules learned
\ by refusal"), so POOL-CHECK refuses the load when the engine's CB-POOL is
\ another size. The per-slot cells are raw storage sized from the constant.
16 TYPED-BUFFER XTS [ -- ]

: POOL-CHECK ( -- )
   CB-POOL 16 <> if
      s" ffi-callback: XTS holds 16 dispatches and the engine's CB-POOL differs"
      70 die
   then ;

POOL-CHECK

create FAULT-CELLS CB-POOL cells allot
create FALLBACK-CELLS CB-POOL cells allot
create FFALLBACK-CELLS CB-POOL cells allot
variable SLOTS

: FAULTS ( n -- ptr n ) cells FAULT-CELLS + ;
: FALLBACKS ( n -- ptr n ) cells FALLBACK-CELLS + ;
: FFALLBACKS ( n -- ptr r ) cells FFALLBACK-CELLS + ;

\ The live marshal frame of the innermost callback on this region.
: FRAME ( -- n )
   data-base CB-FRAME + @ dup 0= if E-FFI-CALLBACK-STATE throw then ;

: INT-CHECK ( n -- ) {: i:n :}
   i 0 < i INT-REGS >= or if E-FFI-ARITY throw then ;

: FLOAT-CHECK ( n -- ) {: i:n :}
   i 0 < i FLOAT-REGS >= or if E-FFI-ARITY throw then ;

: DECLARED-CHECK ( n -- ) {: k:n :}
   k 0 < k SLOTS @ >= or if E-FFI-CALLBACK-STATE throw then ;

public

\ ---- the frame: what the generated dispatch reads and writes -----------------
: ARG@ ( n -- n ) {: i:n :}
   i INT-CHECK
   FRAME i cells + N>CELLS @ ;

: I32-ARG@ ( n -- n )
   ARG@ $FFFFFFFF and dup $80000000 and 0 <> if $FFFFFFFF00000000 or then ;

: U32-ARG@ ( n -- n )
   ARG@ $FFFFFFFF and ;

: PTR-ARG@ ( n -- ptr u8 )
   ARG@ N>BYTES ;

: FARG@ ( n -- r ) {: i:n :}
   i FLOAT-CHECK
   FRAME CB-FRAME-FLOATS + i cells + N>FLOATS @ ;

: RESULT! ( n -- )
   FRAME N>CELLS ! ;

: FRESULT! ( r -- )
   FRAME CB-FRAME-FLOATS + N>FLOATS ! ;

private

\ A body that returned leaves its result in place. A throw is a fault: the code
\ is kept for FAULT@ and the declared fallback goes back to C, by the result
\ kind the declaration wrote into its dispatch.
: SETTLE ( n n n -- ) {: code:n k:n kind:n :}
   code 0= if exit then
   code k FAULTS !
   kind R-FLOAT = if k FFALLBACKS @ FRESULT! exit then
   kind R-VALUE = if k FALLBACKS @ RESULT! then ;

public

\ ---- what the generated source calls -----------------------------------------
\ The body under catch, then the slot and its result kind settle a throw.
: RUN ( [ -- ] n n -- ) {: q k:n kind:n :}
   q catch k kind SETTLE ;

\ A body cell nobody stored into holds zero, which the typed fetch would hand
\ to execute. The dispatch asks first, reading the same cell as a number, and
\ inside its catch: the call is a fault, and C gets the declared fallback.
: BODY-CHECK ( ptr a -- )
   BYTE-VIEW CELL-VIEW @ 0= if E-FFI-CALLBACK-STATE throw then ;

: INSTALL ( [ -- ] n -- ) {: q k:n :}
   k DECLARED-CHECK
   q k XTS ! ;

: DESCRIPTOR ( n -- callback ) {: k:n :}
   k DECLARED-CHECK
   k >CALLBACK ;

\ ---- binding -------------------------------------------------------------------
\ Publishes the dispatch table into the main region, binds the slot to the
\ context and only then answers the stub's address.
: ENTRY ( callback n -- n ) {: cb ctx:n :}
   cb CALLBACK>N {: k:n :}
   0 XTS FFI:>CELL TASK:MAIN-BASE CB-XTS + atomic!
   ctx k TASK:CONTEXT-BIND
   k STUB ;

: UNBIND ( callback -- )
   CALLBACK>N TASK:CONTEXT-UNBIND ;

: FAULT@ ( callback -- n )
   CALLBACK>N FAULTS @ ;

: CLEAR ( callback -- )
   CALLBACK>N 0 swap FAULTS ! ;

private

\ ---- the declaration ----------------------------------------------------------
GEN-CAP CODEGEN:BUFFER GEN
PART-CAP CODEGEN:BUFFER QTYPE             \ the body's quotation type
PART-CAP CODEGEN:BUFFER READS             \ the argument reads, in order

create NAME-BUF TOK-CAP allot
create TOK-BUF TOK-CAP allot
variable NAME-U
variable TOK-U
variable INT-N
variable FLT-N
variable RES-KIND
variable FB-SET
variable FB-VALUE
TYPED-VARIABLE FFB-VALUE r
variable OPEN

: TOK! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if E-FFI-SYNTAX throw then
   u TOK-CAP > if E-FFI-SYNTAX throw then
   a TOK-BUF u BYTE-COPY
   u TOK-U ! ;

: TOK@ ( -- )
   parse-name TOK! ;

: TOK$ ( -- ptr u8 n )
   TOK-BUF TOK-U @ ;

: TOK-IS? ( ptr u8 n -- bool )
   TOK$ STR= ;

: NAME! ( -- )
   TOK$ {: a:ptr u:n :}
   a NAME-BUF u BYTE-COPY
   u NAME-U ! ;

: NAME$ ( -- ptr u8 n )
   NAME-BUF NAME-U @ ;

: QTYPE+ ( ptr u8 n -- )
   QTYPE CODEGEN:APPEND-STRING s"  " QTYPE CODEGEN:APPEND-STRING ;

: READ+ ( n ptr u8 n -- ) {: i:n a:ptr u:n :}
   i READS CODEGEN:APPEND-DECIMAL
   s"  FFI-CB:" READS CODEGEN:APPEND-STRING
   a u READS CODEGEN:APPEND-STRING
   s"  " READS CODEGEN:APPEND-STRING ;

: INT+ ( ptr u8 n -- ) {: a:ptr u:n :}
   INT-N @ INT-REGS >= if E-FFI-ARITY throw then
   INT-N @ a u READ+
   1 INT-N +! ;

: IN-TOKEN ( -- )
   s" n" TOK-IS? if s" n" QTYPE+ s" ARG@" INT+ exit then
   s" i32" TOK-IS? if s" n" QTYPE+ s" I32-ARG@" INT+ exit then
   s" u32" TOK-IS? if s" n" QTYPE+ s" U32-ARG@" INT+ exit then
   s" ptr" TOK-IS? if
      TOK@ s" u8" TOK-IS? 0= if E-FFI-SYNTAX throw then
      s" ptr u8" QTYPE+ s" PTR-ARG@" INT+ exit
   then
   s" r" TOK-IS? if
      FLT-N @ FLOAT-REGS >= if E-FFI-ARITY throw then
      s" r" QTYPE+ FLT-N @ s" FARG@" READ+
      1 FLT-N +! exit
   then
   E-FFI-SYNTAX throw ;

: OUT-TOKEN ( -- )
   RES-KIND @ R-NONE <> if E-FFI-SYNTAX throw then
   s" r" TOK-IS? if s" r" QTYPE+ R-FLOAT RES-KIND ! exit then
   s" n" TOK-IS? s" i32" TOK-IS? or s" u32" TOK-IS? or if
      s" n" QTYPE+ R-VALUE RES-KIND ! exit
   then
   E-FFI-SYNTAX throw ;

: INPUTS ( -- )
   begin
      TOK@
      s" --" TOK-IS? if s" --" QTYPE+ exit then
      s" )" TOK-IS? if E-FFI-SYNTAX throw then
      IN-TOKEN
   again ;

: OUTPUTS ( -- )
   begin
      TOK@
      s" )" TOK-IS? if exit then
      s" --" TOK-IS? if E-FFI-SYNTAX throw then
      OUT-TOKEN
   again ;

: GEN+ ( ptr u8 n -- ) GEN CODEGEN:APPEND-STRING ;
: GEN-N ( n -- ) GEN CODEGEN:APPEND-DECIMAL ;
: GEN-RUN ( -- ) GEN CODEGEN:CONTENTS INCLUDE-EVALUATE ;

: STORE$ ( -- ptr u8 n )
   RES-KIND @ R-VALUE = if s" FFI-CB:RESULT! " exit then
   RES-KIND @ R-FLOAT = if s" FFI-CB:FRESULT! " exit then
   s" " ;

: NEXT-SLOT ( -- n )
   SLOTS @ CB-POOL >= if E-FFI-CALLBACK-FULL throw then
   SLOTS @ 1 SLOTS +! ;

\ Four evaluated pieces, each one line: the body cell, the dispatch, which hands
\ RUN its slot and result kind as literals, the dispatch stored into its slot,
\ and the descriptor.
: EMIT ( n -- ) {: k:n :}
   GEN CODEGEN:RESET
   s" TYPED-VARIABLE " GEN+ NAME$ GEN+ s" -BODY [ " GEN+
   QTYPE CODEGEN:CONTENTS GEN+ s" ]" GEN+
   GEN-RUN
   GEN CODEGEN:RESET
   s" : " GEN+ NAME$ GEN+ s" -DISPATCH ( -- ) [: " GEN+
   NAME$ GEN+ s" -BODY FFI-CB:BODY-CHECK " GEN+
   READS CODEGEN:CONTENTS GEN+ NAME$ GEN+ s" -BODY @ execute " GEN+
   STORE$ GEN+ s" ;] " GEN+ k GEN-N s"  " GEN+ RES-KIND @ GEN-N
   s"  FFI-CB:RUN ;" GEN+
   GEN-RUN
   GEN CODEGEN:RESET
   s" ' " GEN+ NAME$ GEN+ s" -DISPATCH " GEN+ k GEN-N s"  FFI-CB:INSTALL" GEN+
   GEN-RUN
   GEN CODEGEN:RESET
   s" : " GEN+ NAME$ GEN+ s"  ( -- FFI-CB:callback ) " GEN+ k GEN-N
   s"  FFI-CB:DESCRIPTOR ;" GEN+
   GEN-RUN ;

\ A refusal between the two keywords abandons the declaration, so nothing is
\ left open for the next one.
: REFUSE ( -- )
   0 OPEN !
   E-FFI-SYNTAX throw ;

public

: OPEN-CALLBACK ( -- )
   OPEN @ 0 <> if REFUSE then
   0 INT-N ! 0 FLT-N ! R-NONE RES-KIND ! 0 FB-SET !
   QTYPE CODEGEN:RESET READS CODEGEN:RESET
   TOK@ NAME!
   TOK@ s" (" TOK-IS? 0= if E-FFI-SYNTAX throw then
   INPUTS
   OUTPUTS
   1 OPEN ! ;

: SET-FALLBACK ( n -- ) {: v:n :}
   OPEN @ 0= if REFUSE then
   RES-KIND @ R-VALUE <> if REFUSE then
   v FB-VALUE ! 1 FB-SET ! ;

: SET-FFALLBACK ( r -- ) {: v:r :}
   OPEN @ 0= if REFUSE then
   RES-KIND @ R-FLOAT <> if REFUSE then
   v FFB-VALUE ! 1 FB-SET ! ;

: CLOSE-CALLBACK ( -- )
   OPEN @ 0= if E-FFI-SYNTAX throw then
   0 OPEN !
   RES-KIND @ R-NONE <> FB-SET @ 0= and if E-FFI-SYNTAX throw then
   NEXT-SLOT {: k:n :}
   FB-VALUE @ k FALLBACKS !
   FFB-VALUE @ k FFALLBACKS !
   0 k FAULTS !
   k EMIT ;

;package

: CALLBACK: ( -- )
   FFI-CB:OPEN-CALLBACK ;

: FALLBACK ( n -- )
   FFI-CB:SET-FALLBACK ;

: FFALLBACK ( r -- )
   FFI-CB:SET-FFALLBACK ;

: ;CALLBACK ( -- )
   FFI-CB:CLOSE-CALLBACK ;
