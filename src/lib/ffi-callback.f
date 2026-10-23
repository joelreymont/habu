\ ffi-callback.f - C callbacks into checked Habu.
\
\ CALLBACK: NAME ( in -- out ) clauses ;CALLBACK declares a C entry and generates
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
\ ENTRY binds the slot to a context (src/lib/task.f CONTEXT-BIND) and answers the C
\ function pointer; UNBIND releases it. A call arriving on an unbound slot, on
\ a context busy on another thread or on one with no outbound call in flight
\ fails closed: the thunk exits 106 (ENGINE-ERROR:CALLBACK).
\
\ CONTEXTS. A context admits one thread at a time; a second is refused, never
\ made to wait. TASK:SELF-CONTEXT is the calling task's region (C calls back
\ inside a foreign call it made), TASK:CONTEXT an exposed task's (a foreign
\ thread enters), TASK:MAIN-BASE FFI:>CELL the main region (main thread only).
\
\ THE BODY may call C again, but a nested call shares the task's FFI scratch,
\ so a scratch area C reads through a pointer is not safe across it. It may
\ not UNBIND its own slot, keep a ptr u8 argument past its return, or define:
\ exit $4F with a task live, exit 106 otherwise. TASK:PAUSE inside it only
\ yields, so TASK:HALT and TASK:KILL take effect after C returns.
\
\ LIFETIME. A binding lasts until UNBIND, UNEXPOSE, the end of the task whose
\ region it names, or a capture; the caller drains C's calls first. A capture
\ drops idle bindings and ends on a slot with a thread inside, so a stripped
\ application binds in its entry word. A declaration defines words: declare
\ every callback before the first task starts.
\
\ STORAGE CLASS. PROCESS-WIDE: the slots, descriptor and dispatch tables and
\ fault cells are one set for the image. The tables grow contiguously and a
\ slot is never given back, so a C pointer stays valid; the binding rows are
\ src/lib/task.f's.
require src/lib/errors.f
require src/lib/string.f
require src/lib/codegen.f
require src/lib/ffi-abi.f
require src/lib/task.f
require src/habu/address-cells.f

package FFI-CB
public

NEWTYPE callback

private

CAST: >CALLBACK ( n -- callback )
CAST: CALLBACK>N ( callback -- n )
CAST: STUB-XT ( n -- [ n n -- ] )
CAST: ENTRY-XT ( n -- [ -- n ] )

: ARM-STUB ( n n -- )
   data-base NCOMP-DISPATCH:CALLBACK-STUB-CELL + @
   dup 0= if E-NCOMP-STATE throw then STUB-XT execute ;

: STUB-ENTRY ( -- n )
   data-base NCOMP-DISPATCH:CALLBACK-ENTRY-CELL + @
   dup 0= if E-NCOMP-STATE throw then ENTRY-XT execute ;

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
CAST: N>CELLS ( n -- ptr n )
CAST: N>FLOATS ( n -- ptr r )
CAST: N>BYTES ( n -- ptr u8 )

\ The thunk indexes one contiguous descriptor table by stable slot ID. The
\ native entry is a code address, opaque to Habu callers. It is registered as
\ an address cell only for image relocation and is never executed as a Habu xt.
0 cells constant DISPATCH-OFF
1 cells constant ENTRY-OFF
2 cells constant FAULT-OFF
3 cells constant FALLBACK-OFF
4 cells constant FFALLBACK-OFF
5 cells constant ROW-BYTES
PERSISTED-PTR-VARIABLE TABLE
variable CAP
variable SLOTS

CAST: >DISPATCH-CELL ( ptr n -- ptr [ -- ] )
CAST: >FLOAT-CELL ( ptr n -- ptr r )
CAST: >ENTRY-XT ( n -- [ -- ] )

: ROW ( n -- ptr n )
   ROW-BYTES * TABLE @ + CELL-VIEW ;

: DISPATCH ( n -- ptr [ -- ] )
   ROW DISPATCH-OFF + >DISPATCH-CELL ;

: ENTRY-CELL ( n -- ptr n ) ROW ENTRY-OFF + ;
: FAULTS ( n -- ptr n ) ROW FAULT-OFF + ;
: FALLBACKS ( n -- ptr n ) ROW FALLBACK-OFF + ;
: FFALLBACKS ( n -- ptr r ) ROW FFALLBACK-OFF + >FLOAT-CELL ;

: ALIGNED-ROWS ( n -- ptr u8 ) {: size:n :}
   here data-base - negate 7 and allot
   here BYTE-VIEW {: rows:ptr :}
   size allot
   size 0 ?do 0 rows i + c! loop
   rows ;

\ Declarations precede task execution. Once the new table is published, old
\ rows are unreachable; clear their registered code cells and stored values so
\ the image scanner does not read former numeric fallback fields bytewise.
: RETIRE-TABLE ( ptr u8 n -- ) {: old:ptr cap:n :}
   cap 0= if exit then
   old BYTE-VIEW data-base BYTE-VIEW - cap ROW-BYTES *
      ADDRESS-CELLS:REMOVE-XT-SPAN
   cap ROW-BYTES * CELL / 0 ?do 0 old i cells + CELL-VIEW ! loop ;

: GROW ( -- )
   TABLE @ {: old:ptr :}
   CAP @ {: oldcap:n :}
   oldcap dup 0= if drop 1 else 2 * then {: next:n :}
   next ROW-BYTES * ALIGNED-ROWS {: rows:ptr :}
   SLOTS @ 0 > if
      old rows SLOTS @ ROW-BYTES * BYTE-COPY
      SLOTS @ 0 ?do
         i DISPATCH @ rows i ROW-BYTES * + CELL-VIEW >DISPATCH-CELL xt!
         i ENTRY-CELL @ dup 0<> if
            >ENTRY-XT rows i ROW-BYTES * + ENTRY-OFF + CELL-VIEW
            >DISPATCH-CELL xt!
         else drop then
      loop
   then
   rows TABLE !
   next CAP !
   rows FFI:>CELL TASK:MAIN-BASE CB-XTS + atomic!
   old oldcap RETIRE-TABLE ;

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
   q k DISPATCH xt! ;

: ENTRY-ADDRESS ( n -- n ) {: k:n :}
   k DECLARED-CHECK
   k ENTRY-CELL @ ;

: INSTALL-ENTRY ( n n -- ) {: addr:n k:n :}
   k DECLARED-CHECK
   addr >ENTRY-XT k ENTRY-CELL >DISPATCH-CELL xt! ;

: DESCRIPTOR ( n -- callback ) {: k:n :}
   k DECLARED-CHECK
   k >CALLBACK ;

\ ---- binding -------------------------------------------------------------------
\ Publishes the dispatch table into the main region, binds the slot to the
\ context and only then answers the stub's address.
: ENTRY ( callback n -- n ) {: cb ctx:n :}
   cb CALLBACK>N {: k:n :}
   TABLE @ FFI:>CELL TASK:MAIN-BASE CB-XTS + atomic!
   ctx k TASK:CONTEXT-BIND
   k ENTRY-CELL @ ;

: UNBIND ( callback -- )
   CALLBACK>N TASK:CONTEXT-UNBIND ;

: FAULT@ ( callback -- n )
   CALLBACK>N FAULTS @ ;

: CLEAR ( callback -- )
   CALLBACK>N 0 swap FAULTS ! ;

\ FILE replay visits only explicitly owned controls below its current DATA
\ range. Declarations can grow this baked table while a parent file loads, so
\ the controls and the former allocation belong to that range's update set.
\ Newly allocated rows are already inside its ordinary DATA span.
: OWNED-CELLS ( [ ptr u8 n -- ] -- ) {: keep :}
   TABLE BYTE-VIEW CELL keep execute
   CAP BYTE-VIEW CELL keep execute
   SLOTS BYTE-VIEW CELL keep execute
   CAP @ 0 > if TABLE @ CAP @ ROW-BYTES * keep execute then ;

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

\ The C entry belongs to a native emission even when the caller selected the
\ interpreter's legacy compiler for its other definitions. Restore its tier on
\ both success and refusal so a declaration does not change later source.
: GEN-NATIVE ( -- )
   tier@ 1 = if GEN-RUN exit then
   s" 1 set-tier" INCLUDE-EVALUATE
   [: GEN-RUN ;] [: s" 0 set-tier" INCLUDE-EVALUATE ;] finally ;

: STORE$ ( -- ptr u8 n )
   RES-KIND @ R-VALUE = if s" FFI-CB:RESULT! " exit then
   RES-KIND @ R-FLOAT = if s" FFI-CB:FRESULT! " exit then
   s" " ;

: NEXT-SLOT ( -- n )
   SLOTS @ {: k:n :}
   k TASK:DECLARE-CALLBACK
   k CAP @ >= if GROW then
   1 SLOTS +!
   k ;

\ The accessor's ordinary native emission carries an anonymous C entry after
\ its Habu return. NCOMP publishes both as one owned record; only the entry is
\ handed to C, while the accessor can read its relocated descriptor address.
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
   k -1 FFI:CALLBACK-ENTRY ARM-STUB
   GEN CODEGEN:RESET
   s" : " GEN+ NAME$ GEN+ s" -ENTRY ( -- n ) " GEN+
   k GEN-N s"  FFI-CB:ENTRY-ADDRESS ;" GEN+
   GEN-NATIVE
   STUB-ENTRY k INSTALL-ENTRY
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

\ The callback descriptor table holds whole cells, including numeric fallback
\ values. OWNED-CELLS identifies the same live allocation for FILE replay.
: SCAN-SPANS ( [ ptr u8 n -- ] [ ptr u8 n -- ] -- )
   {: cells bytes :}
   CAP @ 0 > if TABLE @ BYTE-VIEW CAP @ ROW-BYTES * cells execute then
   GEN cells bytes CODEGEN:SCAN-SPANS
   QTYPE cells bytes CODEGEN:SCAN-SPANS
   READS cells bytes CODEGEN:SCAN-SPANS
   NAME-BUF TOK-CAP bytes execute
   TOK-BUF TOK-CAP bytes execute ;

;package

: CALLBACK: ( -- )
   FFI-CB:OPEN-CALLBACK ;

: FALLBACK ( n -- )
   FFI-CB:SET-FALLBACK ;

: FFALLBACK ( r -- )
   FFI-CB:SET-FFALLBACK ;

: ;CALLBACK ( -- )
   FFI-CB:CLOSE-CALLBACK ;
