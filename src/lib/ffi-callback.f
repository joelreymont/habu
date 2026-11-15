\ ffi-callback.f - C callbacks into checked Habu: CALLBACK: NAME ( in -- out )
\ clauses ;CALLBACK declares a C entry; FFI-CB:ENTRY binds it to a context
\ (src/lib/task.f CONTEXT-BIND) and answers the C function pointer.
\ STORAGE CLASS. PROCESS-WIDE: the slots, descriptor and dispatch tables and
\ fault cells are one set for the image. The tables grow contiguously and only
\ the rollback of the text that declared a slot gives it back, so a C pointer
\ stays valid; binding rows are task.f's.
require src/lib/error.f
require src/lib/string.f
require src/lib/codegen.f
require src/lib/ffi-abi.f
require src/lib/task.f
require src/habu/address-cells.f
require src/core/rollback-cells.f
require src/habu/data-floor.f

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
   dup 0= if E-COMP-STATE throw then STUB-XT execute ;

: STUB-ENTRY ( -- n )
   data-base NCOMP-DISPATCH:CALLBACK-ENTRY-CELL + @
   dup 0= if E-COMP-STATE throw then ENTRY-XT execute ;

8 constant FLOAT-REGS

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
SLOTS ROLLBACK-CELLS:MARK

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
   rows ;

\ Declarations precede task execution. Once the new table is published, old
\ rows are unreachable; clear their address rows and stored values so
\ the image scanner does not read former numeric fallback fields bytewise.
: RETIRE-TABLE ( ptr u8 n -- ) {: old:ptr cap:n :}
   cap 0= if exit then
   old BYTE-VIEW data-base BYTE-VIEW - cap ROW-BYTES *
      ADDRESS-CELLS:REMOVE-SPAN
   cap ROW-BYTES * CELL / 0 ?do 0 old i cells + CELL-VIEW ! loop ;

: GROW ( -- )
   TABLE @ {: old:ptr :}
   CAP @ {: oldcap:n :}
   oldcap dup 0= if drop 1 else 2 * then {: next:n :}
   next ROW-BYTES * ALIGNED-ROWS {: rows:ptr :}
   \ No rollback cuts a grown table.
   DATA-FLOOR:RAISE
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
   rows FFI:>CELL data-base CB-XTS + atomic!
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

: SETTLE ( n n n -- ) {: code:n k:n kind:n :}
   code 0= if exit then
   code k FAULTS !
   kind R-FLOAT = if k FFALLBACKS @ FRESULT! exit then
   kind R-VALUE = if k FALLBACKS @ RESULT! then ;

public

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

\ Publishes the dispatch table into the main region and binds the slot before
\ answering the stub's address. A call on an unbound slot, on a context busy on
\ another thread or on one with no outbound call in flight fails closed: the
\ thunk exits 106 (ENGINE-ERROR:CALLBACK). A context admits one thread at a
\ time; a second is rejected, never made to wait. TASK:SELF-CONTEXT is the
\ calling task's region (C calls back inside a foreign call it made),
\ TASK:MAIN-BASE FFI:>CELL the main region (main thread only).
\ TASK:CALLER-CONTEXT is each calling thread's own region, inside a foreign
\ call it made, for any number of threads at once; a thread with no region of
\ its own exits 106.
\ A binding lasts until UNBIND, the end of the task whose region it names, or a
\ capture; the caller drains C's calls first. A capture drops idle
\ bindings and ends on a slot with a thread inside, so a stripped application
\ binds in its entry word.
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

private

CODEGEN:BUFFER GEN
CODEGEN:BUFFER QTYPE             \ the body's quotation type
CODEGEN:BUFFER READS             \ the argument reads, in order

variable RES-KIND
variable FB-SET
variable FB-VALUE
TYPED-VARIABLE FFB-VALUE r
variable OPEN

\ The last token read and the name: each parse-name span, kept beside its
\ copy. A copy grows to the token it holds and is released at rest.
DYNAMIC-BUFFER TOK-BUF u8
variable TOK-U
1 PTR-U8-TABLE TOK-SRC-A
variable TOK-SRC-U
DYNAMIC-BUFFER NAME u8
variable NAME-U
1 PTR-U8-TABLE NAME-SRC-A
variable NAME-SRC-U

: NAME$ ( -- ptr u8 n )
   0 NAME NAME-U @ ;

\ A declaration's span and copy cells, at rest before it reads a token and
\ after it closes or is abandoned.
: REST ( -- )
   TOK-BUF-RELEASE NAME-RELEASE
   0 TOK-U !  NULL-PTR TOK-SRC-A 0 ptr-field !  0 TOK-SRC-U !
   0 NAME-U !  NULL-PTR NAME-SRC-A 0 ptr-field !  0 NAME-SRC-U ! ;

\ ---- a bad declaration's rejection ----------------------------------------
\ The caller writes a record's help and ends it with ABANDON. The stop abandons
\ the declaration, so the next CALLBACK: in the file is not rejected for it.
: ABANDON ( -- )
   0 OPEN !  REST  CHECKER-REJECT-RC REJECTION:STOP ;

: SAY ( ptr u8 n -- ) REJECTION:EMIT-STR ;

: TOK-AT ( -- ptr u8 n )
   TOK-SRC-A 0 ptr-field @ TOK-SRC-U @ 0 TOK-BUF TOK-U @ REJECTION:KEPT ;

: NAME-AT ( -- ptr u8 n )
   NAME-SRC-A 0 ptr-field @ NAME-SRC-U @ NAME$ REJECTION:KEPT ;

\ The input ended: the record stands at the last token read, or at the
\ keyword when the declaration has read none.
: ENDED ( -- )
   TOK-U @ 0= if SRCLOC:TOKEN$ else TOK-AT then {: t:ptr tu:n :}
   s" E-FFI-SYNTAX" s" the source ends before the declaration is complete"
   t tu s" the declaration ends here" REJECTION:OPEN-AT REJECTION:HELP
   s" finish the declaration: `CALLBACK:` takes a name and an effect " SAY
   s" closed by `)`" SAY
   ABANDON ;

: TOK-KEEP ( ptr u8 n -- ) {: a:ptr u:n :}
   u TOK-BUF-RESERVE
   a 0 TOK-BUF u BYTE-COPY  u TOK-U !
   a TOK-SRC-A 0 ptr-field !  u TOK-SRC-U ! ;

\ The next token. parse-name's bytes stay valid until the next parse, and each
\ token is matched before then, so it lives on the stack.
: TOK@ ( -- ptr u8 n )
   parse-name {: a:ptr u:n :}
   u 0= if ENDED then
   a u TOK-KEEP
   a u ;

: NAME! ( ptr u8 n -- ) {: a:ptr u:n :}
   u NAME-RESERVE
   a 0 NAME u BYTE-COPY  u NAME-U !
   a NAME-SRC-A 0 ptr-field !  u NAME-SRC-U ! ;

: QTYPE+ ( ptr u8 n -- )
   QTYPE CODEGEN:APPEND-STRING s"  " QTYPE CODEGEN:APPEND-STRING ;

: READ+ ( n ptr u8 n -- ) {: i:n a:ptr u:n :}
   i READS CODEGEN:APPEND-DECIMAL
   s"  FFI-CB:" READS CODEGEN:APPEND-STRING
   a u READS CODEGEN:APPEND-STRING
   s"  " READS CODEGEN:APPEND-STRING ;

: MANY-INTS ( -- )
   s" E-FFI-ARITY"
   s" the callback takes more integer inputs than its registers hold"
   NAME-AT s" too many integer inputs" REJECTION:OPEN-AT
   INT-REGS s" integer registers" REJECTION:LIMIT REJECTION:HELP
   s" C passes a callback its integers in registers alone: pass the extra " SAY
   s" values in memory, through a `ptr u8` input" SAY
   ABANDON ;

: MANY-FLOATS ( -- )
   s" E-FFI-ARITY"
   s" the callback takes more float inputs than its registers hold"
   NAME-AT s" too many float inputs" REJECTION:OPEN-AT
   FLOAT-REGS s" float registers" REJECTION:LIMIT REJECTION:HELP
   s" C passes a callback its floats in registers alone: pass the extra " SAY
   s" values in memory, through a `ptr u8` input" SAY
   ABANDON ;

: BAD-PTR ( -- )
   s" E-FFI-SYNTAX" s" a pointer in a declaration is `ptr u8`"
   TOK-AT s" expected `u8`" REJECTION:OPEN-AT REJECTION:HELP
   s" write `ptr u8`: a callback takes a pointer as a span of bytes" SAY
   ABANDON ;

: BAD-INPUT ( -- )
   s" E-FFI-SYNTAX" s" the declaration names an input a callback cannot take"
   TOK-AT s" not an input type" REJECTION:OPEN-AT REJECTION:HELP
   s" declare each input as `n`, `i32`, `u32`, `r` or `ptr u8`" SAY
   ABANDON ;

: SECOND-RESULT ( -- )
   s" E-FFI-SYNTAX" s" the declaration has more than one result"
   TOK-AT s" a second result" REJECTION:OPEN-AT REJECTION:HELP
   s" declare one result: C takes one value back from a callback" SAY
   ABANDON ;

: BAD-RESULT ( -- )
   s" E-FFI-SYNTAX" s" the declaration names a result a callback cannot give"
   TOK-AT s" not a result type" REJECTION:OPEN-AT REJECTION:HELP
   s" declare the result as `n`, `i32`, `u32` or `r`, or declare none" SAY
   ABANDON ;

: NO-DASHES ( -- )
   s" E-FFI-SYNTAX" s" the effect closes before its `--`"
   TOK-AT s" expected `--` before this" REJECTION:OPEN-AT REJECTION:HELP
   s" separate the inputs from the result with `--`, as `( n -- n )`" SAY
   ABANDON ;

: SECOND-DASHES ( -- )
   s" E-FFI-SYNTAX" s" the effect has a second `--`"
   TOK-AT s" a second `--`" REJECTION:OPEN-AT REJECTION:HELP
   s" keep one `--`, between the inputs and the result" SAY
   ABANDON ;

\ One integer argument read by the named reader. The input words carry two
\ counts on the stack: the integer, then the float arguments read so far.
: INT+ ( n n ptr u8 n -- n n )
   {: i:n f:n a:ptr u:n :}
   i INT-REGS >= if MANY-INTS then
   i a u READ+
   i 1 + f ;

: IN-TOKEN ( n n ptr u8 n -- n n )
   {: i:n f:n a:ptr u:n :}
   a u s" n" STR= if s" n" QTYPE+ i f s" ARG@" INT+ exit then
   a u s" i32" STR= if s" n" QTYPE+ i f s" I32-ARG@" INT+ exit then
   a u s" u32" STR= if s" n" QTYPE+ i f s" U32-ARG@" INT+ exit then
   a u s" ptr" STR= if
      TOK@ s" u8" STR= 0= if BAD-PTR then
      s" ptr u8" QTYPE+ i f s" PTR-ARG@" INT+ exit
   then
   a u s" r" STR= if
      f FLOAT-REGS >= if MANY-FLOATS then
      s" r" QTYPE+ f s" FARG@" READ+
      i f 1 + exit
   then
   BAD-INPUT ;

: OUT-TOKEN ( ptr u8 n -- )
   {: a:ptr u:n :}
   RES-KIND @ R-NONE <> if SECOND-RESULT then
   a u s" r" STR= if s" r" QTYPE+ R-FLOAT RES-KIND ! exit then
   a u s" n" STR= a u s" i32" STR= or a u s" u32" STR= or if
      s" n" QTYPE+ R-VALUE RES-KIND ! exit
   then
   BAD-RESULT ;

\ One input token: true at the -- that ends the inputs.
: INPUT? ( n n -- n n bool )
   TOK@ {: i:n f:n a:ptr u:n :}
   a u s" --" STR= if s" --" QTYPE+ i f true exit then
   a u s" )" STR= if NO-DASHES then
   i f a u IN-TOKEN false ;

\ One output token: true at the ) that ends the effect.
: OUTPUT? ( -- bool )
   TOK@ {: a:ptr u:n :}
   a u s" )" STR= if true exit then
   a u s" --" STR= if SECOND-DASHES then
   a u OUT-TOKEN false ;

: INPUTS ( -- ) 0 0 begin INPUT? until 2drop ;
: OUTPUTS ( -- ) begin OUTPUT? until ;

: GEN+ ( ptr u8 n -- ) GEN CODEGEN:APPEND-STRING ;
: GEN-N ( n -- ) GEN CODEGEN:APPEND-DECIMAL ;
: GEN-RUN ( -- ) GEN CODEGEN:CONTENTS INCLUDE-EVALUATE ;

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
\ its Habu return. The native compiler publishes both as one owned record; only
\ the entry is handed to C, while the accessor can read its relocated
\ descriptor address.
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
   k FFI:CALLBACK-ENTRY ARM-STUB
   GEN CODEGEN:RESET
   s" : " GEN+ NAME$ GEN+ s" -ENTRY ( -- n ) " GEN+
   k GEN-N s"  FFI-CB:ENTRY-ADDRESS ;" GEN+
   GEN-RUN
   STUB-ENTRY k INSTALL-ENTRY
   GEN CODEGEN:RESET
   s" : " GEN+ NAME$ GEN+ s"  ( -- FFI-CB:callback ) " GEN+ k GEN-N
   s"  FFI-CB:DESCRIPTOR ;" GEN+
   GEN-RUN ;

: NESTED ( -- )
   s" E-FFI-SYNTAX" s" a declaration opens while another is open"
   SRCLOC:TOKEN$ s" the open one needs `;CALLBACK` before this" REJECTION:OPEN-AT
   REJECTION:HELP
   s" close the open declaration with `;CALLBACK` first" SAY
   ABANDON ;

: NO-EFFECT ( -- )
   s" E-FFI-SYNTAX" s" the declaration has no effect after its name"
   TOK-AT s" expected `(`" REJECTION:OPEN-AT REJECTION:HELP
   s" write the effect after the name, as `CALLBACK: name ( n -- n )`" SAY
   ABANDON ;

\ FALLBACK or FFALLBACK, as the program wrote it, with no declaration open.
: OUTSIDE ( -- )
   SRCLOC:TOKEN$ {: k:ptr ku:n :}
   s" E-FFI-SYNTAX" REJECTION:OPEN
   s" `" SAY  k ku SAY  s" ` stands outside a declaration" SAY
   k ku REJECTION:AT  s" nothing is open" SAY  REJECTION:HELP
   s" write `" SAY  k ku SAY  s" ` between `CALLBACK:` and `;CALLBACK`" SAY
   ABANDON ;

: RESULT$ ( -- ptr u8 n )
   RES-KIND @ R-NONE = if s" the callback has no result" exit then
   RES-KIND @ R-FLOAT = if s" the callback's result is `r`" exit then
   s" the callback's result is an integer" ;

: FIT$ ( -- ptr u8 n )
   RES-KIND @ R-FLOAT = if s" write `r FFALLBACK` for an `r` result" exit then
   s" write `n FALLBACK` for an integer result" ;

\ FALLBACK or FFALLBACK whose value is not the declared result's kind.
: MISFIT ( -- )
   SRCLOC:TOKEN$ {: k:ptr ku:n :}
   s" E-FFI-SYNTAX" REJECTION:OPEN
   s" `" SAY  k ku SAY  s" ` does not fit the callback's result" SAY
   k ku REJECTION:AT  RESULT$ SAY  REJECTION:HELP
   RES-KIND @ R-NONE = if
      s" remove `" SAY  k ku SAY
      s" `: C takes no value back from this callback" SAY
   else FIT$ SAY then
   ABANDON ;

: STRAY-CLOSE ( -- )
   s" E-FFI-SYNTAX" s" `;CALLBACK` closes no declaration"
   SRCLOC:TOKEN$ s" nothing is open" REJECTION:OPEN-AT REJECTION:HELP
   s" remove this `;CALLBACK`, or open its declaration with " SAY
   s" `CALLBACK:` first" SAY
   ABANDON ;

: NO-FALLBACK ( -- )
   s" E-FFI-SYNTAX" s" the callback returns a value but has no fallback"
   SRCLOC:TOKEN$ s" closes with no fallback" REJECTION:OPEN-AT
   REJECTION:HELP
   RES-KIND @ R-FLOAT = if s" write `r FFALLBACK`"
   else s" write `n FALLBACK`" then SAY
   s"  before `;CALLBACK`: the value C gets when the body throws" SAY
   ABANDON ;

public

: OPEN-CALLBACK ( -- )
   OPEN @ 0 <> if NESTED then
   R-NONE RES-KIND ! 0 FB-SET !  REST
   QTYPE CODEGEN:RESET READS CODEGEN:RESET
   TOK@ NAME!
   TOK@ s" (" STR= 0= if NO-EFFECT then
   INPUTS
   OUTPUTS
   1 OPEN ! ;

: SET-FALLBACK ( n -- ) {: v:n :}
   OPEN @ 0= if OUTSIDE then
   RES-KIND @ R-VALUE <> if MISFIT then
   v FB-VALUE ! 1 FB-SET ! ;

: SET-FFALLBACK ( r -- ) {: v:r :}
   OPEN @ 0= if OUTSIDE then
   RES-KIND @ R-FLOAT <> if MISFIT then
   v FFB-VALUE ! 1 FB-SET ! ;

: CLOSE-CALLBACK ( -- )
   OPEN @ 0= if STRAY-CLOSE then
   0 OPEN !
   RES-KIND @ R-NONE <> FB-SET @ 0= and if NO-FALLBACK then
   NEXT-SLOT {: k:n :}
   FB-VALUE @ k FALLBACKS !
   FFB-VALUE @ k FFALLBACKS !
   0 k FAULTS !
   k EMIT  REST ;

;package

\ CALLBACK: NAME ( in -- out ) clauses ;CALLBACK generates
\   NAME           ( -- FFI-CB:callback )  the descriptor;
\   NAME-BODY      a TYPED-VARIABLE holding the checked body, typed from the
\                  declaration (i32/u32 read as n, ptr u8 as ptr u8);
\   NAME-DISPATCH  ( -- ) reads the arguments from the marshal frame, runs the
\                  body under catch and leaves the result for C, or the declared
\                  fallback after a throw; a body never stored throws
\                  E-FFI-CALLBACK-STATE.
\ Arguments are register arguments only: n, i32, u32, r and ptr u8, at most
\ INT-REGS integer and FLOAT-REGS float ones. A result is n, i32, u32, r or
\ none; a value result needs `n FALLBACK` (r: `r FFALLBACK`), the answer C gets
\ when the body throws. The throw code is kept for FAULT@ until CLEAR.
\ The body may call C again, but a nested call shares the task's FFI scratch,
\ so a scratch area C reads through a pointer is not safe across it. It may
\ not UNBIND its own slot, keep a ptr u8 argument past its return, or define:
\ exit $4F with a task live, exit 106 otherwise. TASK:PAUSE inside it only
\ yields, so TASK:HALT and TASK:KILL take effect after C returns.
\ A declaration defines words: declare every callback before the first task
\ starts.
: CALLBACK: ( -- )
   FFI-CB:OPEN-CALLBACK ;

: FALLBACK ( n -- )
   FFI-CB:SET-FALLBACK ;

: FFALLBACK ( r -- )
   FFI-CB:SET-FFALLBACK ;

: ;CALLBACK ( -- )
   FFI-CB:CLOSE-CALLBACK ;
