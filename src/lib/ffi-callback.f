split-str
\ ffi-callback.f - C callbacks into checked Habu: CALLBACK: NAME ( in -- out )
\ clauses ;CALLBACK declares a C entry; FFI-CB:ENTRY binds it to a context
\ (src/lib/task.f CONTEXT-BIND) and answers the C function pointer.
\ STORAGE CLASS. PROCESS-WIDE: the slots, descriptor and dispatch tables and
\ fault cells are one set for the image. The tables grow contiguously and never
\ give a slot back, so a C pointer stays valid; binding rows are task.f's.
require src/lib/error.f
require src/lib/string.f
require src/lib/codegen.f
require src/lib/ffi-abi.f
require src/lib/task.f
require src/habu/address-cells.f
require src/habu/data-floor.f

package FFI-CB
public

NEWTYPE callback

private
EXPORT BOOT:data-base
EXPORT CHECK:INTRINSIC
EXPORT CHECK:INTRINSIC-TOP-ONLY

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
CAST: N>CELLS ( n -- ^n )
CAST: N>FLOATS ( n -- ^r )
CAST: N>BYTES ( n -- ^u8 )

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

CAST: >DISPATCH-CELL ( ^n -- ^[ -- ] )
CAST: >FLOAT-CELL ( ^n -- ^r )
CAST: >ENTRY-XT ( n -- [ -- ] )

: ROW ( n -- ^n )
   ROW-BYTES * TABLE @ + CELL-VIEW ;

: DISPATCH ( n -- ^[ -- ] )
   ROW DISPATCH-OFF + >DISPATCH-CELL ;

: ENTRY-CELL ( n -- ^n ) ROW ENTRY-OFF + ;
: FAULTS ( n -- ^n ) ROW FAULT-OFF + ;
: FALLBACKS ( n -- ^n ) ROW FALLBACK-OFF + ;
: FFALLBACKS ( n -- ^r ) ROW FFALLBACK-OFF + >FLOAT-CELL ;

: ALIGNED-ROWS ( n -- ^u8 ) {: size:n :}
   align
   here BYTE-VIEW {: rows:^ :}
   size allot
   rows ;

\ Declarations precede task execution. Once the new table is published, old
\ rows are unreachable; clear their address rows and stored values so
\ the image scanner does not read former numeric fallback fields bytewise.
: RETIRE-TABLE ( ^u8 n -- ) {: old:^ cap:n :}
   cap 0= if exit then
   old BYTE-VIEW data-base BYTE-VIEW - cap ROW-BYTES *
      ADDRESS-CELLS:REMOVE-SPAN
   cap ROW-BYTES * CELL / 0 ?do 0 old i cells + CELL-VIEW ! loop ;

: GROW ( -- )
   TABLE @ {: old:^ :}
   CAP @ {: oldcap:n :}
   oldcap dup 0= if drop 1 else 2 * then {: next:n :}
   next ROW-BYTES * ALIGNED-ROWS {: rows:^ :}
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

\ The live marshal frame of the innermost callback on this region. The frame
\ words run only inside a callback, on the indexes its declaration admits;
\ nothing checks either.
: FRAME ( -- n )
   data-base CB-FRAME + @ ;

public

: ARG@ ( n -- n ) {: i:n :}
   FRAME i cells + N>CELLS @ ;

: I32-ARG@ ( n -- n )
   ARG@ $FFFFFFFF and dup $80000000 and 0 <> if $FFFFFFFF00000000 or then ;

: U32-ARG@ ( n -- n )
   ARG@ $FFFFFFFF and ;

: PTR-ARG@ ( n -- ^u8 )
   ARG@ N>BYTES ;

: FARG@ ( n -- r ) {: i:n :}
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
: BODY-CHECK ( ^a -- )
   BYTE-VIEW CELL-VIEW @ 0= if E-FFI-CALLBACK-STATE throw then ;

: INSTALL ( [ -- ] n -- ) {: q k:n :}
   q k DISPATCH xt! ;

: ENTRY-ADDRESS ( n -- n ) {: k:n :}
   k ENTRY-CELL @ ;

: INSTALL-ENTRY ( n n -- ) {: addr:n k:n :}
   addr >ENTRY-XT k ENTRY-CELL >DISPATCH-CELL xt! ;

: DESCRIPTOR ( n -- callback ) {: k:n :}
   k >CALLBACK ;

\ The callbacks declared so far: their slots run from 0 to DECLARED - 1.
: DECLARED ( -- n ) SLOTS @ ;

\ Publishes the dispatch table into the main region and binds the slot before
\ answering the stub's address. A call on an unbound slot, on a context busy on
\ another thread or on one with no outbound call in flight fails closed: the
\ thunk exits 106 (EXIT-STATUS:CALLBACK). A context admits one thread at a
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

CODEGEN:BUFFER NAME
CODEGEN:BUFFER WORD-NAME             \ NAME and a suffix
CODEGEN:BUFFER ROW-TEXT              \ the body's row

0 constant BY-N
1 constant BY-I32
2 constant BY-U32
3 constant BY-PTR
4 constant BY-R

\ The argument reads, in order: each one's register index and reader (BY-*).
INT-REGS FLOAT-REGS + TYPED-BUFFER READ-AT n
INT-REGS FLOAT-REGS + TYPED-BUFFER READ-BY n
variable READS

variable RES-KIND
variable FB-SET
variable FB-VALUE
TYPED-VARIABLE FFB-VALUE r
variable OPEN

\ A message that ends quoting the token the user wrote.
: TOKEN-REJECT ( str str str -- )
   {: m:^ mu:n t:^ tu:n h:^ hu:n :}
   FFI:SYNTAX-OPEN  m mu REJECTION:EMIT-STR
   s" '" REJECTION:EMIT-STR  t tu REJECTION:EMIT-STR
   s" '" REJECTION:EMIT-STR  h hu FFI:HELP-STOP ;

\ The next token src gives. Its bytes stay where the source holds them while
\ the declaration is read, so it lives on the stack.
: TOK@ ( [ -- str ] -- str )
   execute {: a:^ u:n :}
   u 0= if
      s" the text ends inside the callback declaration"
      s" complete it: CALLBACK: NAME ( in -- out ) clauses ;CALLBACK"
      FFI:SYNTAX-REJECT
   then
   a u ;

: TOO-MANY ( n str -- )
   {: cap:n k:^ ku:n :}
   FFI:ARITY-OPEN
   s" a callback takes at most " REJECTION:EMIT-STR
   cap REJECTION:EMIT-U  k ku REJECTION:EMIT-STR
   s" pass the rest in memory through a ^u8 argument"
   FFI:HELP-STOP ;

\ A ':' at either end of the name, with nothing on one side.
: EDGE-COLON? ( str -- bool ) {: a:^ u:n :}
   a c@ [char] : =  a u + 1 - c@ [char] : = or ;

: COLON-HELP$ ( -- str )
   s" name it NAME, or PACKAGE:NAME for a word in package PACKAGE" ;

\ The name's words are NAME, NAME-BODY and the like: one ':' between a
\ package and a word makes them the package's; any other ':' names nothing.
: NAME! ( str -- ) {: a:^ u:n :}
   a u [char] : STR:COUNT-CHAR 1 > if
      s" more than one ':' in callback name " a u COLON-HELP$
      TOKEN-REJECT
   then
   a u EDGE-COLON? if
      s" an empty package or word beside ':' in callback name " a u
      COLON-HELP$ TOKEN-REJECT
   then
   NAME CODEGEN:RESET
   a u NAME CODEGEN:APPEND-STRING ;

: NAME$ ( -- str )
   NAME CODEGEN:CONTENTS ;

: NAME+ ( str -- str ) {: a:^ u:n :}
   WORD-NAME CODEGEN:RESET
   NAME$ WORD-NAME CODEGEN:APPEND-STRING
   a u WORD-NAME CODEGEN:APPEND-STRING
   WORD-NAME CODEGEN:CONTENTS ;

: ARG+ ( n n -- ) {: i:n by:n :}
   i READS @ READ-AT !
   by READS @ READ-BY !
   1 READS +! ;

\ One integer argument read by reader BY. The input words carry two counts on
\ the stack: the integer, then the float arguments read so far.
: INT+ ( n n n -- n n )
   {: i:n f:n by:n :}
   i INT-REGS >= if INT-REGS s"  integer arguments" TOO-MANY then
   i by ARG+
   i 1 + f ;

: IN-TOKEN ( n n str -- n n )
   {: i:n f:n a:^ u:n :}
   a u s" n" STR:= if i f BY-N INT+ exit then
   a u s" i32" STR:= if i f BY-I32 INT+ exit then
   a u s" u32" STR:= if i f BY-U32 INT+ exit then
   a u 1 min s" ^" STR:= if
      a u s" ^u8" STR:= 0= if
         s" expected u8 after ^, found " a 1 + u 1 -
         s" a pointer argument is ^u8" TOKEN-REJECT
      then
      i f BY-PTR INT+ exit
   then
   a u s" r" STR:= if
      f FLOAT-REGS >= if FLOAT-REGS s"  float arguments" TOO-MANY then
      f BY-R ARG+
      i f 1 + exit
   then
   s" unknown argument type " a u
   s" an argument is n, i32, u32, r or ^u8" TOKEN-REJECT ;

: OUT-TOKEN ( str -- )
   {: a:^ u:n :}
   RES-KIND @ R-NONE <> if
      s" a callback has at most one result"
      s" keep one result after --: n, i32, u32 or r"
      FFI:SYNTAX-REJECT
   then
   a u s" r" STR:= if R-FLOAT RES-KIND ! exit then
   a u s" n" STR:= a u s" i32" STR:= or a u s" u32" STR:= or if
      R-VALUE RES-KIND ! exit
   then
   s" unknown result type " a u
   s" a result is n, i32, u32 or r; no result is nothing after --"
   TOKEN-REJECT ;

\ One input token: true at the -- that ends the inputs.
: INPUT? ( n n [ -- str ] -- n n bool )
   TOK@ {: i:n f:n a:^ u:n :}
   a u s" --" STR:= if i f true exit then
   a u s" )" STR:= if
      s" expected '--' before ')'"
      s" write the effect as ( in -- out )"
      FFI:SYNTAX-REJECT
   then
   i f a u IN-TOKEN false ;

\ One output token: true at the ) that ends the effect.
: OUTPUT? ( [ -- str ] -- bool )
   TOK@ {: a:^ u:n :}
   a u s" )" STR:= if true exit then
   a u s" --" STR:= if
      s" a second '--' in the effect"
      s" write one -- between the arguments and the result"
      FFI:SYNTAX-REJECT
   then
   a u OUT-TOKEN false ;

: INPUTS ( [ -- str ] -- ) {: src :} 0 0 begin src INPUT? until 2drop ;
: OUTPUTS ( [ -- str ] -- ) {: src :} begin src OUTPUT? until ;

: NEXT-SLOT ( -- n )
   SLOTS @ {: k:n :}
   k TASK:DECLARE-CALLBACK
   k CAP @ >= if GROW then
   1 SLOTS +!
   k ;

: READ-ITEMS ( n -- ) {: j:n :}
   j READ-AT @ literal
   j READ-BY @ {: by:n :}
   by BY-N = if postpone ARG@ exit then
   by BY-I32 = if postpone I32-ARG@ exit then
   by BY-U32 = if postpone U32-ARG@ exit then
   by BY-PTR = if postpone PTR-ARG@ exit then
   postpone FARG@ ;

: STORE-ITEM ( -- )
   RES-KIND @ R-VALUE = if postpone RESULT! exit then
   RES-KIND @ R-FLOAT = if postpone FRESULT! then ;

\ The word just made: each definer makes one record, none inside a definition.
: LATEST-XT ( -- [ -- ] )
   LATEST XREF-START >ENTRY-XT ;

\ The type of the argument reader BY (BY-*) hands the body.
: ARG-TYPE$ ( n -- str ) {: by:n :}
   by BY-PTR = if s" ^u8" exit then
   by BY-R = if s" r" exit then
   s" n" ;

: ROW+ ( str -- )
   ROW-TEXT CODEGEN:APPEND-STRING  32 ROW-TEXT CODEGEN:APPEND-BYTE ;

\ The body's row: the arguments as it takes them, then its result.
: BODY-ROW ( -- str )
   ROW-TEXT CODEGEN:RESET
   READS @ 0 ?do i READ-BY @ ARG-TYPE$ ROW+ loop
   s" --" ROW+
   RES-KIND @ R-FLOAT = if s" r" ROW+ then
   RES-KIND @ R-VALUE = if s" n" ROW+ then
   ROW-TEXT CODEGEN:CONTENTS 1 - ;

0 constant P-BODY
1 constant P-DISPATCH
2 constant P-ENTRY
3 constant P-NAME
4 constant P-COUNT

\ Word P (P-*) a declaration defines: its name and effect row, both held until
\ the next call, and whether it is the typed variable of a quotation of that row.
: PRODUCT ( n -- str str bool ) {: p:n :}
   p P-BODY = if s" -BODY" NAME+ BODY-ROW true exit then
   p P-DISPATCH = if s" -DISPATCH" NAME+ s" --" false exit then
   p P-ENTRY = if s" -ENTRY" NAME+ s" -- n" false exit then
   NAME$ s" -- FFI-CB:callback" false ;

\ A type a row spells. NAMED reads a qualified family as that package's
\ wherever the definer runs; a pointer takes READ-TYPE.
: ROW-TYPE ( str -- EFFECT:type ) {: a:^ u:n :}
   a c@ [char] ^ = if a u EFFECT:READ-TYPE exit then
   a u EFFECT:NAMED ;

\ The row's types from byte at on: inputs until --, outputs after it.
: ROW-ITEMS ( EFFECT:type bool str n -- EFFECT:type )
   {: out:bool a:^ u:n at:n :}
   a u 32 at STR:SPLIT-NEXT {: p:^ pu:n next:n more:bool :}
   more 0= if exit then
   p pu s" --" STR:= if true a u next RECURSE exit then
   p pu ROW-TYPE  out if EFFECT:OUT else EFFECT:IN then
   out a u next RECURSE ;

: ROW>EFFECT ( str -- EFFECT:type ) {: a:^ u:n :}
   EFFECT:EMPTY false a u 0 ROW-ITEMS ;

\ Opens product P: its typed variable, or its definition.
: OPEN-PRODUCT ( n -- )
   PRODUCT {: a:^ u:n row:^ rowu:n var:bool :}
   var if a u row rowu ROW>EFFECT EFFECT:VARIABLE-OF exit then
   a u row rowu ROW>EFFECT define-word ;

\ NAME-DISPATCH ( -- ): [: NAME-BODY BODY-CHECK <reads> NAME-BODY @ execute
\ <store> ;] k kind RUN
: DISPATCH-WORD ( [ -- ] n -- ) {: body k:n :}
   P-DISPATCH OPEN-PRODUCT
   s" [:" EFFECT:KEYWORD
   body compile,  postpone BODY-CHECK
   READS @ 0 ?do i READ-ITEMS loop
   body compile,  postpone @  postpone execute
   STORE-ITEM
   s" ;]" EFFECT:KEYWORD
   k literal  RES-KIND @ literal  postpone RUN  postpone ; ;

\ The accessor's ordinary native emission carries an anonymous C entry after
\ its Habu return. The native compiler publishes both as one owned record; only
\ the entry is handed to C, while the accessor can read its relocated
\ descriptor address.
: EMIT ( n -- ) {: k:n :}
   P-BODY OPEN-PRODUCT
   LATEST-XT k DISPATCH-WORD
   LATEST-XT k INSTALL
   k FFI:CALLBACK-ENTRY ARM-STUB
   P-ENTRY OPEN-PRODUCT
   k literal  postpone ENTRY-ADDRESS  postpone ;
   STUB-ENTRY k INSTALL-ENTRY
   P-NAME OPEN-PRODUCT
   k literal  postpone DESCRIPTOR  postpone ; ;

\ A rejection between the two keywords abandons the declaration, so nothing
\ is left open for the next one.
: REJECT ( str str -- )
   OPEN @ 0 <> if SOURCE-ROOT:CLOSED then
   0 OPEN !  FFI:SYNTAX-REJECT ;

\ The loader runs this on a declaration still open when its file ends.
: DISCARD ( -- )
   0 OPEN ! ;

: FALLBACK-REJECT ( -- )
   s" a fallback outside a callback declaration"
   s" write it between CALLBACK: NAME ( in -- out ) and ;CALLBACK" REJECT ;

public

\ Runs put on each word a declaration defines, as PRODUCT answers it.
: PRODUCTS ( [ str str bool -- ] -- ) {: put :}
   P-COUNT 0 ?do i PRODUCT put execute loop ;

\ Reads NAME ( in -- out ) from the tokens src gives and answers the name as
\ src gave it.
: READ-CALLBACK ( [ -- str ] -- str ) {: src :}
   R-NONE RES-KIND !  0 READS !
   \ A name may sit on a later line, so only '(' says it is missing.
   src TOK@ 2dup s" (" STR:= if
      s" expected a callback name, found " s" ("
      s" write CALLBACK: NAME ( in -- out )" TOKEN-REJECT
   then
   2dup NAME! {: na:^ nu:n :}
   src TOK@ {: a:^ u:n :}
   a u s" (" STR:= 0= if
      s" expected '(' after the callback name, found " a u
      s" write CALLBACK: NAME ( in -- out )" TOKEN-REJECT
   then
   src INPUTS
   src OUTPUTS
   na nu ;

: OPEN-CALLBACK ( -- )
   SRCLOC:TOKEN$ {: t:^ tu:n :}
   \ The load holds the open declaration's opener: it rejects it there.
   OPEN @ 0 <> if t tu s" ;CALLBACK" ['] DISCARD SOURCE-ROOT:OPENED then
   0 FB-SET !
   [: parse-name ;] READ-CALLBACK {: na:^ nu:n :}
   SRCLOC:TOKEN$ {: e:^ eu:n :}
   \ A claim is rejected at the name, whose words it claims: the name itself first.
   na nu SRCLOC:TOKEN!
   na nu na nu LBUF:CLAIM-NAME
   [: drop 2drop na nu LBUF:CLAIM-NAME ;] PRODUCTS
   e eu SRCLOC:TOKEN!
   1 OPEN !
   t tu s" ;CALLBACK" ['] DISCARD SOURCE-ROOT:OPENED ;

: SET-FALLBACK ( n -- ) {: v:n :}
   OPEN @ 0= if FALLBACK-REJECT then
   RES-KIND @ R-VALUE <> if
      s" FALLBACK needs an n, i32 or u32 result"
      s" an r result takes FFALLBACK; no result takes no fallback" REJECT
   then
   v FB-VALUE ! 1 FB-SET ! ;

: SET-FFALLBACK ( r -- ) {: v:r :}
   OPEN @ 0= if FALLBACK-REJECT then
   RES-KIND @ R-FLOAT <> if
      s" FFALLBACK needs an r result"
      s" an integer result takes FALLBACK; no result takes no fallback" REJECT
   then
   v FFB-VALUE ! 1 FB-SET ! ;

: CLOSE-CALLBACK ( -- )
   OPEN @ 0= if
      s" ;CALLBACK with no callback declaration open"
      s" open one with CALLBACK: NAME ( in -- out )"
      FFI:SYNTAX-REJECT
   then
   0 OPEN !  SOURCE-ROOT:CLOSED
   RES-KIND @ R-NONE <> FB-SET @ 0= and if
      s" a callback with a result needs a fallback"
      s" write n FALLBACK (r: r FFALLBACK) before ;CALLBACK"
      FFI:SYNTAX-REJECT
   then
   NEXT-SLOT {: k:n :}
   FB-VALUE @ k FALLBACKS !
   FFB-VALUE @ k FFALLBACKS !
   0 k FAULTS !
   k EMIT ;

global

\ CALLBACK: NAME ( in -- out ) clauses ;CALLBACK generates
\   NAME           ( -- FFI-CB:callback )  the descriptor;
\   NAME-BODY      a TYPED-VARIABLE holding the checked body, typed from the
\                  declaration (i32/u32 read as n, ^u8 as ^u8);
\   NAME-ENTRY     ( -- n ) the address of the C entry, the one C calls;
\   NAME-DISPATCH  ( -- ) reads the arguments from the marshal frame, runs the
\                  body under catch and leaves the result for C, or the declared
\                  fallback after a throw; a body never stored throws
\                  E-FFI-CALLBACK-STATE.
\ Arguments are register arguments only: n, i32, u32, r and ^u8, at most
\ INT-REGS integer and FLOAT-REGS float ones. A result is n, i32, u32, r or
\ none; a value result needs `n FALLBACK` (r: `r FFALLBACK`), the answer C gets
\ when the body throws. The throw code is kept for FAULT@ until CLEAR.
\ The body may call C again, but a nested call shares the task's FFI scratch,
\ so a scratch area C reads through a pointer is not safe across it. It may
\ not UNBIND its own slot, keep a ^u8 argument past its return, or define:
\ exit $4F with a task live, exit 106 otherwise. TASK:PAUSE inside it only
\ yields, so TASK:HALT and TASK:KILL take effect after C returns.
\ A declaration defines words: declare every callback before the first task
\ starts.
: CALLBACK: ( -- )
   OPEN-CALLBACK ;
INTRINSIC-TOP-ONLY INTRINSIC

: FALLBACK ( n -- )
   SET-FALLBACK ;

: FFALLBACK ( r -- )
   SET-FFALLBACK ;

: ;CALLBACK ( -- )
   CLOSE-CALLBACK ;
INTRINSIC-TOP-ONLY INTRINSIC

;package
