\ icode.f - X64CODE, the x86-64 code layer: ASM-SINK, the sink X64ASM
\ (src/arch/x86-64/asm.f) appends every instruction to, and the labels a stream
\ branches to; the counterpart of src/arch/arm64/icode.f. The seam
\ (src/os/linux-x86-64/sys.f, proc-watch.f) and the image writer (elf.f) call
\ it by prefix, as `X64CODE:WORD`.

require src/core/exit-status.f
require src/lib/error.f
require src/core/roles.f
require src/arch/x86-64/asm.f
require src/os/linux/layout.f
require src/habu/layout.f
require src/lib/arena.f
require src/lib/span.f
require src/lib/vector.f
require src/compiler/native/relax.f

package X64CODE
public

\ The linked text and ELF's writable tail end below the runtime code region.
\ Reserve one protection page for the tail and its alignment.
REGION-OFF PROT-PAGE-MAX - LINUX-LAYOUT:CODE-OFF - constant CODE-CAP-BYTES

private

-1 constant UNBOUND

\ JMP, and JCC, (rel8, growing), CALL, and LEA-REL, (rel32) and MOVABS, (abs64)
\ end in a zero field, a site of the label; a rel counts from the field's end.
0 constant SITE-REL8
1 constant SITE-REL32
2 constant SITE-ABS64
3 constant SITE-BRANCH   \ a rel8 field that may grow, after its opcode, EB or 70+cc

\ The sink's bytes live in an arena: a cross-build runs this file on its host,
\ and the heap here is the target's.
TYPED-VARIABLE SINK-ARENA ^ARENA:arena
TYPED-VARIABLE SINK ^VEC:vector<u8>     \ NULL-PTR until SINK-OPEN makes it
TYPED-VARIABLE SPARE ^VEC:vector<u8>    \ placement's target, then the sink's place
variable NLBL            \ labels made since load; never reused, so a stale one is unknown
variable LBL-BASE                    \ the number of this stream's first label
variable NSITE                       \ sites this stream has recorded
DYNAMIC-BUFFER LBL-AT n              \ label less LBL-BASE -> its offset, or UNBOUND
DYNAMIC-BUFFER SITE-AT n             \ site -> the byte offset of its field
DYNAMIC-BUFFER SITE-LBL n            \ site -> its label's index in LBL-AT
DYNAMIC-BUFFER SITE-KIND n           \ site -> its SITE- kind
\ Control never passes on from the byte before an end: a row is the bytes
\ [END-FROM, END-AT), a run of data or an instruction's last byte. Rows ascend.
variable NEND
DYNAMIC-BUFFER END-FROMS n
DYNAMIC-BUFFER END-ATS n
variable DATA-END                    \ where the last row ends when it is data, or UNBOUND
variable LAST-LBL                    \ the last LBL, offset; a data row runs through no label

public

\ The driver owns ASM-SINK's lifetime: SINK-OPEN before the first byte and
\ SINK-CLOSE when the build is done.
: ASM-SINK ( -- ^VEC:vector<u8> ) SINK @ ;
: CODE ( -- ^u8 ) ASM-SINK VEC:BYTES STR:DATA ;
: ASM-LEN ( -- n ) ASM-SINK VEC:COUNT ;

private

: REJECT ( str -- ) EXIT-STATUS:ENCODE die ;

: SITE-WIDTH ( n -- n ) {: kind:n :}
   kind SITE-REL8 =  kind SITE-BRANCH =  or if 1 exit then
   kind SITE-REL32 = if 4 exit then
   8 ;

\ This stream's labels are the numbers from LBL-BASE below NLBL; the result is
\ the label's index in LBL-AT.
: ?KNOWN ( label -- n ) LABEL>N {: k:n :}
   k LBL-BASE @ < k NLBL @ >= or if s" x64code: unknown label" REJECT then
   k LBL-BASE @ - ;

: SITE+ ( n n n -- ) {: at:n k:n kind:n :}
   NSITE @ {: x:n :}
   x 1 + SITE-AT-RESERVE
   x 1 + SITE-LBL-RESERVE
   x 1 + SITE-KIND-RESERVE
   at x SITE-AT !
   k x SITE-LBL !
   kind x SITE-KIND !
   x 1 + NSITE ! ;

\ The instruction just emitted ends in the site's field.
: END-SITE ( n n -- ) {: k:n kind:n :}
   ASM-LEN kind SITE-WIDTH - k kind SITE+ ;

\ ---- linking -----------------------------------------------------------------
\ A MOVABS site takes base plus its label's offset (elf.f ASM-CODE links at
\ VMBASE CODE-OFF +, so no image is written with a site still zero). Rejected: an
\ unresolved label, a delta its field cannot hold, a label bound twice, a label
\ this stream did not make (rejected at use, before its instruction is emitted),
\ and a site or label past the end of the stream: the backstop for a driver that
\ empties the sink with VEC:RESIZE instead of ASM-RESET, catching only the sites
\ past the new end.
: ?IN-CODE ( n -- )
   ASM-LEN > if s" x64code: label or site past the end of the code" REJECT then ;

: SITE-TARGET ( n -- n ) {: x:n :}
   x SITE-LBL @ LBL-AT @ {: at:n :}
   at UNBOUND = if s" x64code: unresolved label" REJECT then
   at ;

: SITE-END ( n -- n ) {: x:n :}
   x SITE-AT @ x SITE-KIND @ SITE-WIDTH + ;

: SITE-VALUE ( n n -- n ) {: base:n x:n :}
   x SITE-KIND @ SITE-ABS64 = if base x SITE-TARGET + exit then
   x SITE-TARGET x SITE-END - ;

\ A rel8 field is in reach: placement chose its form so, and CUT only shortens
\ the distance from a kept site to its label.
: ?REACH ( n n -- ) {: v:n kind:n :}
   kind SITE-REL32 = if
      v -2147483648 < v 2147483647 > or if s" x64code: rel32 out of reach" REJECT then
   then ;

: SITE-CHECK ( n n -- ) {: base:n x:n :}
   x SITE-END ?IN-CODE
   x SITE-TARGET ?IN-CODE
   base x SITE-VALUE x SITE-KIND @ ?REACH ;

\ The field is written little-endian over the zeros its instruction left.
: SITE-PATCH ( n n -- ) {: base:n x:n :}
   base x SITE-VALUE {: v:n :}
   CODE x SITE-AT @ + {: field:^ :}
   x SITE-KIND @ SITE-WIDTH 0 ?do
      v i 8 * rshift $FF and  field i + c!
   loop ;

: FORGET ( -- )
   NLBL @ LBL-BASE !
   0 NSITE !
   0 NEND !
   UNBOUND DATA-END !
   UNBOUND LAST-LBL !
   LBL-AT-RELEASE
   SITE-AT-RELEASE
   SITE-LBL-RELEASE
   SITE-KIND-RELEASE
   END-FROMS-RELEASE
   END-ATS-RELEASE ;

public

\ Callers add rows in ascending order.
: END+ ( n n -- ) {: from:n at:n :}
   NEND @ {: x:n :}
   x 1 + END-FROMS-RESERVE
   x 1 + END-ATS-RESERVE
   from x END-FROMS !
   at x END-ATS !
   x 1 + NEND ! ;

: ENDS ( -- n ) NEND @ ;
: END-FROM ( n -- n ) END-FROMS @ ;
: END-AT ( n -- n ) END-ATS @ ;

private

\ The instruction just emitted passes control nowhere below it.
: ENDED ( -- ) ASM-LEN dup 1 - swap END+ ;

\ The bytes from the offset on are data.
: DATA+ ( n -- ) {: from:n :}
   ASM-LEN from = if exit then
   DATA-END @ from =  LAST-LBL @ from <>  and if
      ASM-LEN NEND @ 1 - END-ATS !
   else from ASM-LEN END+ then
   ASM-LEN DATA-END ! ;

public

: LBL ( -- label )
   NLBL @ {: k:n :}
   k LBL-BASE @ - {: x:n :}
   x 1 + LBL-AT-RESERVE
   UNBOUND x LBL-AT !
   k 1 + NLBL !
   k >LABEL ;

: LBL, ( label -- )
   ?KNOWN {: k:n :}
   k LBL-AT @ UNBOUND <> if s" x64code: label redefined" REJECT then
   ASM-LEN k LBL-AT !
   ASM-LEN LAST-LBL ! ;

\ Each site's label is checked before its instruction is emitted.
: JMP, ( label -- )
   ?KNOWN {: k:n :}
   0 X64ASM:>REL ASM-SINK X64ASM:ENC-JMP-REL8
   k SITE-BRANCH END-SITE
   ENDED ;

: RET, ( -- ) ASM-SINK X64ASM:ENC-RET ENDED ;
: UD2, ( -- ) ASM-SINK X64ASM:ENC-UD2 ENDED ;
: JMP-REG, ( X64ASM:r64 -- ) ASM-SINK X64ASM:ENC-JMP-REG ENDED ;

: JCC, ( X64ASM:condition label -- )
   ?KNOWN {: c:X64ASM:condition k:n :}
   c 0 X64ASM:>REL ASM-SINK X64ASM:ENC-JCC-REL8
   k SITE-BRANCH END-SITE ;

: CALL, ( label -- )
   ?KNOWN {: k:n :}
   0 X64ASM:>REL ASM-SINK X64ASM:ENC-CALL-REL32
   k SITE-REL32 END-SITE ;

\ lea r, [rip+disp32]: a rip-relative displacement is measured from the
\ instruction's end, which is its field's end.
: LEA-REL, ( X64ASM:r64 label -- )
   ?KNOWN {: r:X64ASM:r64 k:n :}
   r 0 X64ASM:MEM-RIP ASM-SINK X64ASM:ENC-LEA
   k SITE-REL32 END-SITE ;

\ The site is X64ASM's own statement of where the immediate starts.
: MOVABS, ( X64ASM:r64 label -- )
   ?KNOWN {: r:X64ASM:r64 k:n :}
   ASM-LEN X64ASM:MOV-RI64-IMM-OFF + {: at:n :}
   r 0 X64ASM:>IMM64 ASM-SINK X64ASM:ENC-MOV-RI64
   at k SITE-ABS64 SITE+ ;

\ Read before ASM-LINK forgets it: the number the link adds the load address
\ to. An unbound label is rejected as the link rejects it.
: LABEL-AT ( label -- n )
   ?KNOWN LBL-AT @ {: at:n :}
   at UNBOUND = if s" x64code: unresolved label" REJECT then
   at ;

: U64, ( n -- ) {: v:n :}
   ASM-LEN {: from:n :}
   8 0 ?do v i 8 * rshift $FF and ASM-SINK VEC:PUSH c! loop
   from DATA+ ;

private

\ ---- placing growing branches ------------------------------------------------
: GROWING? ( -- bool )
   false NSITE @ 0 ?do i SITE-KIND @ SITE-BRANCH = or loop ;

\ Branch k's growth, its opcode read at its raw first byte; one whose label is
\ not yet bound takes the form that reaches wherever the label lands.
: GROWTH ( n n -- n ) {: k:n d:n :}
   CODE k RELAX:BR-AT @ + c@
   k RELAX:BR-TO @ UNBOUND = if X64ASM:BRANCH-LONG exit then
   d X64ASM:BRANCH-GROWTH ;

: FILE-BRANCHES ( -- )
   RELAX:RESET
   NSITE @ 0 ?do
      i SITE-KIND @ SITE-BRANCH = if
         i SITE-END ?IN-CODE
         i SITE-END X64ASM:BRANCH-HOLE -  i SITE-LBL @ LBL-AT @  X64ASM:BRANCH-HOLE RELAX:BRANCH+ drop
      then
   loop ;

\ The placed bytes in SPARE, each branch in its final form with a zero displacement.
: PLACED-CODE ( -- ^VEC:vector<u8> )
   SPARE @ {: new:^ :}
   new 0 VEC:RESIZE
   ASM-SINK new s" " RELAX:COPY
   RELAX:BRANCHES 0 ?do
      i RELAX:BR-AT @ {: at:n :}
      new at RELAX:PLACED VEC:AT  CODE at + c@  i RELAX:BR-LEN @  0 X64ASM:BRANCH-PUT
   loop
   new ;

\ Branch k's site x: its field ends the branch's final form.
: BRANCH-SITE ( n n -- ) {: k:n x:n :}
   k RELAX:BR-LEN @ {: growth:n :}
   growth 0= if SITE-REL8 else SITE-REL32 then {: kind:n :}
   k RELAX:BR-AT @ RELAX:PLACED  X64ASM:BRANCH-HOLE +  growth +  kind SITE-WIDTH -
   x SITE-AT !
   kind x SITE-KIND ! ;

: PLACE-SITES ( -- )
   0
   NSITE @ 0 ?do
      i SITE-KIND @ SITE-BRANCH = if  dup i BRANCH-SITE  1 +
      else  i SITE-AT @ RELAX:PLACED  i SITE-AT !  then
   loop drop ;

: PLACE-LABELS ( -- )
   NLBL @ LBL-BASE @ - 0 ?do
      i LBL-AT @ {: at:n :}
      at UNBOUND <> if  at RELAX:LANDED  i LBL-AT !  then
   loop ;

\ A row keeps its length and is anchored at its end.
: PLACE-ENDS ( -- )
   NEND @ 0 ?do
      i END-AT  i END-FROM -  {: len:n :}
      i END-AT RELAX:PLACED {: at:n :}
      at len -  i END-FROMS !
      at i END-ATS !
   loop
   DATA-END @ UNBOUND <> if  DATA-END @ RELAX:PLACED  DATA-END !  then
   LAST-LBL @ UNBOUND <> if  LAST-LBL @ RELAX:PLACED  LAST-LBL !  then ;

public

\ Before a driver reads a code offset: each growing branch takes the shortest
\ form that reaches, rel32 while its label is unbound; no placed byte moves again.
: PLACE-BRANCHES ( -- )
   GROWING? 0= if exit then
   FILE-BRANCHES
   0 ['] GROWTH RELAX:SIZE
   PLACE-SITES PLACE-LABELS PLACE-ENDS
   PLACED-CODE  SINK @ SPARE !  SINK ! ;

\ The linker keeps labels through map writing. Other callers finish the stream
\ with ASM-LINK, which also releases its labels and sites.
: FIXUPS! ( n -- ) {: base:n :}
   PLACE-BRANCHES
   NSITE @ 0 ?do base i SITE-CHECK loop
   NSITE @ 0 ?do base i SITE-PATCH loop ;

: ASM-LINK ( n -- ) FIXUPS! FORGET ;

\ Empties the sink and forgets this stream's labels and sites, so nothing of one
\ stream is patched into the next.
: ASM-RESET ( -- )
   ASM-SINK 0 VEC:RESIZE
   FORGET ;

\ Kernel objects retain labels and relocation sites until final placement.
: LABEL-COUNT ( -- n ) NLBL @ LBL-BASE @ - ;
: LABEL-POS ( n -- n ) LBL-AT @ ;
: LABEL-INDEX ( label -- n ) ?KNOWN ;
: LABEL-REF ( n -- label ) LBL-BASE @ + >LABEL ;
: LABEL-BOUND? ( label -- bool ) ?KNOWN LBL-AT @ UNBOUND <> ;
: LABEL-POS! ( n n -- ) LBL-AT ! ;
: SITES ( -- n ) NSITE @ ;
: REF-SITE ( n -- n ) SITE-AT @ ;
: REF-LABEL ( n -- n ) SITE-LBL @ ;
: REF-KIND ( n -- n ) SITE-KIND @ ;
\ Callers add sites in ascending order, as a kernel object holds them: the
\ order PLACE-BRANCHES needs.
: REF+ ( n n n -- ) SITE+ ;

: SINK-OPEN ( -- )
   SINK @ NULL-PTR <> if ASM-RESET else
      ARENA:NEW SINK-ARENA !
      SINK-ARENA @ ARENA:ALLOCATOR 0 [type] u8 VEC:NEW SINK !
      SINK-ARENA @ ARENA:ALLOCATOR 0 [type] u8 VEC:NEW SPARE !
   then ;

: SINK-CLOSE ( -- )
   FORGET  SINK-ARENA @ ARENA:RELEASE  NULL-PTR SINK-ARENA !  NULL-PTR SINK !  NULL-PTR SPARE ! ;

: BYTES, ( str -- )
   ASM-LEN {: from:n :}
   ASM-SINK VEC:APPEND
   from DATA+ ;

: SPACE ( n -- ^u8 )
   ASM-LEN {: k:n at:n :}
   ASM-SINK at k + VEC:ENSURE
   ASM-SINK VEC:SPAN at SPAN:SKIP SPAN:$ STR:DATA ;

private

\ CUT's map: for each offset from CUT-FROM to ASM-LEN, the kept bytes below it.
variable CUT-FROM
DYNAMIC-BUFFER BELOW n

: BELOW-CELL ( n -- ^n ) CUT-FROM @ - BELOW ;

: RANGE@ ( ^n n -- n n ) {: ranges:^ k:n :}
   ranges k 2 * cells + {: r:^ :}
   r @  r cell+ @ ;

: MALFORMED ( -- ) s" x64code: cut ranges malformed" REJECT ;

\ A range may start where the previous one ends.
: ?RANGES ( ^n n n -- ) {: ranges:^ k:n from:n :}
   k 0 <  from 0 <  or if MALFORMED then
   from
   k 0 ?do
      ranges i RANGE@ {: a:n b:n :}
      dup a >  a b >= or if MALFORMED then
      drop b
   loop
   ASM-LEN > if MALFORMED then ;

\ Each kept byte's cell 1, the others' 0.
: MARK ( ^n n -- ) {: ranges:^ k:n :}
   ASM-LEN CUT-FROM @ - 1 + BELOW-RESERVE
   ASM-LEN 1 +  CUT-FROM @ ?do 0 i BELOW-CELL ! loop
   k 0 ?do
      ranges i RANGE@ swap ?do 1 i BELOW-CELL ! loop
   loop ;

: TALLY ( -- )
   0
   ASM-LEN 1 +  CUT-FROM @ ?do
      i BELOW-CELL @  over i BELOW-CELL !  +
   loop
   drop ;

\ An offset after the cut; an unbound label's stays UNBOUND.
: MOVED ( n -- n ) {: p:n :}
   p CUT-FROM @ <  p ASM-LEN >  or if p exit then
   CUT-FROM @  p BELOW-CELL @  + ;

\ The byte at the offset is cut; the end of the code has none.
: GONE? ( n -- bool ) {: p:n :}
   p CUT-FROM @ <  p ASM-LEN >=  or if false exit then
   p BELOW-CELL @  p 1 + BELOW-CELL @  = ;

\ Some of the bytes [a, b) are kept and some are cut.
: SPLIT? ( n n -- bool ) {: a:n b:n :}
   b MOVED a MOVED - {: kept:n :}
   kept 0<>  kept b a - <>  and ;

: ?SITE ( n -- ) {: x:n :}
   x SITE-AT @  x SITE-END  SPLIT? if
      s" x64code: a cut splits a site" REJECT
   then
   x SITE-AT @ GONE? if exit then
   x SITE-LBL @ LBL-AT @ GONE? if
      s" x64code: a kept site names cut code" REJECT
   then ;

: ?END ( n -- ) {: x:n :}
   x END-FROM  x END-AT  SPLIT? if
      s" x64code: a cut splits data" REJECT
   then ;

\ Ascending: each byte moves to its own offset or below it.
: MOVE-CODE ( ^n n -- ) {: ranges:^ k:n :}
   k 0 ?do
      ranges i RANGE@ swap ?do
         CODE i + c@  CODE i MOVED + c!
      loop
   loop ;

: MOVE-SITE ( n n -- ) {: x:n to:n :}
   x SITE-AT @ MOVED to SITE-AT !
   x SITE-LBL @ to SITE-LBL !
   x SITE-KIND @ to SITE-KIND ! ;

: MOVE-SITES ( -- )
   0
   NSITE @ 0 ?do
      i SITE-AT @ GONE? 0= if i over MOVE-SITE 1 + then
   loop
   NSITE ! ;

: MOVE-END ( n n -- ) {: x:n to:n :}
   x END-FROM MOVED to END-FROMS !
   x END-AT MOVED to END-ATS ! ;

: MOVE-ENDS ( -- )
   0
   NEND @ 0 ?do
      i END-FROM GONE? 0= if i over MOVE-END 1 + then
   loop
   NEND !
   UNBOUND DATA-END ! ;

: MOVE-LABELS ( -- )
   LABEL-COUNT 0 ?do
      i LBL-AT @ {: at:n :}
      at GONE? if UNBOUND else at MOVED then i LBL-AT !
   loop ;

public

\ CUT ( ranges count from -- ) keeps only the ranges' bytes of [from, ASM-LEN),
\ moved down with their sites, ends and labels; ranges: count ascending
\ [start, end) pairs.
: CUT ( ^n n n -- ) {: ranges:^ k:n from:n :}
   ranges k from ?RANGES
   from CUT-FROM !
   ranges k MARK  TALLY
   NSITE @ 0 ?do i ?SITE loop
   NEND @ 0 ?do i ?END loop
   ranges k MOVE-CODE
   MOVE-SITES MOVE-ENDS MOVE-LABELS
   ASM-SINK ASM-LEN MOVED VEC:RESIZE
   BELOW-RELEASE ;

;package
