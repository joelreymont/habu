split-str
\ icode.f - X64CODE, the x86-64 code layer: ASM-SINK, the sink X64ASM
\ (src/arch/x86-64/asm.f) appends every instruction to, and the labels a stream
\ branches to; the counterpart of src/arch/arm64/icode.f. The seam
\ (src/os/linux-x86-64/sys.f, proc-watch.f) and the image writer (elf.f) bind to
\ it with `using X64CODE`.

require src/core/exit-status.f
require src/lib/error.f
require src/core/roles.f
require src/arch/x86-64/asm.f
require src/os/linux/layout.f
require src/habu/layout.f
require src/lib/heap.f
require src/lib/span.f
require src/lib/vector.f

package X64CODE
using X64ASM
public

\ The linked text and ELF's writable tail end below the runtime code region.
\ Reserve one protection page for the tail and its alignment.
REGION-OFF PROT-PAGE-MAX - LINUX-LAYOUT:CODE-OFF - constant CODE-CAP-BYTES

private

-1 constant UNBOUND

\ JMP, JCC, CALL, (rel32), JMP8, JCC8, (rel8) and MOVABS, (abs64) emit a zero
\ field, the last bytes of the instruction, and record it as a site of the
\ label; a rel is measured from the field's end. Label numbers count up across
\ streams and are never reused, so a label held past its stream's link or reset
\ names no label of the next one. A site's kind names its field's width.
0 constant SITE-REL8
1 constant SITE-REL32
2 constant SITE-ABS64

TYPED-VARIABLE SINK ^VEC:vector<u8>     \ NULL-PTR until SINK-OPEN makes it
variable NLBL                        \ labels made since load
variable LBL-BASE                    \ the number of this stream's first label
variable NSITE                       \ sites this stream has recorded
DYNAMIC-BUFFER LBL-AT n              \ label less LBL-BASE -> its offset, or UNBOUND
DYNAMIC-BUFFER SITE-AT n             \ site -> the byte offset of its field
DYNAMIC-BUFFER SITE-LBL n            \ site -> its label's index in LBL-AT
DYNAMIC-BUFFER SITE-KIND n           \ site -> SITE-REL8, SITE-REL32 or SITE-ABS64

public

\ The driver owns ASM-SINK's lifetime: SINK-OPEN before the first byte and
\ SINK-CLOSE when the build is done.
: ASM-SINK ( -- ^VEC:vector<u8> ) SINK @ ;
: CODE ( -- ^u8 ) ASM-SINK VEC:SPAN SPAN:$ drop ;
: ASM-LEN ( -- n ) ASM-SINK VEC:COUNT ;

private

: REFUSE ( str -- ) EXIT-STATUS:ENCODE die ;

: SITE-WIDTH ( n -- n ) {: kind:n :}
   kind SITE-REL8 = if 1 exit then
   kind SITE-REL32 = if 4 exit then
   8 ;

\ This stream's labels are the numbers from LBL-BASE below NLBL; the result is
\ the label's index in LBL-AT.
: ?KNOWN ( label -- n ) LABEL>N {: k:n :}
   k LBL-BASE @ < k NLBL @ >= or if s" x64code: unknown label" REFUSE then
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
\ VMBASE CODE-OFF +, so no image is written with a site still zero). Refused: an
\ unresolved label, a delta its field cannot hold, a label bound twice, a label
\ this stream did not make (refused at use, before its instruction is emitted),
\ and a site or label past the end of the stream: the backstop for a driver that
\ empties the sink with VEC:RESIZE instead of ASM-RESET, catching only the sites
\ past the new end.
: ?IN-CODE ( n -- )
   ASM-LEN > if s" x64code: label or site past the end of the code" REFUSE then ;

: SITE-TARGET ( n -- n ) {: x:n :}
   x SITE-LBL @ LBL-AT @ {: at:n :}
   at UNBOUND = if s" x64code: unresolved label" REFUSE then
   at ;

: SITE-END ( n -- n ) {: x:n :}
   x SITE-AT @ x SITE-KIND @ SITE-WIDTH + ;

: SITE-VALUE ( n n -- n ) {: base:n x:n :}
   x SITE-KIND @ SITE-ABS64 = if base x SITE-TARGET + exit then
   x SITE-TARGET x SITE-END - ;

: ?REACH ( n n -- ) {: v:n kind:n :}
   kind SITE-REL8 = if
      v -128 < v 127 > or if s" x64code: rel8 out of reach" REFUSE then
   then
   kind SITE-REL32 = if
      v -2147483648 < v 2147483647 > or if s" x64code: rel32 out of reach" REFUSE then
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
   LBL-AT-RELEASE
   SITE-AT-RELEASE
   SITE-LBL-RELEASE
   SITE-KIND-RELEASE ;

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
   k LBL-AT @ UNBOUND <> if s" x64code: label redefined" REFUSE then
   ASM-LEN k LBL-AT ! ;

\ Each site's label is checked before its instruction is emitted.
: JMP, ( label -- )
   ?KNOWN {: k:n :}
   0 >REL ASM-SINK ENC-JMP-REL32
   k SITE-REL32 END-SITE ;

: JMP8, ( label -- )
   ?KNOWN {: k:n :}
   0 >REL ASM-SINK ENC-JMP-REL8
   k SITE-REL8 END-SITE ;

: JCC, ( condition label -- )
   ?KNOWN {: c:condition k:n :}
   c 0 >REL ASM-SINK ENC-JCC-REL32
   k SITE-REL32 END-SITE ;

: JCC8, ( condition label -- )
   ?KNOWN {: c:condition k:n :}
   c 0 >REL ASM-SINK ENC-JCC-REL8
   k SITE-REL8 END-SITE ;

: CALL, ( label -- )
   ?KNOWN {: k:n :}
   0 >REL ASM-SINK ENC-CALL-REL32
   k SITE-REL32 END-SITE ;

\ The site is X64ASM's own statement of where the immediate starts.
: MOVABS, ( r64 label -- )
   ?KNOWN {: r:r64 k:n :}
   ASM-LEN MOV-RI64-IMM-OFF + {: at:n :}
   r 0 >IMM64 ASM-SINK ENC-MOV-RI64
   at k SITE-ABS64 SITE+ ;

\ Read before ASM-LINK forgets it: the number the link adds the load address
\ to. An unbound label is refused as the link refuses it.
: LABEL-AT ( label -- n )
   ?KNOWN LBL-AT @ {: at:n :}
   at UNBOUND = if s" x64code: unresolved label" REFUSE then
   at ;

: U64, ( n -- ) {: v:n :}
   8 0 ?do v i 8 * rshift $FF and ASM-SINK VEC:PUSH c! loop ;

\ The linker keeps labels through map writing. Other callers finish the stream
\ with ASM-LINK, which also releases its labels and sites.
: FIXUPS! ( n -- ) {: base:n :}
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
: REF+ ( n n n -- ) SITE+ ;

: SINK-OPEN ( -- )
   SINK @ NULL-PTR <> if ASM-RESET else
      HEAP:ALLOCATOR 0 [type] u8 VEC:NEW SINK !
   then ;

: SINK-CLOSE ( -- ) FORGET ASM-SINK VEC:DELETE  NULL-PTR SINK ! ;

: BYTES, ( str -- ) ASM-SINK VEC:APPEND ;

: SPACE ( n -- ^u8 )
   ASM-LEN {: k:n at:n :}
   ASM-SINK at k + VEC:ENSURE
   ASM-SINK VEC:SPAN at SPAN:SKIP SPAN:$ drop ;

;using
;package
