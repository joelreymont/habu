\ icode.f - X64CODE, the x86-64 code layer: ASM-SINK, the sink X64ASM
\ (src/arch/x86-64/asm.f) appends every instruction to, and the labels a stream
\ branches to; the counterpart of src/arch/arm64/icode.f. The seam
\ (src/os/linux-x86-64/sys.f, proc-watch.f) and the image writer (elf.f) bind to
\ it with `using X64CODE`. Refusals end the build with REFUSE-RC, the code
\ src/arch/arm64/icode.f uses; test/x86-64-emit.f runs each one.

require src/lib/errors.f
require src/lib/byte-buffer.f
require src/core/roles.f
require src/arch/x86-64/asm.f
require src/os/linux-x86-64/target-layout.f
require src/habu/layout.f

package X64CODE
using X64ASM
public

\ The text window holds the hand-written kernel alone (`_start`, the helpers,
\ every row) past the header page; elf.f MPAGE refuses a longer stream and
\ src/os/image-bytes.f sizes the image buffer from it. The boot maps the
\ captured engine's code region at VMBASE plus REGION-OFF (src/habu/boot-x64.f),
\ so the window and its read-write tail end below that. rel32 reaches the whole
\ image, so the bound is ARM64's engine code allowance (arm64/icode.f ADR-HI).
$100000 constant CODE-CAP-BYTES

private

72 constant REFUSE-RC
-1 constant UNBOUND

\ JMP, JCC, CALL, (rel32), JMP8, JCC8, (rel8) and MOVABS, (abs64) emit a zero
\ field, the last bytes of the instruction, and record it as a site of the
\ label; a rel is measured from the field's end. Label numbers count up across
\ streams and are never reused, so a label held past its stream's link or reset
\ names no label of the next one. A site's kind names its field's width.
0 constant SITE-REL8
1 constant SITE-REL32
2 constant SITE-ABS64

create SINK BUF:HDR-BYTES allot
variable NLBL                        \ labels made since load
variable LBL-BASE                    \ the number of this stream's first label
variable NSITE                       \ sites this stream has recorded
DYNAMIC-BUFFER LBL-AT n              \ label less LBL-BASE -> its offset, or UNBOUND
DYNAMIC-BUFFER SITE-AT n             \ site -> the byte offset of its field
DYNAMIC-BUFFER SITE-LBL n            \ site -> its label's index in LBL-AT
DYNAMIC-BUFFER SITE-KIND n           \ site -> SITE-REL8, SITE-REL32 or SITE-ABS64

public

\ The driver owns ASM-SINK's lifetime: `ASM-SINK n BUF:N>BLEN BUF:INIT` before
\ the first byte and `ASM-SINK BUF:DISPOSE` when the build is done. CODE and
\ ASM-LEN fail closed on a sink that is not live.
: ASM-SINK ( -- ptr u8 ) SINK ;
: CODE ( -- ptr u8 ) SINK BUF:SPAN$ drop ;
: ASM-LEN ( -- n ) SINK BUF:LEN@ BUF:BLEN>N ;

private

: REFUSE ( ptr u8 n -- ) REFUSE-RC die ;

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
\ empties the sink with BUF:CLEAR instead of ASM-RESET, catching only the sites
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
   CODE x SITE-AT @ + {: field:ptr :}
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
   0 >REL SINK ENC-JMP-REL32
   k SITE-REL32 END-SITE ;

: JMP8, ( label -- )
   ?KNOWN {: k:n :}
   0 >REL SINK ENC-JMP-REL8
   k SITE-REL8 END-SITE ;

: JCC, ( condition label -- )
   ?KNOWN {: c:condition k:n :}
   c 0 >REL SINK ENC-JCC-REL32
   k SITE-REL32 END-SITE ;

: JCC8, ( condition label -- )
   ?KNOWN {: c:condition k:n :}
   c 0 >REL SINK ENC-JCC-REL8
   k SITE-REL8 END-SITE ;

: CALL, ( label -- )
   ?KNOWN {: k:n :}
   0 >REL SINK ENC-CALL-REL32
   k SITE-REL32 END-SITE ;

\ The site is X64ASM's own statement of where the immediate starts.
: MOVABS, ( r64 label -- )
   ?KNOWN {: r:r64 k:n :}
   ASM-LEN MOV-RI64-IMM-OFF + {: at:n :}
   r 0 >IMM64 SINK ENC-MOV-RI64
   at k SITE-ABS64 SITE+ ;

\ Read before ASM-LINK forgets it: the number the link adds the load address
\ to, as src/habu/link-x64.f reads each kernel body's entry into the record that
\ names it. An unbound label is refused as the link refuses it.
: LABEL-AT ( label -- n )
   ?KNOWN LBL-AT @ {: at:n :}
   at UNBOUND = if s" x64code: unresolved label" REFUSE then
   at ;

\ The full engine keeps the assembler's exact text sites after ASM-LINK
\ forgets its temporary labels. A u32 count and five-byte rows start at the
\ returned code end; the page-aligned footer repeats that offset and count and
\ ends with "X64SITE1". A restored snapshot can find this immutable footer
\ behind its appended heap without scanning instruction bytes.
$3145544953343658 constant TEXT-SITE-MAGIC

: U32, ( n -- ) {: v:n :}
   4 0 ?do v i 8 * rshift $FF and SINK BUF:APPEND-BYTE loop ;
: U64, ( n -- ) {: v:n :}
   8 0 ?do v i 8 * rshift $FF and SINK BUF:APPEND-BYTE loop ;

: TEXT-SITES, ( -- n )
   ASM-LEN {: code-end:n :}
   NSITE @ {: count:n :}
   count U32,
   count 0 ?do
      i SITE-END code-end > if s" x64code: text site past code" REFUSE then
      i SITE-AT @ U32,
      i SITE-KIND @ SINK BUF:APPEND-BYTE
   loop
   X64LAYOUT:CODE-OFF ASM-LEN + 16 + PROT-PAGE-MAX 1- +
   PROT-PAGE-MAX 1- invert and X64LAYOUT:CODE-OFF - {: end:n :}
   end CODE-CAP-BYTES > if s" x64code: text sites exceed code window" REFUSE then
   end 16 - ASM-LEN - 0 ?do 0 SINK BUF:APPEND-BYTE loop
   code-end U32,  count U32,  TEXT-SITE-MAGIC U64,
   code-end ;

\ base is the address byte 0 of the stream loads at. Every site is checked before
\ any byte is patched; then the stream's labels and sites are forgotten.
: ASM-LINK ( n -- ) {: base:n :}
   NSITE @ 0 ?do base i SITE-CHECK loop
   NSITE @ 0 ?do base i SITE-PATCH loop
   FORGET ;

\ Empties the sink and forgets this stream's labels and sites, so nothing of one
\ stream is patched into the next.
: ASM-RESET ( -- )
   SINK BUF:CLEAR
   FORGET ;

;using
;package
