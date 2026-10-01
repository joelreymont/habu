\ icode.f - the x86_64 code layer, package X64CODE.
\
\ X64ASM (src/arch/x86-64/asm.f) encodes an instruction by appending its bytes
\ to a sink it is handed; this package is the sink every x86_64 code stream
\ appends into, and the labels that stream branches to. It is the x86_64
\ counterpart of src/arch/arm64/icode.f, and the seam (src/os/linux-x86-64/
\ sys.f, proc-watch.f) and the image writer (elf.f) bind to it with
\ `using X64CODE`.
\
\ THE STREAM. ASM-SINK is a lib/byte-buffer.f header. The driver owns its
\ lifetime: `ASM-SINK n BUF:N>BLEN BUF:INIT` before the first byte and
\ `ASM-SINK BUF:DISPOSE` when the build is done. Beginning another stream
\ belongs to this layer: ASM-RESET empties the sink and forgets the stream's
\ labels and sites in one step, so nothing of one stream is patched into the
\ next. CODE and ASM-LEN read the stream back, and both fail closed on a sink
\ that is not live.
\
\ LABELS AND SITES. LBL makes a label and LBL, binds it where the stream is now.
\ JMP, JCC, CALL, (rel32), JMP8, JCC8, (rel8) and MOVABS, (mov r64, imm64)
\ each emit their instruction with a zero field and record the field as a site
\ of the label. Every field is the last bytes of its instruction, so a rel is
\ measured from the field's end, the x86 rule. Nothing is patched until
\ ASM-LINK, which is given the address byte 0 of the stream loads at: a MOVABS
\ site takes its label's address, that base plus the label's offset (elf.f
\ ASM-CODE links at VMBASE CODE-OFF +, so an image cannot be written with a
\ site still zero). ASM-LINK checks every site before it patches any byte, then
\ forgets the stream's labels and sites as ASM-RESET does. Label numbers count
\ up across streams and are never reused, so a label held past the link or reset
\ that ended its stream names no label of the next one.
\
\ REFUSALS END THE BUILD. Each one dies with REFUSE-RC, the exit code
\ src/arch/arm64/icode.f's refusals use: an unresolved label, a rel8 or rel32
\ delta its field cannot hold, a label bound twice, a label this stream did not
\ make (one from a linked or reset stream, or one never made), refused where it
\ is used and before its instruction is emitted, and a site or label past the
\ end of the stream. The last is the backstop for a driver that empties the sink
\ with a raw BUF:CLEAR instead of ASM-RESET: its stream's sites outlive the
\ bytes they describe, and only the ones past the new end can be caught.
\ test/x86-64-emit.f runs each one.

require lib/errors.f
require lib/byte-buffer.f
require src/core/roles.f
require src/arch/x86-64/asm.f

package X64CODE
using X64ASM
public

\ THE TEXT WINDOW. The image writer takes the text segment as this many bytes
\ past its header page (elf.f MPAGE, which refuses a longer stream), and
\ src/os/image-bytes.f sizes the image buffer from it, so it is defined before
\ either loads. It holds the hand-written kernel alone: `_start`, the helpers
\ and every row. The captured engine is not in it; the boot maps the code
\ region fixed at the image base (VMBASE) plus REGION-OFF
\ (src/habu/boot-x64.f), so the window and the read-write tail behind it must
\ end below that. rel32 reaches the whole image, so no reach bounds the window,
\ and it takes the allowance ARM64 gives its engine code part
\ (src/arch/arm64/icode.f ADR-HI): 1 MiB.
$100000 constant CODE-CAP-BYTES

private

72 constant REFUSE-RC
-1 constant UNBOUND

\ A site's kind names its field's width.
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

\ The rel32 field at byte n, of an instruction this stream took whole rather
\ than encoded, as a site of the label: src/habu/kernel-x64.f PRIM-HIR appends
\ a compiled routine and links its calls this way.
: REL32-SITE ( n label -- )
   ?KNOWN {: at:n k:n :}
   at k SITE-REL32 SITE+ ;

\ The offset a label of this stream is bound at, before ASM-LINK forgets it: the
\ number the link adds the load address to. A writer that places bytes beside
\ the stream reads it there, as src/habu/link-x64.f reads each kernel body's
\ entry into the dictionary record that names it. An unbound label is refused
\ as the link refuses it.
: LABEL-AT ( label -- n )
   ?KNOWN LBL-AT @ {: at:n :}
   at UNBOUND = if s" x64code: unresolved label" REFUSE then
   at ;

\ Patch every site of the stream for code whose byte 0 loads at base, after
\ checking all of them, then forget the stream's labels and sites.
: ASM-LINK ( n -- ) {: base:n :}
   NSITE @ 0 ?do base i SITE-CHECK loop
   NSITE @ 0 ?do base i SITE-PATCH loop
   FORGET ;

\ Begin another stream: empty the sink and forget this one's labels and sites.
: ASM-RESET ( -- )
   SINK BUF:CLEAR
   FORGET ;

;using
;package
