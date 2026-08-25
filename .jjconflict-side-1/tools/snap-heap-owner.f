\ snap-heap-owner.f - name the owner of a persisted DP-heap cell.
\
\ A snapshot image carries the whole DP heap verbatim, so any heap cell that
\ holds a live process address - an execution token in the JIT region, or a
\ pointer the host handed out through mmap - is wrong the moment the image is
\ restored somewhere else. Finding those cells is easy: build the image twice and
\ compare the two DATA payloads. But a byte offset does not say WHICH variable or
\ table is at fault, and guessing an owner from what a cell contains is exactly
\ the mistake this repair is not allowed to make.
\
\ Two maps close that gap, both read out of the live dictionary:
\
\   DUMP      `<heap offset> <name>` for every word that owns a piece of the DP
\             heap. The owner of a drifting offset is the last line whose offset
\             is not greater than it.
\   CODE-MAP  `<JIT region offset> <code length> <name>` for every word that has
\             code, plus the region base and heap top this run got. A program
\             counter caught by a debugger watchpoint on a drifting cell turns
\             into the name of the word that wrote it.
\
\ How a heap owner is recognised, exactly and without a value guess: `create`
\ (and `variable`, which is `create` plus eight bytes) compiles one fixed shape
\ and nothing else compiles that shape - the four-instruction MOVZ/MOVK x9
\ address chain from src/habu/habu2.f C-ADDR-RAW, the two-instruction push
\ stencil, and a return. EMIT-CREATE records the code length as (CP - start) - 4,
\ which is 24 for those seven instructions, so the recorded length plus the four
\ instruction encodings identify the shape completely. The address the word
\ pushes is then read straight out of the chain's immediate fields.
\
\ AND IT RECOGNISES THE ENGINE'S SHAPE ONLY, WHICH IS A DELIBERATE UNDER-REPORT.
\ The four encodings below name x9, because that is the register the engine's one
\ carrier C-ADDR-RAW writes into. The native chain emits address chains too, and
\ into whatever register its allocator picked - so the snapshot relocation pass
\ was widened to take the register off the site's own first word
\ (src/habu/habu2.f EMIT-ADDRS). THIS tool was NOT widened with it, and the
\ reason is that the two are asking different questions. A heap OWNER is a word
\ `create` compiled, and `create` is compiled by the engine in every build: the
\ chain never produces one. What the chain produces is code that USES a heap
\ address, which is not what either map above answers about. Widening the compare
\ to ignore the register would therefore add no owner and would accept four
\ move-wide words naming four DIFFERENT registers, whose immediates spell out no
\ address any word pushed - the exact shape the relocation pass refuses. So the
\ narrow compare stays, and the limit is stated here rather than left to be
\ discovered: a data word the chain ever comes to compile would be missing from
\ DUMP, and the fix then is the site record, not a looser instruction match.
\
\ Run it from a process that has the source under investigation loaded and has
\ not retired its dictionary yet. It reads the dictionary through
\ src/habu/xref.f and the four instruction encodings through src/habu/habu1.f,
\ and deliberately does not `require` either: the snapshot builder assembles its
\ source by inlining those files, so a `require` here would load a second copy.

package SNAP-HEAP-OWNER

\ code length EMIT-CREATE records for the seven-instruction create shape
24 constant BODY-LEN
\ everything in an instruction word except its 16-bit immediate field
$FFE0001F constant OPC-MASK
5 constant IMM-SHIFT
$FFFF constant IMM-MASK
$FFFFFFFF constant WORD-MASK
32 constant HALF-BITS
2 constant WORDS-PER-CELL
8 constant CELL-BYTES

: TRUE ( -- bool )
   0 0= ;

: FALSE ( -- bool )
   TRUE 0= ;

: CELL@ ( n -- n ) {: addr:n :}
   addr XREF-N>REC 0 XREF-CELL@ ;

\ instruction word `idx` of the code that starts at `base`
: INSTR@ ( n n -- n ) {: base:n idx:n :}
   base idx WORDS-PER-CELL / CELL-BYTES * + CELL@ {: pair:n :}
   idx WORDS-PER-CELL mod 0= if pair WORD-MASK and exit then
   pair HALF-BITS rshift WORD-MASK and ;

: OPC= ( n n -- bool ) {: instr:n opc:n :}
   instr OPC-MASK and opc = ;

: IMM ( n -- n ) {: instr:n :}
   instr IMM-SHIFT rshift IMM-MASK and ;

: CHAIN? ( n -- bool ) {: base:n :}
   base 0 INSTR@ W-MOVZ0 OPC= 0= if FALSE exit then
   base 1 INSTR@ W-MOVK1 OPC= 0= if FALSE exit then
   base 2 INSTR@ W-MOVK2 OPC= 0= if FALSE exit then
   base 3 INSTR@ W-MOVK3 OPC= ;

: CHAIN-VALUE ( n -- n ) {: base:n :}
   base 0 INSTR@ IMM
   base 1 INSTR@ IMM 16 lshift or
   base 2 INSTR@ IMM 32 lshift or
   base 3 INSTR@ IMM 48 lshift or ;

: HEAP-OFF ( n -- n ) {: addr:n :}
   addr XREF-N>REC data-base - ;

\ A package record carries raw wordlist roles in fields [0] and [8] instead of a
\ code pointer and a length - habu2.f EM-SNAPSHOT-REBASE-DICT skips them the same
\ way - so reading its start as an address would fault. Ask the record what it is.
: CODE? ( ptr a -- bool ) {: rec:ptr :}
   rec XREF-WORDLIST XREF-NAMESPACE-WL <> ;

: CREATED? ( ptr a -- bool ) {: rec:ptr :}
   rec CODE? 0= if FALSE exit then
   rec XREF-LEN BODY-LEN <> if FALSE exit then
   rec XREF-START CHAIN? ;

: HEAP-ROW ( ptr a -- ) {: rec:ptr :}
   rec XREF-START CHAIN-VALUE HEAP-OFF {: off:n :}
   off 0 < if exit then
   off .
   rec XREF-NAME$ type cr ;

: HEAP-SLOT ( n -- ) {: idx:n :}
   idx XREF-REC {: rec:ptr :}
   rec CREATED? 0= if exit then
   rec HEAP-ROW ;

: CODE-ROW ( ptr a -- ) {: rec:ptr :}
   rec CODE? 0= if exit then
   rec XREF-START dbase@ - {: off:n :}
   off 0 < if exit then
   off .
   rec XREF-LEN .
   rec XREF-NAME$ type cr ;

public

\ every heap owner in dictionary order: `<heap offset> <name>` per line
: DUMP ( -- )
   s" heap-owner heap-map" type cr
   ndict@ 0 ?do i HEAP-SLOT loop ;

\ the whole code map, headed by the region base and heap top this run got
: CODE-MAP ( -- )
   s" heap-owner region-base " type dbase@ .
   s" heap-owner heap-top " type here data-base - . cr
   ndict@ 0 ?do i XREF-REC CODE-ROW loop ;

;package
