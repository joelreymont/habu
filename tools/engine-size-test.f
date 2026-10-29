\ The sidecar's count belongs to framing; only its padded runs belong to the
\ sidecar row. Exercise both payload shapes on a private copy of bin/hb.
\ EST-MAIN measures that copy once: the reading cases run first, a case that
\ alters it restores what it wrote, and the payload case, which grows it, runs
\ last among them.
require src/lib/string.f
require src/lib/test.f
require src/lib/fs-mutate.f
require tools/image-size-lib.f

package IMAGE-SIZE
private

$4020 constant EST-X64-BYTES
create EST-X64-BUF EST-X64-BYTES allot
create EST-X64-PATH PATH-CAP allot
variable EST-X64-PATH-U

using X64LAYOUT   \ the guard: a bare layout name refuses (target-layout.f)

\ A stripped image as AOT-LINK's X64-TABLE-FOOTER, and X64IMAGE:WRITE-FD lay
\ it out: page-rounded text ending in the X64AOT table footer, then the RW
\ tail, an empty region LOAD and the read-only DATA carrier LOAD at the
\ region's end. The table at text offset $800 is the empty one the writer
\ emits for a window with no nonzero byte and no XT cell: a zero run count and
\ a zero XT count, 8 bytes, which the zeroed buffer already holds. Unlike the
\ writer's, this carrier holds $20 file bytes, so its row is file-backed.
$2000 constant EST-X64-TEXT
$800 constant EST-X64-TABLE
8 constant EST-X64-TABLE-BYTES

: EST-X64-PHDR ( n n n n n n -- )
   {: idx:n off:n va:n files:n mem:n flags:n :}
   64 idx 56 * + {: p:n :}
   1 EST-X64-BUF p + !
   flags EST-X64-BUF p 4 + + !
   off EST-X64-BUF p 8 + + !
   va EST-X64-BUF p 16 + + !
   va EST-X64-BUF p 24 + + !
   files EST-X64-BUF p 32 + + !
   mem EST-X64-BUF p 40 + + !
   $1000 EST-X64-BUF p 48 + + ! ;

: EST-X64-IMAGE ( -- )
   EST-X64-BYTES 0 ?do 0 EST-X64-BUF i + c! loop
   $464C457F EST-X64-BUF !
   2 EST-X64-BUF 4 + c!
   2 EST-X64-BUF 16 + c!
   62 EST-X64-BUF 18 + c!
   64 EST-X64-BUF 32 + !
   64 EST-X64-BUF 52 + !
   56 EST-X64-BUF 54 + !
   4 EST-X64-BUF 56 + !
   0 0 $400000 EST-X64-TEXT EST-X64-TEXT 5 EST-X64-PHDR
   1 EST-X64-TEXT $400000 EST-X64-TEXT + $20 $20 6 EST-X64-PHDR
   2 0 $400000 REGION-OFF + 0 REGION 6 EST-X64-PHDR
   3 $4000 $400000 REGION-OFF + REGION + $20 $20 4 EST-X64-PHDR
   $90 EST-X64-BUF X64LAYOUT:CODE-OFF + c!
   EST-X64-TEXT X64AOT-FORMAT:FOOTER-BYTES - {: footer:n :}
   EST-X64-TABLE EST-X64-BUF footer + !
   EST-X64-TABLE-BYTES EST-X64-BUF footer 4 + + !
   X64AOT-FORMAT:MAGIC EST-X64-BUF footer 8 + + !
   1 EST-X64-BUF $4000 + c!
   2 EST-X64-BUF $401F + c! ;

\ Expected rows, in file order: header page $1000 other; text $800 code up to
\ the table; run count 4 and XT count 4 other; table gap $1FF0 - $1808 = $7E8
\ pad; footer 16 other; RW tail $20 other; segment gap $4000 - $2020 = $1FE0
\ pad; DATA carrier $20, 2 written and 30 zero. Other is $1000 + 8 + 16 + $20 =
\ $1038.
: EST-X64-SEGMENTS ( -- )
   s" x86 DATA carrier PT_LOAD has its own file-backed row" T-LABEL
   FS:CLEANUP-RESET
   s" habu-engine-size-x64" FS:MKTEMP-DIR {: root:ptr rootu:n :}
   root rootu FS:CLEANUP-TREE+
   root rootu s" stripped" EST-X64-PATH FS:JOIN-PATH EST-X64-PATH-U !
   EST-X64-IMAGE
   EST-X64-PATH EST-X64-PATH-U @ EST-X64-BUF EST-X64-BYTES FS:WRITE-ALL
   EST-X64-PATH EST-X64-PATH-U @ MEASURE
   CLASS$ s" stripped" T$=
   TOTAL-BYTES EST-X64-BYTES T=
   CODE-BYTES $800 T=
   NAME-BYTES 0 T=
   DATA-WRITTEN 2 T=
   DATA-ZERO 30 T=
   PAD-BYTES $27C8 T=
   OTHER-BYTES $1038 T=
   FS:CLEANUP-RUN ;

\ With the carrier empty the file ends at the page after the RW tail: no
\ data is written, and padding is the table gap $7E8 plus $3000 - $2020 = $FE0.
: EST-X64-EMPTY ( -- )
   s" empty x86 fixed LOADs leave a page-rounded file" T-LABEL
   FS:CLEANUP-RESET
   s" habu-engine-size-x64-empty" FS:MKTEMP-DIR {: root:ptr rootu:n :}
   root rootu FS:CLEANUP-TREE+
   root rootu s" stripped" EST-X64-PATH FS:JOIN-PATH EST-X64-PATH-U !
   EST-X64-IMAGE
   0 EST-X64-BUF $F0 + !
   0 EST-X64-BUF $108 + !  0 EST-X64-BUF $110 + !
   EST-X64-PATH EST-X64-PATH-U @ EST-X64-BUF $3000 FS:WRITE-ALL
   EST-X64-PATH EST-X64-PATH-U @ MEASURE
   CLASS$ s" stripped" T$=
   TOTAL-BYTES $3000 T=
   DATA-WRITTEN 0 T=
   PAD-BYTES $17C8 T=
   s" a negative fixed LOAD size is not an empty segment" T-LABEL
   -1 EST-X64-BUF $108 + !  -1 EST-X64-BUF $110 + !
   EST-X64-PATH EST-X64-PATH-U @ EST-X64-BUF $3000 FS:WRITE-ALL
   [: EST-X64-PATH EST-X64-PATH-U @ MEASURE ;] E-ES-WALK TTHROWSQ
   0 EST-X64-BUF $108 + !  0 EST-X64-BUF $110 + !
   -1 EST-X64-BUF $D0 + !
   EST-X64-PATH EST-X64-PATH-U @ EST-X64-BUF $3000 FS:WRITE-ALL
   [: EST-X64-PATH EST-X64-PATH-U @ MEASURE ;] E-ES-WALK TTHROWSQ
   FS:CLEANUP-RUN ;

;using   \ X64LAYOUT

: EST-U64! ( n n -- ) {: value:n off:n :}
   value IMG@ off + CELL-VIEW ! ;

: EST-COUNTED-SEED ( -- )
   s" seed count bounds records before adjacent plausible data" T-LABEL
   $1100 IMG-RESERVE  $1100 ILEN !  $1100 ETEXT-N !
   $1100 0 ?do 0 IMG@ i + c! loop
   1 $FF8 EST-U64!
   1 $1000 EST-U64!  4 $1008 EST-U64!
   1 $1010 EST-U64!  65 IMG@ $1018 + c!
   5 $1030 EST-U64!  8 $1038 EST-U64!
   1 $1040 EST-U64!  66 IMG@ $1048 + c!
   $1000 COUNTED-RUN 1 T= ;

: EST-SUM ( -- )
   s" the engine's budget rows sum to its file length" T-LABEL
   CLASS$ s" engine" T$=
   TOTAL-BYTES ILEN @ T=
   CODE-BYTES NAME-BYTES + DATA-WRITTEN + DATA-ZERO + PAD-BYTES + OTHER-BYTES +
   ILEN @ T= ;

: EST-OVERLAP ( -- )
   REC0 @ SITE0 ! -1 QUIET ! BUDGET ;

: EST-GAP ( -- )
   SITE0 @ 1+ SITE0 ! -1 QUIET ! BUDGET ;

: EST-OVERFLOW ( -- )
   $7FFFFFFFFFFFFFFF 1 ?RANGE ;

: EST-TILING ( -- )
   s" equal-size overlap and gap cannot pass the budget identity" T-LABEL
   SITE0 @ {: site:n :}
   [: EST-OVERLAP ;] E-ES-WALK TTHROWSQ  site SITE0 !
   [: EST-GAP ;] E-ES-WALK TTHROWSQ  site SITE0 !
   s" an overflowing range is refused before any image access" T-LABEL
   [: EST-OVERFLOW ;] E-ES-WALK TTHROWSQ
   0 QUIET ! ;

: EST-METADATA ( -- )
   \ RELA and the dynamic table are ELF-specific. The general tiling tests
   \ above exercise both containers, including all Mach-O load-command spans.
   MACHO @ if exit then
   s" header metadata includes the final RELA addend, including its zero bytes" T-LABEL
   PHDR-END X64 @ if 400 else 288 then T=
   ELF-META-END X64 @ if 600 else 488 then T=
   \ Alter the last metadata byte in this private copy. Its declared extent
   \ remains the same for both values; only the zero count may change.
   ELF-META-END 1- {: last:n :}
   IMG@ last + c@ {: byte:n :}
   1 IMG@ last + c! ELF-META-END last 1+ T=
   0 IMG@ last + c! ELF-META-END last 1+ T=
   byte IMG@ last + c!
   s" dynamic metadata lengths cannot wrap or run into the code page" T-LABEL
   [: $4001B8 $7FFFFFFFFFFFFFFF META-VA-END drop ;] E-ES-WALK TTHROWSQ
   [: $4001B8 CODE-OFF META-VA-END drop ;] E-ES-WALK TTHROWSQ ;

: EST-SIDECAR ( -- )
   \ The payload rows below are the ARM packed AOT trailer. Intel records
   \ text sites in its ELF footer and dictionary rows in a fixed PT_LOAD.
   X64 @ if exit then
   \ First walk the payload without a sidecar, even if the donor has one.
   SIGNAME-LEN @ 0 > if SIG0 @ 8 - else AOT-END @ then {: at:n :}
   at ETEXT-N !
   AOT0 @ WALK-AT AOT-END !
   SIG-LEN @ 0 T= SIGNAME-LEN @ 0 T=
   FRAME-CELLS @ {: frames:n :}
   -1 QUIET ! BUDGET-BEGIN CODE-OFF TILE-END ! ENGINE-ROWS
   TOTAL @ {: before:n :}
   \ Add a seven-byte payload, padding to four-byte alignment and installer name.
   \ This models the install image's framing, not an executable checker table.
   8 7 PADDED + SIGNAME$ nip PADDED + {: size:n :}
   \ Page rounding promises no spare bytes. Grow only our private buffer;
   \ the component budget below does not read the image's RW segment.
   ILEN @ at size + max dup CELL 1- + CELL / IMG-RESERVE ILEN !
   size 0 ?do 0 IMG@ at i + + c! loop
   7 IMG@ at + c!
   7 0 ?do 65 IMG@ at 8 + i + + c! loop
   SIGNAME$ {: name:ptr u:n :}
   name IMG@ at 16 + + u BYTE-COPY
   at size + ETEXT-N !
   AOT0 @ WALK-AT AOT-END !
   FRAME-CELLS @ frames 1+ T=
   SIG-LEN @ 7 T= SIGNAME-LEN @ u T=
   SIG0 @ at 8 + T= SIGNAME0 @ at 16 + T=
   AOT-END @ at size + T=
   BUDGET-BEGIN CODE-OFF TILE-END ! ENGINE-ROWS 0 QUIET !
   TOTAL @ before size + T= ;

: EST-W32! ( n n -- ) {: w:n off:n :}
   4 0 ?do w i 8 * rshift $FF and IMG@ off i + + c! loop ;

: EST-DATA-CARRIER ( -- )
   s" the DATA owner decoder accepts the compact carrier at the image end" T-LABEL
   4 IMG-RESERVE 12 ILEN !
   $D2C00069 0 EST-W32! $F2A80009 4 EST-W32! $F2802469 8 EST-W32!
   0 CHAIN? TTRUE 0 CHAIN-VALUE $340000123 T=
   s" mismatched registers and truncated carriers are refused" T-LABEL
   $F2802468 8 EST-W32! 0 CHAIN? TFALSE
   $F2802469 8 EST-W32! 11 ILEN ! 0 CHAIN? TFALSE ;

\ A prior stripped Mach-O placed its sparse blob at the end of __text. The
\ candidate must fit that section; a new image starts it after the GOT. Test
\ both declared bounds with a minimal valid sparse header. This catches an
\ old blob being rejected, a header crossing __text, and a DATA interior
\ pointer being accepted. The build tests exercise the full new image path.
: EST-MACHO-BLOB-PLACE ( -- )
   HB-TARGET-MACOS? 0= if exit then
   MACHO-READ:TEXT-END 8 - {: old:n :}
   MACHO-READ:DATA-OFF MACHO-READ:GOT-BYTES + {: now:n :}
   old U32@ old 4 + U32@ now U32@ now 4 + U32@ {: o0:n o1:n n0:n n1:n :}
   0 old EST-W32!  0 old 4 + EST-W32!
   s" the prior Mach-O text payload is bounded by its text section" T-LABEL
   old BLOB-AT? TTRUE
   BLOB-LIMIT MACHO-READ:TEXT-END T=
   old 4 + BLOB-AT? TFALSE
   o0 old EST-W32!  o1 old 4 + EST-W32!
   0 now EST-W32!  0 now 4 + EST-W32!
   s" the current Mach-O payload starts immediately after the GOT" T-LABEL
   now BLOB-AT? TTRUE
   BLOB-LIMIT MACHO-READ:DATA-OFF MACHO-READ:DATA-BYTES + T=
   now 4 + BLOB-AT? TFALSE
   n0 now EST-W32!  n1 now 4 + EST-W32! ;

\ Three one-instruction bodies: call/jump to the third, return, return.
\ A BL returns into the second body; the same displacement in B does not.
: EST-FALLTHROUGH ( -- )
   8 IMG-RESERVE 40 ILEN !
   0 BLOB-OFF ! 12 BLOB-LEN ! 0 REC-N ! 3 SPAN-N ! 16 SPAN0 !
   3 0 ?do
      i 4 * 16 i 8 * + EST-W32!
      4 CODE-SPAN:EXACT 20 i 8 * + EST-W32!
   loop
   $D65F03C0 4 EST-W32! $D65F03C0 8 EST-W32!
   BUILD-CODE-INDEX
   s" a call at a body boundary reaches its callee and its continuation" T-LABEL
   $94000002 0 EST-W32!
   REACH-RESET 0 MARK-ENTRY SWEEP
   REACH-SN @ 3 T= FALL-N @ 1 T=
   s" an unconditional jump has no return continuation" T-LABEL
   $14000002 0 EST-W32!
   REACH-RESET 0 MARK-ENTRY SWEEP
   REACH-SN @ 2 T= 1 MARK @ 0 T= FALL-N @ 0 T=
   s" a call in an unowned gap also reaches its continuation" T-LABEL
   $94000002 0 EST-W32!
   2 SPAN-N ! 24 SPAN0 ! BUILD-CODE-INDEX
   REACH-RESET SCAN-UNOWNED SWEEP
   REACH-SN @ 2 T= FALL-N @ 1 T= ;

: EST-SPAN-REACH ( -- )
   \ The reachability sweep decodes AArch64 calls in the packed AOT blob.
   X64 @ if exit then
   BUILD-CODE-INDEX
   SPAN-N @ 0 > TTRUE
   REACH-RESET ROOTS-SURFACE ROOTS-CAPTURE ROOTS-ENTRY SWEEP
   REACH-SN @ 0 > TTRUE
   FALL-N @ 0 > TTRUE
   COLLECT-DEAD
   DEAD-TOTAL @ 0 T=
   COLLECT-DEAD-SPANS
   s" capture removes anonymous spans unreachable from the shipped surface" T-LABEL
   DEAD-SN @ 0 T= ;

\ The sidecar is written beside a stand-in image path in a private temp dir, so
\ two runs of this fixture never read or remove each other's map.
create EST-IMG PATH-CAP allot
variable EST-IMG-U
create EST-MAP PATH-CAP allot
variable EST-MAP-U

: EST-SPAN-SIDECAR ( -- )
   \ This synthetic span row uses the ARM packed AOT code index.
   X64 @ if exit then
   BUILD-CODE-INDEX
   FS:CLEANUP-RESET
   s" habu-image-size" FS:MKTEMP-DIR {: root:ptr rootu:n :}
   root rootu FS:CLEANUP-TREE+
   root rootu s" hb" EST-IMG FS:JOIN-PATH EST-IMG-U !
   root rootu s" hb.map" EST-MAP FS:JOIN-PATH EST-MAP-U !
   SB-RESET
   S\" habu-names 1\ncolumns rec named start len wid name\n0 0 " SB-APPEND
   0 SPAN-START FMT:SB-U s"  " SB-APPEND
   SPAN0 @ 4 + U32@ FMT:SB-U S\"  -1 EST-ANONYMOUS-SPAN\n" SB-APPEND
   EST-MAP EST-MAP-U @ SB$ FS:WRITE-ALL
   EST-IMG EST-IMG-U @ IMAGE-NAMES:LOAD
   0 SPAN-START 0 SPAN-BYTES IMAGE-NAMES:SPAN-NAME$
   s" EST-ANONYMOUS-SPAN" T$=
   FS:CLEANUP-RUN ;

\ THE SHIPPED PAYLOAD CARRIES NO ENGINE'S WID, and this is the row that reads it
\ off the engine this checkout ships rather than off the build that wrote it. A
\ record's wid field and a package row's two wid cells hold the offset from the
\ window's first wordlist, one-based, so the base cell is the constant
\ (src/habu/aot-decl.f WID-REL-BASE, spelled out here because the engine carries
\ no AOT-BUF to ask) and every non-zero offset is inside the span the same cells
\ declare. Before the rows moved, this base was the number of wordlists the
\ BUILDING engine happened to have: 395 in the engine measured beside this.
variable EST-WID-BAD                    \ offsets outside [1, span]
variable EST-WID-SEEN                   \ ... and the non-zero ones walked

: EST-WID? ( n -- ) {: w:n :}
   w 0= if exit then
   1 EST-WID-SEEN +!
   w 1 >= w WID-SPAN @ <= and 0= if 1 EST-WID-BAD +! then ;

: EST-WID-FORM ( -- )
   \ This is the ARM packed AOT wid table; Intel keeps live dictionary rows.
   X64 @ if exit then
   WID-W0 @ 1 T=
   0 EST-WID-BAD !  0 EST-WID-SEEN !
   NRECS 0 ?do
      i REC-PKG? if
         i REC-PUB-WID EST-WID?  i REC-PRI-WID EST-WID?
      else
         i REC-WID EST-WID?
      then
   loop
   EST-WID-BAD @ 0 T=
   EST-WID-SEEN @ 0 > TTRUE ;

: EST-MAIN ( -- )
   T-RESET
   EST-COUNTED-SEED
   EST-X64-SEGMENTS
   EST-X64-EMPTY
   EST-DATA-CARRIER
   s" bin/hb" MEASURE
   EST-SUM
   EST-WID-FORM
   EST-SPAN-REACH
   EST-SPAN-SIDECAR
   EST-TILING
   EST-METADATA
   EST-MACHO-BLOB-PLACE
   EST-SIDECAR
   EST-FALLTHROUGH
   T-REPORT ;

EST-MAIN
;package
