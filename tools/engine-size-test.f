\ The sidecar's count belongs to framing; only its padded runs belong to the
\ sidecar row. Exercise both payload shapes on a private copy of bin/hb.
require lib/test.f
require lib/fs-mutate.f
require tools/image-size-lib.f

package IMAGE-SIZE
private

: EST-SUM ( -- )
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
   s" bin/hb" MEASURE [: EST-OVERLAP ;] E-ES-WALK TTHROWSQ
   s" bin/hb" MEASURE [: EST-GAP ;] E-ES-WALK TTHROWSQ
   s" an overflowing range is refused before any image access" T-LABEL
   [: EST-OVERFLOW ;] E-ES-WALK TTHROWSQ
   0 QUIET ! ;

: EST-METADATA ( -- )
   s" header metadata includes the final RELA addend, including its zero bytes" T-LABEL
   s" bin/hb" MEASURE
   PHDR-END 288 T= ELF-META-END 488 T=
   \ Alter the last metadata byte in this private copy. Its declared extent
   \ remains the same for both values; only the zero count may change.
   1 IMG@ 487 + c! ELF-META-END 488 T=
   0 IMG@ 487 + c! ELF-META-END 488 T=
   s" dynamic metadata lengths cannot wrap or run into the code page" T-LABEL
   [: $4001B8 $7FFFFFFFFFFFFFFF META-VA-END drop ;] E-ES-WALK TTHROWSQ
   [: $4001B8 CODE-OFF META-VA-END drop ;] E-ES-WALK TTHROWSQ ;

: EST-SIDECAR ( -- )
   s" bin/hb" MEASURE
   CLASS$ s" engine" T$=
   EST-SUM
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
   s" bin/hb" MEASURE
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
create EST-IMG FS-PATH-CAP allot
variable EST-IMG-U
create EST-MAP FS-PATH-CAP allot
variable EST-MAP-U

: EST-SPAN-SIDECAR ( -- )
   s" bin/hb" MEASURE
   BUILD-CODE-INDEX
   CLEANUP-RESET
   s" habu-image-size" HB-TMP-MKDIR {: root:ptr rootu:n :}
   root rootu CLEANUP-TREE+
   root rootu s" hb" EST-IMG JOIN-PATH EST-IMG-U !
   root rootu s" hb.names" EST-MAP JOIN-PATH EST-MAP-U !
   SB-RESET
   S\" habu-names 1\ncolumns rec named start len wid name\n0 0 " SB-APPEND
   0 SPAN-START FMT:SB-U s"  " SB-APPEND
   SPAN0 @ 4 + U32@ FMT:SB-U S\"  -1 EST-ANONYMOUS-SPAN\n" SB-APPEND
   EST-MAP EST-MAP-U @ SB$ WRITE-ALL
   EST-IMG EST-IMG-U @ IMAGE-NAMES:LOAD
   0 SPAN-START 0 SPAN-BYTES IMAGE-NAMES:SPAN-NAME$
   s" EST-ANONYMOUS-SPAN" T$=
   CLEANUP-RUN ;

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
   s" bin/hb" MEASURE
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
   EST-DATA-CARRIER
   EST-WID-FORM
   EST-TILING
   EST-METADATA
   EST-SIDECAR
   EST-FALLTHROUGH
   EST-SPAN-REACH
   EST-SPAN-SIDECAR
   T-REPORT ;

EST-MAIN
;package
