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
   -1 QUIET ! BUDGET-BEGIN ENGINE-ROWS
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
   BUDGET-BEGIN ENGINE-ROWS 0 QUIET !
   TOTAL @ before size + T= ;

: EST-SPAN-REACH ( -- )
   s" bin/hb" MEASURE
   BUILD-CODE-INDEX
   SPAN-N @ 0 > TTRUE
   REACH-RESET ROOTS-SURFACE SWEEP
   REACH-SN @ 0 > TTRUE
   COLLECT-DEAD-SPANS
   DEAD-SN @ 0 > TTRUE ;

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
   s" habu-image-size" TMPDIR-MKDIR {: root:ptr rootu:n :}
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

: EST-MAIN ( -- )
   T-RESET
   EST-SIDECAR
   EST-SPAN-REACH
   EST-SPAN-SIDECAR
   T-REPORT ;

EST-MAIN
;package
