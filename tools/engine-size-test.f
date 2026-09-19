\ The sidecar's count belongs to framing; only its padded runs belong to the
\ sidecar row. Exercise both payload shapes on a private copy of bin/hb.
require lib/test.f
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

: EST-MAIN ( -- )
   T-RESET
   EST-SIDECAR
   T-REPORT ;

EST-MAIN
;package
