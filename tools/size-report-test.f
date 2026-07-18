\ size-report-test.f - regressions for the SIZE-REPORT map parser + reconciler.
\ Run: bin/hb --load tools/size-report.f tools/size-report-test.f

require lib/test.f
require tools/size-report.f

package SIZE-REPORT-TEST

\ Two blocks (LOAD keeps the LAST, the final image-emission pass). The last block
\ carries the emitter-phase rows (main/startup, bar), the baked data row, and the
\ post-sign container rows the driver attributes, so the row totals partition into
\ text vs container and sum to the whole file.
: FIXTURE$ ( -- ptr u8 n )
   s\" main/startup 1\nold 2\nbaked-source 3\nmain/startup 100\nbar 200\nbaked-source 8\ncontainer/header 100\ncontainer/text-pad 4\ncontainer/data-const 16384\ncontainer/signature 50\n" ;

: UNWRAP ( option<n> n -- n ) {: d:n :}
   MATCH option
      none OF d ENDOF
      some OF ENDOF
   ;MATCH ;

: RUN ( -- )
   T-RESET
   FIXTURE$ SIZE-REPORT:LOAD-BYTES
   SIZE-REPORT:COUNT 7 T=
   0 SIZE-REPORT:NAME$ s" main/startup" T$=
   0 SIZE-REPORT:VAL@ 100 T=
   s" bar" SIZE-REPORT:FIND -1 UNWRAP 200 T=
   s" container/data-const" SIZE-REPORT:FIND -1 UNWRAP 16384 T=
   s" nope" SIZE-REPORT:FIND -1 UNWRAP -1 T=
   \ Every row is attributed exactly once: text + container = all.
   SIZE-REPORT:SUM-TEXT 308 T=
   SIZE-REPORT:SUM-CONTAINER 16538 T=
   SIZE-REPORT:SUM-ALL 16846 T=
   \ Emitter-phase code excludes the baked data row and every container row.
   SIZE-REPORT:CODE-TOTAL 300 T=
   \ Header row and the page-floor shave (header + text below one page).
   SIZE-REPORT:HEADER-BYTES 100 T=
   SIZE-REPORT:FLOOR-DIST 408 T=
   T-REPORT ;

RUN

;package
