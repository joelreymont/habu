\ size-report-test.f - regressions for the SIZE-REPORT map parser.
\ Run: bin/hb --load tools/size-report.f tools/size-report-test.f

require lib/test.f
require tools/size-report.f

package SIZE-REPORT-TEST

\ Two blocks: LOAD must keep the LAST (final EMIT-FORTH pass) block and exclude
\ the source blob from the code total.
: FIXTURE$ ( -- ptr u8 n )
   s\" main/startup 100\nfoo 200\nbaked-source 5\nmain/startup 111\nfoo 222\ndictionary-code 44\nbaked-source 7\n" ;

: UNWRAP ( option<n> n -- n ) {: d:n :}
   MATCH option
      none OF d ENDOF
      some OF ENDOF
   ;MATCH ;

: RUN ( -- )
   T-RESET
   FIXTURE$ SIZE-REPORT:LOAD-BYTES
   SIZE-REPORT:COUNT 4 T=
   0 SIZE-REPORT:NAME$ s" main/startup" T$=
   0 SIZE-REPORT:VAL@ 111 T=
   1 SIZE-REPORT:VAL@ 222 T=
   s" foo" SIZE-REPORT:FIND -1 UNWRAP 222 T=
   s" main/startup" SIZE-REPORT:FIND -1 UNWRAP 111 T=
   s" dictionary-code" SIZE-REPORT:FIND -1 UNWRAP 44 T=
   s" nope" SIZE-REPORT:FIND -1 UNWRAP -1 T=
   SIZE-REPORT:CODE-TOTAL 377 T=
   T-REPORT ;

RUN

;package
