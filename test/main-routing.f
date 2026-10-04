\ main-routing.f - exercise the emitted engine's stdin and argv routing.
\ Run with HABU_UNDER_TEST set to the emitted candidate:
\   bin/hb --load test/main-routing.f

require lib/test.f
require lib/string.f
require test/gate-common.f

package MAIN-ROUTING-TEST

private

create PATH-BUF FS-PATH-CAP allot

: FILE$ ( -- ptr u8 n )
   s" route.f" PATH-BUF GT-PATH PATH-BUF swap ;

: READ$ ( -- ptr u8 n )
   s" read.f" PATH-BUF GT-PATH PATH-BUF swap ;

: STACK$ ( -- ptr u8 n )
   s" stack.f" PATH-BUF GT-PATH PATH-BUF swap ;

: RUN ( ptr u8 n [ -- ] -- ) {: input:ptr u:n args :}
   GE-HB-RESET
   args execute
   GE-HB$ input u GE-TIMEOUT-MS GE-RUN-STDIN ;

: PASS ( ptr u8 n ptr u8 n -- ) {: want:ptr wantu:n label:ptr labelu:n :}
   label labelu GE-EXPECT-OK
   want wantu label labelu GE-EXPECT-OUT
   s" " label labelu GE-EXPECT-ERR ;

: FILE-ARG ( -- )
   FILE$ GE-ARG+ ;

: STACK-ARG ( -- )
   STACK$ GE-ARG+ ;

: PLAIN ( -- )
   S\" s\" PIPE\" type cr\n" [: FILE-ARG ;] RUN
   S\" PIPE\n" s" nonempty stdin wins" PASS
   s" " [: FILE-ARG ;] RUN
   S\" FILE\n" s" empty stdin falls back" PASS ;

: LOAD-ROUTE ( -- )
   s" Q" [: s" --load" GE-ARG+ READ$ GE-ARG+ ;] RUN
   S\" Q\n" s" load leaves stdin alone" PASS
   s" Q" [: s" --build" GE-ARG+ READ$ GE-ARG+ ;] RUN
   S\" Q\n" s" build leaves stdin alone" PASS ;

: NO-ARGS ( -- )
   S\" s\" PIPE\" type cr\n" [: ;] RUN
   S\" PIPE\n" s" no args reads stdin" PASS ;

: STACK-SCOPE ( -- )
   s" " [: STACK-ARG ;] RUN
   s" " s" root file keeps the user stack" PASS
   s" " [: s" --load" GE-ARG+ STACK-ARG ;] RUN
   67 s" required file closes the user stack" GE-EXPECT-RC
   s" hb: uncaught throw code -3804" s" required file closes the user stack" GE-EXPECT-ERR-HAS ;

: UNKNOWN ( -- )
   S\" s\" PIPE\" type cr\n" [: s" --unknown" GE-ARG+ FILE-ARG ;] RUN
   64 s" unknown flag before stdin" GE-EXPECT-RC
   s" " s" unknown flag before stdin" GE-EXPECT-OUT
   s" hb: unknown flag: --unknown" s" unknown flag before stdin" GE-EXPECT-ERR-HAS ;

public

: MAIN ( -- )
   T-RESET
   s" habu-main-routing" GT-START
   FILE$ S\" s\" FILE\" type cr\n" WRITE-ALL
   READ$ S\" create MAIN-ROUTE-READ-BYTE 1 allot\n0 MAIN-ROUTE-READ-BYTE 1 read drop\nMAIN-ROUTE-READ-BYTE 1 type cr\n" WRITE-ALL
   STACK$ S\" 7\n" WRITE-ALL
   PLAIN LOAD-ROUTE NO-ARGS STACK-SCOPE UNKNOWN
   GT-CLEANUP
   T-REPORT ;

MAIN

;package
