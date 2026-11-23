split-str
\ main-routing.f - exercise the emitted engine's stdin and argv routing.
\ Needs HABU_UNDER_TEST set to the emitted candidate.

require src/lib/test.f
require src/lib/string.f
require test/gate-common.f
require src/lib/process.f
require src/core/bytes.f

package MAIN-ROUTING-TEST

private

create PATH-BUF PATH-CAP allot

: FILE$ ( -- str )
   s" route.f" PATH-BUF GT-PATH PATH-BUF swap ;

: READ$ ( -- str )
   s" read.f" PATH-BUF GT-PATH PATH-BUF swap ;

: STACK$ ( -- str )
   s" stack.f" PATH-BUF GT-PATH PATH-BUF swap ;

: ODD$ ( -- str )
   S\" q\"\\\nd.f" PATH-BUF GT-PATH PATH-BUF swap ;

\ A stdin program of several MiB, which the stream grows many times to hold.
$480000 constant LONG-SIZE
DYNAMIC-BUFFER PADDED u8

: RUN ( ^u8 n [ -- ] -- ) {: input:^ u:n args :}
   GE-HB-RESET
   args execute
   GE-HB$ input u PROCESS:WAIT-FOREVER GE-RUN-STDIN ;

: PASS ( ^u8 n ^u8 n -- ) {: want:^ wantu:n label:^ labelu:n :}
   label labelu GE-EXPECT-OK
   want wantu label labelu GE-EXPECT-OUT
   s" " label labelu GE-EXPECT-ERR ;

: FILE-ARG ( -- )
   FILE$ GE-ARG+ ;

: STACK-ARG ( -- )
   STACK$ GE-ARG+ ;

: PLAIN ( -- )
   S\" s\" PIPE\" type cr\n" [: FILE-ARG ;] RUN
   S\" FILE\n" s" a named file runs over a nonempty stdin" PASS
   s" Q" [: READ$ GE-ARG+ ;] RUN
   S\" Q\n" s" plain leaves stdin alone" PASS ;

: LOAD-ROUTE ( -- )
   s" Q" [: s" --load" GE-ARG+ READ$ GE-ARG+ ;] RUN
   S\" Q\n" s" load leaves stdin alone" PASS ;

: ODD-LOAD ( -- )
   s" " [: s" --load" GE-ARG+ ODD$ GE-ARG+ ;] RUN
   S\" ODD\n" s" load takes a path holding a quote, a backslash and a line feed" PASS ;

: NO-ARGS ( -- )
   S\" s\" PIPE\" type cr\n" [: ;] RUN
   S\" PIPE\n" s" no args reads stdin" PASS ;

: STACK-SCOPE ( -- )
   s" " [: STACK-ARG ;] RUN
   70 s" root file closes the user stack" GE-EXPECT-RC
   s" error[E-EVAL-RESIDUE]" s" root file closes the user stack" GE-EXPECT-ERR-HAS
   s" stack.f:1:1" s" root file closes the user stack" GE-EXPECT-ERR-HAS
   s" " [: s" --load" GE-ARG+ STACK-ARG ;] RUN
   70 s" required file closes the user stack" GE-EXPECT-RC
   s" error[E-EVAL-RESIDUE]" s" required file closes the user stack" GE-EXPECT-ERR-HAS ;

\ `s" A" type`, newlines, then `s" B" type cr`, LONG-SIZE bytes in all.
: LONG-TEXT ( -- )
   LONG-SIZE PADDED-RESERVE
   LONG-SIZE 0 do $0A i PADDED c! loop
   S\" s\" A\" type" {: head:^ headu:n :}
   S\" s\" B\" type cr" {: tail:^ tailu:n :}
   head 0 PADDED headu BYTE-COPY
   tail LONG-SIZE tailu - 1 - PADDED tailu BYTE-COPY ;

: LONG-STDIN ( -- )
   LONG-TEXT
   GE-HB-RESET
   GE-HB$ 0 PADDED LONG-SIZE PROCESS:WAIT-FOREVER GE-RUN-STDIN
   PADDED-RELEASE
   S\" AB\n" s" a stdin program past the stream's first sizes" GE-EXPECT-OUT
   s" " s" a stdin program past the stream's first sizes" GE-EXPECT-ERR
   s" a stdin program past the stream's first sizes" GE-EXPECT-OK ;

public

: MAIN ( -- )
   T-RESET
   s" habu-main-routing" GT-START
   FILE$ S\" s\" FILE\" type cr\n" FS:WRITE-ALL
   READ$ S\" create MAIN-ROUTE-READ-BYTE 1 allot\n0 MAIN-ROUTE-READ-BYTE 1 read drop\nMAIN-ROUTE-READ-BYTE 1 >str type cr\n" FS:WRITE-ALL
   STACK$ S\" 7\n" FS:WRITE-ALL
   ODD$ S\" s\" ODD\" type cr\n" FS:WRITE-ALL
   PLAIN LOAD-ROUTE ODD-LOAD NO-ARGS STACK-SCOPE LONG-STDIN
   GT-CLEANUP
   T-REPORT ;

MAIN

;package
