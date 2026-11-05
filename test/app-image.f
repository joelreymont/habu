\ app-image.f - an application image built once outside the checkout from a
\ cold native load: run, MAIN with arguments, quit and an uncaught throw on a
\ terminal, a refused definition, and tier-0 words a restored image compiled.
require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-cwd.f
require src/lib/pty.f
require src/lib/engine-candidate.f
require src/lib/process.f
require test/pcap-result.f

package APP-IMAGE-TEST

$10000 constant CAP
create OUT CAP allot
create ERR CAP allot
create ROOT-BUF PATH-CAP allot
create IMAGE-BUF PATH-CAP allot
create STARTUP-BUF PATH-CAP allot
create TIER-ZERO-BUF PATH-CAP allot
variable ROOT-U
variable IMAGE-U
variable STARTUP-U
variable TIER-ZERO-U

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE-BUF IMAGE-U @ ;
: STARTUP$ ( -- ptr u8 n ) STARTUP-BUF STARTUP-U @ ;
: TIER-ZERO$ ( -- ptr u8 n ) TIER-ZERO-BUF TIER-ZERO-U @ ;

\ Every child runs on the engine under test: the images it saves are the subject.
: PREPARE ( -- )
   FS:CLEANUP-RESET
   s" app-image-test" FS:MKTEMP-DIR {: path:ptr size:n :}
   path ROOT-BUF size BYTE-COPY size ROOT-U !
   ROOT$ FS:CLEANUP-TREE+
   ROOT$ s" application" IMAGE-BUF FS:JOIN-PATH IMAGE-U !
   ROOT$ s" startup" STARTUP-BUF FS:JOIN-PATH STARTUP-U !
   ROOT$ s" tier-zero" TIER-ZERO-BUF FS:JOIN-PATH TIER-ZERO-U ! ;

: CLEAN ( n n n -- n ) {: outu:n erru:n rc:n :}
   rc 0<> erru 0<> or if
      s" app-image child failed: " type rc . cr
      OUT outu type ERR erru type
   then
   rc 0 T= erru 0 T=
   outu ;

\ Start cold: the image support selects tier 1 before it requires its
\ dependencies. The build child loads that support cold and reports the tier in
\ force after it, before its subject loads.
: BUILD-SOURCE$ ( -- ptr u8 n )
   S\" require src/habu/app-image.f\ntier@ . cr\nrequire test/app-image-subject.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" ;

: BUILD ( -- )
   PROC-ARGV-ENV-RESET
   s" --" >LEN PROC-ARGV+
   IMAGE$ >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN
   BUILD-SOURCE$ >LEN
   OUT CAP >LEN ERR CAP >LEN PROCESS:WAIT-FOREVER
   RUN-ARGV-ENV-STDIN-CAPTURE PCAP-RESULT:>N CLEAN
   OUT swap S\" 1\n\n" T$=
   IMAGE$ FS:EXECUTABLE? TTRUE ;

: RUN-INPUT ( ptr u8 n ptr u8 n -- n n n )
   {: path:ptr pathu:n input:ptr inputu:n :}
   path pathu >LEN ROOT$ >LEN input inputu >LEN
   OUT CAP >LEN ERR CAP >LEN PROCESS:WAIT-FOREVER
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE PCAP-RESULT:>N ;

: CHECK-REJECTION ( -- )
   PROC-ARGV-ENV-RESET
   IMAGE$ S\" : FRESH-BAD ( n -- n ) 0= ;\n" RUN-INPUT
   {: outu:n erru:n rc:n :}
   rc 70 T= outu 0 T=
   ERR erru s" expected:" CONTAINS? TTRUE
   ERR erru s" actual:" CONTAINS? TTRUE ;

\ One package of a loop, a string literal, a quotation and a create...does>
\ definer, then the tier it was compiled at.
: TIER-PACKAGE+ ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   s" package " SB-APPEND name nameu SB-APPEND
   S\" \npublic\n: LOOPS ( -- ) 4 0 ?do i . loop ;\n: TEXT ( -- ) s\q saved text\q type cr ;\n" SB-APPEND
   S\" : TRIPLE ( n -- n ) [: 3 * ;] execute ;\n: KONST ( n -- ) create , does> ( -- n ) @ 1 + ;\n41 KONST ANSWER\n" SB-APPEND
   S\" : SHOW ( -- ) LOOPS TEXT 5 TRIPLE . ANSWER . ;\n;package\n" SB-APPEND
   S\" tier@ . cr\n" SB-APPEND ;

\ A restored image compiles at tier 0. The same words at tier 1 are the
\ reference the saved image must reproduce.
: TIER-ZERO-SOURCE$ ( -- ptr u8 n )
   SB-RESET
   S\" 0 set-tier\n" SB-APPEND
   s" TIER-ZERO-IMAGE" TIER-PACKAGE+
   S\" 1 set-tier\n" SB-APPEND
   s" TIER-ONE-IMAGE" TIER-PACKAGE+
   S\" 0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" SB-APPEND
   SB$ ;

: TIER-ZERO-TIERS$ ( -- ptr u8 n )
   S\" 0\n\n1\n\n" ;

: CHECK-TIER-ZERO ( -- )
   s" a restored image saves words it compiled at tier 0" T-LABEL
   PROC-ARGV-ENV-RESET
   s" --" >LEN PROC-ARGV+
   TIER-ZERO$ >LEN PROC-ARGV+
   IMAGE$ TIER-ZERO-SOURCE$ RUN-INPUT CLEAN
   OUT swap TIER-ZERO-TIERS$ T$=
   PROC-ARGV-ENV-RESET
   TIER-ZERO$ S\" TIER-ZERO-IMAGE:SHOW cr TIER-ONE-IMAGE:SHOW cr\n" RUN-INPUT CLEAN
   OUT swap S\" 0\n1\n2\n3\nsaved text\n15\n42\n\n0\n1\n2\n3\nsaved text\n15\n42\n\n" T$= ;

: CAPTURE-STARTUP ( -- )
   PROC-ARGV-ENV-RESET
   s" --" >LEN PROC-ARGV+
   STARTUP$ >LEN PROC-ARGV+
   IMAGE$
   S\" 1 set-tier\npackage IMAGE-START-TEST\nvariable STARTS\n: MAIN ( -- ) 1 STARTS +! STARTS @ . cr SCRIPT-ARGC . cr SCRIPT-ARGC 0 ?do i SCRIPT-ARGV$ type cr loop SCRIPT-ARGC 0 > if 0 SCRIPT-ARGV$ s\q quit\q STR= if s\q \q 0 die then 0 SCRIPT-ARGV$ s\q throw\q STR= if -123 throw then then ;\n' MAIN\n;package\nAPP-IMAGE:START!\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n"
   RUN-INPUT CLEAN drop
   STARTUP$ FS:EXECUTABLE? TTRUE ;

: CHECK-STARTUP ( -- )
   PROC-ARGV-ENV-RESET
   STARTUP$ S\" APP-IMAGE-SUBJECT:RUN . cr\n" RUN-INPUT CLEAN
   OUT swap S\" 1\n\n0\n\n43\n\n" T$= ;

: STARTUP-ARGS ( -- )
   s" alpha" >LEN PROC-ARGV+
   s" two words" >LEN PROC-ARGV+
   STARTUP$ S\" APP-IMAGE-SUBJECT:RUN . cr\n" RUN-INPUT CLEAN
   OUT swap S\" 1\n\n2\n\nalpha\ntwo words\n43\n\n" T$= ;

: CHECK-STARTUP-ARGS ( -- )
   PROC-ARGV-ENV-RESET STARTUP-ARGS
   PROC-ARGV-ENV-RESET s" --" >LEN PROC-ARGV+ STARTUP-ARGS ;

: CHECK-STARTUP-EXIT ( -- )
   PROC-ARGV-ENV-RESET
   s" quit" >LEN PROC-ARGV+
   STARTUP$ s" STARTUP-SHOULD-NOT-EXECUTE" RUN-INPUT CLEAN
   OUT swap S\" 1\n\n1\n\nquit\n" T$= ;

\ fd 0 must be a real terminal: a startup failure there enters REPL recovery,
\ which needs its source frame. Output uses a small pipe so the diagnostic stays
\ byte-exact on Linux and macOS.
create PTY-NAME PTY:SLAVE-PATH-CAP allot

\ The pair is src/lib/pty.f's on both targets; the slave is opened here, O_NOCTTY,
\ because this process must not adopt the terminal it hands the child.
: OPEN-PTY ( -- n n )
   PTY-NAME PTY:SLAVE-PATH-CAP PTY:OPEN drop PTY:MASTER>N {: master:n :}
   master >FD FD-CLOEXEC!
   PTY-NAME PTY:PTY-OPEN-FLAGS 0 open {: slave:n :}
   slave 0 >= TTRUE
   master slave ;

: CHECK-STARTUP-THROW ( ptr u8 n ptr u8 n -- )
   {: arg:ptr argu:n diagnostic:ptr diagnosticu:n :}
   PROC-ARGV-ENV-RESET arg argu >LEN PROC-ARGV+
   OPEN-PTY {: master:n slave:n :}
   PIPE-PAIR {: reader writer :}
   STARTUP$ >LEN slave >FD writer writer PROC-SPAWN-ARGV-ENV-IO {: child :}
   slave close writer FD>N close
   child PROC-WAIT-RC MATCH result
      ok OF 67 T= ENDOF
      err OF 67 T= ENDOF
   ;MATCH
   reader FD>N OUT CAP read {: outu:n :}
   reader FD>N close master close
   outu 0 >= TTRUE
   OUT outu diagnostic diagnosticu CONTAINS? TTRUE ;

: CHECK-STARTUP-THROWS ( -- )
   s" throw" s" hb: uncaught throw code -123" CHECK-STARTUP-THROW ;


: CASES ( -- )
   BUILD
   CHECK-REJECTION
   CHECK-TIER-ZERO
   CAPTURE-STARTUP
   CHECK-STARTUP
   CHECK-STARTUP-ARGS
   CHECK-STARTUP-EXIT
   CHECK-STARTUP-THROWS ;

: RUN ( -- )
   T-RESET
   [: PREPARE CASES ;] [: FS:CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
