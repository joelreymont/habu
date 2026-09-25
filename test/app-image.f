\ Build once, then run and recapture outside the source checkout.
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/memory.f
require lib/process-cwd.f
require lib/pty.f
require test/whitebox-child.f

package APP-IMAGE-TEST

$10000 constant CAP
600000 constant TIMEOUT-MS
create OUT CAP allot
create ERR CAP allot
create ROOT-BUF FS-PATH-CAP allot
create IMAGE-BUF FS-PATH-CAP allot
create SECOND-BUF FS-PATH-CAP allot
create THIRD-BUF FS-PATH-CAP allot
create PROBE-BUF FS-PATH-CAP allot
create MUTATED-BUF FS-PATH-CAP allot
create MUTATED-AGAIN-BUF FS-PATH-CAP allot
create DISCARD-BUF FS-PATH-CAP allot
create REPLACED-BUF FS-PATH-CAP allot
create STARTUP-BUF FS-PATH-CAP allot
create REFUSE-BUF FS-PATH-CAP allot
variable ROOT-U
variable IMAGE-U
variable SECOND-U
variable THIRD-U
variable PROBE-U
variable MUTATED-U
variable MUTATED-AGAIN-U
variable DISCARD-U
variable REPLACED-U
variable STARTUP-U
variable REFUSE-U

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE-BUF IMAGE-U @ ;
: SECOND$ ( -- ptr u8 n ) SECOND-BUF SECOND-U @ ;
: THIRD$ ( -- ptr u8 n ) THIRD-BUF THIRD-U @ ;
: PROBE$ ( -- ptr u8 n ) PROBE-BUF PROBE-U @ ;
: MUTATED$ ( -- ptr u8 n ) MUTATED-BUF MUTATED-U @ ;
: MUTATED-AGAIN$ ( -- ptr u8 n ) MUTATED-AGAIN-BUF MUTATED-AGAIN-U @ ;
: DISCARD$ ( -- ptr u8 n ) DISCARD-BUF DISCARD-U @ ;
: REPLACED$ ( -- ptr u8 n ) REPLACED-BUF REPLACED-U @ ;
: STARTUP$ ( -- ptr u8 n ) STARTUP-BUF STARTUP-U @ ;
: REFUSE$ ( -- ptr u8 n ) REFUSE-BUF REFUSE-U @ ;

\ A child reopens the engine's build window - test/address-cell-cap-grown.f
\ rewinds the source window - which the sealed product refuses: `hb: internal
\ engine word: DECLARATIONS`, exit 70. So every spawn here runs on the engine
\ test/whitebox-child.f names, in the temp root this file already owns.
: PREPARE ( -- )
   CLEANUP-RESET
   s" app-image-test" HB-TMP-MKDIR {: path:ptr size:n :}
   path ROOT-BUF size BYTE-COPY size ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" application" IMAGE-BUF JOIN-PATH IMAGE-U !
   ROOT$ s" second" SECOND-BUF JOIN-PATH SECOND-U !
   ROOT$ s" third" THIRD-BUF JOIN-PATH THIRD-U !
   ROOT$ s" require-probe.f" PROBE-BUF JOIN-PATH PROBE-U !
   ROOT$ s" mutated" MUTATED-BUF JOIN-PATH MUTATED-U !
   ROOT$ s" mutated-again" MUTATED-AGAIN-BUF JOIN-PATH MUTATED-AGAIN-U !
   ROOT$ s" discard" DISCARD-BUF JOIN-PATH DISCARD-U !
   ROOT$ s" replaced" REPLACED-BUF JOIN-PATH REPLACED-U !
   ROOT$ s" startup" STARTUP-BUF JOIN-PATH STARTUP-U !
   ROOT$ s" refuse-jit.f" REFUSE-BUF JOIN-PATH REFUSE-U !
   ROOT$ WHITEBOX-CHILD:PROVIDE-IN ;

: RESULT ( result<pcap:captured,pcap:failed> -- n n n )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N erru LEN>N 0
      ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len rc:rc :}
         outu LEN>N erru LEN>N rc RC>N
      ENDOF
   ;MATCH ;

: CLEAN ( n n n -- n ) {: outu:n erru:n rc:n :}
   rc 0<> erru 0<> or if
      s" app-image child failed: " type rc . cr
      OUT outu type ERR erru type
   then
   rc 0 T= erru 0 T=
   outu ;

\ Start cold: every instruction published by the image support load must carry
\ native origin, including its dependencies. The selected tier alone cannot
\ prove that code emitted before the selection was native.
: CHECK-BUILD-TIER ( -- )
   PROC-ARGV-ENV-RESET
   WHITEBOX-CHILD:ENV!
   WHITEBOX-CHILD:ENGINE$ >LEN
   S\" variable IMAGE-LOAD-START cp@ IMAGE-LOAD-START !\nrequire src/habu/app-image.f\nIMAGE-LOAD-START @ cp@ code-origin . cr tier@ . cr\n" >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT CLEAN
   OUT swap S\" 1\n\n1\n\n" T$= ;

create PROBE-SOURCE 4096 allot
variable PROBE-SOURCE-U

: PROBE+ ( ptr u8 n -- ) {: a:ptr u:n :}
   PROBE-SOURCE-U @ u + 4096 <= TTRUE
   a PROBE-SOURCE PROBE-SOURCE-U @ + u BYTE-COPY
   PROBE-SOURCE-U @ u + PROBE-SOURCE-U ! ;

\ Compile the probe into the first image. Later children call the same code
\ through stdin, so capture comparisons add no definitions to DATA.
: WRITE-REQUIRE-PROBE ( -- )
   0 PROBE-SOURCE-U !
   S\" package IMAGE-REQUIRE-PROBE\ncreate SAVED INCLUDE-PATH-CAP allot\npublic\n" PROBE+
   S\" : BORROW ( ptr u8 n -- )\n   REQUIRE-N @ 1- {: idx:n :}\n   idx REQUIRE-SLOT {: old:ptr :}\n   idx REQUIRE-LEN@ {: len:n :}\n" PROBE+
   S\"    old SAVED len BYTE-COPY provided\n   idx REQUIRE-SLOT old <> if 70 throw then\n   idx REQUIRE-LEN@ len <> if 70 throw then\n" PROBE+
   S\"    idx REQUIRE-SLOT len SAVED len CORE-STR= 0= if 70 throw then ;\n" PROBE+
   S\" : FIRST-TWO ( -- )\n   s\q require-pool-one\q BORROW\n   s\q require-pool-two\q BORROW ;\n" PROBE+
   S\" : CHECK-FACT ( ptr u8 n -- )\n   REQUIRE-N @ {: count:n :}\n   required\n   REQUIRE-N @ count <> if 70 throw then ;\n" PROBE+
   S\" : CHECK-TWO ( -- )\n   s\q require-pool-one\q CHECK-FACT\n   s\q require-pool-two\q CHECK-FACT ;\n" PROBE+
   S\" : THIRD ( -- )\n   CHECK-TWO\n   s\q require-pool-three\q BORROW ;\n" PROBE+
   S\" : CHECK-THREE ( -- )\n   CHECK-TWO\n   s\q require-pool-three\q CHECK-FACT ;\n" PROBE+
   S\" : DISCARD ( -- )\n   REQUIRE-N @ {: count:n :}\n   REQUIRE-SNAPSHOT\n   s\q require-pool-discard\q provided\n   REQUIRE-RESTORE\n   REQUIRE-N @ count <> if 70 throw then ;\n" PROBE+
   S\" : CHECK-DISCARD ( -- )\n   s\q require-pool-discard\q SOURCE-ROOT:RESOLVE nip nip\n   if 70 throw then ;\n" PROBE+
   S\" : REPLACE ( -- )\n   REQUIRE-N @ 2 - {: idx:n :}\n   idx REQUIRE-SLOT {: old:ptr :}\n   idx REQUIRE-LEN@ {: len:n :}\n" PROBE+
   S\"    old SAVED len BYTE-COPY\n   REQUIRE-N @ 1- REQUIRE-REG:TRUNCATE\n   s\q require-pool-replace\q provided\n" PROBE+
   S\"    idx REQUIRE-SLOT old <> if 70 throw then\n   idx REQUIRE-LEN@ len <> if 70 throw then\n   idx REQUIRE-SLOT len SAVED len CORE-STR= 0= if 70 throw then ;\n" PROBE+
   S\" : CHECK-REPLACE ( -- )\n   s\q require-pool-replace\q SOURCE-ROOT:RESOLVE\n   {: a:ptr u:n known:bool :}\n   known 0= if 70 throw then\n" PROBE+
   S\"    REQUIRE-N @ 1- REQUIRE-LEN@ u <> if 70 throw then\n   REQUIRE-N @ 1- REQUIRE-SLOT u a u CORE-STR= 0= if 70 throw then\n   s\q require-pool-replace\q CHECK-FACT ;\n;package\n" PROBE+
   PROBE$ PROBE-SOURCE PROBE-SOURCE-U @ ATOMIC-WRITE-FILE ;

: BUILD ( -- )
   WRITE-REQUIRE-PROBE
   PROC-ARGV-ENV-RESET
   s" --" >LEN PROC-ARGV+
   IMAGE$ >LEN PROC-ARGV+
   PROBE$ >LEN PROC-ARGV+
   WHITEBOX-CHILD:ENV!
   WHITEBOX-CHILD:ENGINE$ >LEN
   S\" require src/habu/app-image.f\nrequire test/app-image-subject.f\n1 SCRIPT-ARGV$ required\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT CLEAN drop
   IMAGE$ EXECUTABLE? TTRUE ;

: CHECK-BUILD-SCOPE ( -- )
   REFUSE$
   S\" TRUSTED: REQUEST-JIT ( -- ) 0 set-tier ; immediate\ns\q REQUEST-JIT\q 0 parse-imm\n: NEVER-PUBLISHED ( -- ) REQUEST-JIT ;\n: MAIN ( -- ) ;\n"
   ATOMIC-WRITE-FILE
   PROC-ARGV-ENV-RESET
   s" --" >LEN PROC-ARGV+
   REFUSE$ >LEN PROC-ARGV+
   IMAGE$ >LEN PROC-ARGV+
   WHITEBOX-CHILD:ENV!
   WHITEBOX-CHILD:ENGINE$ >LEN
   S\" require tools/app-build.f\nAPP-BUILD:RUN\n" >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT {: outu:n erru:n rc:n :}
   s" application immediates cannot select JIT while building" T-LABEL
   rc 70 T=
   ERR erru s" executable build requires native tier 1" CONTAINS? TTRUE
   IMAGE$ EXISTS? TFALSE ;

: RUN-INPUT ( ptr u8 n ptr u8 n -- n n n )
   {: path:ptr pathu:n input:ptr inputu:n :}
   path pathu >LEN ROOT$ >LEN input inputu >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE RESULT ;

: CHECK-APPLICATION ( ptr u8 n -- )
   PROC-ARGV-ENV-RESET
   WHITEBOX-CHILD:ENV!
   S\" APP-IMAGE-SUBJECT:SCRATCH-CLEAN\nAPP-IMAGE-SUBJECT:RUN . cr\n: FRESH-INCREMENT ( n -- n ) 1+ ;\nAPP-IMAGE-SUBJECT:RUN FRESH-INCREMENT . cr\n" RUN-INPUT CLEAN
   OUT swap S\" 43\n\n44\n\n" T$= ;

: CHECK-REJECTION ( -- )
   PROC-ARGV-ENV-RESET
   WHITEBOX-CHILD:ENV!
   IMAGE$ S\" : FRESH-BAD ( n -- n ) 0= ;\n" RUN-INPUT
   {: outu:n erru:n rc:n :}
   rc 70 T= outu 0 T=
   ERR erru s" expected:" CONTAINS? TTRUE
   ERR erru s" actual:" CONTAINS? TTRUE ;

: RECAPTURE-TO ( ptr u8 n ptr u8 n -- )
   {: source:ptr sourceu:n target:ptr targetu:n :}
   PROC-ARGV-ENV-RESET
   s" --" >LEN PROC-ARGV+
   target targetu >LEN PROC-ARGV+
   WHITEBOX-CHILD:ENV!
   source sourceu S\" APP-IMAGE-SUBJECT:SCRATCH-CLEAN\nAPP-IMAGE-SUBJECT:RUN drop\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" RUN-INPUT CLEAN drop
   target targetu EXECUTABLE? TTRUE ;

\ Compare the emitted extents, independent of signing metadata. Region bytes
\ retain the application dictionary; DATA and the engine prefix must stop
\ growing when a fresh process recaptures without any new definitions.
: EXTENTS ( ptr u8 n -- n n n ) {: path:ptr pathu:n :}
   path pathu FILE-SIZE {: bytes:n :}
   bytes MEM-ALLOC-BYTES drop {: image:ptr :}
   path pathu image bytes READ-ALL bytes T=
   image IMAGE-TEXT-SIZE-OFF + CELL-VIEW @ {: text:n :}
   image text IMAGE-TEXT-TRAILER-ADJ + SNAP-TRL-BYTES - + {: trailer:ptr :}
   trailer CELL-VIEW @ SNAP-MAGIC T=
   trailer SNAP-TRL-REGLEN + CELL-VIEW @ {: region:n :}
   trailer SNAP-TRL-DATALEN + CELL-VIEW @ {: data:n :}
   image bytes munmap 0 T=
   text IMAGE-TEXT-CONTENT-ADJ - SNAP-TRL-BYTES - region - data -
   region data ;

: RECAPTURE ( -- )
   IMAGE$ SECOND$ RECAPTURE-TO
   SECOND$ THIRD$ RECAPTURE-TO
   IMAGE$ EXTENTS {: prefix:n region:n data:n :}
   SECOND$ EXTENTS data T= region T= prefix T=
   THIRD$ EXTENTS data T= region T= prefix T=
   THIRD$ CHECK-APPLICATION ;

: CAPTURE-PROBE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: source:ptr sourceu:n target:ptr targetu:n input:ptr inputu:n :}
   PROC-ARGV-ENV-RESET
   s" --" >LEN PROC-ARGV+
   target targetu >LEN PROC-ARGV+
   WHITEBOX-CHILD:ENV!
   source sourceu input inputu RUN-INPUT CLEAN drop
   target targetu EXECUTABLE? TTRUE ;

: CHECK-PROBE ( ptr u8 n ptr u8 n -- )
   {: source:ptr sourceu:n input:ptr inputu:n :}
   PROC-ARGV-ENV-RESET
   WHITEBOX-CHILD:ENV!
   source sourceu input inputu RUN-INPUT CLEAN drop ;

\ The baseline image owns DATA-backed rows. FIRST-TWO crosses to a fresh
\ mutable suffix, then appends again while the first new row is borrowed.
\ Each saved image is booted as a new process before the next capture.
: CHECK-REQUIRE-IMAGES ( -- )
   IMAGE$ MUTATED$
   S\" IMAGE-REQUIRE-PROBE:FIRST-TWO\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n"
   CAPTURE-PROBE
   MUTATED$ S\" IMAGE-REQUIRE-PROBE:CHECK-TWO\n" CHECK-PROBE
   MUTATED$ MUTATED-AGAIN$
   S\" IMAGE-REQUIRE-PROBE:THIRD\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n"
   CAPTURE-PROBE
   MUTATED-AGAIN$ S\" IMAGE-REQUIRE-PROBE:CHECK-THREE\n" CHECK-PROBE

   SECOND$ DISCARD$
   S\" IMAGE-REQUIRE-PROBE:DISCARD\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n"
   CAPTURE-PROBE
   SECOND$ EXTENTS {: prefix:n region:n data:n :}
   DISCARD$ EXTENTS data T= region T= prefix T=
   DISCARD$ S\" IMAGE-REQUIRE-PROBE:CHECK-DISCARD\n" CHECK-PROBE

   SECOND$ REPLACED$
   S\" IMAGE-REQUIRE-PROBE:REPLACE\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n"
   CAPTURE-PROBE
   REPLACED$ S\" IMAGE-REQUIRE-PROBE:CHECK-REPLACE\n" CHECK-PROBE ;

: CAPTURE-STARTUP ( -- )
   PROC-ARGV-ENV-RESET
   s" --" >LEN PROC-ARGV+
   STARTUP$ >LEN PROC-ARGV+
   WHITEBOX-CHILD:ENV!
   IMAGE$
   S\" 1 set-tier\npackage IMAGE-START-TEST\nvariable STARTS\n: MAIN ( -- ) 1 STARTS +! STARTS @ . cr SCRIPT-ARGC . cr SCRIPT-ARGC 0 ?do i SCRIPT-ARGV$ type cr loop SCRIPT-ARGC 0 > if 0 SCRIPT-ARGV$ s\q quit\q STR= if s\q \q 0 die then 0 SCRIPT-ARGV$ s\q throw\q STR= if -123 throw then 0 SCRIPT-ARGV$ s\q eval-throw\q STR= if s\q -124 throw\q INCLUDE-EVALUATE then then ;\n' MAIN\n;package\nAPP-IMAGE:START!\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n"
   RUN-INPUT CLEAN drop
   STARTUP$ EXECUTABLE? TTRUE ;

: CHECK-STARTUP ( -- )
   PROC-ARGV-ENV-RESET WHITEBOX-CHILD:ENV!
   STARTUP$ S\" APP-IMAGE-SUBJECT:RUN . cr\n" RUN-INPUT CLEAN
   OUT swap S\" 1\n\n0\n\n43\n\n" T$= ;

: STARTUP-ARGS ( -- )
   s" alpha" >LEN PROC-ARGV+
   s" two words" >LEN PROC-ARGV+
   WHITEBOX-CHILD:ENV!
   STARTUP$ S\" APP-IMAGE-SUBJECT:RUN . cr\n" RUN-INPUT CLEAN
   OUT swap S\" 1\n\n2\n\nalpha\ntwo words\n43\n\n" T$= ;

: CHECK-STARTUP-ARGS ( -- )
   PROC-ARGV-ENV-RESET STARTUP-ARGS
   PROC-ARGV-ENV-RESET s" --" >LEN PROC-ARGV+ STARTUP-ARGS ;

: CHECK-STARTUP-EXIT ( -- )
   PROC-ARGV-ENV-RESET
   s" quit" >LEN PROC-ARGV+ WHITEBOX-CHILD:ENV!
   STARTUP$ s" STARTUP-SHOULD-NOT-EXECUTE" RUN-INPUT CLEAN
   OUT swap S\" 1\n\n1\n\nquit\n" T$= ;

\ fd 0 must be a real terminal: startup failures used to enter REPL recovery
\ before its source frame existed. Output uses a small pipe so the diagnostic
\ remains byte-exact on Linux and macOS.
create PTY-NAME PTY:SLAVE-PATH-CAP allot

\ The pair is lib/pty.f's on both targets; the slave is opened here, O_NOCTTY,
\ because this process must not adopt the terminal it hands the child.
: OPEN-PTY ( -- n n )
   PTY-NAME PTY:SLAVE-PATH-CAP PTY:OPEN drop PTY:MASTER>N {: master:n :}
   master >FD FD-CLOEXEC!
   PTY-NAME PTY:PTY-OPEN-FLAGS 0 open {: slave:n :}
   slave 0 >= TTRUE
   master slave ;

: CHECK-STARTUP-THROW ( ptr u8 n ptr u8 n -- )
   {: arg:ptr argu:n diagnostic:ptr diagnosticu:n :}
   PROC-ARGV-ENV-RESET arg argu >LEN PROC-ARGV+ WHITEBOX-CHILD:ENV!
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
   s" throw" s" hb: uncaught throw code -123" CHECK-STARTUP-THROW
   s" eval-throw" s" hb: uncaught throw code -124" CHECK-STARTUP-THROW ;

: RUN-ADDRESS-OWNER ( bool -- ) {: native:bool :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   native if s" test/compiler/aot-mode.f" >LEN PROC-ARGV+ then
   s" test/native-window-owner-child.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" test/address-cell-owner.f" >LEN PROC-ARGV+
   SECOND$ >LEN OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE RESULT CLEAN
   OUT swap S\" address-cell-owner: ok\nwindow: 0\n" T$= ;

\ Restore a grown DATA-backed registry, then perform the actual source-window
\ rewind. Its complete backing span must survive outside the retiring heap.
: CHECK-ADDRESS-OWNER ( -- )
   PROC-ARGV-ENV-RESET
   s" --" >LEN PROC-ARGV+ SECOND$ >LEN PROC-ARGV+
   WHITEBOX-CHILD:ENV!
   WHITEBOX-CHILD:ENGINE$ >LEN
   S\" require src/habu/app-image.f\nrequire test/address-cell-cap-grown.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT CLEAN drop
   SECOND$ EXECUTABLE? TTRUE
   0 0 <> RUN-ADDRESS-OWNER
   0 0 = RUN-ADDRESS-OWNER ;

: CASES ( -- )
   CHECK-BUILD-TIER
   CHECK-BUILD-SCOPE
   BUILD
   IMAGE$ CHECK-APPLICATION
   CHECK-REJECTION
   RECAPTURE
   CHECK-REQUIRE-IMAGES
   SECOND$ CHECK-APPLICATION
   CAPTURE-STARTUP
   CHECK-STARTUP
   CHECK-STARTUP-ARGS
   CHECK-STARTUP-EXIT
   CHECK-STARTUP-THROWS
   CHECK-ADDRESS-OWNER ;

: RUN ( -- )
   T-RESET
   [: PREPARE CASES ;] [: CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
