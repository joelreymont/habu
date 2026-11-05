\ One stripped application of the run-time subjects and a generated chain, built
\ through tools/hb-build.f, measured, read for its startup shape and run per exit.
require test/gate-common.f
require test/gate-aot-image.f
require src/lib/string.f
require src/lib/engine-candidate.f
require test/aot-builder-engine.f

package STRIPPED-IMAGE-TEST
private

600000 constant TIMEOUT-MS

\ HOLE-BYTES is test/stripped-sparse-data-subject.f's hole; IMAGE-MAX sits below
\ it and above the arm64 image (264,764 bytes); x86-64's is unmeasured here.
1000000 constant HOLE-BYTES
$60000 constant IMAGE-MAX

create SUBJECT PATH-CAP allot
create IMAGE PATH-CAP allot
create DIE-TREE PATH-CAP allot
create EXIT-TREE PATH-CAP allot
variable SUBJECT-U
variable IMAGE-U
variable DIE-TREE-U
variable EXIT-TREE-U

: SUBJECT$ ( -- ptr u8 n ) SUBJECT SUBJECT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;
: DIE-TREE$ ( -- ptr u8 n ) DIE-TREE DIE-TREE-U @ ;
: EXIT-TREE$ ( -- ptr u8 n ) EXIT-TREE EXIT-TREE-U @ ;

: PREPARE ( -- )
   s" stripped-image" GT-START
   s" subject.f" SUBJECT GT-PATH SUBJECT-U !
   s" application" IMAGE GT-PATH IMAGE-U !
   s" die-tree" DIE-TREE GT-PATH DIE-TREE-U ! ;

\ CHAIN-N chained words pass a 1024-member fixed table (a lost one prints no
\ chain=ok); OPEN-PATH reaches PATH0's scratch (aot-owned-cells.f FRESH-BYTES).
1100 constant CHAIN-N

: SB-U+ ( n -- ) {: n:n :}
   n 0 < if E-STR-BOUNDS throw then
   n 10 >= if n 10 / recurse then
   n 10 mod STR-ZERO + SB-APPEND-C ;

: CHAIN-WORD! ( n -- ) {: i:n :}
   SB-RESET
   s" : CW" SB-APPEND i SB-U+ s"  ( n -- n ) " SB-APPEND
   i 0 > if s" CW" SB-APPEND i 1- SB-U+ s"  " SB-APPEND then
   s" dup 0 < if drop 0 then 1 + ;" SB-APPEND 10 SB-APPEND-C
   SUBJECT$ SB$ FS:APPEND-FILE ;

: WRITE-CHAIN ( -- )
   SB-RESET
   S\" package STRIPPED-IMAGE-CHAIN\nprivate\n" SB-APPEND
   S\" variable FD\n: OPEN-PATH ( -- )\n   s\" /dev/null\" PATH0 0 0 open FD !\n" SB-APPEND
   S\"    FD @ 0 < IF s\" openpath: cannot open /dev/null\" 74 die THEN\n" SB-APPEND
   S\"    FD @ close\n   s\" opened\" type cr ;\n" SB-APPEND
   SUBJECT$ SB$ FS:APPEND-FILE
   CHAIN-N 0 ?do i CHAIN-WORD! loop
   SB-RESET
   s" public" SB-APPEND 10 SB-APPEND-C
   s" : RUN ( -- ) 0 CW" SB-APPEND CHAIN-N 1- SB-U+
   s"  " SB-APPEND CHAIN-N SB-U+
   S\"  = if s\" chain=ok\" type cr then OPEN-PATH ;\n;package\n" SB-APPEND
   SUBJECT$ SB$ FS:APPEND-FILE ;

\ MAIN is per host: macOS starts and stops AIO, Linux links aio.f idle. The
\ exit-hook subject runs first, and UNMAP ends the process, so it comes last.
: WRITE-SUBJECT ( -- )
   SUBJECT$ S\" require test/stripped-image-subject.f\nrequire test/exit-hook-subject.f\n" FS:WRITE-ALL
   WRITE-CHAIN
   SB-RESET
   S\" : MAIN ( -- )\n   0 SCRIPT-ARGV$ s\" bounds\" STR= if DATA-SIZE allot exit then\n" SB-APPEND
   S\"    EXIT-HOOK-SUBJECT:RUN\n   EXIT-HOOK-SUBJECT:RETURN? if EXIT-HOOK-SUBJECT:FINISH exit then\n" SB-APPEND
   S\"    s\" global-main\" type cr\n   STRIPPED-IMAGE-CHAIN:RUN\n   STRIPPED-IMAGE-SUBJECT:RUN\n" SB-APPEND
   HB-TARGET-MACOS? if S\"    AIO:START AIO:STOP\n" SB-APPEND then
   S\"    STRIPPED-IMAGE-SUBJECT:UNMAP ;\n" SB-APPEND
   SUBJECT$ SB$ FS:APPEND-FILE ;

\ The fresh subject path has no observed-input index, so hb-build links it
\ with the shared maker instead of restoring an artifact.
: BUILD ( -- )
   AOT-BUILDER-ENGINE:ENSURE
   GE-HB-RESET
   ENGINE-CANDIDATE:PATH$ GE-ARGV+
   s" --load" GE-ARG+ s" tools/hb-build.f" GE-ARG+
   s" --" GE-ARG+ SUBJECT$ GE-ARG+
   s" -o" GE-ARG+ IMAGE$ GE-ARG+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   ENGINE-CANDIDATE:PATH$ TIMEOUT-MS GE-RUN-ENV
   s" stripped image build" GE-EXPECT-OK
   GT-ERR$ nip 0<> if s" stripped image build stderr" GE-FAIL then
   IMAGE$ FS:EXECUTABLE? 0= if s" stripped image executable" GE-FAIL then ;

\ The size says the hole did not travel; the sparse subject's marks say the
\ bytes that must travel did (an image storing nothing satisfies the size).
: CHECK-SIZE ( -- )
   IMAGE$ FS:FILE-SIZE {: bytes:n :}
   s" stripped-image: image bytes " type bytes .
   bytes HOLE-BYTES < 0= if s" stripped image carries the DATA hole" GE-FAIL then
   bytes IMAGE-MAX < 0= if s" stripped image exceeds IMAGE-MAX" GE-FAIL then ;

: EXPECTED-OUT$ ( -- ptr u8 n )
   S\" exit-hook-subject: ok\nglobal-main\nchain=ok\nopened\nstripped-quotation: ok\nstripped-address-pool: ok\n8\n0\n1\n23\nstripped-sparse-data: ok\nstripped\nsize=ok\nstripped-engine-id: ok\naot-xt-cells: ok\nstripped-lifecycle-prepare: ok\nstripped-lifecycle-tasks: ok\nstripped-lifecycle-semaphore: ok\nffi-library-lifetime: ok\n42\n123\nba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad\n" ;

\ A failing subject dies with its message and exit $4A; a missing xt row is a
\ wild call into unmapped builder code. UNMAP's `die` is the exit the registered
\ directory has to go on.
: RUN-IMAGE ( -- )
   GE-HB-RESET
   IMAGE$ GE-ARGV+
   DIE-TREE$ GE-ARG+
   IMAGE$ TIMEOUT-MS GE-RUN-ENV
   71 s" stripped image exit through MEM:UNMAP" GE-EXPECT-RC
   EXPECTED-OUT$ s" stripped image exact stdout" GE-EXPECT-OUT
   S\" memory: unmap failed\n" s" stripped image exact stderr" GE-EXPECT-ERR
   DIE-TREE$ FS:EXISTS? if s" a stripped application's die removes the registered tree" GE-FAIL then ;

\ MAIN leaves by the code it is given, in a directory named for it. Code 0 is
\ the entry's own return through the exit vector to exit(0); any other is an
\ uncaught throw (docs/debugging.md "Reading an exit status"): 1-255 is the
\ status, silently; any other is named on stderr and exits 67. Every exit runs
\ the vector.
: RUN-EXIT ( ptr u8 n n ptr u8 n -- ) {: code:ptr codeu:n rc:n err:ptr erru:n :}
   code codeu EXIT-TREE GT-PATH EXIT-TREE-U !
   GE-HB-RESET
   IMAGE$ GE-ARGV+
   EXIT-TREE$ GE-ARG+
   code codeu GE-ARG+
   IMAGE$ TIMEOUT-MS GE-RUN-ENV
   rc s" stripped image exit status" GE-EXPECT-RC
   S\" exit-hook-subject: ok\n" s" stripped image exit stdout" GE-EXPECT-OUT
   err erru s" stripped image exit stderr" GE-EXPECT-ERR
   EXIT-TREE$ FS:EXISTS? if s" a stripped application's exit removes the registered tree" GE-FAIL then ;

: RUN-EXITS ( -- )
   s" 0" 0 s" " RUN-EXIT
   s" 42" 42 s" " RUN-EXIT
   s" 256" 67 S\" hb: uncaught throw code 256\n" RUN-EXIT
   s" -2500" 67 S\" hb: uncaught throw code -2500\n" RUN-EXIT ;

: RUN-BOUNDS ( -- )
   \ A run of its own: allot past the bound ends the process.
   GE-HB-RESET
   IMAGE$ GE-ARGV+ s" bounds" GE-ARG+
   IMAGE$ TIMEOUT-MS GE-RUN-ENV
   76 s" stripped data-space bound exit" GE-EXPECT-RC
   s" hb: data space out of range: DP " s" stripped data-space bound diagnostic" GE-EXPECT-ERR-HAS
   s"  of " s" stripped data-space bound ceiling" GE-EXPECT-ERR-HAS
   S\"  bytes\n" s" stripped data-space bound units and newline" GE-EXPECT-ERR-HAS ;

\ The ARM startup carries the xt-cell apply loop, so it must satisfy the image
\ gate's ARM instruction model; RUN-IMAGE and RUN-EXITS check the x86 behavior.
: CHECK-SHAPE ( -- )
   HB-TARGET-LINUX-X86-64? if exit then
   IMAGE$ AOT-IMAGE:CODE-RANGE 2drop ;

: BODY ( -- )
   PREPARE WRITE-SUBJECT BUILD CHECK-SIZE RUN-IMAGE RUN-EXITS RUN-BOUNDS CHECK-SHAPE
   s" PASS: stripped image subjects" type cr ;

public
: RUN ( -- ) [: BODY ;] [: GT-CLEANUP ;] finally ;

RUN
;package
