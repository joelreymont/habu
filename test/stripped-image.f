\ Build one stripped application from the run-time subjects
\ (test/stripped-image-subject.f) through tools/hb-build.f, measure it, read its
\ startup shape and run it. Each subject prints its line or dies naming itself;
\ the same image runs test/exit-hook-subject.f once per exit, and each run must
\ remove the directory it registered.
require test/gate-common.f
require test/gate-aot-image.f
require src/lib/string.f
require src/lib/engine-candidate.f
require test/aot-builder-engine.f

package STRIPPED-IMAGE-TEST
private

600000 constant TIMEOUT-MS

\ HOLE-BYTES is test/stripped-sparse-data-subject.f's hole: an image carrying it
\ is at least that long (that subject's image alone was 1,704,128 bytes before
\ sparse DATA extents). IMAGE-MAX bounds the whole image: ARM measured 181,884
\ bytes, bound 80,260 above; the x86-64 fixed-page ELF measured 327,680 bytes,
\ bound one 64 KiB page above. Below HOLE-BYTES, it also refuses the hole alone.
1000000 constant HOLE-BYTES
: IMAGE-MAX ( -- n ) HB-TARGET-LINUX-X86-64? if $60000 else $40000 then ;

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

\ MAIN is per host: the macOS image starts and stops AIO (src/lib/aio.f's
\ definers at run time); the Linux image links aio.f without starting it. The
\ exit-hook subject registers its directory first and a run given its code
\ leaves before the other subjects; UNMAP ends the process, so it comes last.
: WRITE-SUBJECT ( -- )
   SB-RESET
   S\" require test/stripped-image-subject.f\nrequire test/exit-hook-subject.f\n" SB-APPEND
   S\" : MAIN ( -- )\n   EXIT-HOOK-SUBJECT:RUN\n   EXIT-HOOK-SUBJECT:RETURN? if EXIT-HOOK-SUBJECT:FINISH exit then\n" SB-APPEND
   S\"    STRIPPED-IMAGE-SUBJECT:RUN\n" SB-APPEND
   HB-TARGET-MACOS? if S\"    AIO:START AIO:STOP\n" SB-APPEND then
   S\"    STRIPPED-IMAGE-SUBJECT:UNMAP ;\n" SB-APPEND
   SUBJECT$ SB$ FS:WRITE-ALL ;

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
   S\" exit-hook-subject: ok\nstripped-quotation: ok\nstripped-address-pool: ok\n8\n0\n1\n23\nstripped-sparse-data: ok\nstripped\nsize=ok\nstripped-engine-id: ok\naot-xt-cells: ok\nstripped-lifecycle-prepare: ok\nstripped-lifecycle-tasks: ok\nstripped-lifecycle-semaphore: ok\nffi-library-lifetime: ok\n" ;

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

\ The ARM startup carries the xt-cell apply loop, so it must satisfy the image
\ gate's ARM instruction model; RUN-IMAGE and RUN-EXITS check the x86 behavior.
: CHECK-SHAPE ( -- )
   HB-TARGET-LINUX-X86-64? if exit then
   IMAGE$ AOT-IMAGE:CODE-RANGE 2drop ;

: BODY ( -- )
   PREPARE WRITE-SUBJECT BUILD CHECK-SIZE RUN-IMAGE RUN-EXITS CHECK-SHAPE
   s" PASS: stripped image subjects" type cr ;

public
: RUN ( -- ) [: BODY ;] [: GT-CLEANUP ;] finally ;

RUN
;package
