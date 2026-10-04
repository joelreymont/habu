\ Build one stripped application from the family's run-time subjects
\ (test/stripped-image-subject.f) through tools/hb-build.f, then measure it,
\ read its startup shape and run it through its real entry. One build serves
\ every subject: each prints its line or dies naming itself, so the exact
\ stdout says which one failed. The same image runs twice for
\ test/exit-hook-subject.f, once per exit a stripped application takes, and
\ each run must leave the directory it registered removed.
require test/gate-common.f
require test/gate-aot-image.f
require lib/engine-candidate.f

package STRIPPED-IMAGE-TEST
private

600000 constant TIMEOUT-MS

\ HOLE-BYTES is test/stripped-sparse-data-subject.f's hole: an image that
\ carried it is at least that long, as that subject's image alone was before
\ sparse DATA extents (1,704,128 bytes). IMAGE-MAX bounds this whole image,
\ every subject's code and DATA together, which measured 165,372 bytes, so it
\ fails the image if it grows by 96,772 bytes or more, whatever grows.
\ Being below HOLE-BYTES it also refuses the hole by itself; the HOLE-BYTES
\ check names that failure.
1000000 constant HOLE-BYTES
$40000 constant IMAGE-MAX

create SUBJECT FS-PATH-CAP allot
create IMAGE FS-PATH-CAP allot
create DIE-TREE FS-PATH-CAP allot
create RETURN-TREE FS-PATH-CAP allot
variable SUBJECT-U
variable IMAGE-U
variable DIE-TREE-U
variable RETURN-TREE-U

: SUBJECT$ ( -- ptr u8 n ) SUBJECT SUBJECT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;
: DIE-TREE$ ( -- ptr u8 n ) DIE-TREE DIE-TREE-U @ ;
: RETURN-TREE$ ( -- ptr u8 n ) RETURN-TREE RETURN-TREE-U @ ;

: PREPARE ( -- )
   s" stripped-image" GT-START
   s" subject.f" SUBJECT GT-PATH SUBJECT-U !
   s" application" IMAGE GT-PATH IMAGE-U !
   s" die-tree" DIE-TREE GT-PATH DIE-TREE-U !
   s" return-tree" RETURN-TREE GT-PATH RETURN-TREE-U ! ;

\ MAIN is written per host: the macOS image also starts and stops AIO, the
\ run-time use of lib/aio.f's definers; the Linux image links lib/aio.f without
\ starting it. The exit-hook subject registers its directory first, and its
\ return run leaves before the other subjects. UNMAP comes last because it ends
\ the process.
: WRITE-SUBJECT ( -- )
   SB-RESET
   S\" require test/stripped-image-subject.f\nrequire test/exit-hook-subject.f\n" SB-APPEND
   S\" : MAIN ( -- )\n   EXIT-HOOK-SUBJECT:RUN\n   EXIT-HOOK-SUBJECT:RETURN? if exit then\n" SB-APPEND
   S\"    STRIPPED-IMAGE-SUBJECT:RUN\n" SB-APPEND
   HB-TARGET-MACOS? if S\"    AIO:START AIO:STOP\n" SB-APPEND then
   S\"    STRIPPED-IMAGE-SUBJECT:UNMAP ;\n" SB-APPEND
   SUBJECT$ SB$ WRITE-ALL ;

\ A private cache root, as the entry rows use: the maker links this subject on
\ every run instead of hb-build restoring an artifact by its content key.
: BUILD ( -- )
   GE-HB-RESET
   ENGINE-CANDIDATE:PATH$ GE-ARGV+
   s" --load" GE-ARG+ s" tools/hb-build.f" GE-ARG+
   s" --" GE-ARG+ SUBJECT$ GE-ARG+
   s" -o" GE-ARG+ IMAGE$ GE-ARG+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   s" HABU_BUILD_CACHE" >LEN GT-ROOT >LEN PROC-ENV+
   ENGINE-CANDIDATE:PATH$ TIMEOUT-MS GE-RUN-ENV
   s" stripped image build" GE-EXPECT-OK
   GT-ERR$ nip 0<> if s" stripped image build stderr" GE-FAIL then
   IMAGE$ EXECUTABLE? 0= if s" stripped image executable" GE-FAIL then ;

\ The size says the hole did not travel; the sparse subject's marks say the
\ bytes that must travel did, since an image that stored nothing would satisfy
\ the size alone.
: CHECK-SIZE ( -- )
   IMAGE$ FILE-SIZE {: bytes:n :}
   s" stripped-image: image bytes " type bytes .
   bytes HOLE-BYTES < 0= if s" stripped image carries the DATA hole" GE-FAIL then
   bytes IMAGE-MAX < 0= if s" stripped image exceeds IMAGE-MAX" GE-FAIL then ;

: EXPECTED-OUT$ ( -- ptr u8 n )
   S\" exit-hook-subject: ok\nstripped-quotation: ok\n8\n0\n1\n23\nstripped-sparse-data: ok\nstripped\nsize=ok\nstripped-engine-id: ok\naot-xt-cells: ok\nstripped-lifecycle-prepare: ok\nstripped-lifecycle-tasks: ok\nstripped-lifecycle-semaphore: ok\n" ;

\ A subject that fails dies with its own message and exit $4A, and a missing
\ xt row is a wild call into builder code this process does not map, so only an
\ image in which every subject passed and the literal survived meets all three.
\ UNMAP's `die` is also the exit the registered directory has to go on.
: RUN-IMAGE ( -- )
   GE-HB-RESET
   IMAGE$ GE-ARGV+
   DIE-TREE$ GE-ARG+
   IMAGE$ TIMEOUT-MS GE-RUN-ENV
   71 s" stripped image exit through MEM:UNMAP" GE-EXPECT-RC
   EXPECTED-OUT$ s" stripped image exact stdout" GE-EXPECT-OUT
   S\" memory: unmap failed\n" s" stripped image exact stderr" GE-EXPECT-ERR
   DIE-TREE$ EXISTS? if s" a stripped application's die removes the registered tree" GE-FAIL then ;

\ The application's own return: its entry calls the exit vector inline on the
\ way to exit(0).
: RUN-RETURN ( -- )
   GE-HB-RESET
   IMAGE$ GE-ARGV+
   RETURN-TREE$ GE-ARG+
   s" return" GE-ARG+
   IMAGE$ TIMEOUT-MS GE-RUN-ENV
   s" stripped image return" GE-EXPECT-OK
   S\" exit-hook-subject: ok\n" s" stripped image return stdout" GE-EXPECT-OUT
   s" " s" stripped image return stderr" GE-EXPECT-ERR
   RETURN-TREE$ EXISTS? if s" a stripped application's own exit removes the registered tree" GE-FAIL then ;

\ The ARM startup carries the xt-cell apply loop, so it has to keep satisfying
\ the image gate's ARM instruction model. The x86 image's actual DATA, xt-cell,
\ root and exit behavior is checked by RUN-IMAGE and RUN-RETURN above.
: CHECK-SHAPE ( -- )
   HB-TARGET-LINUX-X86-64? if exit then
   IMAGE$ AOT-IMAGE:CODE-RANGE 2drop ;

: BODY ( -- )
   PREPARE WRITE-SUBJECT BUILD CHECK-SIZE RUN-IMAGE RUN-RETURN CHECK-SHAPE
   s" PASS: stripped image subjects" type cr ;

public
: RUN ( -- ) [: BODY ;] [: GT-CLEANUP ;] finally ;

RUN
;package
