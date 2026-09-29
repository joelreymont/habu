\ Build one stripped application from the family's run-time subjects
\ (test/stripped-image-subject.f) through tools/hb-build.f, then measure it,
\ read its startup shape and run it through its real entry. One build serves
\ every subject: each prints its line or dies naming itself, so the exact
\ stdout says which one failed.
require test/gate-common.f
require test/gate-aot-image.f
require lib/engine-candidate.f

package STRIPPED-IMAGE-TEST
private

600000 constant TIMEOUT-MS

\ test/stripped-sparse-data-subject.f's hole, and the ceiling the image stays
\ under. An image that carried the hole is at least HOLE-BYTES; before sparse
\ DATA extents that subject's image alone measured 1,704,128 bytes.
1000000 constant HOLE-BYTES
$40000 constant IMAGE-MAX

create SUBJECT FS-PATH-CAP allot
create IMAGE FS-PATH-CAP allot
variable SUBJECT-U
variable IMAGE-U

: SUBJECT$ ( -- ptr u8 n ) SUBJECT SUBJECT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;

: PREPARE ( -- )
   s" stripped-image" GT-START
   s" subject.f" SUBJECT GT-PATH SUBJECT-U !
   s" application" IMAGE GT-PATH IMAGE-U ! ;

\ MAIN is written per host: the macOS image also starts and stops AIO, the
\ run-time use of lib/aio.f's definers; the Linux image links lib/aio.f without
\ starting it. UNMAP comes last because it ends the process.
: WRITE-SUBJECT ( -- )
   SB-RESET
   S\" require test/stripped-image-subject.f\n: MAIN ( -- )\n   STRIPPED-IMAGE-SUBJECT:RUN\n" SB-APPEND
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
   S\" stripped-quotation: ok\n8\n0\n1\n23\nstripped-sparse-data: ok\nstripped\nsize=ok\naot-xt-cells: ok\nstripped-lifecycle-prepare: ok\nstripped-lifecycle-tasks: ok\n" ;

\ A subject that fails dies with its own message and exit $4A, and a missing
\ xt row is a wild call into builder code this process does not map, so only an
\ image in which every subject passed and the literal survived meets all three.
: RUN-IMAGE ( -- )
   GE-HB-RESET
   IMAGE$ GE-ARGV+
   IMAGE$ TIMEOUT-MS GE-RUN-ENV
   71 s" stripped image exit through MEM:UNMAP" GE-EXPECT-RC
   EXPECTED-OUT$ s" stripped image exact stdout" GE-EXPECT-OUT
   S\" memory: unmap failed\n" s" stripped image exact stderr" GE-EXPECT-ERR ;

\ The startup carries the xt-cell apply loop, so it has to keep satisfying the
\ image gate's model of one: exactly one ADR x9 (the DATA copy, whose row and
\ byte loop the gate re-reads instruction by instruction), the root call
\ immediately after the startup's exit tail, and a reported code range that ends
\ at the data blob - which keeps the xt rows, placed after the blob, out of
\ every instruction reader. CODE-RANGE throws if any of that stops holding.
: CHECK-SHAPE ( -- )
   IMAGE$ AOT-IMAGE:CODE-RANGE 2drop ;

: BODY ( -- )
   PREPARE WRITE-SUBJECT BUILD CHECK-SIZE RUN-IMAGE CHECK-SHAPE
   s" PASS: stripped image subjects" type cr ;

public
: RUN ( -- ) [: BODY ;] [: GT-CLEANUP ;] finally ;

RUN
;package
