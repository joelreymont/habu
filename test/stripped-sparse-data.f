\ A stripped image stores its persistent DATA as the span's non-zero extents, not
\ as the span. Build one application whose DATA is a million-byte `allot`ed hole
\ between two initialised cells, then measure the image and run it.
\
\ RED BEFORE THE SPARSE IMAGE: the whole span was emitted verbatim, so this
\ subject's image measured 1,704,128 bytes of which 99.9% were zero. The size
\ assertion below is what says the hole no longer travels; the subject itself
\ (test/stripped-sparse-data-subject.f) is what says the bytes that must travel
\ still do, since an image that stored nothing would satisfy the size alone.
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-candidate.f

package STRIPPED-SPARSE-DATA-TEST

$10000 constant CAP
600000 constant TIMEOUT-MS

\ The subject's own hole, and the ceiling the image has to stay under. An image
\ that carried the hole is at least HOLE-BYTES; the sparse image measured 65,728,
\ which is the executable's page-aligned floor rather than its content.
1000000 constant HOLE-BYTES
$40000 constant IMAGE-MAX

create OUT CAP allot
create ERR CAP allot
create ROOT-BUF FS-PATH-CAP allot
create SUBJECT-BUF FS-PATH-CAP allot
create IMAGE-BUF FS-PATH-CAP allot
variable ROOT-U
variable SUBJECT-U
variable IMAGE-U

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: SUBJECT$ ( -- ptr u8 n ) SUBJECT-BUF SUBJECT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE-BUF IMAGE-U @ ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" stripped-sparse-data" TMPDIR-MKDIR {: path:ptr pathu:n :}
   path ROOT-BUF pathu BYTE-COPY pathu ROOT-U !
   ROOT$ CLEANUP-TREE+
   SOURCE-ROOT:CURRENT$ s" stripped-sparse-data-subject.f"
      SUBJECT-BUF JOIN-PATH SUBJECT-U !
   ROOT$ s" application" IMAGE-BUF JOIN-PATH IMAGE-U ! ;

: SHOW-FAILURE ( n n n -- ) {: outu:n erru:n rc:n :}
   rc 0<> erru 0<> or if
      s" stripped-sparse-data child failed: " type rc . cr
      OUT outu type ERR erru type
   then ;

: BUILD ( -- bool )
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/hb-build.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   SUBJECT$ >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   IMAGE$ >LEN PROC-ARGV+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N erru LEN>N rc SHOW-FAILURE
   rc 0 T=
   erru LEN>N 0 T=
   IMAGE$ EXECUTABLE? TTRUE
   rc 0= IMAGE$ EXECUTABLE? and ;

: REPORT-SIZE ( n -- ) {: bytes:n :}
   s" stripped-sparse-data: image " type bytes . s" bytes, hole " type
   HOLE-BYTES . cr ;

: CHECK-SIZE ( -- )
   IMAGE$ FILE-SIZE {: bytes:n :}
   bytes REPORT-SIZE
   bytes 0 > TTRUE
   bytes HOLE-BYTES < TTRUE
   bytes IMAGE-MAX < TTRUE ;

: RUN-IMAGE ( -- )
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   IMAGE$ >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N erru LEN>N rc SHOW-FAILURE
   rc 0 T=
   erru LEN>N 0 T=
   OUT outu LEN>N S\" stripped-sparse-data: ok\n" T$= ;

: BODY ( -- )
   PREPARE
   BUILD if CHECK-SIZE RUN-IMAGE then ;

: RUN ( -- )
   T-RESET
   [: BODY ;] [: CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
