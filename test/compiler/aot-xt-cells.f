\ Build one stripped application whose persistent data holds DECLARED execution
\ tokens, and run the fresh image through its real entry.
\
\ RED BEFORE THIS LANE: hb-build refused the subject outright - "stripped AOT
\ persistent data holds a code/dict pointer (defer or ' word ,)", exit 70 - so a
\ program that binds a defer at load time could ship only as a --repl image
\ (dot habu-let-a-stripped-0a064bf5). The image now carries one row per declared
\ cell and restores it against its own code base, so this runs.
require lib/test.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-candidate.f
\ The image gate reads a built image the way the positive AOT gate does, so it
\ arrives with the same stack under it (test/gate-aot-positive.f).
require lib/source.f
require lib/build.f
require lib/codesign.f
require tools/build-fixpoint.f
require tools/cli-run.f
require tools/hb-build-lib.f
require tools/json.f
require tools/gate-json-assert-core.f
require tools/aot-call-report-lib.f
require test/gate-common.f
require test/gate-build-common.f
require test/gate-aot-image.f

package AOT-XT-CELL-TEST

$10000 constant CAP
600000 constant TIMEOUT-MS
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
   s" aot-xt-cells" HB-TMP-MKDIR {: path:ptr pathu:n :}
   path ROOT-BUF pathu BYTE-COPY pathu ROOT-U !
   ROOT$ CLEANUP-TREE+
   SOURCE-ROOT:CURRENT$ s" aot-xt-cells-subject.f"
      SUBJECT-BUF JOIN-PATH SUBJECT-U !
   ROOT$ s" application" IMAGE-BUF JOIN-PATH IMAGE-U ! ;

: SHOW-FAILURE ( n n n -- ) {: outu:n erru:n rc:n :}
   rc 0<> erru 0<> or if
      s" aot-xt-cells child failed: " type rc . cr
      OUT outu type ERR erru type
   then ;

\ No --repl: this is the stripped build, which is what the lane is about.
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

\ The image's own output is the whole proof: an unrelocated cell holds a builder
\ address that this process does not map, so a missing row is a wild call and not
\ a wrong answer.
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
   OUT outu LEN>N S\" aot-xt-cells: ok\n" T$= ;

\ The startup carries an apply loop now, so it has to keep satisfying the image
\ gate's model of one: exactly one ADR x9 (the DATA copy, whose whole row and
\ byte loop the gate re-reads instruction by instruction), the root call
\ immediately after the startup's exit tail, and a reported code range that ends
\ at the data blob - which is what keeps the xt rows, placed after the blob, out
\ of every instruction reader. CODE-RANGE throws if any of that stops holding.
: CHECK-IMAGE-SHAPE ( -- )
   IMAGE$ AOT-IMAGE:CODE-RANGE 2drop ;

: BODY ( -- )
   PREPARE
   BUILD if RUN-IMAGE CHECK-IMAGE-SHAPE then ;

: RUN ( -- )
   T-RESET
   [: BODY ;] [: CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
