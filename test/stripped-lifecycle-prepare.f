\ Build one stripped application whose load-time task operation cached the
\ maker's foreign addresses, and run the fresh image through its real entry.
require lib/test.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-candidate.f

package STRIPPED-LIFECYCLE-PREPARE-TEST

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
   s" stripped-lifecycle-prepare" HB-TMP-MKDIR {: path:ptr pathu:n :}
   path ROOT-BUF pathu BYTE-COPY pathu ROOT-U !
   ROOT$ CLEANUP-TREE+
   SOURCE-ROOT:CURRENT$ s" stripped-lifecycle-prepare-subject.f"
      SUBJECT-BUF JOIN-PATH SUBJECT-U !
   ROOT$ s" application" IMAGE-BUF JOIN-PATH IMAGE-U ! ;

: SHOW-FAILURE ( n n n -- ) {: outu:n erru:n rc:n :}
   rc 0<> erru 0<> or if
      s" stripped-lifecycle-prepare child failed: " type rc . cr
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
   OUT outu LEN>N S\" stripped-lifecycle-prepare: ok\n" T$= ;

: BODY ( -- )
   PREPARE
   BUILD if RUN-IMAGE then ;

: RUN ( -- )
   T-RESET
   [: BODY ;] [: CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
