\ Build stripped applications whose LOAD-TIME task work left process-local state
\ in the image's data - the maker's foreign addresses in lib/task.f's eight XT
\ cells, and a TCB holding this process's thread and mappings - and run each
\ fresh image through its real entry. The third case is the refusal: a task still
\ activated when the capture begins.
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
create RUNNING-BUF FS-PATH-CAP allot
variable ROOT-U
variable SUBJECT-U
variable IMAGE-U
variable RUNNING-U

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: SUBJECT$ ( -- ptr u8 n ) SUBJECT-BUF SUBJECT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE-BUF IMAGE-U @ ;
: RUNNING$ ( -- ptr u8 n ) RUNNING-BUF RUNNING-U @ ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" stripped-lifecycle-prepare" HB-TMP-MKDIR {: path:ptr pathu:n :}
   path ROOT-BUF pathu BYTE-COPY pathu ROOT-U !
   ROOT$ CLEANUP-TREE+
   SOURCE-ROOT:CURRENT$ s" stripped-lifecycle-running-subject.f"
      RUNNING-BUF JOIN-PATH RUNNING-U ! ;

\ One case: a subject file beside this one, and the image it builds under the
\ scratch root.
: CASE! ( ptr u8 n ptr u8 n -- ) {: subj:ptr subju:n img:ptr imgu:n :}
   SOURCE-ROOT:CURRENT$ subj subju SUBJECT-BUF JOIN-PATH SUBJECT-U !
   ROOT$ img imgu IMAGE-BUF JOIN-PATH IMAGE-U ! ;

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

: RUN-IMAGE ( ptr u8 n -- ) {: want:ptr wantu:n :}
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   IMAGE$ >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N erru LEN>N rc SHOW-FAILURE
   rc 0 T=
   erru LEN>N 0 T=
   OUT outu LEN>N want wantu T$= ;

\ THE REFUSAL IS MEASURED AT IMAGE-LIFECYCLE:PREPARE AND NOT THROUGH hb-build,
\ because hb-build's later phases mutate the dictionary and a live task refuses
\ that first, with a bare exit $4F and no message at all (measured on this
\ engine). The subject calls the one word both capture paths run, so what is
\ pinned here is the sweep's own answer for an activated task.
: RUNNING-REFUSED ( -- )
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   RUNNING$ >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   rc 0 T<>
   ERR erru LEN>N s" activated task at capture" CONTAINS? TTRUE
   ERR erru LEN>N s" TCB.THREAD" CONTAINS? TTRUE ;

: BODY ( -- )
   PREPARE
   s" stripped-lifecycle-prepare-subject.f" s" application" CASE!
   BUILD if S\" stripped-lifecycle-prepare: ok\n" RUN-IMAGE then
   s" stripped-lifecycle-tasks-subject.f" s" tasks-application" CASE!
   BUILD if S\" stripped-lifecycle-tasks: ok\n" RUN-IMAGE then
   RUNNING-REFUSED ;

: RUN ( -- )
   T-RESET
   [: BODY ;] [: CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
