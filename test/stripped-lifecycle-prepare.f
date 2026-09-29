\ A task still ACTIVATED when a capture begins is refused by name. The subjects
\ whose LOAD-TIME task work left process-local state in the image's data - the
\ maker's foreign addresses in lib/task.f's eight XT cells, and a TCB holding
\ this process's mappings - link and run in test/stripped-image.f's application
\ (test/stripped-lifecycle-prepare-subject.f, -tasks-subject.f).
require lib/test.f
require lib/string.f
require lib/fs.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-candidate.f

package STRIPPED-LIFECYCLE-PREPARE-TEST

$10000 constant CAP
600000 constant TIMEOUT-MS
create OUT CAP allot
create ERR CAP allot
create RUNNING-BUF FS-PATH-CAP allot
variable RUNNING-U

: RUNNING$ ( -- ptr u8 n ) RUNNING-BUF RUNNING-U @ ;

: PREPARE ( -- )
   SOURCE-ROOT:CURRENT$ s" stripped-lifecycle-running-subject.f"
      RUNNING-BUF JOIN-PATH RUNNING-U ! ;

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

: RUN ( -- )
   T-RESET
   PREPARE
   RUNNING-REFUSED
   T-REPORT ;

RUN
;package
