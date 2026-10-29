\ boot-census-test.f - tools/perf/boot-census.f through its entry point, in a
\ child, on tools/perf/boot-census-fixture.f: one fork child that keeps its image
\ and argv and spawns one /bin/sleep. Fork total 0 means a child went unregistered
\ or a fork read as an exec; fork total 2 means an exec read as a fork. Argv
\ shapes cover only HB-FAMILY? paths: bin/hb, hb-under-test, fixpoint stages. macOS only.
\ Run: bin/hb --load tools/perf/boot-census-test.f
require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require src/lib/test/outcome.f

package BOOT-CENSUS-TEST

60000 constant TIMEOUT-MS              \ a loaded host
64 constant USAGE-RC                   \ ARGV:E-USAGE
$10000 constant CAP

variable ROOT-U
variable LOG-U
variable OUT-U
variable ERR-U
variable LOGTXT-U

create ROOT-BUF PATH-CAP allot
create LOG-BUF PATH-CAP allot
create OUT CAP allot
create ERR CAP allot
create LOGTXT CAP allot

: ROOT$ ( -- ptr u8 n )     ROOT-BUF ROOT-U @ ;
: LOG$ ( -- ptr u8 n )      LOG-BUF LOG-U @ ;
: OUT$ ( -- ptr u8 n )      OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n )      ERR ERR-U @ ;
: LOGTXT$ ( -- ptr u8 n )   LOGTXT LOGTXT-U @ ;
: TOOL$ ( -- ptr u8 n )     s" tools/perf/boot-census.f" ;
: FIXTURE$ ( -- ptr u8 n )  s" tools/perf/boot-census-fixture.f" ;

\ The count on the census stdout line that starts with LABEL; -1 without one.
: TOTAL ( ptr u8 n -- n ) {: la:ptr lu:n :}
   0 begin dup OUT-U @ <= while
      {: start:n :}
      OUT$ 10 start SPLIT-NEXT {: line:ptr lineu:n next:n valid:bool :}
      valid line lineu la lu STARTS-WITH? and if
         line lu + lineu lu - STR>NUMBER? MATCH option
           some OF ENDOF
           none OF -1 ENDOF
         ;MATCH exit
      then
      next
   repeat drop -1 ;

\ --- the child run ------------------------------------------------------------

: ARGV-HEAD ( -- )
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   TOOL$ >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+ ;

: ARG+ ( ptr u8 n -- )   >LEN PROC-ARGV+ ;

: RUN-TOOL ( n -- )               \ the exit status the case wants
   {: want:n :}
   ENGINE-CANDIDATE:PATH$ >LEN  OUT CAP >LEN  ERR CAP >LEN  TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME {: outu:len erru:len oc :}
   outu LEN>N OUT-U !  erru LEN>N ERR-U !
   TOOL$ OUT$ ERR$ oc want T-OUTCOME-EXITED= ;

: HAS ( ptr u8 n ptr u8 n -- )
   {: hay:ptr hayu:n a:ptr u:n :}
   a u T-LABEL
   hay hayu a u CONTAINS? TTRUE ;

\ --- cases --------------------------------------------------------------------

: SHOW-RUN ( -- )
   s" boot-census stdout:" type cr OUT$ type
   s" boot-census stderr:" type cr ERR$ type ;

: CASE-TREE ( -- )
   T-FAILURES {: before:n :}
   ARGV-HEAD
   LOG$ ARG+  ENGINE-CANDIDATE:PATH$ ARG+  s" --load" ARG+  FIXTURE$ ARG+
   s" the census exits 0 on the fixture" T-LABEL
   0 RUN-TOOL
   s" total exec events (fresh images): at least 2" T-LABEL
   s" total exec events (fresh images): " TOTAL 2 >= TTRUE
   OUT$ s"        1  /bin/sleep" HAS
   OUT$ s" total fork children (no exec): 1" HAS
   OUT$ s" lost registrations: 0" HAS
   OUT$ s" hb-family exec argv shapes:" HAS
   OUT$ s"        1  tools/perf/boot-census-fixture.f" HAS
   LOG$ LOGTXT CAP FS:READ-ALL LOGTXT-U !
   LOGTXT$ s"  ROOT " HAS
   LOGTXT$ s"  CHILD " HAS
   LOGTXT$ s"  EXIT " HAS
   LOGTXT$ s"  DONE alive=0 lost=0 status=0" HAS
   T-FAILURES before > if SHOW-RUN then ;

: CASE-STATUS ( -- )
   ARGV-HEAD
   LOG$ ARG+  s" /usr/bin/false" ARG+
   s" the census exits with its command's status" T-LABEL
   1 RUN-TOOL ;

\ This exec replaces a process without changing the path or argv. Comparing
\ image text alone would erase it from the census.
: CASE-REEXEC ( -- )
   ARGV-HEAD
   LOG$ ARG+ ENGINE-CANDIDATE:PATH$ ARG+ s" --load" ARG+ FIXTURE$ ARG+
   s" --" ARG+ s" reexec" ARG+
   s" same-image reexec exits 0" T-LABEL
   0 RUN-TOOL
   s" same-image reexec counts both images" T-LABEL
   s" total exec events (fresh images): " TOTAL 2 T=
   OUT$ s" total fork children (no exec): 0" HAS
   OUT$ s" lost registrations: 0" HAS
   OUT$ s"        2  tools/perf/boot-census-fixture.f -- reexec" HAS
   LOG$ LOGTXT CAP FS:READ-ALL LOGTXT-U !
   LOGTXT$ s"  EXEC " HAS
   LOGTXT$ s"  DONE alive=0 lost=0 status=0" HAS ;

: CASE-ENV ( -- )
   ARGV-HEAD
   s" BOOT_CENSUS_MARK" >LEN s" inherited" >LEN PROC-ENV+
   LOG$ ARG+  s" /usr/bin/env" ARG+
   s" the census runs its command in its own environment" T-LABEL
   0 RUN-TOOL
   OUT$ s" BOOT_CENSUS_MARK=inherited" HAS ;

: CASE-USAGE ( -- )
   ARGV-HEAD
   s" the census refuses a missing LOGFILE and CMD" T-LABEL
   USAGE-RC RUN-TOOL ;

: PREPARE ( -- )
   FS:CLEANUP-RESET
   s" habu-boot-census" FS:MKTEMP-DIR {: a:ptr u:n :}
   u PATH-CAP > if E-FS-CAPACITY throw then
   a ROOT-BUF u BYTE-COPY  u ROOT-U !
   ROOT$ FS:CLEANUP-TREE+
   ROOT$ s" census.log" LOG-BUF FS:JOIN-PATH LOG-U ! ;

: CLEANUP ( -- )
   FS:CLEANUP-RUN
   ROOT$ FS:EXISTS? TFALSE ;

: CASES ( -- )
   T-RESET
   HB-TARGET-MACOS? 0= if
      s" boot-census-test: the census follows processes with kqueue and libproc, macOS only" type cr
      exit
   then
   PREPARE
   CASE-TREE
   CASE-REEXEC
   CASE-STATUS
   CASE-ENV
   CASE-USAGE
   CLEANUP ;

CASES

;package

T-REPORT
