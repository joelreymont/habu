\ A task still ACTIVATED when a capture begins is refused by name, and so is a
\ pool semaphore still held in a chunk the pool grew. The subjects
\ whose LOAD-TIME task work left process-local state in the image's data - the
\ maker's foreign addresses in package FFI's symbol table, and a TCB holding
\ this process's mappings - link and run in test/stripped-image.f's application
\ (test/stripped-lifecycle-prepare-subject.f, -tasks-subject.f).
require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f

package STRIPPED-LIFECYCLE-PREPARE-TEST

$10000 constant CAP
600000 constant TIMEOUT-MS
create OUT CAP allot
create ERR CAP allot
create RUNNING-BUF PATH-CAP allot
variable RUNNING-U
create POOL-BUF PATH-CAP allot
variable POOL-U

: RUNNING$ ( -- ptr u8 n ) RUNNING-BUF RUNNING-U @ ;
: POOL$ ( -- ptr u8 n ) POOL-BUF POOL-U @ ;

: PREPARE ( -- )
   SOURCE-ROOT:CURRENT$ s" stripped-lifecycle-running-subject.f"
      RUNNING-BUF FS:JOIN-PATH RUNNING-U !
   SOURCE-ROOT:CURRENT$ s" stripped-lifecycle-pool-subject.f"
      POOL-BUF FS:JOIN-PATH POOL-U ! ;

\ THE REFUSAL IS MEASURED AT IMAGE-LIFECYCLE:PREPARE AND NOT THROUGH hb-build,
\ because hb-build's later phases mutate the dictionary and a live task refuses
\ that first, with a bare exit $4F and no message at all (measured on this
\ engine). The subject calls the one word both capture paths run, so what is
\ pinned here is the sweep's own answer for an activated task.
: CAPTURE-ERR ( ptr u8 n -- len n )
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   erru rc ;

: RUNNING-REFUSED ( -- )
   RUNNING$ CAPTURE-ERR {: erru:len rc:n :}
   rc 0 T<>
   ERR erru LEN>N s" activated task at capture" CONTAINS? TTRUE
   ERR erru LEN>N s" TCB.THREAD" CONTAINS? TTRUE ;

: POOL-REFUSED ( -- )
   POOL$ CAPTURE-ERR {: erru:len rc:n :}
   rc 0 T<>
   ERR erru LEN>N s" live pool semaphore at capture" CONTAINS? TTRUE ;

: RUN ( -- )
   T-RESET
   PREPARE
   RUNNING-REFUSED
   POOL-REFUSED
   T-REPORT ;

RUN
;package
