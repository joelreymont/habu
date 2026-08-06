\ subject-case-run.f - run ONE source through the real SUBJECT:RUN and report
\ what the fork produced.
\
\   bin/hb --load tools/subject-case-run.f -- test/type-decl-suite.f
\
\ A candidate-validation `shared` case is not run as a child engine. lib/test/
\ subject.f forks the running harness and evaluates the source in the child, so
\ a case that passes standalone can still fail there - it meets a dictionary the
\ standalone run never has, and a package name a resident library owns and
\ protects is a fail-closed exit. The harness records only digests of what the
\ child wrote, so such a failure arrives as an exit code and a hash. This runs
\ one case through that same entry point and prints the outcome, the capture
\ lengths, every captured byte as a decimal code, and the raw text.
\
\ The decimal dump is the authoritative part of the report: a case cannot forge
\ a report line through its own output, because its bytes are rendered as
\ numbers. The raw sections come last, after their own marker lines, and are
\ there to be read.
\
\ The tool exits 0 whenever it produced a report. The case's own outcome belongs
\ in the report, not in the tool's exit code, so a script cannot mistake one for
\ the other.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/process.f
require lib/process-argv.f
require lib/test/subject.f

package SUBJECT-CASE-RUN

\ The case caps are candidate-validation's own, so a case this tool accepts is a
\ case that harness accepts.
$40000 constant SRC-CAP
$8000 constant IO-CAP
120000 constant TIMEOUT-MS
32 constant SP-C

create SRC SRC-CAP allot
create OUT IO-CAP allot
create ERR IO-CAP allot

variable OUT-U
variable ERR-U
variable SRC-U

: N. ( n -- ) {: n:n :}
   n 0 < if E-STR-BOUNDS throw then
   n 10 >= if n 10 / RECURSE then
   n 10 mod 48 + emit ;

: SP ( -- )
   SP-C emit ;

: BYTES. ( ptr u8 n -- ) {: a:ptr u:n :}
   0 begin dup u < while
      SP
      dup a + c@ N.
      1+
   repeat drop ;

: STREAM. ( ptr u8 n ptr u8 n -- ) {: label:ptr labelu:n a:ptr u:n :}
   label labelu type SP u N.
   a u BYTES. cr ;

: RAW. ( ptr u8 n ptr u8 n -- ) {: label:ptr labelu:n a:ptr u:n :}
   label labelu type cr
   a u type cr ;

\ Printed while the lengths are still under the outcome on the stack, so the
\ report names the kind and code without keeping a second copy of either.
: OUTCOME. ( len len outcome -- len len )
   s" outcome " type
   MATCH outcome
     exited OF s" exited " type N. ENDOF
     signaled OF s" signaled " type N. ENDOF
     timeout OF s" timeout" type ENDOF
   ;MATCH
   cr ;

public

: RUN-PATH ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu SRC SRC-CAP READ-ALL SRC-U !
   s" subject-case " type path pathu type cr
   s" src " type SRC-U @ N. cr
   SRC SRC-U @ OUT IO-CAP >LEN ERR IO-CAP >LEN TIMEOUT-MS >MS SUBJECT:RUN
   OUTCOME.
   LEN>N ERR-U !
   LEN>N OUT-U !
   s" out" OUT OUT-U @ STREAM.
   s" err" ERR ERR-U @ STREAM.
   s" raw-stdout" OUT OUT-U @ RAW.
   s" raw-stderr" ERR ERR-U @ RAW. ;

: MAIN ( -- )
   SCRIPT-ARGC 1 <> if
      s" usage: bin/hb --load tools/subject-case-run.f -- <source-path>" 64 die
   then
   0 SCRIPT-ARGV$ RUN-PATH ;

;package

SUBJECT-CASE-RUN:MAIN
