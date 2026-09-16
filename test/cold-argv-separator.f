\ cold-argv-separator.f - the `--` argv separator on an engine that boots its
\ prefix from source.
\
\ The engine scans argv for `--` in two halves, and on an UNSEEDED engine the
\ whole cold prefix runs between them: every prefix row is a subroutine that
\ leaves x13 holding whatever it last used it for, so the caller's scan index
\ never survived. It started at argv[0] instead of argv[2] and found the
\ separator anyway - argv[0] is the program and argv[1] is the flag that
\ selected the route, and neither can be `--` - until a prefix row began leaving
\ an arena pointer there. Then the first compare read an index past argc, no
\ separator was found, and `--` and everything after it went to the loader as
\ files: `include: cannot open <tree>/--`, exit 74 (dot 0eaf921f, which moved
\ the index into the loop that reads it).
\
\ A seeded engine emits no prefix rows at all, so only an unseeded host can show
\ this, and the host test/cold-engine.f emits is exactly one. Both cases load
\ the same driver through the real command line: without a separator, and with
\ one plus two arguments that must reach the driver instead of the loader.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require test/cold-engine.f

package COLD-ARGV-SEPARATOR

$4000 constant CS-CAP
$1000 constant CS-SRC-CAP
60000 constant CS-TIMEOUT-MS

create CS-ROOT FS-PATH-CAP allot   variable CS-ROOT-U
create CS-HOST FS-PATH-CAP allot   variable CS-HOST-U
create CS-DRV FS-PATH-CAP allot    variable CS-DRV-U
create CS-SRC CS-SRC-CAP allot     variable CS-SRC-U
create CS-OUT CS-CAP allot
create CS-ERR CS-CAP allot

: CS-ROOT$ ( -- ptr u8 n ) CS-ROOT CS-ROOT-U @ ;
: CS-HOST$ ( -- ptr u8 n ) CS-HOST CS-HOST-U @ ;
: CS-DRV$ ( -- ptr u8 n ) CS-DRV CS-DRV-U @ ;

: CS+ ( ptr u8 n -- ) {: a:ptr u:n :}
   CS-SRC-U @ u + CS-SRC-CAP > if E-FS-CAPACITY throw then
   a CS-SRC CS-SRC-U @ + u BYTE-COPY
   CS-SRC-U @ u + CS-SRC-U ! ;

: CS-NL ( -- )
   s\" \n" CS+ ;

: CS-LINE ( ptr u8 n -- )
   CS+ CS-NL ;

\ The driver answers both halves of the contract in one capture: that the loader
\ reached user source at all, and what the separator left behind it.
: CS-DRV-SRC ( -- )
   0 CS-SRC-U !
   s" : CS-REPORT ( -- )" CS-LINE
   S\"    s\" cold-sep: ran\" type cr" CS-LINE
   S\"    s\" cold-sep: argc \" type SCRIPT-ARGC . cr" CS-LINE
   s"    SCRIPT-ARGC 0 > if" CS-LINE
   S\"       s\" cold-sep: arg0 \" type 0 SCRIPT-ARGV$ type cr" CS-LINE
   s"    then ;" CS-LINE
   s" CS-REPORT" CS-LINE ;

: CS-SETUP ( -- )
   CLEANUP-RESET
   s" habu-cold-sep" TMPDIR-MKDIR {: a:ptr u:n :}
   a CS-ROOT u BYTE-COPY
   u CS-ROOT-U !
   CS-ROOT$ CLEANUP-TREE+
   CS-ROOT$ s" hb-cold" CS-HOST JOIN-PATH CS-HOST-U !
   CS-ROOT$ s" drv.f" CS-DRV JOIN-PATH CS-DRV-U !
   CS-DRV-SRC
   CS-DRV$ CS-SRC CS-SRC-U @ WRITE-ALL
   CS-HOST$ COLD-ENGINE:PROVIDE ;

: CS-ARGV0 ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+ ;

: CS-ARG ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

\ Outcome capture, not throw-only: a run that dies on the separator is an
\ expected failure this file has to report, not a shell status to collapse into.
: CS-RUN ( -- n n n )                              \ -> outu erru rc
   CS-HOST$ >LEN CS-OUT CS-CAP >LEN CS-ERR CS-CAP >LEN CS-TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N e LEN>N 0 ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :} o LEN>N e LEN>N c RC>N ENDOF
   ;MATCH ;

\ The defect names itself on the child's stderr, so a red case prints what the
\ child said instead of leaving a bare status behind.
: CS-SHOW-CHILD ( n n -- ) {: erru:n rc:n :}
   rc 0 = erru 0 = and if exit then
   s" cold-sep: child rc " type rc . cr
   s" cold-sep: child stderr: " type CS-ERR erru type cr ;

: CS-OUT$ ( n -- ptr u8 n ) {: u:n :}
   CS-OUT u ;

\ ---- cases -----------------------------------------------------------------

\ The baseline: the same host, the same driver, no separator on the line. It
\ holds under the defect too, which is what makes the pair a measurement.
: CS-NO-SEPARATOR ( -- )
   CS-ARGV0  CS-DRV$ CS-ARG
   CS-RUN {: outu:n erru:n rc:n :}
   erru rc CS-SHOW-CHILD
   s" an unseeded --load with no separator exits 0" T-LABEL
   rc 0 T=
   s" an unseeded --load with no separator runs the driver" T-LABEL
   outu CS-OUT$ s" cold-sep: ran" CONTAINS? TTRUE
   s" a line with no separator leaves the driver no arguments" T-LABEL
   outu CS-OUT$ s" cold-sep: argc 0" CONTAINS? TTRUE
   s" an unseeded --load with no separator says nothing on stderr" T-LABEL
   erru 0 T= ;

\ The regression: with `-- art extra` the loader must stop at the separator. A
\ scan that missed it handed the loader three more files and died on the first,
\ so an empty stderr and a zero status are the two halves of the refusal.
: CS-WITH-SEPARATOR ( -- )
   CS-ARGV0  CS-DRV$ CS-ARG  s" --" CS-ARG  s" art" CS-ARG  s" extra" CS-ARG
   CS-RUN {: outu:n erru:n rc:n :}
   erru rc CS-SHOW-CHILD
   s" an unseeded --load past a separator exits 0" T-LABEL
   rc 0 T=
   s" the separator is never handed to the loader as a file" T-LABEL
   erru 0 T=
   s" an unseeded --load past a separator runs the driver" T-LABEL
   outu CS-OUT$ s" cold-sep: ran" CONTAINS? TTRUE
   s" the arguments after the separator reach the driver" T-LABEL
   outu CS-OUT$ s" cold-sep: argc 2" CONTAINS? TTRUE
   outu CS-OUT$ s" cold-sep: arg0 art" CONTAINS? TTRUE ;

public

: COLD-ARGV-SEPARATOR-MAIN ( -- )
   T-RESET
   CS-SETUP
   CS-NO-SEPARATOR
   CS-WITH-SEPARATOR
   CLEANUP-RUN
   T-REPORT
   s" cold-argv-separator: ok" type cr ;

;package

COLD-ARGV-SEPARATOR:COLD-ARGV-SEPARATOR-MAIN
