\ subject-case-run-test.f - tools/subject-case-run.f reports the fork, not the text.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
\   lib/fs-mutate.f lib/process.f lib/process-argv.f tools/subject-case-run-test.f
\
\ The tool is spawned exactly the way a person runs it - `bin/hb --load
\ tools/subject-case-run.f -- <case>` in a fresh engine - so the load path under
\ test is the command's own.
\
\ Two fixtures, and the report is read by LINE POSITION, never by searching the
\ text. The first fixture is built to fool a text search: it exits 0 and writes
\ nothing to stderr, but its stdout is two lines that copy the report's own
\ outcome and stderr lines, claiming exit 84 and three stderr bytes. A test that
\ searched for `outcome exited 84` would pass on it; this one requires the field
\ lines to be the true ones and finds the case's forgery down in the raw section
\ where captured bytes belong. The second fixture is the seal collision that
\ made the tool necessary: `package MEM` reopens the package lib/memory.f owns
\ and protects, which is a fail-closed exit 84 - and its stderr is pinned here
\ byte for byte, because the case row's only evidence used to be a hash of it.
\ The guard used to write the bare token, so the whole diagnostic was the three
\ bytes `MEM`; it now names itself first (src/habu/habu2.f SEAL-MSG:LMSG), and this
\ fixture is where a silent change to that text stops.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f

package SUBJECT-CASE-RUN-TEST

$2000 constant CAP
60000 constant TIMEOUT-MS
10 constant LF-C

create OUT CAP allot
create ERR CAP allot
create EMPTY 1 allot                    \ zero-length stdin
create ROOT FS-PATH-CAP allot
create DECOY FS-PATH-CAP allot
create SEAL FS-PATH-CAP allot

variable ROOT-U
variable DECOY-U
variable SEAL-U
variable OUT-U
variable ERR-U
variable RC
variable EXITED
variable POS
variable LN
variable END

: PATH! ( ptr u8 n ptr u8 ptr n -- ) {: src:ptr u:n dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   src dst u BYTE-COPY
   u lenp ! ;

\ The two fixture sources. Their byte lengths and their exact output are pinned
\ in the checks below, so editing either text fails loudly rather than quietly
\ weakening the assertion.
: DECOY$ ( -- ptr u8 n )
   SB-RESET
   s\" s\" outcome exited 84\" type cr" SB-APPEND LF-C SB-APPEND-C
   s\" s\" err 3 77 69 77\" type cr" SB-APPEND LF-C SB-APPEND-C
   SB$ ;

: SEAL$ ( -- ptr u8 n )
   SB-RESET
   s" package MEM" SB-APPEND LF-C SB-APPEND-C
   s" ;package" SB-APPEND LF-C SB-APPEND-C
   SB$ ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" habu-subject-case-run" TMPDIR-MKDIR ROOT ROOT-U PATH!
   ROOT ROOT-U @ CLEANUP-TREE+
   ROOT ROOT-U @ s" decoy.f" DECOY JOIN-PATH DECOY-U !
   ROOT ROOT-U @ s" seal.f" SEAL JOIN-PATH SEAL-U !
   DECOY DECOY-U @ DECOY$ WRITE-ALL
   SEAL SEAL-U @ SEAL$ WRITE-ALL ;

: STORE! ( len len outcome -- )
   MATCH outcome
     exited OF RC ! 0 0= EXITED ! ENDOF
     signaled OF RC ! 0 0= 0= EXITED ! ENDOF
     timeout OF 0 RC ! 0 0= 0= EXITED ! ENDOF
   ;MATCH
   LEN>N ERR-U !
   LEN>N OUT-U ! ;

: RUN-TOOL ( ptr u8 n -- ) {: p:ptr u:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/subject-case-run.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   p u >LEN PROC-ARGV+
   s" bin/hb" >LEN  EMPTY 0 >LEN  OUT CAP >LEN
   ERR CAP >LEN  TIMEOUT-MS >MS RUN-ARGV-STDIN-CAPTURE-OUTCOME STORE! ;

: LINE-END ( n -- n )
   begin dup OUT-U @ < while
      dup OUT + c@ LF-C = if exit then
      1+
   repeat ;

\ Line `want` of the tool's stdout. A report line is identified by its position,
\ so nothing a case prints can be mistaken for a field.
: LINE-AT ( n -- ptr u8 n ) {: want:n :}
   0 POS !  0 LN !
   begin
      POS @ OUT-U @ > if E-STR-BOUNDS throw then
      POS @ LINE-END END !
      LN @ want <>
   while
      LN @ 1+ LN !
      END @ 1+ POS !
   repeat
   OUT POS @ + END @ POS @ - ;

: LINE= ( n ptr u8 n -- ) {: i:n want:ptr wantu:n :}
   i LINE-AT want wantu T$= ;

: CASE-LINE$ ( ptr u8 n -- ptr u8 n ) {: p:ptr u:n :}
   SB-RESET
   s" subject-case " SB-APPEND
   p u SB-APPEND
   SB$ ;

: REPORTED ( -- )
   s" the tool itself exits 0 and says nothing on stderr" T-LABEL
   EXITED @ TTRUE
   RC @ 0 T=
   ERR-U @ 0 T= ;

\ A case that exits 0 while printing two lines that copy the report's own field
\ lines. Every field must still read the fork's truth, and the copies must show
\ up only in the raw section.
: DECOY-CASE ( -- )
   DECOY DECOY-U @ RUN-TOOL
   s" decoy case: the tool reports the case it was given" T-LABEL
   REPORTED
   0 DECOY DECOY-U @ CASE-LINE$ LINE=
   1 s" src 57" LINE=
   s" decoy case: the outcome line is the fork's, not the case's text" T-LABEL
   2 s" outcome exited 0" LINE=
   s" decoy case: every captured byte is dumped as a decimal code" T-LABEL
   3 s" out 33 111 117 116 99 111 109 101 32 101 120 105 116 101 100 32 56 52 10 101 114 114 32 51 32 55 55 32 54 57 32 55 55 10" LINE=
   s" decoy case: an empty stderr is reported as empty" T-LABEL
   4 s" err 0" LINE=
   s" decoy case: the forged lines land in the raw section, below the fields" T-LABEL
   5 s" raw-stdout" LINE=
   6 s" outcome exited 84" LINE=
   7 s" err 3 77 69 77" LINE= ;

\ The seal collision itself: `package MEM` in a fork of a harness that has
\ lib/memory.f resident. Exit 84, empty stdout, and a labelled stderr line that
\ names the guard, the offending token, and ends in one newline.
: SEAL-CASE ( -- )
   SEAL SEAL-U @ RUN-TOOL
   s" seal case: the tool reports the case it was given" T-LABEL
   REPORTED
   0 SEAL SEAL-U @ CASE-LINE$ LINE=
   1 s" src 21" LINE=
   s" seal case: reopening a protected package is a fail-closed exit 84" T-LABEL
   2 s" outcome exited 84" LINE=
   3 s" out 0" LINE=
   s" seal case: every stderr byte is named, not hashed" T-LABEL
   4 s" err 54 104 98 58 32 112 114 111 116 101 99 116 101 100 32 112 97 99 107 97 103 101 32 115 101 97 108 101 100 32 97 103 97 105 110 115 116 32 117 115 101 114 32 115 111 117 114 99 101 58 32 77 69 77 10" LINE=
   5 s" raw-stdout" LINE=
   7 s" raw-stderr" LINE=
   s" seal case: the guard names itself before the offending token" T-LABEL
   8 s" hb: protected package sealed against user source: MEM" LINE= ;

public

: TEST ( -- )
   T-RESET
   PREPARE
   DECOY-CASE
   SEAL-CASE
   CLEANUP-RUN
   T-REPORT
   s" subject-case-run-test: ok" type cr ;

;package

SUBJECT-CASE-RUN-TEST:TEST
