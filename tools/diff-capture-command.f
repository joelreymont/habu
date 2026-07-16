\ diff-capture-command.f - operation-pinned command capture.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/fs.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-cwd.f
require tools/lint/diff-file.f
require tools/lint/diff-frame.f
require tools/diff-capture-types.f

package DIFF-CMD
private

256 constant ARG-MAX
32768 constant ARG-CAP

create ROOT FS-PATH-CAP allot
create META-PATH FS-PATH-CAP allot
create RAW-PATH FS-PATH-CAP allot
create OUT-PATH FS-PATH-CAP allot
create ERR-PATH FS-PATH-CAP allot
create JJ-PATH FS-PATH-CAP allot
create ARG-BUF ARG-CAP allot
create ARG-OFF ARG-MAX cells allot
create ARG-LEN ARG-MAX cells allot

variable ROOT-U
variable META-PATH-U
variable RAW-PATH-U
variable OUT-PATH-U
variable ERR-PATH-U
variable JJ-PATH-U
PTR-VARIABLE REPO-A
variable REPO-U
PTR-VARIABLE FROM-A
variable FROM-U
PTR-VARIABLE TO-A
variable TO-U
PTR-VARIABLE OP-A
variable OP-U
PTR-VARIABLE META-A
variable META-U
PTR-VARIABLE RAW-A
variable RAW-U

variable OUT-FD
variable ERR-FD
variable RUN-RC
variable ARG-N
variable ARG-U

1 LAYOUT-BUFFER LAST-PHASE-V DIFF-CAPTURE:command-phase
1 LAYOUT-BUFFER LAST-OUTCOME-V DIFF-CAPTURE:command-outcome
variable LAST-RC-N
variable LAST-CODE-N
variable LAST-OUT-CODE-N
variable LAST-ERR-CODE-N
variable COMMAND-READY
PTR-VARIABLE LAST-OUT-A
variable LAST-OUT-U
PTR-VARIABLE LAST-ERR-A
variable LAST-ERR-U
PTR-VARIABLE RUN-OUT-A
variable RUN-OUT-U
PTR-VARIABLE RUN-EXE-A
variable RUN-EXE-U
PTR-VARIABLE RUN-CWD-A
variable RUN-CWD-U
PTR-VARIABLE LAST-EXE-A
variable LAST-EXE-U
variable OUT-OPENED
variable ERR-OPENED

: LAST-PHASE-AT ( -- ptr DIFF-CAPTURE:command-phase )
   0 LAST-PHASE-V ;

: LAST-OUTCOME-AT ( -- ptr DIFF-CAPTURE:command-outcome )
   0 LAST-OUTCOME-V ;

: LAST-PHASE! ( DIFF-CAPTURE:command-phase -- )
   LAST-PHASE-AT ! ;

: LAST-OUTCOME! ( DIFF-CAPTURE:command-outcome -- )
   LAST-OUTCOME-AT ! ;

: ROOT$ ( -- ptr u8 n )
   ROOT ROOT-U @ ;

: REPO$ ( -- ptr u8 n )
   REPO-U @ 0 > if REPO-A @ REPO-U @ else s" ." then ;

: JJ$ ( -- ptr u8 n )
   JJ-PATH JJ-PATH-U @ ;

: RUN-EXE$ ( -- ptr u8 n )
   RUN-EXE-A @ RUN-EXE-U @ ;

: RUN-CWD$ ( -- ptr u8 n )
   RUN-CWD-A @ RUN-CWD-U @ ;

: PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- )
   {: root:ptr rootu:n name:ptr nameu:n dst:ptr lenp:ptr :}
   root rootu name nameu dst JOIN-PATH lenp ! ;

: ROOT! ( ptr u8 n -- ) {: a:ptr u:n :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a ROOT u BYTE-COPY
   u ROOT-U ! ;

: SET-PATHS ( -- )
   ROOT$ s" metadata.jsonl" META-PATH META-PATH-U PATH!
   ROOT$ s" raw.diff" RAW-PATH RAW-PATH-U PATH!
   ROOT$ s" command.out" OUT-PATH OUT-PATH-U PATH!
   ROOT$ s" command.err" ERR-PATH ERR-PATH-U PATH! ;

: FIND-JJ ( -- )
   s" jj" >LEN JJ-PATH FIND-EXECUTABLE MATCH option
      none OF E-DIFF-CAPTURE throw ENDOF
      some OF LEN>N JJ-PATH-U ! ENDOF
   ;MATCH ;

: OPEN-OUT ( ptr u8 n -- fd ) {: a:ptr u:n :}
   a u FS-PATHZ
   FS-O-WRONLY FS-O-CREAT or FS-O-TRUNC or
   FS-MODE-0644 open {: fd:n :}
   fd 0 < if E-FS-OPEN throw then
   fd >FD ;

defer COMMAND-OPEN ( ptr u8 n -- fd )

: COMMAND-OPEN-DEFAULT ( ptr u8 n -- fd )
   OPEN-OUT ;

: RESET-COMMAND-OPEN ( -- )
   [: COMMAND-OPEN-DEFAULT ;] is COMMAND-OPEN ;

RESET-COMMAND-OPEN

: CLOSE-FD ( ptr n -- ) {: p:ptr :}
   p @ dup 0 >= if close else drop then \ close has no result; ownership is cleared.
   -1 p ! ;

: SPAWN ( -- )
   RUN-EXE$ >LEN RUN-CWD$ >LEN -1 >FD OUT-FD @ >FD ERR-FD @ >FD
   PROC-RUN-ARGV-ENV-CWD-IO-RC MATCH result
      ok  OF drop 0 RUN-RC ! ENDOF
      err OF RUN-RC ! ENDOF
   ;MATCH ;

defer COMMAND-SPAWN ( -- )

: RESET-COMMAND-SPAWN ( -- )
   [: SPAWN ;] is COMMAND-SPAWN ;

RESET-COMMAND-SPAWN

: LOAD-OUT-DEFAULT ( -- )
   RUN-OUT-A @ RUN-OUT-U @ DIFF-FILE:LOAD LAST-OUT-U ! LAST-OUT-A ! ;

: LOAD-ERR-DEFAULT ( -- )
   ERR-PATH ERR-PATH-U @ DIFF-FILE:LOAD LAST-ERR-U ! LAST-ERR-A ! ;

defer LOAD-OUT-REPORT ( -- )
defer LOAD-ERR-REPORT ( -- )

: RESET-REPORT-LOADS ( -- )
   [: LOAD-OUT-DEFAULT ;] is LOAD-OUT-REPORT
   [: LOAD-ERR-DEFAULT ;] is LOAD-ERR-REPORT ;

RESET-REPORT-LOADS

: LOAD-OUT-CODE ( -- n )
   OUT-OPENED @ 0= if 0 exit then
   [: LOAD-OUT-REPORT ;] catch ;

: LOAD-ERR-CODE ( -- n )
   ERR-OPENED @ 0= if 0 exit then
   [: LOAD-ERR-REPORT ;] catch ;

: OPEN-AND-SPAWN ( -- )
   RUN-OUT-A @ RUN-OUT-U @ COMMAND-OPEN FD>N OUT-FD !
   true OUT-OPENED !
   ERR-PATH ERR-PATH-U @ COMMAND-OPEN FD>N ERR-FD !
   true ERR-OPENED !
   COMMAND-SPAWN ;

: RESET-COMMAND-REPORT ( DIFF-CAPTURE:command-phase ptr u8 n -- )
   {: phase:DIFF-CAPTURE:command-phase exe:ptr exeu:n :}
   true COMMAND-READY !
   phase LAST-PHASE!
   exe LAST-EXE-A ! exeu LAST-EXE-U !
   DIFF--CAPTURE-COMMAND--OUTCOME:SUCCEEDED LAST-OUTCOME!
   0 RUN-RC !
   0 LAST-RC-N !
   0 LAST-CODE-N !
   0 LAST-OUT-CODE-N !
   0 LAST-ERR-CODE-N !
   0 LAST-OUT-U !
   0 LAST-ERR-U !
   false OUT-OPENED !
   false ERR-OPENED !
   -1 OUT-FD !
   -1 ERR-FD ! ;

: RECORD-COMMAND ( n -- ) {: code:n :}
   RUN-RC @ LAST-RC-N !
   code LAST-CODE-N !
   code 0<> if
      DIFF--CAPTURE-COMMAND--OUTCOME:FAULT LAST-OUTCOME!
      exit
   then
   RUN-RC @ 0<> if DIFF--CAPTURE-COMMAND--OUTCOME:EXITED LAST-OUTCOME! then ;

: THROW-COMMAND-RESULT ( n n n -- )
   {: code:n out-code:n err-code:n :}
   code 0<> if code throw then
   RUN-RC @ 0<> if E-DIFF-CAPTURE throw then
   out-code 0<> if out-code throw then
   err-code 0<> if err-code throw then ;

: RUN-COMMAND ( DIFF-CAPTURE:command-phase ptr u8 n ptr u8 n ptr u8 n -- )
   {: phase:DIFF-CAPTURE:command-phase exe:ptr exeu:n cwd:ptr cwdu:n out:ptr outu:n :}
   phase exe exeu RESET-COMMAND-REPORT
   exe RUN-EXE-A ! exeu RUN-EXE-U !
   cwd RUN-CWD-A ! cwdu RUN-CWD-U !
   out RUN-OUT-A ! outu RUN-OUT-U !
   [: OPEN-AND-SPAWN ;] catch {: code:n :}
   OUT-FD CLOSE-FD
   ERR-FD CLOSE-FD
   code RECORD-COMMAND
   LOAD-OUT-CODE dup LAST-OUT-CODE-N ! {: out-code:n :}
   LOAD-ERR-CODE dup LAST-ERR-CODE-N ! {: err-code:n :}
   code out-code err-code THROW-COMMAND-RESULT ;

: ARG-SLOT ( n ptr a -- ptr n ) {: idx:n table:ptr :}
   idx 0 < idx ARG-MAX >= or if E-PROC-OUTPUT throw then
   table idx cells + ;

: ARGS-RESET ( -- )
   PROC-ARGV-ENV-CWD-RESET
   0 ARG-N !
   0 ARG-U ! ;

: ARG ( ptr u8 n -- ) {: a:ptr u:n :}
   ARG-N @ ARG-MAX >= if E-PROC-OUTPUT throw then
   u 0 < ARG-U @ u + ARG-U @ < or if E-PROC-OUTPUT throw then
   ARG-U @ u + ARG-CAP > if E-PROC-OUTPUT throw then
   ARG-U @ ARG-N @ ARG-OFF ARG-SLOT !
   u ARG-N @ ARG-LEN ARG-SLOT !
   a ARG-BUF ARG-U @ + u BYTE-COPY
   ARG-U @ u + ARG-U !
   ARG-N @ 1+ ARG-N !
   a u >LEN PROC-ARGV+ ;

: REPO-ARG ( -- )
   REPO-U @ 0 > if
      s" -R" ARG
      REPO-A @ REPO-U @ ARG
   then ;

: PINNED-ARGS ( -- )
   ARGS-RESET
   s" --ignore-working-copy" ARG
   REPO-ARG
   s" --at-operation" ARG
   OP-A @ OP-U @ ARG ;

: SNAPSHOT-ARGS ( -- )
   ARGS-RESET
   REPO-ARG
   s" op" ARG s" log" ARG
   s" --limit" ARG s" 1" ARG
   s" --no-graph" ARG
   s" --color=never" ARG
   s" -T" ARG
   S\" id ++ \q\\n\q" ARG ;

: REV-ARGS ( ptr u8 n -- ) {: rev:ptr revu:n :}
   PINNED-ARGS
   s" log" ARG
   s" --no-graph" ARG
   s" -r" ARG
   rev revu ARG
   s" -T" ARG
   s" commit_id" ARG ;

: LOAD-EXACT ( ptr u8 n -- ptr u8 n ) {: path:ptr pathu:n :}
   path pathu DIFF-FILE:LOAD {: a:ptr u:n :}
   u 0 <= if E-DIFF-CAPTURE-ID throw then
   a u ;

: RUN-JJ ( DIFF-CAPTURE:command-phase ptr u8 n -- )
   {: phase:DIFF-CAPTURE:command-phase out:ptr outu:n :}
   phase JJ$ REPO$ out outu RUN-COMMAND ;

: SNAPSHOT ( -- )
   SNAPSHOT-ARGS
   DIFF--CAPTURE-COMMAND--PHASE:SNAPSHOT OUT-PATH OUT-PATH-U @ RUN-JJ
   OUT-PATH OUT-PATH-U @ LOAD-EXACT {: a:ptr u:n :}
   u 1 <= if E-DIFF-CAPTURE-ID throw then
   a u 1- + c@ $0A <> if E-DIFF-CAPTURE-ID throw then
   a u 1- DIFF:OBJECT-ID? 0= if E-DIFF-CAPTURE-ID throw then
   a OP-A ! u 1- OP-U ! ;

defer SNAPSHOT-BARRIER ( -- )

: SNAPSHOT-BARRIER-DEFAULT ( -- ) ;

: RESET-SNAPSHOT-BARRIER ( -- )
   [: SNAPSHOT-BARRIER-DEFAULT ;] is SNAPSHOT-BARRIER ;

RESET-SNAPSHOT-BARRIER

: RESOLVE ( DIFF-CAPTURE:command-phase ptr u8 n -- ptr u8 n )
   {: phase:DIFF-CAPTURE:command-phase rev:ptr revu:n :}
   rev revu REV-ARGS
   phase OUT-PATH OUT-PATH-U @ RUN-JJ
   OUT-PATH OUT-PATH-U @ LOAD-EXACT
   2dup DIFF:COMMIT-ID? 0= if 2drop E-DIFF-CAPTURE-ID throw then ;

: META-TEMPLATE$ ( -- ptr u8 n )
   SB-RESET
   S\" \q[\q ++ json(status) ++ \q,\q ++ " SB-APPEND
   S\" if(source.file_type(), json(source.path()), \q\\\q\\\q\q) ++ \q,\q ++ " SB-APPEND
   S\" json(source.file_type()) ++ \q,\q ++ json(source.executable()) ++ \q,\q ++ " SB-APPEND
   S\" json(source.conflict()) ++ \q,\q ++ " SB-APPEND
   S\" if(target.file_type(), json(target.path()), \q\\\q\\\q\q) ++ \q,\q ++ " SB-APPEND
   S\" json(target.file_type()) ++ \q,\q ++ json(target.executable()) ++ \q,\q ++ " SB-APPEND
   S\" json(target.conflict()) ++ \q]\\n\q" SB-APPEND
   SB$ ;

: DIFF-ARGS ( -- )
   PINNED-ARGS
   s" diff" ARG
   s" --from" ARG FROM-A @ FROM-U @ ARG
   s" --to" ARG TO-A @ TO-U @ ARG ;

: REQUIRE-COMMAND ( -- )
   COMMAND-READY @ 0= if E-DIFF-CAPTURE throw then ;

public

EXPORT SNAPSHOT

: CONFIGURE ( ptr u8 n ptr u8 n -- )
   {: root:ptr rootu:n repo:ptr repou:n :}
   root rootu ROOT!
   repo REPO-A ! repou REPO-U !
   SET-PATHS ;

: RESET-REPORT ( -- )
   false COMMAND-READY !
   0 LAST-RC-N !
   0 LAST-CODE-N !
   0 LAST-OUT-CODE-N !
   0 LAST-ERR-CODE-N !
   0 LAST-OUT-U !
   0 LAST-ERR-U ! ;

: JJ! ( -- )
   FIND-JJ ;

: BARRIER ( -- )
   SNAPSHOT-BARRIER ;

: RESOLVE-REVISIONS ( ptr u8 n ptr u8 n -- )
   {: from:ptr fromu:n to:ptr tou:n :}
   DIFF--CAPTURE-COMMAND--PHASE:RESOLVE-FROM from fromu RESOLVE FROM-U ! FROM-A !
   DIFF--CAPTURE-COMMAND--PHASE:RESOLVE-TO to tou RESOLVE TO-U ! TO-A ! ;

: CAPTURE-METADATA ( -- )
   DIFF-ARGS
   s" -T" ARG META-TEMPLATE$ ARG
   DIFF--CAPTURE-COMMAND--PHASE:METADATA META-PATH META-PATH-U @ RUN-JJ ;

: CAPTURE-RAW ( -- )
   DIFF-ARGS
   s" --git" ARG
   s" --color=never" ARG
   DIFF--CAPTURE-COMMAND--PHASE:RAW RAW-PATH RAW-PATH-U @ RUN-JJ ;

: LOAD-CAPTURES ( -- )
   META-PATH META-PATH-U @ DIFF-FILE:LOAD META-U ! META-A !
   RAW-PATH RAW-PATH-U @ DIFF-FILE:LOAD RAW-U ! RAW-A ! ;

: META$ ( -- ptr u8 n )
   META-A @ META-U @ ;

: META-PATH$ ( -- ptr u8 n )
   META-PATH META-PATH-U @ ;

: RAW$ ( -- ptr u8 n )
   RAW-A @ RAW-U @ ;

: FROM$ ( -- ptr u8 n )
   FROM-A @ FROM-U @ ;

: TO$ ( -- ptr u8 n )
   TO-A @ TO-U @ ;

: COMMAND? ( -- bool )
   COMMAND-READY @ if true else false then ;

: LAST-PHASE ( -- DIFF-CAPTURE:command-phase )
   REQUIRE-COMMAND
   LAST-PHASE-AT @ ;

: LAST-OUTCOME ( -- DIFF-CAPTURE:command-outcome )
   REQUIRE-COMMAND
   LAST-OUTCOME-AT @ ;

: LAST-RC ( -- n )
   REQUIRE-COMMAND
   LAST-RC-N @ ;

: LAST-CODE ( -- n )
   REQUIRE-COMMAND
   LAST-CODE-N @ ;

: LAST-OUT-CODE ( -- n )
   REQUIRE-COMMAND
   LAST-OUT-CODE-N @ ;

: LAST-ERR-CODE ( -- n )
   REQUIRE-COMMAND
   LAST-ERR-CODE-N @ ;

: LAST-OUT$ ( -- ptr u8 n )
   REQUIRE-COMMAND
   LAST-OUT-A @ LAST-OUT-U @ ;

: LAST-ERR$ ( -- ptr u8 n )
   REQUIRE-COMMAND
   LAST-ERR-A @ LAST-ERR-U @ ;

: LAST-EXE$ ( -- ptr u8 n )
   REQUIRE-COMMAND
   LAST-EXE-A @ LAST-EXE-U @ ;

: ARG-COUNT ( -- n )
   REQUIRE-COMMAND
   ARG-N @ ;

: ARG$ ( n -- ptr u8 n ) {: idx:n :}
   REQUIRE-COMMAND
   ARG-BUF idx ARG-OFF ARG-SLOT @ +
   idx ARG-LEN ARG-SLOT @ ;

;package
