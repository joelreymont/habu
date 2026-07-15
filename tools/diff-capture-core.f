\ diff-capture-core.f - immutable-revision jj diff artifact producer.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-cwd.f
require src/core/sha256.f
require lib/json-write.f
require lib/fmt.f
require tools/json.f
require tools/lint/diff-file.f
require tools/lint/diff-frame-write.f

package DIFF-CAPTURE
public

ENUM command-phase
   snapshot
   resolve-from
   resolve-to
   metadata
   raw
   old-content
   new-content
;ENUM

ENUM command-outcome
   succeeded
   exited
   fault
;ENUM

ENUM capture-outcome
   ok
   primary-failed
   cleanup-failed
   combined-failed
;ENUM

private

14 constant REC-CELLS
0 constant R-STATUS
1 constant R-OLD-OFF
2 constant R-OLD-U
3 constant R-NEW-OFF
4 constant R-NEW-U
5 constant R-RAW-OFF
6 constant R-RAW-U
7 constant R-FORM
8 constant R-BODY
9 constant R-MODE
10 constant R-OLD-KIND
11 constant R-NEW-KIND
12 constant R-OLD-EXEC
13 constant R-NEW-EXEC

0 constant KIND-ABSENT
1 constant KIND-FILE
2 constant KIND-SYMLINK
3 constant KIND-GITLINK
8000 constant PEEK-CAP
$20 constant SHA-U
256 constant ARG-MAX
32768 constant ARG-CAP

create ROOT FS-PATH-CAP allot
create META-PATH FS-PATH-CAP allot
create RAW-PATH FS-PATH-CAP allot
create OUT-PATH FS-PATH-CAP allot
create ERR-PATH FS-PATH-CAP allot
create JJ-PATH FS-PATH-CAP allot
create OLD-DIGEST SHA-U allot
create NEW-DIGEST SHA-U allot
create PEEK-BUF PEEK-CAP allot
create ARG-BUF ARG-CAP allot
create ARG-OFF ARG-MAX cells allot
create ARG-LEN ARG-MAX cells allot

variable ROOT-U
variable META-PATH-U
variable RAW-PATH-U
variable OUT-PATH-U
variable ERR-PATH-U
variable JJ-PATH-U

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
PTR-VARIABLE REC-A
variable REC-N
PTR-VARIABLE POOL-A
variable POOL-CAP
variable POOL-U
PTR-VARIABLE FRAME-A
variable FRAME-U
PTR-VARIABLE CAP-OUT-A
variable CAP-OUT-U
PTR-VARIABLE CAP-REPO-A
variable CAP-REPO-U
PTR-VARIABLE CAP-FROM-A
variable CAP-FROM-U
PTR-VARIABLE CAP-TO-A
variable CAP-TO-U

variable OUT-FD
variable ERR-FD
variable RUN-RC
variable ROW-I
variable LINE-START
variable RAW-CUR
variable FRAME-CAP
variable OLD-SIZE
variable NEW-SIZE
variable OLD-BINARY
variable NEW-BINARY
variable PEEK-FD
variable PEEK-BINARY
PTR-VARIABLE PEEK-PATH-A
variable PEEK-PATH-U
variable ARG-N
variable ARG-U

1 LAYOUT-BUFFER LAST-PHASE-V command-phase
1 LAYOUT-BUFFER LAST-OUTCOME-V command-outcome
1 LAYOUT-BUFFER LAST-CAPTURE-V capture-outcome
variable LAST-RC-N
variable LAST-CODE-N
variable LAST-CAPTURE-CODE-N
variable LAST-PRIMARY-N
variable LAST-CLEANUP-N
variable REPORT-READY
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
variable ROOT-READY

: LAST-PHASE-AT ( -- ptr command-phase )
   0 LAST-PHASE-V ;

: LAST-OUTCOME-AT ( -- ptr command-outcome )
   0 LAST-OUTCOME-V ;

: LAST-CAPTURE-AT ( -- ptr capture-outcome )
   0 LAST-CAPTURE-V ;

: LAST-PHASE! ( command-phase -- )
   LAST-PHASE-AT ! ;

: LAST-OUTCOME! ( command-outcome -- )
   LAST-OUTCOME-AT ! ;

: LAST-CAPTURE! ( capture-outcome -- )
   LAST-CAPTURE-AT ! ;

: ROOT$ ( -- ptr u8 n )
   ROOT ROOT-U @ ;

: META-PATH$ ( -- ptr u8 n )
   META-PATH META-PATH-U @ ;

: RAW-PATH$ ( -- ptr u8 n )
   RAW-PATH RAW-PATH-U @ ;

: OUT-PATH$ ( -- ptr u8 n )
   OUT-PATH OUT-PATH-U @ ;

: ERR-PATH$ ( -- ptr u8 n )
   ERR-PATH ERR-PATH-U @ ;

: JJ$ ( -- ptr u8 n )
   JJ-PATH JJ-PATH-U @ ;

: CAP-CWD$ ( -- ptr u8 n )
   CAP-REPO-U @ 0 > if CAP-REPO-A @ CAP-REPO-U @ else s" ." then ;

: RUN-EXE$ ( -- ptr u8 n )
   RUN-EXE-A @ RUN-EXE-U @ ;

: RUN-CWD$ ( -- ptr u8 n )
   RUN-CWD-A @ RUN-CWD-U @ ;

: FROM$ ( -- ptr u8 n )
   FROM-A @ FROM-U @ ;

: TO$ ( -- ptr u8 n )
   TO-A @ TO-U @ ;

: OP$ ( -- ptr u8 n )
   OP-A @ OP-U @ ;

: META$ ( -- ptr u8 n )
   META-A @ META-U @ ;

: RAW$ ( -- ptr u8 n )
   RAW-A @ RAW-U @ ;

: PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: root:ptr rootu:n name:ptr nameu:n dst:ptr lenp:ptr :}
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

: JJ! ( -- )
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
   p @ dup 0 >= if close else drop then \ close has no result; this cleanup always clears ownership.
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

: LOAD-REPORTS ( -- )
   OUT-OPENED @ if
      RUN-OUT-A @ RUN-OUT-U @ DIFF-FILE:LOAD LAST-OUT-U ! LAST-OUT-A !
   then
   ERR-OPENED @ if
      ERR-PATH$ DIFF-FILE:LOAD LAST-ERR-U ! LAST-ERR-A !
   then ;

: OPEN-AND-SPAWN ( -- )
   RUN-OUT-A @ RUN-OUT-U @ COMMAND-OPEN FD>N OUT-FD !
   true OUT-OPENED !
   ERR-PATH$ COMMAND-OPEN FD>N ERR-FD !
   true ERR-OPENED !
   COMMAND-SPAWN ;

: RESET-REPORT ( command-phase ptr u8 n -- ) {: phase:command-phase exe:ptr exeu:n :}
   true REPORT-READY !
   phase LAST-PHASE!
   exe LAST-EXE-A ! exeu LAST-EXE-U !
   construct command-outcome succeeded LAST-OUTCOME!
   0 LAST-RC-N !
   0 LAST-CODE-N !
   0 LAST-CAPTURE-CODE-N !
   0 LAST-OUT-U !
   0 LAST-ERR-U !
   false OUT-OPENED !
   false ERR-OPENED !
   -1 OUT-FD !
   -1 ERR-FD ! ;

: RUN-COMMAND ( command-phase ptr u8 n ptr u8 n ptr u8 n -- )
   {: phase:command-phase exe:ptr exeu:n cwd:ptr cwdu:n out:ptr outu:n :}
   phase exe exeu RESET-REPORT
   exe RUN-EXE-A ! exeu RUN-EXE-U !
   cwd RUN-CWD-A ! cwdu RUN-CWD-U !
   out RUN-OUT-A ! outu RUN-OUT-U !
   [: OPEN-AND-SPAWN ;] catch {: code:n :}
   OUT-FD CLOSE-FD
   ERR-FD CLOSE-FD
   [: LOAD-REPORTS ;] catch {: capture-code:n :}
   capture-code LAST-CAPTURE-CODE-N !
   code 0<> if
      construct command-outcome fault LAST-OUTCOME!
      code LAST-CODE-N !
      code throw
   then
   capture-code 0<> if
      construct command-outcome fault LAST-OUTCOME!
      capture-code LAST-CODE-N !
      capture-code throw
   then
   RUN-RC @ LAST-RC-N !
   RUN-RC @ 0<> if
      construct command-outcome exited LAST-OUTCOME!
      E-DIFF-CAPTURE throw
   then ;

: RUN-JJ-COMMAND ( command-phase ptr u8 n -- ) {: phase:command-phase out:ptr outu:n :}
   phase JJ$ CAP-CWD$ out outu RUN-COMMAND ;

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
   CAP-REPO-U @ 0 > if
      s" -R" ARG
      CAP-REPO-A @ CAP-REPO-U @ ARG
   then ;

: PINNED-ARGS ( -- )
   ARGS-RESET
   s" --ignore-working-copy" ARG
   REPO-ARG
   s" --at-operation" ARG OP$ ARG ;

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

: SNAPSHOT ( -- )
   SNAPSHOT-ARGS
   construct command-phase snapshot OUT-PATH$ RUN-JJ-COMMAND
   OUT-PATH$ LOAD-EXACT {: a:ptr u:n :}
   u 1 <= if E-DIFF-CAPTURE-ID throw then
   a u 1- + c@ $0A <> if E-DIFF-CAPTURE-ID throw then
   a u 1- DIFF:OBJECT-ID? 0= if E-DIFF-CAPTURE-ID throw then
   a OP-A ! u 1- OP-U ! ;

defer SNAPSHOT-BARRIER ( -- )
: SNAPSHOT-BARRIER-DEFAULT ( -- ) ;
: RESET-SNAPSHOT-BARRIER ( -- )
   [: SNAPSHOT-BARRIER-DEFAULT ;] is SNAPSHOT-BARRIER ;
RESET-SNAPSHOT-BARRIER

: RESOLVE ( command-phase ptr u8 n -- ptr u8 n ) {: phase:command-phase rev:ptr revu:n :}
   rev revu REV-ARGS
   phase OUT-PATH$ RUN-JJ-COMMAND
   OUT-PATH$ LOAD-EXACT
   2dup DIFF:COMMIT-ID? 0= if 2drop E-DIFF-CAPTURE-ID throw then ;

: RESOLVE-REVISIONS ( ptr u8 n ptr u8 n -- )
   {: from:ptr fromu:n to:ptr tou:n :}
   construct command-phase resolve-from from fromu RESOLVE FROM-U ! FROM-A !
   construct command-phase resolve-to to tou RESOLVE TO-U ! TO-A ! ;

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
   s" --from" ARG FROM$ ARG
   s" --to" ARG TO$ ARG ;

: CAPTURE-METADATA ( -- )
   DIFF-ARGS
   s" -T" ARG META-TEMPLATE$ ARG
   construct command-phase metadata META-PATH$ RUN-JJ-COMMAND ;

: CAPTURE-RAW ( -- )
   DIFF-ARGS
   s" --git" ARG
   s" --color=never" ARG
   construct command-phase raw RAW-PATH$ RUN-JJ-COMMAND ;

: LOAD-CAPTURES ( -- )
   META-PATH$ DIFF-FILE:LOAD META-U ! META-A !
   RAW-PATH$ DIFF-FILE:LOAD RAW-U ! RAW-A ! ;

: COUNT-ROWS ( -- n )
   0
   0 begin dup META-U @ < while
      dup META-A @ + c@ $0A = if swap 1+ swap then
      1+
   repeat drop
   META-U @ 0 > if META-A @ META-U @ 1- + c@ $0A <> if drop E-DIFF-SYNTAX throw then then ;

: ALLOC-ROWS ( -- )
   COUNT-ROWS dup REC-N ! drop
   REC-N @ MEM-MAX-CELLS REC-CELLS / > if E-DIFF-FRAME-CAP throw then
   REC-N @ 0= if 1 else REC-N @ REC-CELLS * then
   >COUNT MEM-ALLOC-CELLS REC-A !
   META-U @ 0= if 1 else META-U @ then
   MEM-ALLOC-BYTES drop POOL-A !
   META-U @ POOL-CAP !
   0 POOL-U ! ;

: REC-SLOT ( n n -- ptr a ) {: row:n field:n :}
   row 0 < row REC-N @ >= or if E-DIFF-SYNTAX throw then
   field 0 < field REC-CELLS >= or if E-DIFF-SYNTAX throw then
   row MEM-MAX-CELLS REC-CELLS / > if E-DIFF-SYNTAX throw then
   REC-A @ row REC-CELLS * field + cells + ;

: REC@ ( n n -- n )
   REC-SLOT @ ;

: REC! ( n n n -- ) {: value:n row:n field:n :}
   value row field REC-SLOT ! ;

: POOL+ ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 0 < if E-DIFF-SYNTAX throw then
   POOL-U @ u + POOL-U @ < if E-DIFF-SYNTAX throw then
   POOL-U @ u + POOL-CAP @ > if E-DIFF-SYNTAX throw then
   0 begin dup u < while
      dup a + c@ 0= if E-DIFF-SYNTAX throw then
      1+
   repeat drop
   POOL-U @ {: off:n :}
   a POOL-A @ off + u BYTE-COPY
   off u + POOL-U !
   off ;

: TYPE-KIND ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 0= if KIND-ABSENT exit then
   a u s" file" STR= if KIND-FILE exit then
   a u s" symlink" STR= if KIND-SYMLINK exit then
   a u s" git-submodule" STR= if KIND-GITLINK exit then
   E-DIFF-SYNTAX throw ;

: STATUS-BYTE ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u s" modified" STR= if 0 exit then
   a u s" added" STR= if 1 exit then
   a u s" removed" STR= if 2 exit then
   a u s" renamed" STR= if 3 exit then
   a u s" copied" STR= if 4 exit then
   E-DIFF-SYNTAX throw ;

: NODE$ ( n n -- ptr u8 n )
   JSON-ARR@ JSON-STRING$ ;

: NODE-BOOL ( n n -- bool )
   JSON-ARR@ JSON-BOOL@ ;

: STORE-PATH ( n bool ptr u8 n n n -- )
   {: row:n present:bool a:ptr u:n off-field:n len-field:n :}
   present if
      u 0 <= if E-DIFF-SYNTAX throw then
   else
      u 0<> if E-DIFF-SYNTAX throw then
   then
   present if a u POOL+ else 0 then row off-field REC!
   present if u else 0 then row len-field REC! ;

: STATUS-PRESENCE ( n bool bool -- ) {: status:n old?:bool new?:bool :}
   status case
      0 of old? 0= new? 0= or if E-DIFF-SYNTAX throw then endof
      1 of old? new? 0= or if E-DIFF-SYNTAX throw then endof
      2 of old? 0= new? or if E-DIFF-SYNTAX throw then endof
      3 of old? 0= new? 0= or if E-DIFF-SYNTAX throw then endof
      4 of old? 0= new? 0= or if E-DIFF-SYNTAX throw then endof
      E-DIFF-SYNTAX throw
   endcase ;

: STATUS-PATHS ( n ptr u8 n ptr u8 n -- )
   {: status:n old:ptr oldu:n new:ptr newu:n :}
   status case
      0 of old oldu new newu STR= 0= if E-DIFF-SYNTAX throw then endof
      3 of old oldu new newu STR= if E-DIFF-SYNTAX throw then endof
      4 of old oldu new newu STR= if E-DIFF-SYNTAX throw then endof
   endcase ;

: PARSE-ROW ( ptr u8 n n -- ) {: a:ptr u:n row:n :}
   a u JSON-PARSE {: root:n :}
   root JSON-KIND J-ARR <> if E-DIFF-SYNTAX throw then
   root JSON-COUNT 9 <> if E-DIFF-SYNTAX throw then
   root 0 NODE$ STATUS-BYTE {: status:n :}
   status row R-STATUS REC!
   root 2 NODE$ TYPE-KIND {: old-kind:n :}
   root 6 NODE$ TYPE-KIND {: new-kind:n :}
   old-kind row R-OLD-KIND REC!
   new-kind row R-NEW-KIND REC!
   root 4 NODE-BOOL if E-DIFF-SYNTAX throw then
   root 8 NODE-BOOL if E-DIFF-SYNTAX throw then
   root 3 NODE-BOOL {: old-exec:bool :}
   root 7 NODE-BOOL {: new-exec:bool :}
   old-kind KIND-ABSENT <> {: old?:bool :}
   new-kind KIND-ABSENT <> {: new?:bool :}
   old? 0= old-exec and if E-DIFF-SYNTAX throw then
   new? 0= new-exec and if E-DIFF-SYNTAX throw then
   old-exec if 1 else 0 then row R-OLD-EXEC REC!
   new-exec if 1 else 0 then row R-NEW-EXEC REC!
   root 1 NODE$ {: old:ptr oldu:n :}
   root 5 NODE$ {: new:ptr newu:n :}
   status old? new? STATUS-PRESENCE
   status old oldu new newu STATUS-PATHS
   row old? old oldu R-OLD-OFF R-OLD-U STORE-PATH
   row new? new newu R-NEW-OFF R-NEW-U STORE-PATH ;

: PARSE-ROWS ( -- )
   0 ROW-I !
   0 LINE-START !
   0 begin dup META-U @ < while
      dup META-A @ + c@ $0A = if
         META-A @ LINE-START @ + over LINE-START @ -
         ROW-I @ PARSE-ROW
         ROW-I @ 1+ ROW-I !
         dup 1+ LINE-START !
      then
      1+
   repeat drop
   ROW-I @ REC-N @ <> if E-DIFF-SYNTAX throw then ;

: OLD$ ( n -- ptr u8 n ) {: row:n :}
   POOL-A @ row R-OLD-OFF REC@ + row R-OLD-U REC@ ;

: NEW$ ( n -- ptr u8 n ) {: row:n :}
   POOL-A @ row R-NEW-OFF REC@ + row R-NEW-U REC@ ;

: OLD? ( n -- bool )
   R-OLD-KIND REC@ KIND-ABSENT <> ;

: NEW? ( n -- bool )
   R-NEW-KIND REC@ KIND-ABSENT <> ;

: HEX-C ( n -- n )
   dup 10 < if $30 + else 10 - $61 + then ;

: FILESET$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   SB-RESET
   S\" root-file:\q" SB-APPEND
   0 begin dup u < while
      dup a + c@ {: c:n :}
      $5C SB-APPEND-C
      $78 SB-APPEND-C
      c 4 rshift HEX-C SB-APPEND-C
      c $F and HEX-C SB-APPEND-C
      1+
   repeat drop
   $22 SB-APPEND-C
   SB$ ;

: CONTENT-ARGS ( ptr u8 n ptr u8 n -- ) {: rev:ptr revu:n path:ptr pathu:n :}
   PINNED-ARGS
   s" file" ARG s" show" ARG
   s" -r" ARG rev revu ARG
   s" --" ARG path pathu FILESET$ ARG ;

: PEEK-BODY ( ptr u8 n -- bool ) {: path:ptr pathu:n :}
   path pathu FS-PATHZ 0 0 open dup PEEK-FD !
   dup 0 < if drop E-FS-OPEN throw then
   PEEK-BUF PEEK-CAP read {: got:n :}
   got 0 < if E-FS-IO throw then
   0 begin dup got < while
      dup PEEK-BUF + c@ 0= if drop true exit then
      1+
   repeat drop
   false ;

: PEEK-RUN ( -- )
   PEEK-PATH-A @ PEEK-PATH-U @ PEEK-BODY PEEK-BINARY ! ;

: FILE-BINARY? ( ptr u8 n -- bool ) {: path:ptr pathu:n :}
   path PEEK-PATH-A ! pathu PEEK-PATH-U !
   -1 PEEK-FD !
   false PEEK-BINARY !
   [: PEEK-RUN ;] catch {: code:n :}
   PEEK-FD @ dup 0 >= if close else drop then
   code 0<> if code throw then
   PEEK-BINARY @ if true else false then ;

: EMPTY-CONTENT ( ptr u8 ptr n ptr n -- ) {: digest:ptr sizep:ptr binaryp:ptr :}
   s" " digest SHA256
   0 sizep !
   0 binaryp ! ;

: FILE-CONTENT ( command-phase ptr u8 n ptr u8 n n ptr u8 ptr n ptr n -- )
   {: phase:command-phase rev:ptr revu:n path:ptr pathu:n kind:n digest:ptr sizep:ptr binaryp:ptr :}
   kind KIND-ABSENT = kind KIND-GITLINK = or if
      digest sizep binaryp EMPTY-CONTENT
      exit
   then
   rev revu path pathu CONTENT-ARGS
   phase OUT-PATH$ RUN-JJ-COMMAND
   OUT-PATH$ FILE-SIZE sizep !
   OUT-PATH$ digest SHA256-FILE dup 0<> if throw then drop
   kind KIND-FILE = if OUT-PATH$ FILE-BINARY? else false then
   if 1 else 0 then binaryp ! ;

: CONTENT-SAME? ( -- bool )
   OLD-SIZE @ NEW-SIZE @ =
   OLD-DIGEST SHA-U NEW-DIGEST SHA-U STR= and ;

: DECLARE-FORM ( n -- ) {: row:n :}
   row R-OLD-KIND REC@ KIND-GITLINK =
   row R-NEW-KIND REC@ KIND-GITLINK = or if
      row OLD? row NEW? and if
         row R-OLD-KIND REC@ row R-NEW-KIND REC@ <> if E-DIFF-SYNTAX throw then
      then
      5 row R-FORM REC!
      exit
   then
   row R-BODY REC@ 0<> if
      OLD-BINARY @ 0<> NEW-BINARY @ 0<> or if 1 else 0 then row R-FORM REC!
      exit
   then
   row R-MODE REC@ 0<> if 2 row R-FORM REC! exit then
   row R-STATUS REC@ case
      1 of 3 row R-FORM REC! endof
      2 of 3 row R-FORM REC! endof
      3 of 4 row R-FORM REC! endof
      4 of 4 row R-FORM REC! endof
      E-DIFF-SYNTAX throw
   endcase ;

: DECLARE-ROW ( n -- ) {: row:n :}
   construct command-phase old-content FROM$ row OLD$ row R-OLD-KIND REC@
   OLD-DIGEST OLD-SIZE OLD-BINARY FILE-CONTENT
   construct command-phase new-content TO$ row NEW$ row R-NEW-KIND REC@
   NEW-DIGEST NEW-SIZE NEW-BINARY FILE-CONTENT
   CONTENT-SAME? 0= if 1 else 0 then row R-BODY REC!
   row OLD? row NEW? and if
      row R-OLD-KIND REC@ row R-NEW-KIND REC@ <>
      row R-OLD-EXEC REC@ row R-NEW-EXEC REC@ <> or
   else
      false
   then if 1 else 0 then row R-MODE REC!
   row DECLARE-FORM ;

: DECLARE-ROWS ( -- )
   0 begin dup REC-N @ < while
      dup DECLARE-ROW
      1+
   repeat drop ;

defer CONTENT-PROVIDER ( -- )
: RESET-CONTENT-PROVIDER ( -- )
   [: DECLARE-ROWS ;] is CONTENT-PROVIDER ;
RESET-CONTENT-PROVIDER

: BYTE>STATUS ( n -- DIFF:status )
   case
      0 of DIFF-STATUS:MODIFIED endof
      1 of DIFF-STATUS:ADDED endof
      2 of DIFF-STATUS:REMOVED endof
      3 of DIFF-STATUS:RENAMED endof
      4 of DIFF-STATUS:COPIED endof
      E-DIFF-SYNTAX throw
   endcase ;

: FORM>BYTE ( DIFF:form -- n )
   MATCH DIFF:form
      text   OF 0 ENDOF
      binary OF 1 ENDOF
      mode   OF 2 ENDOF
      empty  OF 3 ENDOF
      pure   OF 4 ENDOF
      gitlink OF 5 ENDOF
   ;MATCH ;

: SCAN-ROW ( n bool -- ) {: row:n next?:bool :}
   row R-STATUS REC@ BYTE>STATUS
   row OLD? row OLD$
   row NEW? row NEW$
   next?
   next? if
      row 1+ OLD? row 1+ OLD$
      row 1+ NEW? row 1+ NEW$
   else
      false s" " false s" "
   then
   RAW-A @ RAW-CUR @ + RAW-U @ RAW-CUR @ -
   DIFF:SCAN-SECTION {: used:n shape:DIFF:form body:bool mode:bool :}
   RAW-CUR @ row R-RAW-OFF REC!
   used row R-RAW-U REC!
   shape FORM>BYTE row R-FORM REC@ <> if E-DIFF-SYNTAX throw then
   body if 1 else 0 then row R-BODY REC@ <> if E-DIFF-SYNTAX throw then
   mode if 1 else 0 then row R-MODE REC@ <> if E-DIFF-SYNTAX throw then
   RAW-CUR @ used + RAW-CUR @ < if E-DIFF-SYNTAX throw then
   RAW-CUR @ used + RAW-CUR ! ;

: SPLIT-ROWS ( -- )
   0 RAW-CUR !
   0 begin dup REC-N @ < while
      dup dup REC-N @ 1- < SCAN-ROW
      1+
   repeat drop
   REC-N @ 0= if
      RAW-U @ 0<> if E-DIFF-SYNTAX throw then
   else
      RAW-CUR @ RAW-U @ <> if E-DIFF-SYNTAX throw then
   then ;

: FRAME-SIZE ( -- n )
   FROM-U @ TO-U @ DIFF-WRITE:HEADER-SIZE FRAME-CAP !
   0 begin dup REC-N @ < while
      dup {: row:n :}
      FRAME-CAP @
      row R-OLD-U REC@
      row R-NEW-U REC@
      row R-RAW-U REC@
      DIFF-WRITE:SECTION-SIZE FRAME-CAP !
      1+
   repeat drop
   FRAME-CAP @ DIFF-WRITE:FINISH-SIZE ;

: BYTE>FORM ( n -- DIFF:form )
   case
      0 of DIFF-FORM:TEXT endof
      1 of DIFF-FORM:BINARY endof
      2 of DIFF-FORM:MODE endof
      3 of DIFF-FORM:EMPTY endof
      4 of DIFF-FORM:PURE endof
      5 of DIFF-FORM:GITLINK endof
      E-DIFF-SYNTAX throw
   endcase ;

: EMIT-ROW ( n -- ) {: row:n :}
   row R-STATUS REC@ BYTE>STATUS
   row R-FORM REC@ BYTE>FORM
   row R-BODY REC@ 0<> if true else false then
   row R-MODE REC@ 0<> if true else false then
   row OLD? row OLD$
   row NEW? row NEW$
   RAW-A @ row R-RAW-OFF REC@ + row R-RAW-U REC@
   DIFF-WRITE:SECTION ;

: BUILD-FRAME ( -- )
   FRAME-SIZE {: cap:n :}
   cap MEM-ALLOC-BYTES drop FRAME-A !
   FRAME-A @ cap FROM$ TO$ DIFF-WRITE:START
   0 begin dup REC-N @ < while
      dup EMIT-ROW
      1+
   repeat drop
   DIFF-WRITE:FINISH FRAME-U ! drop
   FRAME-U @ cap <> if E-DIFF-FRAME-CAP throw then ;

: CAPTURE-BODY ( -- )
   JJ!
   SNAPSHOT
   SNAPSHOT-BARRIER
   CAP-FROM-A @ CAP-FROM-U @ CAP-TO-A @ CAP-TO-U @ RESOLVE-REVISIONS
   CAPTURE-METADATA
   CAPTURE-RAW
   LOAD-CAPTURES
   ALLOC-ROWS
   PARSE-ROWS
   CONTENT-PROVIDER
   SPLIT-ROWS
   BUILD-FRAME ;

defer CAPTURE-CLEAN ( -- )
: CAPTURE-CLEAN-DEFAULT ( -- )
   ROOT$ REMOVE-TREE ;
: RESET-CAPTURE-CLEAN ( -- )
   [: CAPTURE-CLEAN-DEFAULT ;] is CAPTURE-CLEAN ;
RESET-CAPTURE-CLEAN

defer CAPTURE-PUBLISH ( -- )
: CAPTURE-PUBLISH-DEFAULT ( -- )
   CAP-OUT-A @ CAP-OUT-U @ FRAME-A @ FRAME-U @ ATOMIC-WRITE-FILE ;
: RESET-CAPTURE-PUBLISH ( -- )
   [: CAPTURE-PUBLISH-DEFAULT ;] is CAPTURE-PUBLISH ;
RESET-CAPTURE-PUBLISH

: RESET-CAPTURE-RESULT ( -- )
   construct capture-outcome ok LAST-CAPTURE!
   0 LAST-PRIMARY-N !
   0 LAST-CLEANUP-N ! ;

: SET-CAPTURE-RESULT ( n n -- ) {: primary:n cleanup:n :}
   primary LAST-PRIMARY-N !
   cleanup LAST-CLEANUP-N !
   primary 0<> if
      cleanup 0<> if
         construct capture-outcome combined-failed
      else
         construct capture-outcome primary-failed
      then
   else
      cleanup 0<> if
         construct capture-outcome cleanup-failed
      else
         construct capture-outcome ok
      then
   then
   LAST-CAPTURE! ;

: PREPARE-ROOT ( -- )
   s" habu-diff-capture" TMPDIR-MKDIR {: root:ptr rootu:n :}
   root rootu ROOT!
   true ROOT-READY !
   SET-PATHS ;

: CLEAN-CODE ( -- n )
   ROOT-READY @ 0= if 0 exit then
   [: CAPTURE-CLEAN ;] catch dup 0= if false ROOT-READY ! then ;

: CAPTURE-PRIMARY ( -- )
   PREPARE-ROOT
   CAPTURE-BODY ;

: THROW-RESULT ( n n -- ) {: primary:n cleanup:n :}
   primary cleanup SET-CAPTURE-RESULT
   primary 0<> if primary throw then
   cleanup 0<> if cleanup throw then ;

: PUBLISH ( -- )
   [: CAPTURE-PUBLISH ;] catch {: code:n :}
   code 0<> if code 0 THROW-RESULT then ;

: CAPTURE ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: repo:ptr repou:n output:ptr outputu:n from:ptr fromu:n to:ptr tou:n :}
   false REPORT-READY !
   false ROOT-READY !
   RESET-CAPTURE-RESULT
   repo CAP-REPO-A ! repou CAP-REPO-U !
   output CAP-OUT-A ! outputu CAP-OUT-U !
   from CAP-FROM-A ! fromu CAP-FROM-U !
   to CAP-TO-A ! tou CAP-TO-U !
   [: CAPTURE-PRIMARY ;] catch {: primary:n :}
   CLEAN-CODE {: cleanup:n :}
   primary cleanup THROW-RESULT
   PUBLISH ;

: ARG$ ( n -- ptr u8 n ) {: idx:n :}
   ARG-BUF idx ARG-OFF ARG-SLOT @ +
   idx ARG-LEN ARG-SLOT @ ;

: PHASE$ ( command-phase -- ptr u8 n )
   MATCH command-phase
      snapshot     OF s" snapshot" ENDOF
      resolve-from OF s" resolve-from" ENDOF
      resolve-to   OF s" resolve-to" ENDOF
      metadata     OF s" metadata" ENDOF
      raw          OF s" raw" ENDOF
      old-content  OF s" old-content" ENDOF
      new-content  OF s" new-content" ENDOF
   ;MATCH ;

: OUTCOME$ ( command-outcome -- ptr u8 n )
   MATCH command-outcome
      succeeded OF s" succeeded" ENDOF
      exited    OF s" exited" ENDOF
      fault     OF s" fault" ENDOF
   ;MATCH ;

: CAPTURE-OUTCOME$ ( capture-outcome -- ptr u8 n )
   MATCH capture-outcome
      ok              OF s" succeeded" ENDOF
      primary-failed  OF s" primary-failed" ENDOF
      cleanup-failed  OF s" cleanup-failed" ENDOF
      combined-failed OF s" combined-failed" ENDOF
   ;MATCH ;

: NUMBER$ ( n -- ptr u8 n )
   SB-RESET SB-INT SB$ ;

: REPORT-ARGV ( -- )
   s" argv" JW-KEY
   JW-ARRAY-START
   LAST-EXE-A @ LAST-EXE-U @ JW-STRING
   0 begin dup ARG-N @ < while
      JW-COMMA
      dup ARG$ JW-STRING
      1+
   repeat drop
   JW-ARRAY-END ;

: RENDER-REPORT ( -- ptr u8 n )
   JW-RESET
   JW-OBJECT-START
   s" phase" LAST-PHASE-AT @ PHASE$ JW-FIELD-S JW-COMMA
   REPORT-ARGV JW-COMMA
   s" outcome" LAST-OUTCOME-AT @ OUTCOME$ JW-FIELD-S JW-COMMA
   s" rc" LAST-RC-N @ NUMBER$ JW-FIELD-RAW JW-COMMA
   s" code" LAST-CODE-N @ NUMBER$ JW-FIELD-RAW JW-COMMA
   s" capture_code" LAST-CAPTURE-CODE-N @ NUMBER$ JW-FIELD-RAW JW-COMMA
   s" capture_outcome" LAST-CAPTURE-AT @ CAPTURE-OUTCOME$ JW-FIELD-S JW-COMMA
   s" primary_code" LAST-PRIMARY-N @ NUMBER$ JW-FIELD-RAW JW-COMMA
   s" cleanup_code" LAST-CLEANUP-N @ NUMBER$ JW-FIELD-RAW JW-COMMA
   s" stdout" LAST-OUT-A @ LAST-OUT-U @ JW-FIELD-S JW-COMMA
   s" stderr" LAST-ERR-A @ LAST-ERR-U @ JW-FIELD-S
   JW-OBJECT-END
   JW$ ;

public

: LAST-PHASE ( -- command-phase )
   LAST-PHASE-AT @ ;

: LAST-OUTCOME ( -- command-outcome )
   LAST-OUTCOME-AT @ ;

: LAST-CAPTURE-OUTCOME ( -- capture-outcome )
   LAST-CAPTURE-AT @ ;

: LAST-RC ( -- n )
   LAST-RC-N @ ;

: LAST-CODE ( -- n )
   LAST-CODE-N @ ;

: LAST-PRIMARY ( -- n )
   LAST-PRIMARY-N @ ;

: LAST-CLEANUP ( -- n )
   LAST-CLEANUP-N @ ;

: LAST-OUT$ ( -- ptr u8 n )
   LAST-OUT-A @ LAST-OUT-U @ ;

: LAST-ERR$ ( -- ptr u8 n )
   LAST-ERR-A @ LAST-ERR-U @ ;

: REPORT$ ( -- ptr u8 n )
   RENDER-REPORT ;

: REPORT? ( -- bool )
   REPORT-READY @ if true else false then ;

: RUN ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: output:ptr outputu:n from:ptr fromu:n to:ptr tou:n :}
   s" " output outputu from fromu to tou CAPTURE ;

: RUN-IN ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   CAPTURE ;

;package
