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
require tools/json.f
require tools/lint/diff-frame-write.f

package DIFF-CAPTURE
private

9 constant REC-CELLS
0 constant R-STATUS
1 constant R-OLD-OFF
2 constant R-OLD-U
3 constant R-NEW-OFF
4 constant R-NEW-U
5 constant R-RAW-OFF
6 constant R-RAW-U
7 constant R-FORM
8 constant R-BODY

create ROOT FS-PATH-CAP allot
create META-PATH FS-PATH-CAP allot
create RAW-PATH FS-PATH-CAP allot
create OUT-PATH FS-PATH-CAP allot
create ERR-PATH FS-PATH-CAP allot
create JJ-PATH FS-PATH-CAP allot

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

: CLOSE-FD ( ptr n -- ) {: p:ptr :}
   p @ dup 0 >= if close else drop then
   -1 p ! ;

: SPAWN ( -- )
   JJ$ >LEN -1 >FD OUT-FD @ >FD ERR-FD @ >FD
   PROC-RUN-ARGV-IO-RC MATCH result
      ok  OF drop 0 RUN-RC ! ENDOF
      err OF RUN-RC ! ENDOF
   ;MATCH ;

: RUN-COMMAND ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu OPEN-OUT FD>N OUT-FD !
   ERR-PATH$ OPEN-OUT FD>N ERR-FD !
   [: SPAWN ;] catch {: code:n :}
   OUT-FD CLOSE-FD
   ERR-FD CLOSE-FD
   code 0<> if code throw then
   RUN-RC @ 0<> if E-DIFF-CAPTURE throw then
   ERR-PATH$ FILE-SIZE 0<> if E-DIFF-CAPTURE-STDERR throw then ;

: ARG ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: REPO-ARG ( -- )
   CAP-REPO-U @ 0 > if
      s" -R" ARG
      CAP-REPO-A @ CAP-REPO-U @ ARG
   then ;

: PINNED-ARGS ( -- )
   PROC-ARGV-RESET
   s" --ignore-working-copy" ARG
   REPO-ARG
   s" --at-operation" ARG OP$ ARG ;

: OP-ARGS ( -- )
   PROC-ARGV-RESET
   s" --ignore-working-copy" ARG
   REPO-ARG
   s" operation" ARG
   s" log" ARG
   s" --no-graph" ARG
   s" --limit" ARG
   s" 1" ARG
   s" -T" ARG
   s" id" ARG ;

: SNAPSHOT-ARGS ( -- )
   PROC-ARGV-RESET
   REPO-ARG
   s" log" ARG
   s" --no-graph" ARG
   s" -r" ARG s" @" ARG
   s" -T" ARG s" " ARG ;

: REV-ARGS ( ptr u8 n -- ) {: rev:ptr revu:n :}
   PINNED-ARGS
   s" log" ARG
   s" --no-graph" ARG
   s" -r" ARG
   rev revu ARG
   s" -T" ARG
   s" commit_id" ARG ;

: LOAD-EXACT ( ptr u8 n -- ptr u8 n ) {: path:ptr pathu:n :}
   path pathu FILE-SIZE {: u:n :}
   u 0 <= if E-DIFF-CAPTURE-ID throw then
   u MEM-ALLOC-BYTES drop {: a:ptr :}
   path pathu a u READ-ALL u <> if E-FS-IO throw then
   a u ;

: RESOLVE-OPERATION ( -- )
   OP-ARGS
   OUT-PATH$ RUN-COMMAND
   OUT-PATH$ LOAD-EXACT
   2dup DIFF:OBJECT-ID? 0= if 2drop E-DIFF-CAPTURE-ID throw then
   OP-U ! OP-A ! ;

: SNAPSHOT ( -- )
   SNAPSHOT-ARGS
   OUT-PATH$ RUN-COMMAND
   OUT-PATH$ FILE-SIZE 0<> if E-DIFF-CAPTURE throw then ;

: RESOLVE ( ptr u8 n -- ptr u8 n )
   REV-ARGS
   OUT-PATH$ RUN-COMMAND
   OUT-PATH$ LOAD-EXACT
   2dup DIFF:OBJECT-ID? 0= if 2drop E-DIFF-CAPTURE-ID throw then ;

: RESOLVE-REVISIONS ( ptr u8 n ptr u8 n -- )
   {: from:ptr fromu:n to:ptr tou:n :}
   from fromu RESOLVE FROM-U ! FROM-A !
   to tou RESOLVE TO-U ! TO-A ! ;

: META-TEMPLATE$ ( -- ptr u8 n )
   S\" \q[\q ++ json(status) ++ \q,\q ++ json(source.path()) ++ \q,\q ++ json(source.file_type()) ++ \q,\q ++ json(source.executable()) ++ \q,\q ++ json(source.conflict()) ++ \q,\q ++ json(target.path()) ++ \q,\q ++ json(target.file_type()) ++ \q,\q ++ json(target.executable()) ++ \q,\q ++ json(target.conflict()) ++ \q]\\n\q" ;

: DIFF-ARGS ( -- )
   PINNED-ARGS
   s" diff" ARG
   s" --from" ARG FROM$ ARG
   s" --to" ARG TO$ ARG ;

: CAPTURE-METADATA ( -- )
   DIFF-ARGS
   s" -T" ARG META-TEMPLATE$ ARG
   META-PATH$ RUN-COMMAND ;

: CAPTURE-RAW ( -- )
   DIFF-ARGS
   s" --git" ARG
   s" --color=never" ARG
   RAW-PATH$ RUN-COMMAND ;

: LOAD-CAPTURES ( -- )
   META-PATH$ FILE-SIZE {: metau:n :}
   metau 0= if
      1 MEM-ALLOC-BYTES drop META-A !
      0 META-U !
   else
      metau MEM-ALLOC-64K-SPAN drop META-A !
      META-PATH$ META-A @ metau READ-ALL META-U !
      META-U @ metau <> if E-FS-IO throw then
   then
   RAW-PATH$ FILE-SIZE {: rawu:n :}
   rawu 0= if
      1 MEM-ALLOC-BYTES drop RAW-A !
      0 RAW-U !
   else
      rawu MEM-ALLOC-64K-SPAN drop RAW-A !
      RAW-PATH$ RAW-A @ rawu READ-ALL RAW-U !
      RAW-U @ rawu <> if E-FS-IO throw then
   then ;

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

: TYPE-PRESENT? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0= if false exit then
   a u s" file" STR= if true exit then
   a u s" symlink" STR= if true exit then
   a u s" git-submodule" STR= if true exit then
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
   present if a u POOL+ else 0 then row off-field REC!
   present if u else 0 then row len-field REC! ;

: PARSE-ROW ( ptr u8 n n -- ) {: a:ptr u:n row:n :}
   a u JSON-PARSE {: root:n :}
   root JSON-KIND J-ARR <> if E-DIFF-SYNTAX throw then
   root JSON-COUNT 9 <> if E-DIFF-SYNTAX throw then
   root 0 NODE$ STATUS-BYTE row R-STATUS REC!
   root 2 NODE$ TYPE-PRESENT? {: old?:bool :}
   root 6 NODE$ TYPE-PRESENT? {: new?:bool :}
   root 4 NODE-BOOL if E-DIFF-SYNTAX throw then
   root 8 NODE-BOOL if E-DIFF-SYNTAX throw then
   root 3 NODE-BOOL drop
   root 7 NODE-BOOL drop
   row old? root 1 NODE$ R-OLD-OFF R-OLD-U STORE-PATH
   row new? root 5 NODE$ R-NEW-OFF R-NEW-U STORE-PATH ;

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
   R-OLD-U REC@ 0 > ;

: NEW? ( n -- bool )
   R-NEW-U REC@ 0 > ;

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
   DIFF:SCAN-SECTION {: used:n shape:DIFF:form body:bool :}
   RAW-CUR @ row R-RAW-OFF REC!
   used row R-RAW-U REC!
   shape FORM>BYTE row R-FORM REC!
   body if 1 else 0 then row R-BODY REC!
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
   RESOLVE-OPERATION
   CAP-FROM-A @ CAP-FROM-U @ CAP-TO-A @ CAP-TO-U @ RESOLVE-REVISIONS
   CAPTURE-METADATA
   CAPTURE-RAW
   LOAD-CAPTURES
   ALLOC-ROWS
   PARSE-ROWS
   SPLIT-ROWS
   BUILD-FRAME
   CAP-OUT-A @ CAP-OUT-U @ FRAME-A @ FRAME-U @ ATOMIC-WRITE-FILE ;

: CAPTURE ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: repo:ptr repou:n output:ptr outputu:n from:ptr fromu:n to:ptr tou:n :}
   repo CAP-REPO-A ! repou CAP-REPO-U !
   output CAP-OUT-A ! outputu CAP-OUT-U !
   from CAP-FROM-A ! fromu CAP-FROM-U !
   to CAP-TO-A ! tou CAP-TO-U !
   s" habu-diff-capture" TMPDIR-MKDIR {: root:ptr rootu:n :}
   root rootu ROOT!
   SET-PATHS
   [: CAPTURE-BODY ;] catch {: code:n :}
   code 0<> if
      [: ROOT$ REMOVE-TREE ;] catch drop \ Cleanup cannot replace the primary failure.
      code throw
   then
   ROOT$ REMOVE-TREE ;

public

: RUN ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: output:ptr outputu:n from:ptr fromu:n to:ptr tou:n :}
   s" " output outputu from fromu to tou CAPTURE ;

: RUN-IN ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   CAPTURE ;

;package
