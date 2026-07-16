\ diff-capture-metadata.f - capture metadata model and parser.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/memory.f
require src/core/sha256.f
require tools/json.f
require tools/lint/diff-path.f

package DIFF-META
private

15 constant REC-CELLS
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
14 constant R-CONTENT

0 constant KIND-ABSENT
1 constant KIND-FILE
2 constant KIND-SYMLINK
3 constant KIND-GITLINK
$20 constant SHA-U

PTR-VARIABLE META-A
variable META-U
PTR-VARIABLE REC-A
variable REC-N
PTR-VARIABLE POOL-A
variable POOL-CAP
variable POOL-U
variable ROW-I
variable LINE-START

: REC-SLOT ( n n -- ptr a ) {: row:n field:n :}
   row 0 < row REC-N @ >= or if E-DIFF-SYNTAX throw then
   field 0 < field REC-CELLS >= or if E-DIFF-SYNTAX throw then
   row MEM-MAX-CELLS REC-CELLS / > if E-DIFF-SYNTAX throw then
   REC-A @ row REC-CELLS * field + cells + ;

: REC@ ( n n -- n )
   REC-SLOT @ ;

: REC! ( n n n -- ) {: value:n row:n field:n :}
   value row field REC-SLOT ! ;

: COUNT-ROWS ( -- n )
   0
   0 begin dup META-U @ < while
      dup META-A @ + c@ $0A = if swap 1+ swap then
      1+
   repeat drop
   META-U @ 0 > if
      META-A @ META-U @ 1- + c@ $0A <> if drop E-DIFF-SYNTAX throw then
   then ;

: ALLOC-ROWS ( -- )
   COUNT-ROWS dup REC-N ! drop
   REC-N @ MEM-MAX-CELLS REC-CELLS / > if E-DIFF-FRAME-CAP throw then
   REC-N @ 0= if 1 else REC-N @ REC-CELLS * then
   >COUNT MEM-ALLOC-CELLS REC-A !
   META-U @ 0= if 1 else META-U @ then
   MEM-ALLOC-BYTES drop POOL-A !
   META-U @ POOL-CAP !
   0 POOL-U ! ;

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
   present a u DIFF-PATH:VALIDATE-SIDE
   present if a u POOL+ else 0 then row off-field REC!
   present if u else 0 then row len-field REC! ;

: STATUS-PRESENCE ( n bool bool -- )
   {: status:n old?:bool new?:bool :}
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
   0 row R-CONTENT REC!
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

: SIDE-CONTENT-CHECK ( n n bool n -- )
   {: kind:n size:n binary:bool digestu:n :}
   size 0 < digestu SHA-U <> or if E-DIFF-CAPTURE throw then
   kind KIND-ABSENT = kind KIND-GITLINK = or if
      size 0<> binary or if E-DIFF-CAPTURE throw then
      exit
   then
   kind KIND-FILE <> binary and if E-DIFF-CAPTURE throw then ;

: CONTENT-SAME? ( n ptr u8 n ptr u8 -- bool )
   {: old-size:n old-digest:ptr new-size:n new-digest:ptr :}
   old-size new-size =
   old-digest SHA-U new-digest SHA-U STR= and ;

: DECLARE-FORM ( n bool bool -- )
   {: row:n old-binary:bool new-binary:bool :}
   row R-OLD-KIND REC@ KIND-GITLINK =
   row R-NEW-KIND REC@ KIND-GITLINK = or if
      row OLD? row NEW? and if
         row R-OLD-KIND REC@ row R-NEW-KIND REC@ <> if E-DIFF-SYNTAX throw then
      then
      5 row R-FORM REC!
      exit
   then
   row R-BODY REC@ 0<> if
      old-binary new-binary or if 1 else 0 then row R-FORM REC!
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

: SET-CONTENT-ROW ( n n bool ptr u8 n n bool ptr u8 n -- )
   {: row:n old-size:n old-binary:bool old-digest:ptr old-digestu:n
      new-size:n new-binary:bool new-digest:ptr new-digestu:n :}
   row 0 < row REC-N @ >= or if E-DIFF-CAPTURE throw then
   row R-CONTENT REC@ 0<> if E-DIFF-CAPTURE throw then
   row R-OLD-KIND REC@ old-size old-binary old-digestu SIDE-CONTENT-CHECK
   row R-NEW-KIND REC@ new-size new-binary new-digestu SIDE-CONTENT-CHECK
   old-size old-digest new-size new-digest CONTENT-SAME? 0=
   if 1 else 0 then row R-BODY REC!
   row OLD? row NEW? and if
      row R-OLD-KIND REC@ row R-NEW-KIND REC@ <>
      row R-OLD-EXEC REC@ row R-NEW-EXEC REC@ <> or
   else
      false
   then if 1 else 0 then row R-MODE REC!
   row old-binary new-binary DECLARE-FORM
   1 row R-CONTENT REC! ;

: COMPLETE? ( -- bool )
   0 begin dup REC-N @ < while
      dup R-CONTENT REC@ 0= if drop false exit then
      1+
   repeat drop
   true ;

public

EXPORT OLD$
EXPORT NEW$
EXPORT OLD?
EXPORT NEW?
EXPORT COMPLETE?

: LOAD ( ptr u8 n -- )
   META-U ! META-A !
   ALLOC-ROWS
   PARSE-ROWS ;

: COUNT ( -- n )
   REC-N @ ;

: STATUS ( n -- n )
   R-STATUS REC@ ;

: FORM ( n -- n )
   R-FORM REC@ ;

: BODY? ( n -- bool )
   R-BODY REC@ 0<> if true else false then ;

: MODE? ( n -- bool )
   R-MODE REC@ 0<> if true else false then ;

: BODY ( n -- n )
   R-BODY REC@ ;

: MODE ( n -- n )
   R-MODE REC@ ;

: CONTENT! ( n n bool ptr u8 n n bool ptr u8 n -- )
   SET-CONTENT-ROW ;

: RAW-RANGE! ( n n n -- ) {: row:n off:n u:n :}
   off row R-RAW-OFF REC!
   u row R-RAW-U REC! ;

: RAW-OFF ( n -- n )
   R-RAW-OFF REC@ ;

: RAW-U ( n -- n )
   R-RAW-U REC@ ;

;package
