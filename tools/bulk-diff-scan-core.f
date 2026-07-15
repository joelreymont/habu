\ bulk-diff-scan-core.f - checked jj materialized-directory content scan.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/json-write.f
require lib/fmt.f
require tools/json.f
require tools/diff-side-content.f

package BULK-DIFF
private

10 constant REC-CELLS
0 constant R-OLD-OFF
1 constant R-OLD-U
2 constant R-OLD-KIND-OFF
3 constant R-OLD-KIND-U
4 constant R-OLD-SIZE
5 constant R-NEW-OFF
6 constant R-NEW-U
7 constant R-NEW-KIND-OFF
8 constant R-NEW-KIND-U
9 constant R-NEW-SIZE

$10000 constant IO-CAP

ENUM scan-phase metadata-load metadata-parse stat read encode ;ENUM
ENUM side-role none old new ;ENUM

PTR-VARIABLE LEFT-A
variable LEFT-U
PTR-VARIABLE RIGHT-A
variable RIGHT-U
PTR-VARIABLE META-A
variable META-U
PTR-VARIABLE REC-A
variable REC-N
PTR-VARIABLE POOL-A
variable POOL-CAP
variable POOL-U
PTR-VARIABLE OUT-A
variable OUT-U
variable OUT-CAP

create FULL-PATH FS-PATH-CAP allot
variable FULL-PATH-U
create IO-BUF IO-CAP allot
PTR-VARIABLE LINK-A
variable LINK-CAP

variable ROW-I
variable LINE-START
variable PAYLOAD-U

1 LAYOUT-BUFFER FAIL-PHASE-V scan-phase
variable FAIL-ROW
1 LAYOUT-BUFFER FAIL-SIDE-V side-role
PTR-VARIABLE FAIL-PATH-A
variable FAIL-PATH-U
PTR-VARIABLE PATH-HEX-A
variable PATH-HEX-CAP
variable PATH-HEX-U

: LEFT$ ( -- ptr u8 n )
   LEFT-A @ LEFT-U @ ;

: RIGHT$ ( -- ptr u8 n )
   RIGHT-A @ RIGHT-U @ ;

: META$ ( -- ptr u8 n )
   META-A @ META-U @ ;

: REC-SLOT ( n n -- ptr a ) {: row:n field:n :}
   row 0 < row REC-N @ >= or if E-SIDE-SYNTAX throw then
   field 0 < field REC-CELLS >= or if E-SIDE-SYNTAX throw then
   REC-A @ row REC-CELLS * field + cells + ;

: REC@ ( n n -- n )
   REC-SLOT @ ;

: REC! ( n n n -- ) {: value:n row:n field:n :}
   value row field REC-SLOT ! ;

: KIND$ ( n n n -- ptr u8 n ) {: row:n off-field:n len-field:n :}
   POOL-A @ row off-field REC@ + row len-field REC@ ;

: FAIL-PHASE-AT ( -- ptr scan-phase )
   0 FAIL-PHASE-V ;

: FAIL-SIDE-AT ( -- ptr side-role )
   0 FAIL-SIDE-V ;

: FAIL! ( scan-phase n side-role ptr u8 n -- )
   {: phase:scan-phase row:n side:side-role path:ptr pathu:n :}
   phase FAIL-PHASE-AT ! row FAIL-ROW ! side FAIL-SIDE-AT !
   path FAIL-PATH-A ! pathu FAIL-PATH-U ! ;

: FAIL-META ( n -- )
   construct scan-phase metadata-parse swap construct side-role none META$ FAIL! ;

: LOAD-SIZED ( ptr u8 n n -- ptr u8 n ) {: path:ptr pathu:n u:n :}
   u 0 < if E-FS-IO throw then
   u 0= if 1 else u then MEM-ALLOC-BYTES drop {: a:ptr :}
   path pathu a u READ-ALL u <> if E-FS-IO throw then
   a u ;

: LOAD-EXACT ( ptr u8 n -- ptr u8 n ) {: path:ptr pathu:n :}
   path pathu path pathu FILE-SIZE LOAD-SIZED ;

: COUNT-ROWS ( -- n )
   0
   0 begin dup META-U @ < while
      META-A @ over + c@ $0A = if swap 1+ swap then
      1+
   repeat drop
   META-U @ 0 > if
      META-A @ META-U @ 1- + c@ $0A <> if drop E-SIDE-SYNTAX throw then
   then ;

: ALLOC-ROWS ( -- )
   COUNT-ROWS dup REC-N !
   dup 0= if drop 1 else REC-CELLS * then
   >COUNT MEM-ALLOC-CELLS REC-A !
   META-U @ 0= if 1 else META-U @ then dup POOL-CAP !
   MEM-ALLOC-BYTES drop POOL-A !
   0 POOL-U ! ;

: POOL-BYTES+ ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 0 <= if E-SIDE-SYNTAX throw then
   POOL-U @ u + POOL-U @ < if E-SIDE-CAPACITY throw then
   POOL-U @ u + POOL-CAP @ > if E-SIDE-CAPACITY throw then
   POOL-U @ {: off:n :}
   a POOL-A @ off + u BYTE-COPY
   off u + POOL-U !
   off ;

: POOL-PATH+ ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u DIFF-CONTENT:SAFE-PATH? 0= if E-SIDE-SYNTAX throw then
   a u POOL-BYTES+ ;

: TYPE-KIND ( ptr u8 n -- content-kind ) {: a:ptr u:n :}
   u 0= if DIFF-CONTENT:ABSENT-KIND exit then
   a u s" file" STR= if DIFF-CONTENT:FILE-KIND exit then
   a u s" symlink" STR= if DIFF-CONTENT:SYMLINK-KIND exit then
   a u s" git-submodule" STR= if DIFF-CONTENT:GITLINK-KIND exit then
   E-SIDE-SYNTAX throw ;

: ABSENT? ( content-kind -- bool )
   MATCH content-kind
      absent  OF true ENDOF
      file    OF false ENDOF
      symlink OF false ENDOF
      gitlink OF false ENDOF
   ;MATCH ;

: FILE-KIND? ( content-kind -- bool )
   MATCH content-kind
      absent  OF false ENDOF
      file    OF true ENDOF
      symlink OF false ENDOF
      gitlink OF false ENDOF
   ;MATCH ;

: SYMLINK-KIND? ( content-kind -- bool )
   MATCH content-kind
      absent  OF false ENDOF
      file    OF false ENDOF
      symlink OF true ENDOF
      gitlink OF false ENDOF
   ;MATCH ;

: GITLINK? ( content-kind -- bool )
   MATCH content-kind
      absent  OF false ENDOF
      file    OF false ENDOF
      symlink OF false ENDOF
      gitlink OF true ENDOF
   ;MATCH ;

: STATUS-CHECK ( ptr u8 n -- ) {: a:ptr u:n :}
   a u s" modified" STR= if exit then
   a u s" added" STR= if exit then
   a u s" removed" STR= if exit then
   a u s" renamed" STR= if exit then
   a u s" copied" STR= if exit then
   E-SIDE-SYNTAX throw ;

: NODE$ ( n n -- ptr u8 n )
   JSON-ARR@ JSON-STRING$ ;

: NODE-BOOL ( n n -- bool )
   JSON-ARR@ JSON-BOOL@ ;

: STORE-SIDE ( n content-kind ptr u8 n ptr u8 n n n n n -- )
   {: row:n kind:content-kind path:ptr pathu:n type:ptr typeu:n off-field:n len-field:n kind-off:n kind-len:n :}
   kind ABSENT? if
      pathu 0<> if E-SIDE-SYNTAX throw then
      0 row off-field REC! 0 row len-field REC!
   else
      path pathu POOL-PATH+ row off-field REC! pathu row len-field REC!
   then
   typeu 0= if
      0 row kind-off REC! 0 row kind-len REC!
   else
      type typeu POOL-BYTES+ row kind-off REC! typeu row kind-len REC!
   then ;

: OLD-KIND ( n -- content-kind ) {: row:n :}
   row R-OLD-KIND-OFF R-OLD-KIND-U KIND$ TYPE-KIND ;

: NEW-KIND ( n -- content-kind ) {: row:n :}
   row R-NEW-KIND-OFF R-NEW-KIND-U KIND$ TYPE-KIND ;

: PARSE-ROW ( ptr u8 n n -- ) {: a:ptr u:n row:n :}
   construct scan-phase metadata-parse row construct side-role none META$ FAIL!
   a u JSON-PARSE {: root:n :}
   root JSON-KIND J-ARR <> if E-SIDE-SYNTAX throw then
   root JSON-COUNT 9 <> if E-SIDE-SYNTAX throw then
   root 0 NODE$ STATUS-CHECK
   root 4 NODE-BOOL if E-SIDE-SYNTAX throw then
   root 8 NODE-BOOL if E-SIDE-SYNTAX throw then
   root 3 NODE-BOOL drop
   root 7 NODE-BOOL drop
   root 2 NODE$ {: old-type:ptr old-typeu:n :}
   root 6 NODE$ {: new-type:ptr new-typeu:n :}
   old-type old-typeu TYPE-KIND {: old-kind:content-kind :}
   new-type new-typeu TYPE-KIND {: new-kind:content-kind :}
   row old-kind root 1 NODE$ old-type old-typeu
   R-OLD-OFF R-OLD-U R-OLD-KIND-OFF R-OLD-KIND-U STORE-SIDE
   row new-kind root 5 NODE$ new-type new-typeu
   R-NEW-OFF R-NEW-U R-NEW-KIND-OFF R-NEW-KIND-U STORE-SIDE ;

: PARSE-ROWS ( -- )
   0 ROW-I ! 0 LINE-START !
   0 begin dup META-U @ < while
      META-A @ over + c@ $0A = if
         META-A @ LINE-START @ + over LINE-START @ - ROW-I @ PARSE-ROW
         ROW-I @ 1+ ROW-I !
         dup 1+ LINE-START !
      then
      1+
   repeat drop
   ROW-I @ REC-N @ <> if E-SIDE-SYNTAX throw then ;

: ROOT-CHECK ( ptr u8 n -- ) {: root:ptr rootu:n :}
   rootu 0 <= if E-FS-PATH throw then
   root rootu FS-TRY-LSTAT 0= if E-FS-STAT throw then
   FS-STAT-MODE@ S-IFMT and S-IFDIR <> if E-FS-STAT throw then ;

: PARENT-CHECK ( ptr u8 n ptr u8 n -- )
   {: root:ptr rootu:n rel:ptr relu:n :}
   0 begin dup relu < while
      rel over + c@ $2F = if
         root rootu rel over FULL-PATH JOIN-PATH {: u:n :}
         FULL-PATH u FS-TRY-LSTAT 0= if drop E-FS-STAT throw then
         FS-STAT-MODE@ S-IFMT and S-IFDIR <> if drop E-FS-STAT throw then
      then
      1+
   repeat drop ;

: FULL-PATH! ( ptr u8 n ptr u8 n -- )
   {: root:ptr rootu:n rel:ptr relu:n :}
   root rootu rel relu PARENT-CHECK
   root rootu rel relu FULL-PATH JOIN-PATH FULL-PATH-U ! ;

: MODE-SIZE ( content-kind -- n ) {: kind:content-kind :}
   FULL-PATH FULL-PATH-U @ FS-TRY-LSTAT 0= if E-FS-STAT throw then
   FS-STAT-MODE@ S-IFMT and {: mode:n :}
   kind FILE-KIND? if
      mode S-IFREG <> if E-FS-STAT throw then
   else
      kind SYMLINK-KIND? 0= mode S-IFLNK <> or if E-FS-STAT throw then
   then
   FS-STAT-SIZE@ ;

: FIELD>SIDE ( n -- side-role )
   R-OLD-KIND-OFF = if construct side-role old else construct side-role new then ;

: SIZE-SIDE ( ptr u8 n n n n n n -- )
   {: root:ptr rootu:n row:n off-field:n len-field:n kind-field:n size-field:n :}
   kind-field R-OLD-KIND-OFF = if row OLD-KIND else row NEW-KIND then
   {: kind:content-kind :}
   kind ABSENT? kind GITLINK? or if
      0 row size-field REC! exit
   then
   POOL-A @ row off-field REC@ + row len-field REC@ {: path:ptr pathu:n :}
   construct scan-phase stat row kind-field FIELD>SIDE path pathu FAIL!
   root rootu path pathu FULL-PATH!
   kind MODE-SIZE row size-field REC! ;

: SIZE-ROW ( n -- ) {: row:n :}
   LEFT$ row R-OLD-OFF R-OLD-U R-OLD-KIND-OFF R-OLD-SIZE SIZE-SIDE
   RIGHT$ row R-NEW-OFF R-NEW-U R-NEW-KIND-OFF R-NEW-SIZE SIZE-SIDE ;

: SIZE-ROWS ( -- )
   LEFT$ ROOT-CHECK RIGHT$ ROOT-CHECK
   0 begin dup REC-N @ < while
      dup SIZE-ROW
      1+
   repeat drop ;

: PAYLOAD+ ( n -- ) {: add:n :}
   PAYLOAD-U @ add + PAYLOAD-U @ < if E-SIDE-CAPACITY throw then
   PAYLOAD-U @ add + PAYLOAD-U ! ;

: SIZE-ARTIFACT ( -- n )
   0 PAYLOAD-U !
   0 begin dup REC-N @ < while
      dup {: row:n :}
      row R-OLD-U REC@ DIFF-CONTENT:SIDE-SIZE
      row R-NEW-U REC@ DIFF-CONTENT:SIDE-SIZE
      DIFF-CONTENT:ROW-SIZE PAYLOAD+
      1+
   repeat drop
   PAYLOAD-U @ DIFF-CONTENT:ARTIFACT-SIZE ;

: STREAM-CHUNK ( ptr u8 n -- )
   DIFF-CONTENT:SIDE-CHUNK ;

: STREAM-FILE ( -- )
   FULL-PATH FULL-PATH-U @ IO-BUF IO-CAP
   [: STREAM-CHUNK ;] FS:STREAM-REGULAR ;

: ENSURE-LINK ( n -- ) {: need:n :}
   need 0= if 1 else need then {: want:n :}
   want LINK-CAP @ <= if exit then
   want MEM-ALLOC-BYTES drop LINK-A !
   want LINK-CAP ! ;

: STREAM-LINK ( n -- ) {: size:n :}
   size $7FFFFFFFFFFFFFFF = if E-SIDE-CAPACITY throw then
   size 1+ {: cap:n :}
   cap ENSURE-LINK
   FULL-PATH FULL-PATH-U @ LINK-A @ cap READ-LINK {: got:n :}
   got size <> if E-FS-IO throw then
   LINK-A @ got DIFF-CONTENT:SIDE-CHUNK ;

: EMIT-SIDE ( ptr u8 n n n n n n -- )
   {: root:ptr rootu:n row:n off-field:n len-field:n kind-field:n size-field:n :}
   kind-field R-OLD-KIND-OFF = if row OLD-KIND else row NEW-KIND then
   {: kind:content-kind :}
   row size-field REC@ {: size:n :}
   kind ABSENT? if
      false kind s" " 0 DIFF-CONTENT:SIDE-BEGIN
      DIFF-CONTENT:SIDE-END exit
   then
   POOL-A @ row off-field REC@ + row len-field REC@ {: path:ptr pathu:n :}
   construct scan-phase read row kind-field FIELD>SIDE path pathu FAIL!
   true kind path pathu size DIFF-CONTENT:SIDE-BEGIN
   kind GITLINK? if DIFF-CONTENT:SIDE-END exit then
   root rootu path pathu FULL-PATH!
   kind FILE-KIND? if STREAM-FILE else size STREAM-LINK then
   DIFF-CONTENT:SIDE-END ;

: EMIT-ROW ( n -- ) {: row:n :}
   row R-OLD-U REC@ DIFF-CONTENT:SIDE-SIZE {: oldu:n :}
   row R-NEW-U REC@ DIFF-CONTENT:SIDE-SIZE {: newu:n :}
   row oldu newu DIFF-CONTENT:ROW-BEGIN
   LEFT$ row R-OLD-OFF R-OLD-U R-OLD-KIND-OFF R-OLD-SIZE EMIT-SIDE
   RIGHT$ row R-NEW-OFF R-NEW-U R-NEW-KIND-OFF R-NEW-SIZE EMIT-SIDE
   DIFF-CONTENT:ROW-END ;

: BUILD-ARTIFACT ( -- )
   construct scan-phase encode -1 construct side-role none META$ FAIL!
   SIZE-ARTIFACT dup OUT-CAP ! MEM-ALLOC-BYTES drop OUT-A !
   OUT-A @ OUT-CAP @ REC-N @ META$ DIFF-CONTENT:START
   0 begin dup REC-N @ < while
      dup EMIT-ROW
      1+
   repeat drop
   DIFF-CONTENT:FINISH OUT-U ! OUT-A ! ;

: SCAN ( -- )
   0 FAIL-META
   ALLOC-ROWS
   PARSE-ROWS
   SIZE-ROWS
   BUILD-ARTIFACT ;

: HEX-NIB ( n -- n )
   dup 10 < if $30 + else 10 - $61 + then ;

: PATH-HEX-ENSURE ( n -- ) {: need:n :}
   need PATH-HEX-CAP @ <= if exit then
   need MEM-ALLOC-BYTES drop PATH-HEX-A !
   need PATH-HEX-CAP ! ;

: PATH-HEX! ( -- )
   FAIL-PATH-U @ $3FFFFFFFFFFFFFFF > if E-SIDE-CAPACITY throw then
   FAIL-PATH-U @ 2 * dup PATH-HEX-ENSURE PATH-HEX-U !
   0 begin dup FAIL-PATH-U @ < while
      FAIL-PATH-A @ over + c@ {: c:n :}
      dup 2 * PATH-HEX-A @ + c 4 rshift $F and HEX-NIB swap c!
      dup 2 * 1+ PATH-HEX-A @ + c $F and HEX-NIB swap c!
      1+
   repeat drop ;

: PHASE$ ( -- ptr u8 n )
   FAIL-PHASE-AT @ MATCH scan-phase
      metadata-load  OF s" metadata-load" ENDOF
      metadata-parse OF s" metadata-parse" ENDOF
      stat           OF s" stat" ENDOF
      read           OF s" read" ENDOF
      encode         OF s" encode" ENDOF
   ;MATCH ;

: SIDE$ ( -- ptr u8 n )
   FAIL-SIDE-AT @ MATCH side-role
      none OF s" none" ENDOF
      old  OF s" old" ENDOF
      new  OF s" new" ENDOF
   ;MATCH ;

: NUM$ ( n -- ptr u8 n )
   SB-RESET SB-INT SB$ ;

public

: RUN ( ptr u8 n ptr u8 n ptr u8 n -- ptr u8 n )
   {: left:ptr leftu:n right:ptr rightu:n meta-path:ptr meta-pathu:n :}
   left LEFT-A ! leftu LEFT-U ! right RIGHT-A ! rightu RIGHT-U !
   construct scan-phase metadata-load -1 construct side-role none
   meta-path meta-pathu FAIL!
   meta-path meta-pathu LOAD-EXACT META-U ! META-A !
   SCAN
   OUT-A @ OUT-U @ ;

: REPORT ( n -- ptr u8 n ) {: code:n :}
   PATH-HEX!
   JW-RESET
   JW-OBJECT-START
   s" phase" PHASE$ JW-FIELD-S JW-COMMA
   s" row" FAIL-ROW @ NUM$ JW-FIELD-RAW JW-COMMA
   s" side" SIDE$ JW-FIELD-S JW-COMMA
   s" path_hex" PATH-HEX-A @ PATH-HEX-U @ JW-FIELD-S JW-COMMA
   s" code" code NUM$ JW-FIELD-RAW
   JW-OBJECT-END
   JW$ ;

;package
