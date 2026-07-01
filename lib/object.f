\ object.f - checked object-record codec for future linkable Habu builds.
\
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f, and
\ lib/content-key.f.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/content-key.f

package OBJ

$40000 constant CAP
$1000 constant FIELD-CAP
64 constant HASH-U
8 constant ROW-BASE
9 constant TAB
10 constant LF
13 constant CR
32 constant SPACE
65 constant UP-A
71 constant UP-G
97 constant LOW-A
103 constant LOW-G

create BUF CAP allot
create DG 40 allot

variable OUT-LEN
variable SOURCE?
variable TARGET?
variable CHECKER?
variable COMPILER?
variable SEEN
variable FIND-SEEN
variable LOAD-OFF

: TRUE ( -- bool )
   0 0= ;

: FALSE ( -- bool )
   TRUE 0= ;

: CLEAR ( -- )
   0 OUT-LEN !
   0 SOURCE? !
   0 TARGET? !
   0 CHECKER? !
   0 COMPILER? ! ;

: ROOM ( n -- ) {: n:n :}
   n 0 < if E-OBJ-CAPACITY throw then
   OUT-LEN @ n + CAP > if E-OBJ-CAPACITY throw then ;

: C+ ( n -- ) {: c:n :}
   1 ROOM
   c 0 < if E-OBJ-FIELD throw then
   c STR-BYTE-MAX > if E-OBJ-FIELD throw then
   c BUF OUT-LEN @ + c!
   OUT-LEN @ 1+ OUT-LEN ! ;

: BYTES+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u ROOM
   a BUF OUT-LEN @ + u BYTE-COPY
   OUT-LEN @ u + OUT-LEN ! ;

: LF+ ( -- )
   LF C+ ;

: TAB+ ( -- )
   TAB C+ ;

: MAGIC+ ( -- )
   s" HBOBJ" BYTES+
   TAB+
   s" 1" BYTES+
   LF+ ;

: FLAG! ( ptr a -- ) {: flag:ptr :}
   flag @ 0 <> if E-OBJ-SCHEMA throw then
   -1 flag ! ;

: READY ( -- )
   SOURCE? @ 0= if E-OBJ-SCHEMA throw then
   TARGET? @ 0= if E-OBJ-SCHEMA throw then
   CHECKER? @ 0= if E-OBJ-SCHEMA throw then
   COMPILER? @ 0= if E-OBJ-SCHEMA throw then ;

: FIELD-BYTE ( n -- ) {: c:n :}
   c TAB = if E-OBJ-FIELD throw then
   c LF = if E-OBJ-FIELD throw then
   c CR = if E-OBJ-FIELD throw then
   c SPACE < if E-OBJ-FIELD throw then
   c STR-BYTE-MAX > if E-OBJ-FIELD throw then ;

: FIELD ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= if E-OBJ-FIELD throw then
   u FIELD-CAP > if E-OBJ-FIELD throw then
   0 begin dup u < while
      dup a + c@ FIELD-BYTE
      1+
   repeat drop
   a u BYTES+ ;

: HEX? ( n -- bool ) {: c:n :}
   c STR-ZERO >= c STR-ZERO 10 + < and if TRUE exit then
   c UP-A >= c UP-G < and if TRUE exit then
   c LOW-A >= c LOW-G < and ;

: HASH-FIELD ( ptr u8 n -- ) {: a:ptr u:n :}
   u HASH-U <> if E-OBJ-FIELD throw then
   0 begin dup u < while
      dup a + c@ HEX? 0= if E-OBJ-FIELD throw then
      1+
   repeat drop
   a u FIELD ;

: HEX-FIELD ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= if E-OBJ-FIELD throw then
   u 1 and 0 <> if E-OBJ-FIELD throw then
   0 begin dup u < while
      dup a + c@ HEX? 0= if E-OBJ-FIELD throw then
      1+
   repeat drop ;

: TAG+ ( ptr u8 n -- )
   BYTES+
   TAB+ ;

: LINE1 ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n tag:ptr tagu:n :}
   tag tagu TAG+
   a u FIELD
   LF+ ;

: LINE2 ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: a:ptr u:n b:ptr v:n tag:ptr tagu:n :}
   tag tagu TAG+
   a u FIELD
   TAB+
   b v FIELD
   LF+ ;

: U+ ( n -- ) {: n:n :}
   n 0 < if E-OBJ-FIELD throw then
   n 10 >= if n 10 / RECURSE then
   n 10 mod STR-ZERO + C+ ;

: HEX-NIB+ ( n -- ) {: n:n :}
   n 10 < if n STR-ZERO + C+ exit then
   n 10 - LOW-A + C+ ;

: HEX-BYTE+ ( n -- ) {: b:n :}
   b 0 < if E-OBJ-FIELD throw then
   b STR-BYTE-MAX > if E-OBJ-FIELD throw then
   b 4 rshift HEX-NIB+
   b 15 and HEX-NIB+ ;

: HEX-BYTES+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= if E-OBJ-FIELD throw then
   u CAP 2 / > if E-OBJ-CAPACITY throw then
   u 2 * ROOM
   0 begin dup u < while
      dup a + c@ HEX-BYTE+
      1+
   repeat drop ;

: LINE3N ( ptr u8 n n ptr u8 n ptr u8 n -- )
   {: a:ptr u:n off:n b:ptr v:n tag:ptr tagu:n :}
   tag tagu TAG+
   a u FIELD
   TAB+
   off U+
   TAB+
   b v FIELD
   LF+ ;

: SECTION+ ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n tag:ptr tagu:n :}
   tag tagu TAG+
   a u HEX-BYTES+
   LF+ ;

: LINE-VALID ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= if E-OBJ-SCHEMA throw then
   a c@ TAB = if E-OBJ-SCHEMA throw then
   a u 1 - + c@ TAB = if E-OBJ-SCHEMA throw then
   0 SEEN !
   0 begin dup u < while
      dup a + c@ {: c:n :}
      c CR = if E-OBJ-SCHEMA throw then
      c LF = if E-OBJ-SCHEMA throw then
      c SPACE < c TAB <> and if E-OBJ-SCHEMA throw then
      c TAB = if
         SEEN @ 0 <> if E-OBJ-SCHEMA throw then
         -1 SEEN !
      else
         0 SEEN !
      then
      1+
   repeat drop ;

: TAB-COUNT ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 0 begin dup u < while
      dup a + c@ TAB = if swap 1+ swap then
      1+
   repeat drop ;

: TAG? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n tag:ptr tagu:n :}
   u tagu <= if FALSE exit then
   a tagu tag tagu STR= 0= if FALSE exit then
   a tagu + c@ TAB = ;

: EXPECT-TABS ( ptr u8 n n -- ) {: a:ptr u:n want:n :}
   a u TAB-COUNT want <> if E-OBJ-SCHEMA throw then ;

: TAB-AT ( ptr u8 n n -- n ) {: a:ptr u:n nth:n :}
   0 FIND-SEEN !
   0 begin dup u < while
      dup a + c@ TAB = if
         FIND-SEEN @ nth = if exit then
         FIND-SEEN @ 1+ FIND-SEEN !
      then
      1+
   repeat drop -1 ;

: FIELD$ ( ptr u8 n n -- ptr u8 n ) {: a:ptr u:n idx:n :}
   idx 0= if
      0
   else
      a u idx 1 - TAB-AT dup 0 < if E-OBJ-SCHEMA throw then 1+
   then {: st:n :}
   a u idx TAB-AT dup 0 < if drop u then {: ed:n :}
   ed st <= if E-OBJ-SCHEMA throw then
   a st + ed st - ;

: PARSE-RELOC-OFF ( ptr u8 n -- ) {: a:ptr u:n :}
   a u 2 FIELD$ STR>NUMBER? if
      0 < if E-OBJ-FIELD throw then
   else
      drop E-OBJ-FIELD throw
   then ;

: PARSE-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u LINE-VALID
   a u s" source" TAG? if
      a u 1 EXPECT-TABS
      SOURCE? FLAG!
      a u 1 FIELD$ HASH-FIELD
      exit
   then
   a u s" target" TAG? if a u 1 EXPECT-TABS TARGET? FLAG! exit then
   a u s" checker" TAG? if a u 1 EXPECT-TABS CHECKER? FLAG! exit then
   a u s" compiler" TAG? if a u 1 EXPECT-TABS COMPILER? FLAG! exit then
   a u s" require" TAG? if a u 1 EXPECT-TABS exit then
   a u s" text" TAG? if a u 1 EXPECT-TABS a u 1 FIELD$ HEX-FIELD exit then
   a u s" data" TAG? if a u 1 EXPECT-TABS a u 1 FIELD$ HEX-FIELD exit then
   a u s" noret" TAG? if a u 1 EXPECT-TABS exit then
   a u s" package" TAG? if a u 2 EXPECT-TABS exit then
   a u s" export" TAG? if a u 2 EXPECT-TABS exit then
   a u s" import" TAG? if a u 2 EXPECT-TABS exit then
   a u s" type" TAG? if a u 2 EXPECT-TABS exit then
   a u s" reloc" TAG? if
      a u 3 EXPECT-TABS
      a u PARSE-RELOC-OFF
      exit
   then
   E-OBJ-SCHEMA throw ;

: NEXT-LINE ( ptr u8 n -- ptr u8 n bool ) {: a:ptr u:n :}
   a u LF LOAD-OFF @ SPLIT-NEXT >r LOAD-OFF ! r> ;

: MAGIC-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u s" HBOBJ	1" STR= 0= if E-OBJ-SCHEMA throw then ;

: END-LF ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= if E-OBJ-SCHEMA throw then
   a u 1 - + c@ LF <> if E-OBJ-SCHEMA throw then ;

: LF-OFF ( n -- n ) {: off:n :}
   off begin dup OUT-LEN @ < while
      dup BUF + c@ LF = if exit then
      1+
   repeat drop -1 ;

: ROW-START ( n -- n ) {: idx:n :}
   READY
   idx 0 < if E-OBJ-FIELD throw then
   ROW-BASE 0 begin over OUT-LEN @ < while
      dup idx = if drop exit then
      swap LF-OFF dup 0 < if E-OBJ-SCHEMA throw then
      1+ swap 1+
   repeat
   2drop E-OBJ-FIELD throw ;

: ROW-END ( n -- n ) {: start:n :}
   start LF-OFF dup 0 < if E-OBJ-SCHEMA throw then ;

: ROW-SLICE ( n -- ptr u8 n ) {: idx:n :}
   idx ROW-START {: start:n :}
   start ROW-END {: finish:n :}
   BUF start + finish start - ;

public

: RESET ( -- )
   CLEAR
   MAGIC+ ;

: SOURCE! ( ptr u8 n -- ) {: a:ptr u:n :}
   SOURCE? FLAG!
   s" source" TAG+
   a u HASH-FIELD
   LF+ ;

: TARGET! ( ptr u8 n -- )
   TARGET? FLAG!
   s" target" LINE1 ;

: CHECKER! ( ptr u8 n -- )
   CHECKER? FLAG!
   s" checker" LINE1 ;

: COMPILER! ( ptr u8 n -- )
   COMPILER? FLAG!
   s" compiler" LINE1 ;

: REQUIRE+ ( ptr u8 n -- )
   s" require" LINE1 ;

: TEXT+ ( ptr u8 n -- )
   s" text" SECTION+ ;

: DATA+ ( ptr u8 n -- )
   s" data" SECTION+ ;

: PACKAGE+ ( ptr u8 n ptr u8 n -- )
   s" package" LINE2 ;

: EXPORT+ ( ptr u8 n ptr u8 n -- )
   s" export" LINE2 ;

: IMPORT+ ( ptr u8 n ptr u8 n -- )
   s" import" LINE2 ;

: TYPE+ ( ptr u8 n ptr u8 n -- )
   s" type" LINE2 ;

: RELOC+ ( ptr u8 n n ptr u8 n -- )
   s" reloc" LINE3N ;

: NORET+ ( ptr u8 n -- )
   s" noret" LINE1 ;

: BYTES$ ( -- ptr u8 n )
   READY
   BUF OUT-LEN @ ;

: ROW-COUNT ( -- n )
   READY
   0 0 begin dup OUT-LEN @ < while
      dup ROW-BASE >= if
         dup BUF + c@ LF = if swap 1+ swap then
      then
      1+
   repeat drop ;

: ROW$ ( n -- ptr u8 n )
   ROW-SLICE ;

: ROW-TAG$ ( n -- ptr u8 n )
   ROW-SLICE 0 FIELD$ ;

: ROW-FIELD# ( n -- n )
   ROW-SLICE TAB-COUNT ;

: ROW-FIELD$ ( n n -- ptr u8 n ) {: row:n field:n :}
   field 0 < if E-OBJ-FIELD throw then
   row ROW-FIELD# {: total:n :}
   field total >= if E-OBJ-FIELD throw then
   row ROW-SLICE field 1 + FIELD$ ;

: LOAD ( ptr u8 n -- ) {: a:ptr u:n :}
   u CAP > if E-OBJ-CAPACITY throw then
   a u END-LF
   CLEAR
   0 LOAD-OFF !
   a u NEXT-LINE 0= if E-OBJ-SCHEMA throw then
   MAGIC-LINE
   begin LOAD-OFF @ u < while
      a u NEXT-LINE 0= if E-OBJ-SCHEMA throw then
      PARSE-LINE
   repeat
   READY
   a BUF u BYTE-COPY
   u OUT-LEN ! ;

: KEY-HEX ( ptr u8 -- ) {: dst:ptr :}
   BYTES$ DG SHA256
   CK-RESET
   s" habu-object-record-key-v1" CK-TEXT+
   DG CK-DIGEST+
   dst CK-FINAL-HEX ;

end-package

OBJ:RESET
