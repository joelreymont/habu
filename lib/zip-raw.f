\ zip-raw.f - byte-preserving ZIP framing; libzip only decompresses members.
require lib/zip-ffi.f
require lib/fs.f
require lib/fs-mutate.f

package ZIP
using FFI

$04034B50 constant LOCAL-SIG
$02014B50 constant CENTRAL-SIG
$06054B50 constant END-SIG
$06064B50 constant END64-SIG
$07064B50 constant LOCATOR-SIG
$08074B50 constant DESCRIPTOR-SIG
$FFFFFFFF constant U32-MAX
$FFFF constant U16-MAX
$1E constant LOCAL-BYTES
$2E constant CENTRAL-BYTES
$16 constant END-BYTES
$38 constant END64-BYTES
$14 constant LOCATOR-BYTES
16 constant MEMBER-CELLS

: PATH-PTR ( ptr n -- ptr ptr u8 ) 6 ptr-field ;
: PATH-LEN ( ptr n -- ptr n ) 7 cells + ;
: RAW-PTR ( ptr n -- ptr ptr u8 ) 8 ptr-field ;
: RAW-LEN ( ptr n -- ptr n ) 9 cells + ;
: MEMBERS ( ptr n -- ptr ptr n ) 10 ptr-field ;
: MEMBER-COUNT ( ptr n -- ptr n ) 11 cells + ;
: END-OFF ( ptr n -- ptr n ) 12 cells + ;
: DIRECTORY-OFF ( ptr n -- ptr n ) 13 cells + ;
: DIRECTORY-SIZE ( ptr n -- ptr n ) 14 cells + ;
: ZIP-BASE ( ptr n -- ptr n ) 15 cells + ;
: PREFIX-SIZE ( ptr n -- ptr n ) 16 cells + ;
: DIRECTORY-TAIL ( ptr n -- ptr n ) 17 cells + ;
: OUT-PTR ( ptr n -- ptr ptr u8 ) 18 ptr-field ;
: OUT-LEN ( ptr n -- ptr n ) 19 cells + ;
: OUT-CAP ( ptr n -- ptr n ) 20 cells + ;
: OUT-DIRECTORY ( ptr n -- ptr n ) 21 cells + ;
: TEMP-PTR ( ptr n -- ptr ptr u8 ) 22 ptr-field ;
: TEMP-LEN ( ptr n -- ptr n ) 23 cells + ;
: TEMP-FD ( ptr n -- ptr n ) 24 cells + ;
: TEMP-LIVE ( ptr n -- ptr n ) 25 cells + ;
: END64-OFF ( ptr n -- ptr n ) 26 cells + ;
: DIRTY ( ptr n -- ptr n ) 27 cells + ;

: CENTRAL-OFF ( ptr n -- ptr n ) 0 cells + ;
: CENTRAL-LEN ( ptr n -- ptr n ) 1 cells + ;
: LOCAL-OFF ( ptr n -- ptr n ) 2 cells + ;
: LOCAL-LEN ( ptr n -- ptr n ) 3 cells + ;
: COMPRESSED-SIZE ( ptr n -- ptr n ) 4 cells + ;
: PLAIN-SIZE ( ptr n -- ptr n ) 5 cells + ;
: ZIP64-OFFSET ( ptr n -- ptr n ) 6 cells + ;
: REPLACEMENT ( ptr n -- ptr ptr u8 ) 7 ptr-field ;
: REPLACE-LEN ( ptr n -- ptr n ) 8 cells + ;
: REPLACE-CRC ( ptr n -- ptr n ) 9 cells + ;
: NEW-LOCAL ( ptr n -- ptr n ) 10 cells + ;

: SAFE+ ( n n -- n ) + dup SIZE-CHECK ;
: SAFE* ( n n -- n ) {: left:n right:n :}
   left SIZE-CHECK right SIZE-CHECK
   right 0 <> if left MAX-SIZE right / > if E-SIZE throw then then
   left right * ;

: LE@ ( ptr u8 n -- n ) {: data:ptr width:n :}
   0 width 0 ?do data i + c@ i 8 * lshift or loop ;
: LE! ( n ptr u8 n -- ) {: value:n data:ptr width:n :}
   width 0 ?do value i 8 * rshift data i + c! loop ;
: U16@ ( ptr u8 -- n ) 2 LE@ ;
: U32@ ( ptr u8 -- n ) 4 LE@ ;
: U64@ ( ptr u8 -- n ) 8 LE@ dup SIZE-CHECK ;
: U16! ( n ptr u8 -- ) 2 LE! ;
: U32! ( n ptr u8 -- ) 4 LE! ;
: U64! ( n ptr u8 -- ) 8 LE! ;

: RAW-SPAN ( ptr n n n -- ptr u8 ) {: node:ptr off:n len:n :}
   off SIZE-CHECK len SIZE-CHECK
   off node RAW-LEN @ > if E-READ throw then
   len node RAW-LEN @ off - > if E-READ throw then
   node RAW-PTR @ off + ;
: RAW-U16 ( ptr n n -- n ) 2 RAW-SPAN U16@ ;
: RAW-U32 ( ptr n n -- n ) 4 RAW-SPAN U32@ ;
: RAW-U64 ( ptr n n -- n ) 8 RAW-SPAN U64@ ;
: SIGNATURE! ( ptr n n n -- ) {: node:ptr off:n sig:n :}
   node off RAW-U32 sig <> if E-READ throw then ;

: END-MATCH? ( ptr n n -- bool ) {: node:ptr off:n :}
   node off RAW-U32 END-SIG <> if false exit then
   node off $14 + RAW-U16 off + END-BYTES + node RAW-LEN @ = ;
: FIND-END ( ptr n -- n ) {: node:ptr :}
   node RAW-LEN @ END-BYTES - dup 0 < if E-READ throw then
   begin dup node RAW-LEN @ END-BYTES - U16-MAX - 0 max >= while
      node over END-MATCH? if exit then 1-
   repeat E-READ throw ;

: HAS-END64? ( ptr n -- bool ) {: node:ptr :}
   node END-OFF @ LOCATOR-BYTES < if false exit then
   node node END-OFF @ LOCATOR-BYTES - RAW-U32 LOCATOR-SIG = ;
: END64-MATCH? ( ptr n n -- bool ) {: node:ptr off:n :}
   node off RAW-U32 END64-SIG <> if false exit then
   node off 4 + 8 RAW-SPAN 8 LE@
   node END-OFF @ LOCATOR-BYTES - off - $C - = ;

: LOCATOR-OFFSET ( ptr n -- n )
   dup END-OFF @ LOCATOR-BYTES - 8 + RAW-U64 ;

: DIRECT-END64? ( ptr n n -- bool ) {: node:ptr off:n :}
   off node END-OFF @ LOCATOR-BYTES - END64-BYTES - > if false exit then
   node off END64-MATCH? ;

: PREFIX-END64? ( ptr n n -- bool ) {: node:ptr off:n :}
   node off END64-MATCH? 0= if false exit then
   node LOCATOR-OFFSET {: located:n :}
   off located < if false exit then
   node off $28 + 8 RAW-SPAN 8 LE@ {: size:n :}
   size 0 < size located > or if false exit then
   node off $30 + 8 RAW-SPAN 8 LE@ located size - <> if false exit then
   node off $20 + 8 RAW-SPAN 8 LE@ node OBJECT @ C-COUNT <> if false exit then
   size 0= if true exit then
   size CENTRAL-BYTES < if false exit then
   node off size - RAW-U32 CENTRAL-SIG = ;

: FIND-END64 ( ptr n -- n ) {: node:ptr :}
   node LOCATOR-OFFSET dup node swap DIRECT-END64? if exit then drop
   node END-OFF @ LOCATOR-BYTES - END64-BYTES -
   begin dup 0 >= while
      node over PREFIX-END64? if exit then 1-
   repeat E-READ throw ;

: END-CLASSIC ( ptr n -- ) {: node:ptr :}
   node END-OFF @ {: off:n :}
   node off 4 + RAW-U32 0 <> if E-READ throw then
   node off 8 + RAW-U16 node off $A + RAW-U16 <> if E-READ throw then
   node off $A + RAW-U16 node MEMBER-COUNT !
   node off $C + RAW-U32 node DIRECTORY-SIZE !
   off node DIRECTORY-SIZE @ - node DIRECTORY-OFF !
   node DIRECTORY-OFF @ node off $10 + RAW-U32 - node ZIP-BASE ! ;

: END-WIDE ( ptr n -- ) {: node:ptr :}
   node FIND-END64 {: off:n :}
   off node END64-OFF !
   node off $10 + RAW-U64 0 <> if E-READ throw then
   node off $18 + RAW-U64 node off $20 + RAW-U64 <> if E-READ throw then
   node off $20 + RAW-U64 node MEMBER-COUNT !
   node off $28 + RAW-U64 node DIRECTORY-SIZE !
   off node DIRECTORY-SIZE @ - node DIRECTORY-OFF !
   node DIRECTORY-OFF @ node off $30 + RAW-U64 - node ZIP-BASE ! ;

: MEMBER-AT ( ptr n n -- ptr n ) {: node:ptr idx:n :}
   node MEMBERS @ idx MEMBER-CELLS * cells + ;
: CENTRAL-PTR ( ptr n ptr n -- ptr u8 )
   CENTRAL-OFF @ CENTRAL-BYTES RAW-SPAN ;
: CENTRAL-NAME ( ptr n ptr n -- ptr u8 n )
   CENTRAL-PTR dup CENTRAL-BYTES + swap $1C + U16@ ;
: EXTRA-SPAN ( ptr n ptr n -- ptr u8 n )
   CENTRAL-PTR {: data:ptr :}
   data CENTRAL-BYTES + data $1C + U16@ + data $1E + U16@ ;

: EXTRA-CHECK ( ptr u8 n -- ) {: data:ptr len:n :}
   len 4 < if E-READ throw then
   data 2 + U16@ len 4 - > if E-READ throw then ;
: EXTRA-FIND ( ptr u8 n n -- ptr u8 n ) {: data:ptr len:n tag:n :}
   data len begin dup 0 > while
      2dup EXTRA-CHECK over U16@ tag = if exit then
      over 2 + U16@ 4 + {: step:n :}
      swap step + swap step -
   repeat ;

: WIDE-FIELD ( ptr u8 n -- ptr u8 n n ) {: data:ptr len:n :}
   len 8 < if E-READ throw then
   data 8 + len 8 - data U64@ ;
: PARSE-WIDE ( ptr n ptr n -- ) {: node:ptr member:ptr :}
   node member CENTRAL-PTR {: central:ptr :}
   node member EXTRA-SPAN 1 EXTRA-FIND {: extra:ptr len:n :}
   len 0= if exit then
   extra 4 + extra 2 + U16@
   central $18 + U32@ U32-MAX = if WIDE-FIELD member PLAIN-SIZE ! then
   central $14 + U32@ U32-MAX = if WIDE-FIELD member COMPRESSED-SIZE ! then
   central $2A + U32@ U32-MAX = if
      over node RAW-PTR @ - member ZIP64-OFFSET !
      WIDE-FIELD member LOCAL-OFF !
   then
   central $22 + U16@ U16-MAX = if
      dup 4 < if E-READ throw then over U32@ 0 <> if E-READ throw then
   then 2drop ;

: MEMBER-CENTRAL ( ptr n ptr n n -- n ) {: node:ptr member:ptr off:n :}
   node off CENTRAL-SIG SIGNATURE! off member CENTRAL-OFF !
   node member CENTRAL-PTR {: data:ptr :}
   CENTRAL-BYTES data $1C + U16@ + data $1E + U16@ + data $20 + U16@ +
   dup member CENTRAL-LEN ! off + {: next:n :}
   node off member CENTRAL-LEN @ RAW-SPAN drop
   data $14 + U32@ member COMPRESSED-SIZE !
   data $18 + U32@ member PLAIN-SIZE !
   data $2A + U32@ member LOCAL-OFF !
   data $22 + U16@ dup 0 <> swap U16-MAX <> and if E-READ throw then
   node member PARSE-WIDE next ;

: WIDE-DESCRIPTOR? ( ptr n ptr n -- bool ) {: node:ptr member:ptr :}
   node member LOCAL-OFF @ LOCAL-BYTES RAW-SPAN {: local:ptr :}
   local $12 + U32@ U32-MAX = local $16 + U32@ U32-MAX = or
   member COMPRESSED-SIZE @ U32-MAX >= or member PLAIN-SIZE @ U32-MAX >= or ;

: DESCRIPTOR-MATCH? ( ptr n ptr n n bool -- bool ) {: node:ptr member:ptr off:n wide:bool :}
   node off RAW-U32 node member CENTRAL-PTR $10 + U32@ <> if false exit then
   wide if
      node off 4 + RAW-U64 member COMPRESSED-SIZE @ =
      node off $C + RAW-U64 member PLAIN-SIZE @ = and
   else
      node off 4 + RAW-U32 member COMPRESSED-SIZE @ =
      node off 8 + RAW-U32 member PLAIN-SIZE @ = and
   then ;

: DESCRIPTOR-LEN ( ptr n ptr n n -- n ) {: node:ptr member:ptr off:n :}
   node member WIDE-DESCRIPTOR? {: wide:bool :}
   wide if $14 else $C then {: len:n :}
   node off RAW-U32 DESCRIPTOR-SIG = if
      node member off 4 + wide DESCRIPTOR-MATCH? if len 4 + exit then
   then
   node member off wide DESCRIPTOR-MATCH? 0= if E-READ throw then len ;

: MEMBER-LOCAL ( ptr n ptr n -- ) {: node:ptr member:ptr :}
   member LOCAL-OFF @ node ZIP-BASE @ SAFE+ {: off:n :}
   off member LOCAL-OFF ! node off LOCAL-SIG SIGNATURE!
   node off LOCAL-BYTES RAW-SPAN {: data:ptr :}
   LOCAL-BYTES data $1A + U16@ + data $1C + U16@ +
   member COMPRESSED-SIZE @ SAFE+ {: len:n :}
   data 6 + U16@ 8 and 0 <> if
      len node member off len SAFE+ DESCRIPTOR-LEN SAFE+
   else len then member LOCAL-LEN !
   node off member LOCAL-LEN @ RAW-SPAN drop
   off node PREFIX-SIZE @ min node PREFIX-SIZE ! ;

: PARSE-MEMBERS ( ptr n -- ) {: node:ptr :}
   node node MEMBER-COUNT @ MEMBER-CELLS cells SAFE* BUFFER-ALLOC CELL-VIEW node MEMBERS !
   node DIRECTORY-OFF @ node PREFIX-SIZE !
   node DIRECTORY-OFF @ node MEMBER-COUNT @ 0 ?do
      node i MEMBER-AT {: member:ptr :}
      node member rot MEMBER-CENTRAL node member MEMBER-LOCAL
   loop dup node DIRECTORY-TAIL !
   node DIRECTORY-OFF @ node DIRECTORY-SIZE @ SAFE+ > if E-READ throw then ;

: RAW-LOAD ( ptr n -- ) {: node:ptr :}
   node PATH-PTR @ node PATH-LEN @ FILE-SIZE dup SIZE-CHECK {: len:n :}
   node len BUFFER-ALLOC {: data:ptr :}
   data node RAW-PTR ! len node RAW-LEN !
   node PATH-PTR @ node PATH-LEN @ data len READ-ALL len <> if E-READ throw then
   node FIND-END node END-OFF !
   node HAS-END64? if node END-WIDE else node END-CLASSIC then
   node ZIP-BASE @ SIZE-CHECK
   node MEMBER-COUNT @ node OBJECT @ C-COUNT <> if E-READ throw then
   node PARSE-MEMBERS ;

: CRC-BIT ( n -- n )
   dup 1 and 0 <> if 1 rshift $EDB88320 xor else 1 rshift then ;
: CRC-BYTE ( n n -- n ) xor 8 0 ?do CRC-BIT loop ;
: CRC32 ( ptr u8 n -- n ) {: data:ptr len:n :}
   U32-MAX len 0 ?do data i + c@ CRC-BYTE loop U32-MAX xor ;

: OUT-RESERVE ( ptr n n -- ptr u8 ) {: node:ptr len:n :}
   node OUT-LEN @ len SAFE+ node OUT-CAP @ > if E-SIZE throw then
   node OUT-PTR @ node OUT-LEN @ + ;
: EMIT-BYTES ( ptr n ptr u8 n -- ) {: node:ptr data:ptr len:n :}
   data node len OUT-RESERVE len BYTE-COPY len node OUT-LEN +! ;
: EMIT-RAW ( ptr n n n -- ) {: node:ptr off:n len:n :}
   node node off len RAW-SPAN len EMIT-BYTES ;
: EMIT-NUMBER ( ptr n n n -- ) {: node:ptr value:n width:n :}
   value node width OUT-RESERVE width LE! width node OUT-LEN +! ;
: EMIT-U16 ( ptr n n -- ) 2 EMIT-NUMBER ;
: EMIT-U32 ( ptr n n -- ) 4 EMIT-NUMBER ;
: EMIT-U64 ( ptr n n -- ) 8 EMIT-NUMBER ;
: OUT-AT ( ptr n n -- ptr u8 ) swap OUT-PTR @ + ;
: REPLACED? ( ptr n -- bool ) REPLACEMENT @ NULL? 0= ;
: OUTPUT-SIZE ( ptr n -- n )
   dup REPLACED? if REPLACE-LEN else PLAIN-SIZE then @ ;
: OUTPUT-COMPRESSED ( ptr n -- n )
   dup REPLACED? if REPLACE-LEN else COMPRESSED-SIZE then @ ;
: SIZE64? ( ptr n -- bool )
   dup OUTPUT-SIZE U32-MAX >= swap OUTPUT-COMPRESSED U32-MAX >= or ;
: OFFSET64? ( ptr n -- bool ) NEW-LOCAL @ U32-MAX >= ;
: CENTRAL64? ( ptr n -- bool ) dup SIZE64? swap OFFSET64? or ;

\ Existing bytes plus replacements bound payload size; each record needs at
\ most 48 extra ZIP64 bytes, and the end records need fewer than 256 bytes.
: OUTPUT-ALLOC ( ptr n -- ) {: node:ptr :}
   node RAW-LEN @ node MEMBER-COUNT @ $40 SAFE* SAFE+ $100 SAFE+
   node MEMBER-COUNT @ 0 ?do
      node i MEMBER-AT REPLACE-LEN @ SAFE+
   loop {: cap:n :}
   node cap BUFFER-ALLOC node OUT-PTR ! cap node OUT-CAP !
   0 node OUT-LEN ! ;

: EMIT-EXTRAS ( ptr n ptr u8 n -- ) {: node:ptr data:ptr len:n :}
   data len begin dup 0 > while
      2dup EXTRA-CHECK over 2 + U16@ 4 + {: step:n :}
      over U16@ 1 <> if over node swap step EMIT-BYTES then
      swap step + swap step -
   repeat 2drop ;

: EMIT-WIDE-SIZES ( ptr n ptr n -- ) {: node:ptr member:ptr :}
   node member OUTPUT-SIZE EMIT-U64
   node member OUTPUT-COMPRESSED EMIT-U64 ;
: LOCAL-EXTRAS ( ptr n ptr n ptr u8 -- ) {: node:ptr member:ptr data:ptr :}
   node data LOCAL-BYTES + data $1A + U16@ + data $1C + U16@ EMIT-EXTRAS
   member SIZE64? if
      node 1 EMIT-U16 node $10 EMIT-U16 node member EMIT-WIDE-SIZES
   then ;

: PATCH-CONTENT ( ptr n ptr u8 n -- ) {: member:ptr data:ptr delta:n :}
   data 6 delta + + U16@ $800 and data 6 delta + + U16!
   0 data 8 delta + + U16!
   member REPLACE-CRC @ data $E delta + + U32!
   member REPLACE-LEN @ U32-MAX min data $12 delta + + U32!
   member REPLACE-LEN @ U32-MAX min data $16 delta + + U32! ;

: WRITE-LOCAL ( ptr n ptr n -- ) {: node:ptr member:ptr :}
   node OUT-LEN @ node ZIP-BASE @ - dup SIZE-CHECK member NEW-LOCAL !
   member REPLACED? 0= if
      node member LOCAL-OFF @ member LOCAL-LEN @ EMIT-RAW exit
   then
   node OUT-LEN @ {: start:n :}
   node member LOCAL-OFF @ LOCAL-BYTES RAW-SPAN {: original:ptr :}
   node original LOCAL-BYTES original $1A + U16@ + EMIT-BYTES
   node OUT-LEN @ {: extras:n :}
   node member original LOCAL-EXTRAS
   node start OUT-AT {: header:ptr :}
   node OUT-LEN @ extras - dup U16-MAX > if E-SIZE throw then header $1C + U16!
   member header 0 PATCH-CONTENT
   member SIZE64? if $2D else $14 then header 4 + U16!
   node member REPLACEMENT @ member REPLACE-LEN @ EMIT-BYTES ;

: CENTRAL-EXTRAS ( ptr n ptr n -- ) {: node:ptr member:ptr :}
   node node member EXTRA-SPAN EMIT-EXTRAS
   member CENTRAL64? 0= if exit then
   node 1 EMIT-U16
   member SIZE64? if $10 else 0 then
   member OFFSET64? if 8 + then node swap EMIT-U16
   member SIZE64? if node member EMIT-WIDE-SIZES then
   member OFFSET64? if node member NEW-LOCAL @ EMIT-U64 then ;

: PATCH-CENTRAL ( ptr n ptr n n n -- ) {: node:ptr member:ptr start:n extras:n :}
   node start OUT-AT {: header:ptr :}
   extras dup U16-MAX > if E-SIZE throw then header $1E + U16!
   member NEW-LOCAL @ U32-MAX min header $2A + U32!
   member OUTPUT-COMPRESSED U32-MAX min header $14 + U32!
   member OUTPUT-SIZE U32-MAX min header $18 + U32!
   0 header $22 + U16!
   member CENTRAL64? if $2D header 6 + U16! then
   member SIZE64? if
      U32-MAX header $14 + U32! U32-MAX header $18 + U32!
   then
   member REPLACED? if member header 2 PATCH-CONTENT then ;

: REBUILD-CENTRAL ( ptr n ptr n -- ) {: node:ptr member:ptr :}
   node OUT-LEN @ {: start:n :}
   node member CENTRAL-PTR {: original:ptr :}
   node original CENTRAL-BYTES original $1C + U16@ + EMIT-BYTES
   node OUT-LEN @ {: extra-start:n :}
   node member CENTRAL-EXTRAS
   node OUT-LEN @ extra-start - {: extras:n :}
   node original CENTRAL-BYTES + original $1C + U16@ + original $1E + U16@ +
   original $20 + U16@ EMIT-BYTES
   node member start extras PATCH-CENTRAL ;

: COPY-CENTRAL ( ptr n ptr n -- ) {: node:ptr member:ptr :}
   node OUT-LEN @ {: start:n :}
   node member CENTRAL-OFF @ member CENTRAL-LEN @ EMIT-RAW
   member ZIP64-OFFSET @ 0 <> if
      member NEW-LOCAL @
      node start member ZIP64-OFFSET @ + member CENTRAL-OFF @ - OUT-AT U64!
   else member NEW-LOCAL @ node start OUT-AT $2A + U32! then ;

: OFFSET-INSERTION ( ptr u8 ptr u8 -- n ) {: central:ptr extra:ptr :}
   extra central - 4 +
   central $18 + U32@ U32-MAX = if 8 + then
   central $14 + U32@ U32-MAX = if 8 + then ;

: PROMOTE-EXISTING ( ptr n ptr n ptr u8 -- n ) {: node:ptr member:ptr extra:ptr :}
   node member CENTRAL-PTR {: central:ptr :}
   central extra OFFSET-INSERTION {: split:n :}
   node OUT-LEN @ extra central - + {: extra-out:n :}
   node member CENTRAL-OFF @ split EMIT-RAW
   node member NEW-LOCAL @ EMIT-U64
   node member CENTRAL-OFF @ split + member CENTRAL-LEN @ split - EMIT-RAW
   extra 2 + U16@ 8 + node extra-out OUT-AT 2 + U16! 8 ;

: PROMOTE-NEW ( ptr n ptr n -- n ) {: node:ptr member:ptr :}
   node member CENTRAL-PTR {: central:ptr :}
   CENTRAL-BYTES central $1C + U16@ + central $1E + U16@ + {: split:n :}
   node member CENTRAL-OFF @ split EMIT-RAW
   node 1 EMIT-U16 node 8 EMIT-U16 node member NEW-LOCAL @ EMIT-U64
   node member CENTRAL-OFF @ split + member CENTRAL-LEN @ split - EMIT-RAW $C ;

: PROMOTE-CENTRAL ( ptr n ptr n -- ) {: node:ptr member:ptr :}
   node OUT-LEN @ {: start:n :}
   node member EXTRA-SPAN 1 EXTRA-FIND {: extra:ptr len:n :}
   len 0= if node member PROMOTE-NEW else node member extra PROMOTE-EXISTING then
   {: added:n :}
   node start OUT-AT {: header:ptr :}
   header $1E + U16@ added + dup U16-MAX > if E-SIZE throw then header $1E + U16!
   header 6 + U16@ $2D max header 6 + U16! U32-MAX header $2A + U32! ;

: WRITE-CENTRAL ( ptr n ptr n -- ) {: node:ptr member:ptr :}
   member REPLACED? if node member REBUILD-CENTRAL exit then
   member OFFSET64? member ZIP64-OFFSET @ 0= and if
      node member PROMOTE-CENTRAL
   else node member COPY-CENTRAL then ;

: WRITE-MEMBERS ( ptr n -- ) {: node:ptr :}
   node 0 node PREFIX-SIZE @ EMIT-RAW
   node MEMBER-COUNT @ 0 ?do node node i MEMBER-AT WRITE-LOCAL loop
   node OUT-LEN @ node OUT-DIRECTORY !
   node MEMBER-COUNT @ 0 ?do node node i MEMBER-AT WRITE-CENTRAL loop
   node node DIRECTORY-TAIL @
   node DIRECTORY-OFF @ node DIRECTORY-SIZE @ + node DIRECTORY-TAIL @ - EMIT-RAW ;

: NEED-END64? ( ptr n n -- bool ) {: node:ptr dirsize:n :}
   node END64-OFF @ 0 <> node MEMBER-COUNT @ U16-MAX >= or
   dirsize U32-MAX >= or node OUT-DIRECTORY @ node ZIP-BASE @ - U32-MAX >= or ;

: NEW-END64 ( ptr n -- ) {: node:ptr :}
   node END64-SIG EMIT-U32 node $2C EMIT-U64
   node $2D EMIT-U16 node $2D EMIT-U16 node 0 EMIT-U64
   node 0 EMIT-U64 node 0 EMIT-U64 node 0 EMIT-U64 node 0 EMIT-U64 ;

: WRITE-END64 ( ptr n n -- ) {: node:ptr dirsize:n :}
   node OUT-LEN @ {: start:n :}
   node END64-OFF @ 0 <> if
      node node END64-OFF @ node END-OFF @ LOCATOR-BYTES - node END64-OFF @ - EMIT-RAW
   else node NEW-END64 then
   node start OUT-AT {: header:ptr :}
   node MEMBER-COUNT @ header $18 + U64! node MEMBER-COUNT @ header $20 + U64!
   dirsize header $28 + U64!
   node OUT-DIRECTORY @ node ZIP-BASE @ - header $30 + U64!
   node LOCATOR-SIG EMIT-U32 node 0 EMIT-U32
   node start node ZIP-BASE @ - EMIT-U64 node 1 EMIT-U32 ;

: WRITE-END ( ptr n -- ) {: node:ptr :}
   node OUT-LEN @ node OUT-DIRECTORY @ - {: dirsize:n :}
   node dirsize NEED-END64? if node dirsize WRITE-END64 then
   node OUT-LEN @ {: start:n :}
   node node END-OFF @ node RAW-LEN @ node END-OFF @ - EMIT-RAW
   node start OUT-AT {: header:ptr :}
   node MEMBER-COUNT @ U16-MAX min dup header 8 + U16! header $A + U16!
   dirsize U32-MAX min header $C + U32!
   node OUT-DIRECTORY @ node ZIP-BASE @ - U32-MAX min header $10 + U32! ;

: BUILD-OUTPUT ( ptr n -- )
   dup OUTPUT-ALLOC dup WRITE-MEMBERS WRITE-END ;

: DIRECTORY$ ( ptr u8 n -- ptr u8 n ) {: path:ptr len:n :}
   0 len 0 ?do path i + c@ $2F = if drop i 1+ then loop
   dup 0= if drop s" ." else 1- path swap then ;

: TEMP-TEMPLATE ( ptr n -- ) {: node:ptr :}
   node PATH-PTR @ node PATH-LEN @ DIRECTORY$ {: dir:ptr len:n :}
   s" /.habu-zip-XXXXXX" {: suffix:ptr size:n :}
   node len size + 1+ BUFFER-ALLOC {: data:ptr :}
   dir data len BYTE-COPY suffix data len + size BYTE-COPY
   0 data len size + + c! data node TEMP-PTR ! len size + node TEMP-LEN ! ;

: TEMP-OPEN ( ptr n -- ) {: node:ptr :}
   node TEMP-TEMPLATE
   node TEMP-PTR @ node TEMP-LEN @ 1+ C-MKSTEMP
   dup $7FFFFFFF > over 0 < or if E-COMMIT throw then
   node TEMP-FD ! 1 node TEMP-LIVE ! ;

: WRITE-STEP ( ptr n n -- n ) {: node:ptr off:n :}
   node TEMP-FD @ node OUT-PTR @ off + node OUT-LEN @ off - write
   dup 0 <= if E-COMMIT throw then off SAFE+ ;
: TEMP-WRITE ( ptr n -- ) {: node:ptr :}
   0 begin dup node OUT-LEN @ < while node swap WRITE-STEP repeat drop
   node TEMP-FD @ -1 node TEMP-FD ! C-CLOSE-FD 0 <> if E-COMMIT throw then ;
: TEMP-INSTALL ( ptr n -- ) {: node:ptr :}
   node TEMP-PTR @ node TEMP-LEN @ node PATH-PTR @ node PATH-LEN @ STAT-MODE CHMOD-MODE
   node TEMP-PTR @ node TEMP-LEN @ node PATH-PTR @ node PATH-LEN @ RENAME-FILE
   0 node TEMP-LIVE ! ;
: COMMIT-WORK ( ptr n -- ptr n )
   dup BUILD-OUTPUT dup TEMP-OPEN dup TEMP-WRITE dup TEMP-INSTALL ;

: TEMP-CLEANUP ( ptr n -- ) {: node:ptr :}
   node TEMP-LIVE @ 0= if exit then
   node TEMP-FD @ 0 >= if node TEMP-FD @ close then
   node TEMP-PTR @ node TEMP-LEN @ FS-PATHZ unlink drop 0 node TEMP-LIVE ! ;
: COMMIT-RAW ( ptr n -- )
   dup DIRTY @ 0= if drop exit then
   [: COMMIT-WORK ;] catch {: node:ptr code:n :}
   code 0 <> if node TEMP-CLEANUP E-COMMIT throw then ;

;using
;package
