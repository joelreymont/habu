\ Read a program's <program>.map by column name: its code rows, the regions
\ the link appended after the code and its packages. An unknown version
\ supplies no rows.
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fmt.f
require src/habu/code-span.f

package IMAGE-NAMES

DYNAMIC-BUFFER TEXT n
create PATH PATH-CAP allot
variable USED variable POS variable ROWS
variable COL-START variable COL-LEN variable COL-WID variable COL-NAME
variable COL-KIND variable COL-ORIGIN variable COL-SOURCE
\ A namespace row holds its package's public wid in the start column and its
\ private wid in the len column. Its name points into TEXT, which only LOAD
\ moves, and LOAD indexes the rows again.
STRUCTURE pkg DERIVE addr
   FIELD pub n FIELD priv n FIELD name ptr u8 FIELD size n
;STRUCTURE
DYNAMIC-BUFFER NS-ROWS pkg
variable NS-N
\ One line of a group's report, in the order its key first appears.
STRUCTURE owner DERIVE addr
   FIELD key ptr u8 FIELD size n FIELD sum n
;STRUCTURE
DYNAMIC-BUFFER OWNERS owner
variable OWNER-N
variable ATTR-TOTAL variable ATTR-BAKED variable ATTR-LOADED
variable ATTR-GENERATED variable ATTR-UNKNOWN variable ATTR-OK

\ The version-1 sidecar uses one space between fields; the header names columns
\ so a reordered or extended map is read by meaning, not by column position.
: FIELD$ ( ptr u8 n n -- ptr u8 n ) {: a:ptr u:n col:n :}
   0
   col 0 ?do
      {: start:n :}
      a u 32 start SPLIT-NEXT 0= if drop 2drop s" " unloop exit then
      {: field:ptr size:n next:n :} next
   loop
   {: start:n :} a u 32 start SPLIT-NEXT 2drop ;

: COLUMN ( ptr u8 n ptr u8 n -- n ) {: a:ptr u:n name:ptr size:n :}
   1 begin
      {: col:n :}
      a u col FIELD$ dup 0= if 2drop -1 exit then
      name size STR= if col 1- exit then
      col 1+
   again ;

: LOADED? ( -- bool ) USED @ 0<> ;

: LINE$ ( -- ptr u8 n )
   0 TEXT BYTE-VIEW USED @ 10 POS @ SPLIT-NEXT
   drop POS ! ;

: NUMBER ( ptr u8 n -- n )
   STR>NUMBER? MATCH option none OF -1 ENDOF some OF ENDOF ;MATCH ;

: NUM-COL ( ptr u8 n n -- n ) FIELD$ NUMBER ;

: ROW-LEN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u COL-LEN @ NUM-COL dup CODE-SPAN:VALID? if CODE-SPAN:BYTES else drop -1 then ;

: ROW-BYTES ( ptr u8 n -- n )
   ROW-LEN dup 0 < if
      s" image-names: a row's length is not a length" 74 die then ;

: KIND? ( ptr u8 n ptr u8 n -- bool )
   {: row:ptr rowu:n kind:ptr kindu:n :}
   row rowu COL-KIND @ FIELD$ kind kindu STR= ;

: ROW-CHARGED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u COL-KIND @ FIELD$ s" runtime" STR= if true exit then
   a u COL-KIND @ FIELD$ s" word" STR= ;

: ORIGIN+ ( ptr u8 n n -- ) {: a:ptr u:n bytes:n :}
   a u COL-ORIGIN @ FIELD$ s" baked" STR= if
      ATTR-BAKED @ bytes + ATTR-BAKED ! exit then
   a u COL-ORIGIN @ FIELD$ s" loaded" STR= if
      ATTR-LOADED @ bytes + ATTR-LOADED ! exit then
   a u COL-ORIGIN @ FIELD$ s" generated" STR= if
      ATTR-GENERATED @ bytes + ATTR-GENERATED ! exit then
   ATTR-UNKNOWN @ bytes + ATTR-UNKNOWN ! ;

: NS+ ( ptr u8 n -- ) {: row:ptr rowu:n :}
   NS-N @ 1+ NS-ROWS-RESERVE
   NS-N @ NS-ROWS {: pkg:ptr :}
   row rowu COL-START @ NUM-COL pkg PKG-PUB !
   row rowu COL-LEN @ NUM-COL pkg PKG-PRIV !
   row rowu COL-NAME @ FIELD$ pkg PKG-SIZE ! pkg PKG-NAME !
   NS-N @ 1+ NS-N ! ;

: INDEX-NAMESPACES ( -- )
   0 NS-N ! ROWS @ POS !
   begin POS @ USED @ < while
      LINE$ {: row:ptr rowu:n :}
      COL-KIND @ 0 >= if
         row rowu COL-KIND @ FIELD$ s" namespace" STR=
      else false then if row rowu NS+ then
   repeat ;

\ The kinds of the rows after the code: the regions the link appended and the pad before one.
: TAIL? ( ptr u8 n -- bool )
   {: kind:ptr kindu:n :}
   kind kindu s" data" STR= if true exit then
   kind kindu s" data-list" STR= if true exit then
   kind kindu s" names" STR= if true exit then
   kind kindu s" records" STR= if true exit then
   kind kindu s" record-list" STR= if true exit then
   kind kindu s" pad" STR= ;

: UNCHARGED? ( ptr u8 n -- bool )
   {: kind:ptr kindu:n :}
   kind kindu s" namespace" STR= if true exit then
   kind kindu s" alias" STR= if true exit then
   kind kindu TAIL? ;

\ At most 64 bytes of the kind: a corrupt map's field can pass the string builder.
: UNKNOWN-KIND ( ptr u8 n -- )
   {: kind:ptr kindu:n :}
   SB-RESET s" image-names: a map row of unknown kind " SB-APPEND
   kind kindu 64 min SB-APPEND SB$ 74 die ;

public

\ A missing map or an unknown header leaves no rows: LOADED? is false.
: LOAD ( ptr u8 n -- )
   {: path:ptr pathu:n :}
   0 USED ! 0 ATTR-OK ! 0 NS-N !
   pathu 4 + PATH-CAP > if E-FS-CAPACITY throw then
   path PATH pathu BYTE-COPY
   s" .map" drop PATH pathu + 4 BYTE-COPY
   PATH pathu 4 + FS:FILE? 0= if exit then
   PATH pathu 4 + FS:FILE-SIZE {: size:n :}
   size CELL + CELL / TEXT-RESERVE
   PATH pathu 4 + 0 TEXT BYTE-VIEW size FS:READ-ALL size <> if
      s" image-names: short read" 74 die then
   size USED ! 0 POS !
   LINE$ s" habu-names 1" STR= 0= if 0 USED ! exit then
   LINE$ {: header:ptr headeru:n :}
   header headeru 0 FIELD$ s" columns" STR= 0= if 0 USED ! exit then
   header headeru s" start" COLUMN COL-START !
   header headeru s" len" COLUMN COL-LEN !
   header headeru s" wid" COLUMN COL-WID !
   header headeru s" name" COLUMN COL-NAME !
   COL-START @ COL-LEN @ min COL-WID @ min COL-NAME @ min 0 < if
      0 USED ! exit then
   header headeru s" kind" COLUMN COL-KIND !
   header headeru s" origin" COLUMN COL-ORIGIN !
   header headeru s" source" COLUMN COL-SOURCE !
   POS @ ROWS !
   INDEX-NAMESPACES ;

\ The ordered rows must tile the physical app/code region without a duplicate
\ or a gap; a map from another image attributes nothing unless they do. An
\ alias row names code a row before it charged.
: CODE-ATTR ( n -- bool ) {: size:n :}
   0 ATTR-TOTAL ! 0 ATTR-BAKED ! 0 ATTR-LOADED !
   0 ATTR-GENERATED ! 0 ATTR-UNKNOWN ! 0 ATTR-OK !
   LOADED? 0= COL-KIND @ 0 < or COL-ORIGIN @ 0 < or if false exit then
   ROWS @ POS !
   begin POS @ USED @ < while
      LINE$ {: row:ptr rowu:n :}
      row rowu ROW-CHARGED? if
         row rowu COL-START @ NUM-COL ATTR-TOTAL @ <> if false exit then
         row rowu ROW-LEN {: bytes:n :}
         bytes 0 <= if false exit then
         ATTR-TOTAL @ bytes + size > if false exit then
         row rowu bytes ORIGIN+
         ATTR-TOTAL @ bytes + ATTR-TOTAL !
      else
         row rowu COL-KIND @ FIELD$
         2dup UNCHARGED? if 2drop else UNKNOWN-KIND then
      then
   repeat
   ATTR-TOTAL @ size = dup ATTR-OK ! ;

\ The code's end: the start of the first row after the code, -1 when the map
\ has none.
: CODE-END ( -- n )
   LOADED? 0= COL-KIND @ 0 < or if -1 exit then
   ROWS @ POS !
   begin POS @ USED @ < while
      LINE$ {: row:ptr rowu:n :}
      row rowu COL-KIND @ FIELD$ TAIL? if
         row rowu COL-START @ NUM-COL exit then
   repeat -1 ;

\ The summed length of every row of a kind.
: KIND-BYTES ( ptr u8 n -- n )
   {: kind:ptr kindu:n :}
   LOADED? 0= COL-KIND @ 0 < or if 0 exit then
   0 ROWS @ POS !
   begin POS @ USED @ < while
      LINE$ {: row:ptr rowu:n :}
      row rowu kind kindu KIND? if row rowu ROW-BYTES + then
   repeat ;

\ The start and length of the one row of a kind, 0 0 when there is none.
: REGION ( ptr u8 n -- n n )
   {: kind:ptr kindu:n :}
   0 0 false
   LOADED? COL-KIND @ 0 >= and if
      ROWS @ POS !
      begin POS @ USED @ < while
         LINE$ {: row:ptr rowu:n :}
         row rowu kind kindu KIND? if
            if s" image-names: two rows of one region" 74 die then
            2drop row rowu COL-START @ NUM-COL row rowu ROW-BYTES true
         then
      repeat
   then drop ;

private

: ATTR? ( -- bool ) ATTR-OK @ 0<> ;

: PACKAGE$ ( n -- ptr u8 n ) {: wid:n :}
   wid 0= if s" global" exit then
   wid -1 = if s" -" exit then
   NS-N @ 0 ?do
      i NS-ROWS {: pkg:ptr :}
      pkg PKG-PUB @ wid = pkg PKG-PRIV @ wid = or if
         pkg PKG-NAME @ pkg PKG-SIZE @ unloop exit
      then
   loop s" -" ;

: GROUP-KEY$ ( ptr u8 n n -- ptr u8 n ) {: row:ptr rowu:n group:n :}
   group 0= if row rowu COL-WID @ NUM-COL PACKAGE$ exit then
   COL-SOURCE @ 0 < if s" -" exit then
   row rowu COL-SOURCE @ FIELD$ ;

\ The pointer holds until the next call: a new owner may move OWNERS.
: OWNER ( ptr u8 n -- ptr owner ) {: key:ptr keyu:n :}
   OWNER-N @ 0 ?do
      i OWNERS {: seen:ptr :}
      seen OWNER-KEY @ seen OWNER-SIZE @ key keyu STR= if
         seen unloop exit
      then
   loop
   OWNER-N @ 1+ OWNERS-RESERVE
   OWNER-N @ OWNERS {: new:ptr :}
   key new OWNER-KEY ! keyu new OWNER-SIZE ! 0 new OWNER-SUM !
   OWNER-N @ 1+ OWNER-N ! new ;

\ Rows whose keys print the same sum into one owner.
: SUM-OWNERS ( n -- ) {: group:n :}
   0 OWNER-N ! ROWS @ POS !
   begin POS @ USED @ < while
      LINE$ {: row:ptr rowu:n :}
      row rowu ROW-CHARGED? if
         row rowu ROW-LEN
         row rowu group GROUP-KEY$ OWNER OWNER-SUM +!
      then
   repeat ;

: REPORT-GROUP ( n ptr u8 n -- )
   {: group:n label:ptr labelu:n :}
   cr label labelu type cr
   s" bytes" type 9 emit s" owner" type cr
   group SUM-OWNERS
   OWNER-N @ 0 ?do
      i OWNERS {: own:ptr :}
      own OWNER-SUM @ FMT:.U 9 emit
      own OWNER-KEY @ own OWNER-SIZE @ type cr
   loop ;

public

: REPORT-CODE ( -- )
   cr s" retained code attribution: " type
   ATTR? 0= if s" unavailable" type cr exit then
   ATTR-TOTAL @ FMT:.U s"  bytes = baked " type
   ATTR-BAKED @ FMT:.U s" , loaded " type
   ATTR-LOADED @ FMT:.U s" , generated " type
   ATTR-GENERATED @ FMT:.U s" , unknown " type
   ATTR-UNKNOWN @ FMT:.U cr
   s" bytes" type 9 emit s" origin" type 9 emit
   s" package" type 9 emit s" word" type 9 emit s" source" type cr
   ROWS @ POS !
   begin POS @ USED @ < while
      LINE$ {: row:ptr rowu:n :}
      row rowu ROW-CHARGED? if
         row rowu ROW-LEN FMT:.U 9 emit
         row rowu COL-ORIGIN @ FIELD$ type 9 emit
         row rowu COL-WID @ NUM-COL PACKAGE$ type 9 emit
         row rowu COL-NAME @ FIELD$ type 9 emit
         COL-SOURCE @ 0 >= if
            row rowu COL-SOURCE @ FIELD$ type
         else s" -" type then cr
      then
   repeat
   0 s" by package" REPORT-GROUP
   1 s" by source" REPORT-GROUP ;

;package
