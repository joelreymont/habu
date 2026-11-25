\ Read a program's <program>.map by column name: its code rows, the regions
\ the link appended after the code and its packages. An unknown version
\ supplies no rows.
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fmt.f
require src/habu/code-span.f
require src/core/exit-status.f

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
   FIELD pub n FIELD priv n FIELD name ^u8 FIELD size n
;STRUCTURE
DYNAMIC-BUFFER NS-ROWS pkg
variable NS-N
\ One line of a group's report, in the order its key first appears.
STRUCTURE owner DERIVE addr
   FIELD key ^u8 FIELD size n FIELD sum n
;STRUCTURE
DYNAMIC-BUFFER OWNERS owner
variable OWNER-N
variable ATTR-TOTAL variable ATTR-BAKED variable ATTR-LOADED
variable ATTR-GENERATED variable ATTR-UNKNOWN variable ATTR-OK

\ The version-1 sidecar uses one space between fields; the header names columns
\ so a reordered or extended map is read by meaning, not by column position.
: FIELD$ ( str n -- str ) {: s:str col:n :}
   0
   col 0 ?do
      {: start:n :}
      s 32 start STR:SPLIT-NEXT 0= if drop drop s" " unloop exit then
      {: field:str next:n :} next
   loop
   {: start:n :} s 32 start STR:SPLIT-NEXT drop drop ;

: COLUMN ( str str -- n ) {: s:str name:str :}
   1 begin
      {: col:n :}
      s col FIELD$ dup STR:LEN 0= if drop -1 exit then
      name STR:= if col 1- exit then
      col 1+
   again ;

: LOADED? ( -- bool ) USED @ 0<> ;

: LINE$ ( -- str )
   0 TEXT BYTE-VIEW USED @ >str 10 POS @ STR:SPLIT-NEXT
   drop POS ! ;

: NUMBER ( str -- n )
   STR:>NUMBER? MATCH option none OF -1 ENDOF some OF ENDOF ;MATCH ;

: NUM-COL ( str n -- n ) FIELD$ NUMBER ;

: ROW-LEN ( str -- n ) {: s:str :}
   s COL-LEN @ NUM-COL dup CODE-SPAN:VALID? if CODE-SPAN:BYTES else drop -1 then ;

: ROW-BYTES ( str -- n )
   ROW-LEN dup 0 < if
      s" image-names: a row's length is not a length" EXIT-STATUS:DATAERR die then ;

: KIND? ( str str -- bool )
   {: row:str kind:str :}
   row COL-KIND @ FIELD$ kind STR:= ;

: ROW-CHARGED? ( str -- bool ) {: s:str :}
   s COL-KIND @ FIELD$ s" runtime" STR:= if true exit then
   s COL-KIND @ FIELD$ s" word" STR:= ;

: ORIGIN+ ( str n -- ) {: s:str bytes:n :}
   s COL-ORIGIN @ FIELD$ s" baked" STR:= if
      ATTR-BAKED @ bytes + ATTR-BAKED ! exit then
   s COL-ORIGIN @ FIELD$ s" loaded" STR:= if
      ATTR-LOADED @ bytes + ATTR-LOADED ! exit then
   s COL-ORIGIN @ FIELD$ s" generated" STR:= if
      ATTR-GENERATED @ bytes + ATTR-GENERATED ! exit then
   ATTR-UNKNOWN @ bytes + ATTR-UNKNOWN ! ;

: NS+ ( str -- ) {: row:str :}
   NS-N @ 1+ NS-ROWS-RESERVE
   NS-N @ NS-ROWS {: pkg:^ :}
   row COL-START @ NUM-COL pkg PKG-PUB !
   row COL-LEN @ NUM-COL pkg PKG-PRIV !
   row COL-NAME @ FIELD$ dup STR:LEN pkg PKG-SIZE ! STR:DATA pkg PKG-NAME !
   NS-N @ 1+ NS-N ! ;

: INDEX-NAMESPACES ( -- )
   0 NS-N ! ROWS @ POS !
   begin POS @ USED @ < while
      LINE$ {: row:str :}
      COL-KIND @ 0 >= if
         row COL-KIND @ FIELD$ s" namespace" STR:=
      else false then if row NS+ then
   repeat ;

\ The kinds of the rows after the code: the regions the link appended and the pad before one.
: TAIL? ( str -- bool )
   {: kind:str :}
   kind s" data" STR:= if true exit then
   kind s" data-list" STR:= if true exit then
   kind s" names" STR:= if true exit then
   kind s" records" STR:= if true exit then
   kind s" record-list" STR:= if true exit then
   kind s" pad" STR:= ;

: UNCHARGED? ( str -- bool )
   {: kind:str :}
   kind s" namespace" STR:= if true exit then
   kind s" alias" STR:= if true exit then
   kind TAIL? ;

\ At most 64 bytes of the kind: a corrupt map's field can pass the string builder.
: UNKNOWN-KIND ( str -- )
   {: kind:str :}
   SB-RESET s" image-names: a map row of unknown kind " SB-APPEND
   kind STR:DATA kind STR:LEN 64 min >str SB-APPEND
   SB$ EXIT-STATUS:DATAERR die ;

public

\ Writes <path>.map into dst, a PATH-CAP buffer; false when it would pass PATH-CAP.
: MAP-PATH! ( str ^u8 -- n bool )
   {: path:str dst:^ :}
   path STR:LEN 4 + PATH-CAP > if 0 false exit then
   path STR:DATA dst path STR:LEN BYTE-COPY
   s" .map" STR:DATA dst path STR:LEN + 4 BYTE-COPY
   path STR:LEN 4 + true ;

\ <path>.map in PATH until the next call, false when it would pass PATH-CAP.
: MAP$ ( str -- str bool )
   {: path:str :}
   path PATH MAP-PATH! {: mapu:n ok:bool :}
   ok if PATH mapu >str true else path false then ;

\ A missing map or an unknown header leaves no rows: LOADED? is false.
: LOAD ( str -- )
   {: path:str :}
   0 USED ! 0 ATTR-OK ! 0 NS-N !
   path MAP$ 0= if E-FS-CAPACITY throw then
   {: map:str :}
   map FS:FILE? 0= if exit then
   map FS:FILE-SIZE {: size:n :}
   size CELL + CELL / TEXT-RESERVE
   map 0 TEXT BYTE-VIEW size FS:READ-ALL size <> if
      s" image-names: short read" EXIT-STATUS:IOERR die then
   size USED ! 0 POS !
   LINE$ s" habu-names 1" STR:= 0= if 0 USED ! exit then
   LINE$ {: header:str :}
   header 0 FIELD$ s" columns" STR:= 0= if 0 USED ! exit then
   header s" start" COLUMN COL-START !
   header s" len" COLUMN COL-LEN !
   header s" wid" COLUMN COL-WID !
   header s" name" COLUMN COL-NAME !
   COL-START @ COL-LEN @ min COL-WID @ min COL-NAME @ min 0 < if
      0 USED ! exit then
   header s" kind" COLUMN COL-KIND !
   header s" origin" COLUMN COL-ORIGIN !
   header s" source" COLUMN COL-SOURCE !
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
      LINE$ {: row:str :}
      row ROW-CHARGED? if
         row COL-START @ NUM-COL ATTR-TOTAL @ <> if false exit then
         row ROW-LEN {: bytes:n :}
         bytes 0 <= if false exit then
         ATTR-TOTAL @ bytes + size > if false exit then
         row bytes ORIGIN+
         ATTR-TOTAL @ bytes + ATTR-TOTAL !
      else
         row COL-KIND @ FIELD$
         dup UNCHARGED? if drop else UNKNOWN-KIND then
      then
   repeat
   ATTR-TOTAL @ size = dup ATTR-OK ! ;

\ The code's end: the start of the first row after the code, -1 when the map
\ has none.
: CODE-END ( -- n )
   LOADED? 0= COL-KIND @ 0 < or if -1 exit then
   ROWS @ POS !
   begin POS @ USED @ < while
      LINE$ {: row:str :}
      row COL-KIND @ FIELD$ TAIL? if
         row COL-START @ NUM-COL exit then
   repeat -1 ;

\ The summed length of every row of a kind.
: KIND-BYTES ( str -- n )
   {: kind:str :}
   LOADED? 0= COL-KIND @ 0 < or if 0 exit then
   0 ROWS @ POS !
   begin POS @ USED @ < while
      LINE$ {: row:str :}
      row kind KIND? if row ROW-BYTES + then
   repeat ;

\ The start and length of the one row of a kind, 0 0 when there is none.
: REGION ( str -- n n )
   {: kind:str :}
   0 0 false
   LOADED? COL-KIND @ 0 >= and if
      ROWS @ POS !
      begin POS @ USED @ < while
         LINE$ {: row:str :}
         row kind KIND? if
            if s" image-names: two rows of one region" EXIT-STATUS:DATAERR die then
            2drop row COL-START @ NUM-COL row ROW-BYTES true
         then
      repeat
   then drop ;

private

: ATTR? ( -- bool ) ATTR-OK @ 0<> ;

: PACKAGE$ ( n -- str ) {: wid:n :}
   wid 0= if s" global" exit then
   wid -1 = if s" -" exit then
   NS-N @ 0 ?do
      i NS-ROWS {: pkg:^ :}
      pkg PKG-PUB @ wid = pkg PKG-PRIV @ wid = or if
         pkg PKG-NAME @ pkg PKG-SIZE @ >str unloop exit
      then
   loop s" -" ;

: GROUP-KEY$ ( str n -- str ) {: row:str group:n :}
   group 0= if row COL-WID @ NUM-COL PACKAGE$ exit then
   COL-SOURCE @ 0 < if s" -" exit then
   row COL-SOURCE @ FIELD$ ;

\ The pointer holds until the next call: a new owner may move OWNERS.
: OWNER ( str -- ^owner ) {: key:str :}
   OWNER-N @ 0 ?do
      i OWNERS {: seen:^ :}
      seen OWNER-KEY @ seen OWNER-SIZE @ >str key STR:= if
         seen unloop exit
      then
   loop
   OWNER-N @ 1+ OWNERS-RESERVE
   OWNER-N @ OWNERS {: new:^ :}
   key STR:DATA new OWNER-KEY ! key STR:LEN new OWNER-SIZE !
   0 new OWNER-SUM !
   OWNER-N @ 1+ OWNER-N ! new ;

\ Rows whose keys print the same sum into one owner.
: SUM-OWNERS ( n -- ) {: group:n :}
   0 OWNER-N ! ROWS @ POS !
   begin POS @ USED @ < while
      LINE$ {: row:str :}
      row ROW-CHARGED? if
         row ROW-LEN
         row group GROUP-KEY$ OWNER OWNER-SUM +!
      then
   repeat ;

: REPORT-GROUP ( n str -- )
   {: group:n label:str :}
   cr label type cr
   s" bytes" type 9 emit s" owner" type cr
   group SUM-OWNERS
   OWNER-N @ 0 ?do
      i OWNERS {: own:^ :}
      own OWNER-SUM @ FMT:.U 9 emit
      own OWNER-KEY @ own OWNER-SIZE @ >str type cr
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
      LINE$ {: row:str :}
      row ROW-CHARGED? if
         row ROW-LEN FMT:.U 9 emit
         row COL-ORIGIN @ FIELD$ type 9 emit
         row COL-WID @ NUM-COL PACKAGE$ type 9 emit
         row COL-NAME @ FIELD$ type 9 emit
         COL-SOURCE @ 0 >= if
            row COL-SOURCE @ FIELD$ type
         else s" -" type then cr
      then
   repeat
   0 s" by package" REPORT-GROUP
   1 s" by source" REPORT-GROUP ;

;package
