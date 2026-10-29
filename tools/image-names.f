\ Read native-build's optional <image>.map file. Columns are addressed by
\ name; unknown versions or malformed rows supply no name. Matching a full
\ span also checks its length, so a stale sidecar cannot label a resized body.
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fmt.f
require src/lib/sort.f
require src/core/sha256.f
require src/habu/code-span.f

package IMAGE-NAMES

DYNAMIC-BUFFER TEXT n
create PATH PATH-CAP allot
variable USED variable POS variable ROWS
variable COL-START variable COL-LEN variable COL-WID variable COL-NAME
variable COL-KIND variable COL-ORIGIN variable COL-SOURCE
variable BOUND
variable GROUP-TOTAL
DYNAMIC-BUFFER CODE-ROWS n
DYNAMIC-BUFFER NS-ROWS n
variable CODE-N variable NS-N
create HASH-CTX SHA256-FILE-CTX-BYTES allot
create HASH-HEX 64 allot
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

: LINE$ ( -- ptr u8 n )
   0 TEXT BYTE-VIEW USED @ 10 POS @ SPLIT-NEXT
   drop POS ! ;

: NUMBER ( ptr u8 n -- n )
   STR>NUMBER? MATCH option none OF -1 ENDOF some OF ENDOF ;MATCH ;

: ROW$ ( ptr u8 n n n -- ptr u8 n ) {: a:ptr u:n off:n want:n :}
   a u COL-START @ FIELD$ NUMBER {: start:n :}
   a u COL-LEN @ FIELD$ NUMBER {: size:n :}
   start 0 < size CODE-SPAN:VALID? 0= or if s" " exit then
   size CODE-SPAN:BYTES {: span:n :}
   want 0 < if
      off start >= off start - span < and
   else
      off start = want span = and
   then if a u COL-NAME @ FIELD$ exit then s" " ;

: NUM-COL ( ptr u8 n n -- n ) FIELD$ NUMBER ;

: ROW-LEN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u COL-LEN @ NUM-COL dup CODE-SPAN:VALID? if CODE-SPAN:BYTES else drop -1 then ;

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

: AT-LINE$ ( n -- ptr u8 n )
   {: at:n :}
   0 TEXT BYTE-VIEW USED @ 10 at SPLIT-NEXT 2drop ;

: CODE-ROW$ ( n -- ptr u8 n ) CODE-ROWS @ AT-LINE$ ;
: CODE-START ( n -- n ) CODE-ROW$ COL-START @ NUM-COL ;

: ROW-BEFORE? ( n n -- bool )
   {: a:n b:n :}
   a AT-LINE$ COL-START @ NUM-COL {: lo:n :}
   b AT-LINE$ COL-START @ NUM-COL {: hi:n :}
   lo hi = if a b < else lo hi < then ;

\ Keep source order within equal spans: aliases and unnamed fallback rows may
\ share an address. Namespace rows have WIDs in the start/len columns instead.
: INDEX-ROWS ( -- )
   0 CODE-N ! 0 NS-N ! ROWS @ POS !
   begin POS @ USED @ < while
      POS @ {: at:n :}
      LINE$ {: row:ptr rowu:n :}
      COL-KIND @ 0 >= if
         row rowu COL-KIND @ FIELD$ s" namespace" STR=
      else false then if
         NS-N @ 1+ NS-ROWS-RESERVE
         at NS-N @ NS-ROWS ! NS-N @ 1+ NS-N !
      else
         CODE-N @ 1+ CODE-ROWS-RESERVE
         at CODE-N @ CODE-ROWS ! CODE-N @ 1+ CODE-N !
      then
   repeat
   CODE-N @ 1 > if
      0 CODE-ROWS CODE-N @ [: ROW-BEFORE? ;] SORT:SORT!
   then ;

: FIRST-CODE ( n -- n )
   {: off:n :}
   0 CODE-N @ begin 2dup < while
      {: lo:n hi:n :}
      lo hi + 2 / {: mid:n :}
      mid CODE-START off < if mid 1+ hi else lo mid then
   repeat drop ;

public

-1 constant OWNER-UNKNOWN
0 constant OWNER-GLOBAL
1 constant OWNER-PUBLIC
2 constant OWNER-PRIVATE

\ Read the map and verify its image binding without indexing name rows.
: LOAD-BOUND ( ptr u8 n -- )
   {: path:ptr pathu:n :}
   0 USED ! 0 BOUND ! 0 ATTR-OK ! 0 CODE-N ! 0 NS-N !
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
   POS @ USED @ < if
      LINE$ {: first:ptr firstu:n :}
      first firstu 0 FIELD$ s" image-sha256" STR= if
         HASH-CTX path pathu HASH-HEX SHA256-FILE-HEX-IN
            dup 0<> if throw then drop
         first firstu 1 FIELD$ HASH-HEX 64 STR= 0= if
            0 USED ! exit
         then
         -1 BOUND ! POS @ ROWS !
      then
   then ;

: LOAD ( ptr u8 n -- ) LOAD-BOUND INDEX-ROWS ;

: BOUND? ( -- bool ) BOUND @ 0<> ;

\ The image hash binds external labels; the ordered rows must then tile the
\ physical app/code region without a duplicate or a gap.
: CODE-ATTR ( n -- bool ) {: size:n :}
   0 ATTR-TOTAL ! 0 ATTR-BAKED ! 0 ATTR-LOADED !
   0 ATTR-GENERATED ! 0 ATTR-UNKNOWN ! 0 ATTR-OK !
   BOUND? 0= COL-KIND @ 0 < or COL-ORIGIN @ 0 < or if false exit then
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
         row rowu COL-KIND @ FIELD$ s" namespace" STR= 0= if false exit then
      then
   repeat
   ATTR-TOTAL @ size = dup ATTR-OK ! ;

: ATTR? ( -- bool ) ATTR-OK @ 0<> ;
: ATTR-BYTES ( -- n ) ATTR-TOTAL @ ;
: BAKED-BYTES ( -- n ) ATTR-BAKED @ ;
: LOADED-BYTES ( -- n ) ATTR-LOADED @ ;
: GENERATED-BYTES ( -- n ) ATTR-GENERATED @ ;
: UNKNOWN-BYTES ( -- n ) ATTR-UNKNOWN @ ;

: BAKED-SOURCE? ( -- bool )
   ATTR? 0= COL-SOURCE @ 0 < or if false exit then
   ROWS @ POS !
   begin POS @ USED @ < while
      LINE$ {: row:ptr rowu:n :}
      row rowu ROW-CHARGED?
      row rowu COL-ORIGIN @ FIELD$ s" baked" STR= and if
         row rowu COL-SOURCE @ FIELD$ s" -" STR= 0= if true exit then
      then
   repeat false ;

: PACKAGE$ ( n -- ptr u8 n ) {: wid:n :}
   wid 0= if s" global" exit then
   wid -1 = if s" -" exit then
   COL-KIND @ 0 < if s" -" exit then
   NS-N @ 0 ?do
      i NS-ROWS @ AT-LINE$ {: row:ptr rowu:n :}
      row rowu COL-START @ NUM-COL wid =
      row rowu COL-LEN @ NUM-COL wid = or if
         row rowu COL-NAME @ FIELD$ unloop exit
      then
   loop s" -" ;

: GROUP-KEY$ ( ptr u8 n n -- ptr u8 n ) {: row:ptr rowu:n group:n :}
   group 0= if row rowu COL-WID @ NUM-COL PACKAGE$ exit then
   COL-SOURCE @ 0 < if s" -" exit then
   row rowu COL-SOURCE @ FIELD$ ;

: GROUP-SEEN? ( ptr u8 n n n -- bool )
   {: key:ptr keyu:n group:n before:n :}
   ROWS @ POS !
   begin POS @ before < while
      LINE$ {: row:ptr rowu:n :}
      row rowu ROW-CHARGED? if
         row rowu group GROUP-KEY$ key keyu STR= if true exit then
      then
   repeat false ;

: GROUP-BYTES ( ptr u8 n n -- n ) {: key:ptr keyu:n group:n :}
   0 GROUP-TOTAL !
   ROWS @ POS !
   begin POS @ USED @ < while
      LINE$ {: row:ptr rowu:n :}
      row rowu ROW-CHARGED? if
         row rowu group GROUP-KEY$ key keyu STR= if
            row rowu ROW-LEN GROUP-TOTAL +!
         then
      then
   repeat GROUP-TOTAL @ ;

: REPORT-GROUP ( n ptr u8 n -- )
   {: group:n label:ptr labelu:n :}
   cr label labelu type cr
   s" bytes" type 9 emit s" owner" type cr
   ROWS @ POS !
   begin POS @ USED @ < while
      POS @ {: at:n :}
      LINE$ {: row:ptr rowu:n :}
      POS @ {: next:n :}
      row rowu ROW-CHARGED? if
         row rowu group GROUP-KEY$ {: key:ptr keyu:n :}
         key keyu group at GROUP-SEEN? 0= if
            key keyu group GROUP-BYTES FMT:.U 9 emit key keyu type cr
         then
      then
      next POS !
   repeat ;

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

\ Wordlist ids are local to an image. Match the package namespace and whether
\ the record belongs to its public or private wordlist instead of comparing ids.
: DONOR-OWNER? ( n ptr u8 n n -- bool )
   {: wid:n pkg:ptr pkgu:n role:n :}
   role OWNER-GLOBAL = if wid 0= exit then
   wid 0 <= if false exit then
   role OWNER-PUBLIC <> role OWNER-PRIVATE <> and if false exit then
   COL-KIND @ 0 < if false exit then
   NS-N @ 0 ?do
      i NS-ROWS @ AT-LINE$ {: row:ptr rowu:n :}
      row rowu COL-NAME @ FIELD$ pkg pkgu STR= if
         role OWNER-PUBLIC = if
            row rowu COL-START @ NUM-COL wid =
         else
            row rowu COL-LEN @ NUM-COL wid =
         then unloop exit
      then
   loop false ;

\ Donor lookup accepts the identical original span, actual record name and
\ wordlist owner. An absent source remains absent.
: DONOR-SOURCE$ ( n n ptr u8 n ptr u8 n n -- ptr u8 n bool )
   {: off:n bytes:n name:ptr nameu:n pkg:ptr pkgu:n role:n :}
   BOUND? 0= COL-SOURCE @ 0 < or if s" " false exit then
   off FIRST-CODE begin dup CODE-N @ < while
      {: idx:n :}
      idx CODE-ROW$ {: row:ptr rowu:n :}
      idx CODE-START off <> if s" " false exit then
      row rowu ROW-LEN bytes = if
         row rowu COL-NAME @ FIELD$ name nameu STR= if
            row rowu COL-WID @ NUM-COL pkg pkgu role DONOR-OWNER? if
               row rowu COL-SOURCE @ FIELD$ dup 0 > if
                  2dup s" -" STR= 0= if true exit then
               then 2drop
               s" " false exit
            then
         then
      then
      idx 1+
   repeat drop s" " false ;

: DONOR-RECORD? ( n n ptr u8 n ptr u8 n n -- bool )
   {: off:n bytes:n name:ptr nameu:n pkg:ptr pkgu:n role:n :}
   BOUND? 0= if false exit then
   role OWNER-UNKNOWN = if false exit then
   off FIRST-CODE begin dup CODE-N @ < while
      {: idx:n :}
      idx CODE-ROW$ {: row:ptr rowu:n :}
      idx CODE-START off <> if false exit then
      row rowu ROW-LEN bytes = if
         row rowu COL-NAME @ FIELD$ name nameu STR= if
            row rowu COL-WID @ NUM-COL pkg pkgu role DONOR-OWNER? if true exit then
         then
      then
      idx 1+
   repeat drop false ;

\ An unnamed closure member can still be the exact body of a record that the
\ capture stripped. Prefer that record over the extra anonymous span row; the
\ latter proves baked bytes but has no name or source to recover.
: DONOR-ANON-ROW ( n n -- ptr u8 n bool ) {: off:n bytes:n :}
   BOUND? 0= COL-KIND @ 0 < or COL-ORIGIN @ 0 < or if NULL$ false exit then
   CODE-N @ off FIRST-CODE ?do
      i CODE-ROW$ {: row:ptr rowu:n :}
      i CODE-START off <> if leave then
      row rowu ROW-LEN bytes =
      row rowu COL-KIND @ FIELD$ s" word" STR= and
      row rowu COL-ORIGIN @ FIELD$ s" baked" STR= and
      row rowu COL-NAME @ FIELD$ s" -" STR= 0= and if
         row rowu true unloop exit
      then
   loop
   CODE-N @ off FIRST-CODE ?do
      i CODE-ROW$ {: row:ptr rowu:n :}
      i CODE-START off <> if leave then
      row rowu ROW-LEN bytes =
      row rowu COL-KIND @ FIELD$ s" word" STR= and
      row rowu COL-ORIGIN @ FIELD$ s" baked" STR= and if
         row rowu true unloop exit
      then
   loop NULL$ false ;

: DONOR-ROW-NAME$ ( ptr u8 n -- ptr u8 n ) COL-NAME @ FIELD$ ;
: DONOR-ROW-WID ( ptr u8 n -- n ) COL-WID @ NUM-COL ;
: DONOR-ROW-SOURCE$ ( ptr u8 n -- ptr u8 n )
   COL-SOURCE @ 0 < if 2drop s" -" exit then
   COL-SOURCE @ FIELD$ ;

\ A stripped map has no dictionary WIDs. The linker tags donor-local positive
\ WIDs below -1, leaving 0 global and -1 unknown, so its own WIDs cannot
\ collide with namespace ids from the captured donor map.
: DONOR-WID ( n -- n ) {: wid:n :}
   wid 0 > if 0 wid - 1- exit then
   wid ;

: DONOR-NAMESPACES-EACH ( [ n n ptr u8 n -- ] -- )
   {: visit :}
   BOUND? 0= COL-KIND @ 0 < or if exit then
   ROWS @ POS !
   begin POS @ USED @ < while
      LINE$ {: row:ptr rowu:n :}
      row rowu COL-KIND @ FIELD$ s" namespace" STR= if
         row rowu COL-START @ NUM-COL DONOR-WID
         row rowu COL-LEN @ NUM-COL DONOR-WID
         row rowu COL-NAME @ FIELD$ visit execute
      then
   repeat ;

: SPAN-NAME$ ( n n -- ptr u8 n ) {: off:n size:n :}
   ROWS @ POS !
   begin POS @ USED @ < while
      LINE$ off size ROW$ dup 0 > if exit then 2drop
   repeat s" " ;

: NAME$ ( n -- ptr u8 n ) -1 SPAN-NAME$ ;

;package
