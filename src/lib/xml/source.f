\ source.f - caller-owned XML source views and reversible lexical/original byte
\ coordinates. Part of package XML (src/lib/xml.f).
\
\ SOURCE-BYTES validates the whole wire encoding and scalar range and answers
\ the cell-aligned storage SOURCE-INIT needs; SOURCE-INIT validates again before
\ publishing. Neither checks well-formedness. UTF-8 and US-ASCII borrow the
\ original bytes; UTF-16 is converted to canonical UTF-8 in caller storage.
\ Storage and original bytes stay live until SOURCE-CLOSE, after every reader
\ and editor borrowing them has closed. SOURCE-OWN turns a source into a
\ one-cell XML:source-owner for movable records; the header, storage and bytes
\ stay put until OWNER-CLOSE, which invalidates every copy (later use and a
\ second close are E-STATE). Nothing counts borrowers: the caller sequences
\ close and use, and a failed initialisation publishes nothing.
\
\ SOURCE-ORIGINAL$ includes the BOM, SOURCE-BOM$ is that prefix (0, 2 or 3
\ bytes) and SOURCE-LEXICAL$ drops only it. A reader over a source reports
\ lexical UTF-8 offsets only. SOURCE-RANGE maps a lexical half-open range on
\ scalar boundaries to the original offsets EDIT:REPLACE takes, BOM included,
\ in time linear in the offset. ENCODE-SIZE and ENCODE write replacement
\ markup, already escaped, in the wire encoding without a BOM, validating
\ input, capacity and aliasing first. Edit flow: SOURCE-INIT, drain a reader to
\ EOF, SOURCE-RANGE, ENCODE, EDIT:INIT on SOURCE-ORIGINAL$, EDIT:REPLACE,
\ EDIT:WRITE, reopen.
\
\ Accepted wire forms: UTF-8 with or without BOM; declared US-ASCII without BOM
\ (a byte above $7F anywhere, replacement markup included, is E-ENCODING;
\ numeric references stay ASCII); UTF-16 LE or BE with BOM and no or a generic
\ UTF-16 declaration; BOM-less UTF-16 starting `<?` with a matching UTF-16LE or
\ UTF-16BE declaration, checked by the first NEXT. Labels are case-insensitive
\ and the target `xml` lowercase (XML 1.0 section 4.3.3). Anything else, UTF-32
\ included, is E-ENCODING; bad surrogates are E-UTF16 and a lexical cut off a
\ UTF-8 boundary E-BOUNDARY.
require src/lib/xml/scalar.f

package XML
public
DEFLINEAR XML:source
STRUCTURE source-owner OPAQUE FIELD state ptr n ;STRUCTURE
ENUM encoding utf8 utf16-le utf16-be us-ascii ;ENUM
-9216 constant E-UTF16
-9217 constant E-BOUNDARY

private
CAST: ENCODING>N ( encoding -- n )
CAST: >ENCODING ( n -- encoding )

7 constant SRC-HEADER-CELLS
0 constant SRC-ORIGINAL-IDX
1 cells constant SRC-ORIGINAL-LEN
2 constant SRC-LEXICAL-IDX
3 cells constant SRC-LEXICAL-LEN
4 cells constant SRC-ENCODING-IDX
5 cells constant SRC-BOM-LEN
6 cells constant SRC-STORAGE-LEN

\ The token carries the caller-owned header; every span stays caller-owned.
LINEAR: SRC-MINT ( ptr n -- XML:source )
LINEAR: SRC-ERASE ( XML:source -- ptr n )

: SRC-STATE ( XML:source -- XML:source ptr n )
   SRC-ERASE dup SRC-MINT swap ;

: OWNER-STATE ( source-owner -- ptr n )
   SOURCE-OWNER-UNMAKE
   dup 0= if E-STATE throw then
   dup SRC-STORAGE-LEN + @ 0= if E-STATE throw then ;

: SRC-ORIGINAL$ ( ptr n -- ptr u8 n )
   {: state :}
   state SRC-ORIGINAL-IDX ptr-field @ state SRC-ORIGINAL-LEN + @ ;

: SRC-LEXICAL$ ( ptr n -- ptr u8 n )
   {: state :}
   state SRC-LEXICAL-IDX ptr-field @ state SRC-LEXICAL-LEN + @ ;

: SRC-ENCODING-N ( ptr n -- n )
   SRC-ENCODING-IDX + @ ;

: SRC-BOM$ ( ptr n -- ptr u8 n )
   {: state :}
   state SRC-ORIGINAL-IDX ptr-field @ state SRC-BOM-LEN + @ ;

: SRC-STARTS? ( ptr u8 n ptr u8 n -- bool )
   {: source size:n signature count:n :}
   size count < if false exit then
   source count signature count BYTES= ;

: SRC-UNIT ( ptr u8 n n -- n )
   {: source cursor:n encoding:n :}
   source cursor + c@ {: first:n :}
   source cursor 1+ + c@ {: second:n :}
   encoding XML-ENCODING:UTF16-LE ENCODING>N = if
      second 8 lshift first or
   else
      first 8 lshift second or
   then ;

: SRC-DECL-CHAR ( ptr u8 n n n n -- n )
   {: source size:n bom:n encoding:n index:n :}
   encoding XML-ENCODING:UTF8 ENCODING>N = if 1 else 2 then
   {: width:n :}
   index 0 < index size bom - width / >= or if -1 exit then
   bom index width * + {: cursor:n :}
   width 1 = if source cursor + c@ else
      source cursor encoding SRC-UNIT then ;

: FOLD-ASCII ( n -- n )
   dup $41 >= over $5A <= and if $20 + then ;

: FOLD-EQUAL? ( ptr u8 n ptr u8 n -- bool )
   {: first first-size:n second second-size:n :}
   first-size second-size <> if false exit then
   first-size 0 ?do
      first i + c@ FOLD-ASCII second i + c@ FOLD-ASCII <> if
         false unloop exit
      then
   loop
   true ;

: SRC-DECL-MATCH? ( ptr u8 n n n n ptr u8 n -- bool )
   {: source size:n bom:n encoding:n cursor:n expected count:n :}
   count 0 ?do
      source size bom encoding cursor i + SRC-DECL-CHAR
      expected i + c@ <> if false unloop exit then
   loop
   true ;

: SRC-DECL-WHITE ( ptr u8 n n n n -- n )
   {: source size:n bom:n encoding:n cursor:n :}
   cursor
   begin
      {: next:n :}
      source size bom encoding next SRC-DECL-CHAR XML-SPACE? 0= if
         next exit
      then
      next 1+
   again ;

\ Read one quoted pseudoattribute value after its name; the reader validates
\ the full declaration grammar after source encoding has been selected.
: SRC-DECL-ASSIGN ( ptr u8 n n n n -- n n )
   {: source size:n bom:n encoding:n cursor:n :}
   source size bom encoding cursor SRC-DECL-WHITE {: equal:n :}
   source size bom encoding equal SRC-DECL-CHAR $3D <> if -1 -1 exit then
   source size bom encoding equal 1+ SRC-DECL-WHITE {: open:n :}
   source size bom encoding open SRC-DECL-CHAR {: quote:n :}
   quote $22 <> quote $27 <> and if -1 -1 exit then
   open 1+ {: start:n :}
   start
   begin
      {: cursor:n :}
      source size bom encoding cursor SRC-DECL-CHAR {: byte:n :}
      byte -1 = byte $3C = or byte $3E = or if -1 -1 exit then
      byte quote = if start cursor 1+ exit then
      cursor 1+
   again ;

: SRC-DECL-ASCII? ( ptr u8 n n n -- bool )
   {: source size:n bom:n encoding:n :}
   source size bom encoding 0 s" <?xml" SRC-DECL-MATCH? 0= if false exit then
   source size bom encoding 5 SRC-DECL-CHAR XML-SPACE? 0= if false exit then
   source size bom encoding 5 SRC-DECL-WHITE {: version:n :}
   source size bom encoding version s" version" SRC-DECL-MATCH? 0= if false exit then
   source size bom encoding version 7 + SRC-DECL-ASSIGN
   {: value:n after:n :}
   value -1 = after value 1+ <= or if false exit then
   source size bom encoding after SRC-DECL-CHAR XML-SPACE? 0= if false exit then
   source size bom encoding after SRC-DECL-WHITE {: label:n :}
   source size bom encoding label s" encoding" SRC-DECL-MATCH? 0= if false exit then
   source size bom encoding label 8 + SRC-DECL-ASSIGN
   {: start:n end:n :}
   start -1 = end start - 1- 8 <> or if false exit then
   s" US-ASCII" drop {: ascii :}
   8 0 ?do
      source size bom encoding start i + SRC-DECL-CHAR FOLD-ASCII
      ascii i + c@ FOLD-ASCII <> if false unloop exit then
   loop
   source size bom encoding end SRC-DECL-CHAR dup XML-SPACE? swap $3F = or ;

: SRC-CHECK-DECL ( ptr u8 n n n -- n n )
   {: source size:n encoding:n bom:n :}
   source size bom encoding SRC-DECL-ASCII? if
      encoding XML-ENCODING:UTF8 ENCODING>N <> bom 0<> or if
         E-ENCODING throw
      then
      XML-ENCODING:US-ASCII ENCODING>N bom exit
   then
   encoding bom ;

: SRC-DETECT ( ptr u8 n -- n n )
   {: source size:n :}
   source size s\" \z\z\xFE\xFF" SRC-STARTS?
   source size s\" \xFF\xFE\z\z" SRC-STARTS? or
   source size s\" \z\z\z<" SRC-STARTS? or
   source size s\" <\z\z\z" SRC-STARTS? or if E-ENCODING throw then
   source size s\" \xFF\xFE" SRC-STARTS? if
      source size XML-ENCODING:UTF16-LE ENCODING>N 2 SRC-CHECK-DECL exit
   then
   source size s\" \xFE\xFF" SRC-STARTS? if
      source size XML-ENCODING:UTF16-BE ENCODING>N 2 SRC-CHECK-DECL exit
   then
   source size s\" \xEF\xBB\xBF" SRC-STARTS? if
      source size XML-ENCODING:UTF8 ENCODING>N 3 SRC-CHECK-DECL exit
   then
   source size s\" <\z?\z" SRC-STARTS? if
      source size XML-ENCODING:UTF16-LE ENCODING>N 0 SRC-CHECK-DECL exit
   then
   source size s\" \z<\z?" SRC-STARTS? if
      source size XML-ENCODING:UTF16-BE ENCODING>N 0 SRC-CHECK-DECL exit
   then
   source size s\" <\z" SRC-STARTS?
   source size s\" \z<" SRC-STARTS? or if E-ENCODING throw then
   source size XML-ENCODING:UTF8 ENCODING>N 0 SRC-CHECK-DECL ;

: SRC-NEXT ( ptr u8 n n n -- n n )
   {: source size:n cursor:n encoding:n :}
   size cursor - 2 < if E-UTF16 throw then
   source cursor encoding SRC-UNIT {: first:n :}
   first $DC00 >= first $DFFF <= and if E-UTF16 throw then
   first $D800 >= first $DBFF <= and if
      size cursor - 4 < if E-UTF16 throw then
      source cursor 2 + encoding SRC-UNIT {: second:n :}
      second $DC00 < second $DFFF > or if E-UTF16 throw then
      first $D800 - 10 lshift second $DC00 - + $10000 +
      cursor 4 +
   else
      first cursor 2 +
   then
   {: scalar:n next:n :}
   scalar REQUIRE-SCALAR
   scalar next ;

: SRC-SCAN ( ptr u8 n n n -- n )
   {: source size:n bom:n encoding:n :}
   encoding XML-ENCODING:US-ASCII ENCODING>N = if
      source size ASCII-WIRE
      size 0 ?do
         source i + c@ {: byte:n :}
         byte REQUIRE-SCALAR
      loop
      size exit
   then
   encoding XML-ENCODING:UTF8 ENCODING>N = if
      bom
      begin dup size < while
         source size rot SCALAR-AT nip
      repeat
      drop size bom - exit
   then
   size bom - 1 and 0<> if E-UTF16 throw then
   0 bom
   begin dup size < while
      {: total:n cursor:n :}
      source size cursor encoding SRC-NEXT
      {: scalar:n next:n :}
      scalar SCALAR-WIDTH {: width:n :}
      width MAX-SIZE total - > if E-CAPACITY throw then
      total width + next
   repeat
   drop ;

: SRC-BYTES ( ptr u8 n -- n n n )
   {: source size:n :}
   source size SPAN-CHECK
   source size SRC-DETECT {: encoding:n bom:n :}
   source size bom encoding SRC-SCAN {: lexical:n :}
   encoding XML-ENCODING:UTF8 ENCODING>N =
   encoding XML-ENCODING:US-ASCII ENCODING>N = or if
      SRC-HEADER-CELLS cells encoding bom exit
   then
   lexical MAX-SIZE SRC-HEADER-CELLS cells - CELL 1- - > if
      E-CAPACITY throw
   then
   lexical CELL 1- + CELL 1- invert and
   SRC-HEADER-CELLS cells + encoding bom ;

: SRC-STORAGE-CHECK ( ptr n n ptr u8 n n -- )
   {: storage cap:n original size:n needed:n :}
   storage BYTE-VIEW cap SPAN-CHECK
   storage BYTE-VIEW BYTE-ADDRESS CELL 1- and 0<> if E-STORAGE throw then
   cap needed < if E-CAPACITY throw then
   original size storage BYTE-VIEW cap OVERLAP? if E-ALIAS throw then ;

: SRC-WRITE-UTF16 ( ptr u8 n n n ptr u8 -- )
   {: original size:n bom:n encoding:n output :}
   output bom
   begin dup size < while
      {: target cursor:n :}
      original size cursor encoding SRC-NEXT
      {: scalar:n next:n :}
      scalar target PUT-SCALAR next
   repeat
   2drop ;

: SRC-INITIALIZE ( ptr n n ptr u8 n n n -- )
   {: storage cap:n original size:n encoding:n bom:n :}
   original storage SRC-ORIGINAL-IDX ptr-field !
   size storage SRC-ORIGINAL-LEN + !
   encoding storage SRC-ENCODING-IDX + !
   bom storage SRC-BOM-LEN + !
   cap storage SRC-STORAGE-LEN + !
   encoding XML-ENCODING:UTF8 ENCODING>N =
   encoding XML-ENCODING:US-ASCII ENCODING>N = or if
      original bom + storage SRC-LEXICAL-IDX ptr-field !
      size bom - storage SRC-LEXICAL-LEN + !
   else
      storage SRC-HEADER-CELLS cells + BYTE-VIEW
      storage SRC-LEXICAL-IDX ptr-field !
      original size bom encoding
      storage SRC-LEXICAL-IDX ptr-field @ SRC-WRITE-UTF16
      original size bom encoding SRC-SCAN
      storage SRC-LEXICAL-LEN + !
   then ;

public
: SOURCE-BYTES ( ptr u8 n -- n )
   SRC-BYTES 2drop ;

: SOURCE-INIT ( ptr n n ptr u8 n -- XML:source )
   {: storage cap:n original size:n :}
   original size SRC-BYTES {: needed:n encoding:n bom:n :}
   storage cap original size needed SRC-STORAGE-CHECK
   storage cap original size encoding bom SRC-INITIALIZE
   storage SRC-MINT ;

: SOURCE-CLOSE ( XML:source -- )
   SRC-ERASE drop ;

: SOURCE-OWN ( XML:source -- source-owner )
   SRC-ERASE SOURCE-OWNER-MAKE ;

: OWNER-CLOSE ( source-owner -- )
   OWNER-STATE SRC-STORAGE-LEN + 0 swap ! ;

: OWNER-ORIGINAL$ ( source-owner -- ptr u8 n )
   OWNER-STATE SRC-ORIGINAL$ ;

: OWNER-LEXICAL$ ( source-owner -- ptr u8 n )
   OWNER-STATE SRC-LEXICAL$ ;

: OWNER-ENCODING ( source-owner -- encoding )
   OWNER-STATE SRC-ENCODING-N >ENCODING ;

: OWNER-BOM$ ( source-owner -- ptr u8 n )
   OWNER-STATE SRC-BOM$ ;

: SOURCE-ORIGINAL$ ( XML:source -- XML:source ptr u8 n )
   SRC-STATE SRC-ORIGINAL$ ;

: SOURCE-LEXICAL$ ( XML:source -- XML:source ptr u8 n )
   SRC-STATE SRC-LEXICAL$ ;

: SOURCE-ENCODING ( XML:source -- XML:source encoding )
   SRC-STATE SRC-ENCODING-N >ENCODING ;

: SOURCE-BOM$ ( XML:source -- XML:source ptr u8 n )
   SRC-STATE SRC-BOM$ ;

private
: SRC-MAP ( ptr n n -- n )
   {: state endpoint:n :}
   state SRC-LEXICAL$ {: lexical size:n :}
   state SRC-BOM-LEN + @ {: mapped:n :}
   state SRC-ENCODING-N XML-ENCODING:UTF8 ENCODING>N =
   state SRC-ENCODING-N XML-ENCODING:US-ASCII ENCODING>N = or if
      \ SOURCE-INIT validated every scalar; only continuation bytes split one.
      endpoint 0<> endpoint size <> and if
         lexical endpoint + c@ $C0 and $80 = if E-BOUNDARY throw then
      then
      mapped endpoint + exit
   then
   0 mapped
   begin over endpoint < while
      {: cursor:n original:n :}
      lexical size cursor SCALAR-AT {: scalar:n next:n :}
      next endpoint > if E-BOUNDARY throw then
      original scalar $10000 < if 2 else 4 then +
      next swap
   repeat
   nip ;

: SRC-RANGE ( ptr n n n -- n n )
   {: state start:n count:n :}
   state SRC-LEXICAL$ nip {: size:n :}
   start 0 < count 0 < or if E-RANGE throw then
   start size > if E-RANGE throw then
   count size start - > if E-RANGE throw then
   state start SRC-MAP {: first:n :}
   state start count + SRC-MAP first -
   first swap ;

: SRC-RANGE-ARGS ( XML:source off len -- XML:source ptr n n n )
   {: start:off count:len :}
   SRC-STATE start OFF>N count LEN>N ;

public
: SOURCE-RANGE ( XML:source off len -- XML:source off len )
   SRC-RANGE-ARGS SRC-RANGE >LEN swap >OFF swap ;

: OWNER-RANGE ( source-owner off len -- off len )
   {: owner start:off count:len :}
   owner OWNER-STATE start OFF>N count LEN>N SRC-RANGE
   >LEN swap >OFF swap ;

private
: SRC-ENCODE-SIZE ( ptr n ptr u8 n -- n )
   {: state input size:n :}
   input size SPAN-CHECK
   state SRC-ENCODING-N XML-ENCODING:US-ASCII ENCODING>N = if
      input size ASCII-WIRE
      size 0 ?do input i + c@ REQUIRE-SCALAR loop
      size exit
   then
   state SRC-ENCODING-N XML-ENCODING:UTF8 ENCODING>N = if
      0
      begin dup size < while
         input size rot SCALAR-AT nip
      repeat
      drop size exit
   then
   0 0
   begin dup size < while
      {: total:n cursor:n :}
      input size cursor SCALAR-AT {: scalar:n next:n :}
      scalar $10000 < if 2 else 4 then {: width:n :}
      width MAX-SIZE total - > if E-CAPACITY throw then
      total width + next
   repeat
   drop ;

: SRC-ENCODE-ARGS ( XML:source ptr u8 n -- XML:source ptr n ptr u8 n )
   {: input size:n :}
   SRC-STATE input size ;

public
: ENCODE-SIZE ( XML:source ptr u8 n -- XML:source n )
   SRC-ENCODE-ARGS SRC-ENCODE-SIZE ;

: OWNER-ENCODE-SIZE ( source-owner ptr u8 n -- n )
   {: owner input size:n :}
   owner OWNER-STATE input size SRC-ENCODE-SIZE ;

private
: SRC-PUT-UNIT ( n n ptr u8 -- ptr u8 )
   {: unit:n encoding:n output :}
   encoding XML-ENCODING:UTF16-LE ENCODING>N = if
      unit $FF and output PUT-BYTE
      unit 8 rshift swap PUT-BYTE
   else
      unit 8 rshift output PUT-BYTE
      unit $FF and swap PUT-BYTE
   then ;

: SRC-ENCODE-WRITE ( ptr n ptr u8 n ptr u8 -- )
   {: state input size:n output :}
   state SRC-ENCODING-N {: encoding:n :}
   encoding XML-ENCODING:UTF8 ENCODING>N =
   encoding XML-ENCODING:US-ASCII ENCODING>N = or if
      input output size >LEN BYTE-COPY-LEN exit
   then
   output 0
   begin dup size < while
      {: target cursor:n :}
      input size cursor SCALAR-AT {: scalar:n next:n :}
      scalar $10000 < if
         scalar encoding target SRC-PUT-UNIT
      else
         scalar $10000 - {: pair:n :}
         pair 10 rshift $D800 + encoding target SRC-PUT-UNIT
         {: after :}
         pair $3FF and $DC00 + encoding after SRC-PUT-UNIT
      then
      next
   repeat
   2drop ;

\ The fewest bytes size bytes of UTF-8 encode to: all of them in UTF-8, and in
\ UTF-16 two for every three, the width of a three-byte scalar.
: SRC-ENCODE-FLOOR ( ptr n n -- n ) {: state size:n :}
   state SRC-ENCODING-N XML-ENCODING:UTF8 ENCODING>N =
   state SRC-ENCODING-N XML-ENCODING:US-ASCII ENCODING>N = or if
      size exit
   then
   size 3 / 2 * ;

: SRC-ENCODE-INTO ( ptr n ptr u8 n ptr u8 n -- n )
   {: state input size:n output cap:n :}
   input size SPAN-CHECK
   output cap SPAN-CHECK
   state size SRC-ENCODE-FLOOR cap > if E-CAPACITY throw then   \ measured before the input is read
   state input size SRC-ENCODE-SIZE {: needed:n :}
   needed cap > if E-CAPACITY throw then
   input size output cap OVERLAP? if E-ALIAS throw then
   state SRC-ORIGINAL$ output cap OVERLAP? if E-ALIAS throw then
   state BYTE-VIEW state SRC-STORAGE-LEN + @
   output cap OVERLAP? if E-ALIAS throw then
   state input size output SRC-ENCODE-WRITE
   needed ;

: SRC-INTO-ARGS ( XML:source ptr u8 n ptr u8 n -- XML:source ptr n ptr u8 n ptr u8 n )
   {: input size:n output cap:n :}
   SRC-STATE input size output cap ;

public
: ENCODE ( XML:source ptr u8 n ptr u8 n -- XML:source n )
   SRC-INTO-ARGS SRC-ENCODE-INTO ;

: OWNER-ENCODE ( source-owner ptr u8 n ptr u8 n -- n )
   {: owner input size:n output cap:n :}
   owner OWNER-STATE input size output cap SRC-ENCODE-INTO ;

;package
