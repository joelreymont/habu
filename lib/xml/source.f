\ Caller-owned XML source view and reversible lexical/original byte coordinates.
require lib/xml/scalar.f

package XML
public
DEFLINEAR XML:source
ENUM encoding utf8 utf16-le utf16-be ;ENUM
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

: SRC-DETECT ( ptr u8 n -- n n )
   {: source size:n :}
   source size s\" \z\z\xFE\xFF" SRC-STARTS?
   source size s\" \xFF\xFE\z\z" SRC-STARTS? or
   source size s\" \z\z\z<" SRC-STARTS? or
   source size s\" <\z\z\z" SRC-STARTS? or if E-ENCODING throw then
   source size s\" \xFF\xFE" SRC-STARTS? if
      XML-ENCODING:UTF16-LE ENCODING>N 2 exit
   then
   source size s\" \xFE\xFF" SRC-STARTS? if
      XML-ENCODING:UTF16-BE ENCODING>N 2 exit
   then
   source size s\" \xEF\xBB\xBF" SRC-STARTS? if
      XML-ENCODING:UTF8 ENCODING>N 3 exit
   then
   source size s\" <\z?\z" SRC-STARTS? if
      XML-ENCODING:UTF16-LE ENCODING>N 0 exit
   then
   source size s\" \z<\z?" SRC-STARTS? if
      XML-ENCODING:UTF16-BE ENCODING>N 0 exit
   then
   source size s\" <\z" SRC-STARTS?
   source size s\" \z<" SRC-STARTS? or if E-ENCODING throw then
   XML-ENCODING:UTF8 ENCODING>N 0 ;

: SRC-UNIT ( ptr u8 n n -- n )
   {: source cursor:n encoding:n :}
   source cursor + c@ {: first:n :}
   source cursor 1+ + c@ {: second:n :}
   encoding XML-ENCODING:UTF16-LE ENCODING>N = if
      second 8 lshift first or
   else
      first 8 lshift second or
   then ;

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
   encoding XML-ENCODING:UTF8 ENCODING>N = if
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
   encoding XML-ENCODING:UTF8 ENCODING>N = if
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
   0 mapped
   begin over endpoint < while
      {: cursor:n original:n :}
      lexical size cursor SCALAR-AT {: scalar:n next:n :}
      next endpoint > if E-BOUNDARY throw then
      state SRC-ENCODING-N XML-ENCODING:UTF8 ENCODING>N = if
         original next cursor - +
      else
         original scalar $10000 < if 2 else 4 then +
      then
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

private
: SRC-ENCODE-SIZE ( ptr n ptr u8 n -- n )
   {: state input size:n :}
   input size SPAN-CHECK
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
   encoding XML-ENCODING:UTF8 ENCODING>N = if
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
   state SRC-ENCODING-N XML-ENCODING:UTF8 ENCODING>N = if size exit then
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

;package
