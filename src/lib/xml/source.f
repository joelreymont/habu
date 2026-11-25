\ source.f - XML source views and reversible lexical/original byte
\ coordinates. Part of package XML (src/lib/xml.f). UTF-8 and US-ASCII borrow
\ the original bytes; UTF-16 is converted to canonical UTF-8 in the source's
\ storage. The source takes its storage from the allocator it is given and
\ keeps it; SOURCE-CLOSE or OWNER-CLOSE frees it. On an arena's allocator the
\ free does nothing, and the arena's release ends a source nobody closed. The
\ original bytes are borrowed until then. Readers and editors close first;
\ nothing counts borrowers. After the close every copy of the owner is dead.
\ A rejected initialisation takes nothing.
\ SOURCE-INIT accepts UTF-8 with or without BOM; declared US-ASCII without BOM
\ (a byte above $7F anywhere, replacement markup included, is E-ENCODING;
\ numeric references stay ASCII); UTF-16 LE or BE with BOM and no or a generic
\ UTF-16 declaration; BOM-less UTF-16 starting `<?` with a matching UTF-16LE or
\ UTF-16BE declaration, checked by the first NEXT. Labels are case-insensitive
\ and the target `xml` lowercase (XML 1.0 section 4.3.3). Anything else, UTF-32
\ included, is E-ENCODING; bad surrogates are E-UTF16.
require src/lib/xml/scalar.f
require src/lib/vector.f
require src/lib/allocator.f
require src/lib/xml/reject.f

package XML
public
DEFLINEAR XML:source
ENUM encoding DERIVE eq utf8 utf16-le utf16-be us-ascii ;ENUM
-9216 s" UTF-16 input has an odd length or a lone or cut-off surrogate" ERROR:CODE E-UTF16
-9217 s" a lexical offset falls inside a UTF-8 sequence" ERROR:CODE E-BOUNDARY

private
\ UTF-8 and US-ASCII point lexical into original after the BOM and leave
\ marks and copy null; UTF-16 points it at copy's elements, which never grow.
STRUCTURE header DERIVE addr
   FIELD original str
   FIELD lexical str
   FIELD encoding encoding
   FIELD bom-len n
   FIELD allocator ^ALLOC:allocator
   FIELD marks ^VEC:vector<n>
   FIELD copy ^VEC:vector<u8>
;STRUCTURE

public
STRUCTURE source-owner OPAQUE FIELD state ^header ;STRUCTURE

private
\ Checkpoint k-1, marks element k-1, is the original offset of the first
\ scalar boundary at or after lexical offset k*SRC-MARK-STRIDE.
64 constant SRC-MARK-STRIDE

LINEAR: SRC-MINT ( ^header -- XML:source )
LINEAR: SRC-ERASE ( XML:source -- ^header )

: SRC-STATE ( XML:source -- XML:source ^header )
   SRC-ERASE dup SRC-MINT swap ;

: OWNER-STATE ( source-owner -- ^header )
   SOURCE-OWNER-UNMAKE ;

: SRC-ORIGINAL$ ( ^header -- str )
   HEADER-ORIGINAL @ ;

: SRC-LEXICAL$ ( ^header -- str )
   HEADER-LEXICAL @ ;

: SRC-ENCODING ( ^header -- encoding )
   HEADER-ENCODING @ ;

: SRC-BOM$ ( ^header -- str )
   {: state :}
   state HEADER-ORIGINAL @ STR:DATA state HEADER-BOM-LEN @ >str ;

: SRC-STARTS? ( str str -- bool )
   {: source:str signature:str :}
   source STR:LEN signature STR:LEN < if false exit then
   source STR:DATA signature STR:LEN >str signature BYTES= ;

: SRC-UNIT ( str n encoding -- n )
   {: source:str cursor:n encoding:encoding :}
   source STR:DATA cursor + c@ {: first:n :}
   source STR:DATA cursor 1+ + c@ {: second:n :}
   encoding XML-ENCODING:UTF16-LE XML-ENCODING:EQ if
      second 8 lshift first or
   else
      first 8 lshift second or
   then ;

: SRC-DECL-CHAR ( str n encoding n -- n )
   {: source:str bom:n encoding:encoding index:n :}
   encoding XML-ENCODING:UTF8 XML-ENCODING:EQ if 1 else 2 then
   {: width:n :}
   index 0 < index source STR:LEN bom - width / >= or if -1 exit then
   bom index width * + {: cursor:n :}
   width 1 = if source STR:DATA cursor + c@ else
      source cursor encoding SRC-UNIT then ;

: FOLD-ASCII ( n -- n )
   dup $41 >= over $5A <= and if $20 + then ;

: FOLD-EQUAL? ( str str -- bool )
   {: first:str second:str :}
   first STR:LEN second STR:LEN <> if false exit then
   first STR:LEN 0 ?do
      first STR:DATA i + c@ FOLD-ASCII second STR:DATA i + c@ FOLD-ASCII <> if
         false unloop exit
      then
   loop
   true ;

: SRC-DECL-MATCH? ( str n encoding n str -- bool )
   {: source:str bom:n encoding:encoding cursor:n expected:str :}
   expected STR:LEN 0 ?do
      source bom encoding cursor i + SRC-DECL-CHAR
      expected STR:DATA i + c@ <> if false unloop exit then
   loop
   true ;

: SRC-DECL-WHITE ( str n encoding n -- n )
   {: source:str bom:n encoding:encoding cursor:n :}
   cursor
   begin
      {: next:n :}
      source bom encoding next SRC-DECL-CHAR XML-SPACE? 0= if
         next exit
      then
      next 1+
   again ;

\ Read one quoted pseudoattribute value after its name; the reader validates
\ the full declaration grammar after source encoding has been selected.
: SRC-DECL-ASSIGN ( str n encoding n -- n n )
   {: source:str bom:n encoding:encoding cursor:n :}
   source bom encoding cursor SRC-DECL-WHITE {: equal:n :}
   source bom encoding equal SRC-DECL-CHAR $3D <> if -1 -1 exit then
   source bom encoding equal 1+ SRC-DECL-WHITE {: open:n :}
   source bom encoding open SRC-DECL-CHAR {: quote:n :}
   quote $22 <> quote $27 <> and if -1 -1 exit then
   open 1+ {: start:n :}
   start
   begin
      {: cursor:n :}
      source bom encoding cursor SRC-DECL-CHAR {: byte:n :}
      byte -1 = byte $3C = or byte $3E = or if -1 -1 exit then
      byte quote = if start cursor 1+ exit then
      cursor 1+
   again ;

\ The index, as SRC-DECL-CHAR counts, of the encoding label's first character
\ when the declaration names US-ASCII, else -1.
: SRC-DECL-ASCII ( str n encoding -- n )
   {: source:str bom:n encoding:encoding :}
   source bom encoding 0 s" <?xml" SRC-DECL-MATCH? 0= if -1 exit then
   source bom encoding 5 SRC-DECL-CHAR XML-SPACE? 0= if -1 exit then
   source bom encoding 5 SRC-DECL-WHITE {: version:n :}
   source bom encoding version s" version" SRC-DECL-MATCH? 0= if -1 exit then
   source bom encoding version 7 + SRC-DECL-ASSIGN
   {: value:n after:n :}
   value -1 = after value 1+ <= or if -1 exit then
   source bom encoding after SRC-DECL-CHAR XML-SPACE? 0= if -1 exit then
   source bom encoding after SRC-DECL-WHITE {: label:n :}
   source bom encoding label s" encoding" SRC-DECL-MATCH? 0= if -1 exit then
   source bom encoding label 8 + SRC-DECL-ASSIGN
   {: start:n end:n :}
   start -1 = end start - 1- 8 <> or if -1 exit then
   s" US-ASCII" STR:DATA {: ascii :}
   8 0 ?do
      source bom encoding start i + SRC-DECL-CHAR FOLD-ASCII
      ascii i + c@ FOLD-ASCII <> if -1 unloop exit then
   loop
   source bom encoding end SRC-DECL-CHAR dup XML-SPACE? swap $3F = or
   if start else -1 then ;

: DECL-ASCII-EXPECTED$ ( -- str )
   s" an encoding that matches the document's bytes, such as `UTF-8` or `UTF-16`" ;
: DECL-ASCII-FIX$ ( -- str )
   s" declare the encoding the document is saved in, or save it as US-ASCII without a byte order mark" ;

: SRC-CHECK-DECL ( str encoding n -- encoding n )
   {: source:str encoding:encoding bom:n :}
   source bom encoding SRC-DECL-ASCII {: label:n :}
   label 0< if encoding bom exit then
   encoding XML-ENCODING:UTF8 XML-ENCODING:EQ 0= if
      source bom label 2 * +
      encoding XML-ENCODING:UTF16-BE XML-ENCODING:EQ
      DECL-ASCII-EXPECTED$ DECL-ASCII-FIX$ E-ENCODING REJECT-UNITS
   then
   bom 0<> if
      source WHOLE bom label + DECL-ASCII-EXPECTED$
      DECL-ASCII-FIX$ E-ENCODING REJECT-FROM
   then
   XML-ENCODING:US-ASCII bom ;

\ Rejects UTF-16 with no byte order mark that does not begin `<?`, big-endian
\ when be is set.
: BOMLESS-START ( str bool -- )
   0 swap
   s" a byte order mark or `<?xml` at the start of a UTF-16 document"
   s" begin the document with a byte order mark or an XML declaration, or save it as UTF-8"
   E-ENCODING REJECT-UNITS ;

: SRC-DETECT ( str -- encoding n )
   {: source:str :}
   source s\" \z\z\xFE\xFF" SRC-STARTS?
   source s\" \xFF\xFE\z\z" SRC-STARTS? or
   source s\" \z\z\z<" SRC-STARTS? or
   source s\" <\z\z\z" SRC-STARTS? or if
      source WHOLE 0 s" a UTF-8 or UTF-16 document"
      s" save the document as UTF-8 or UTF-16" E-ENCODING
      REJECT-FROM
   then
   source s\" \xFF\xFE" SRC-STARTS? if
      source XML-ENCODING:UTF16-LE 2 SRC-CHECK-DECL exit
   then
   source s\" \xFE\xFF" SRC-STARTS? if
      source XML-ENCODING:UTF16-BE 2 SRC-CHECK-DECL exit
   then
   source s\" \xEF\xBB\xBF" SRC-STARTS? if
      source XML-ENCODING:UTF8 3 SRC-CHECK-DECL exit
   then
   source s\" <\z?\z" SRC-STARTS? if
      source XML-ENCODING:UTF16-LE 0 SRC-CHECK-DECL exit
   then
   source s\" \z<\z?" SRC-STARTS? if
      source XML-ENCODING:UTF16-BE 0 SRC-CHECK-DECL exit
   then
   source s\" <\z" SRC-STARTS? if source false BOMLESS-START then
   source s\" \z<" SRC-STARTS? if source true BOMLESS-START then
   source XML-ENCODING:UTF8 0 SRC-CHECK-DECL ;

\ Rejects at off, after a high surrogate, in UTF-16 bytes.
: UNPAIRED-HIGH ( str n bool -- )
   s" a low surrogate after the high surrogate"
   s" write the low surrogate after the high one, or remove the high surrogate"
   E-UTF16 REJECT-UNITS ;

: SRC-NEXT ( str n encoding -- n n )
   {: source:str cursor:n encoding:encoding :}
   encoding XML-ENCODING:UTF16-BE XML-ENCODING:EQ {: be:bool :}
   source STR:LEN cursor - 2 < if
      source cursor be s" a UTF-16 unit of two bytes"
      s" write the last character whole, or remove the stray byte"
      E-UTF16 REJECT-UNITS
   then
   source cursor encoding SRC-UNIT {: first:n :}
   first $DC00 >= first $DFFF <= and if
      source cursor be
      s" a character, or a high surrogate before a low one"
      s" write the high surrogate before the low one, or remove the low surrogate"
      E-UTF16 REJECT-UNITS
   then
   first $D800 >= first $DBFF <= and if
      source STR:LEN cursor - 4 < if source cursor 2 + be UNPAIRED-HIGH then
      source cursor 2 + encoding SRC-UNIT {: second:n :}
      second $DC00 < second $DFFF > or if
         source cursor 2 + be UNPAIRED-HIGH
      then
      first $D800 - 10 lshift second $DC00 - + $10000 +
      cursor 4 +
   else
      first cursor 2 +
   then
   {: scalar:n next:n :}
   scalar XML-SCALAR? 0= if
      source cursor be SCALAR-EXPECTED$ SCALAR-FIX$ E-SCALAR REJECT-UNITS
   then
   scalar next ;

: SRC-SCAN ( str n encoding -- n )
   {: source:str bom:n encoding:encoding :}
   encoding XML-ENCODING:US-ASCII XML-ENCODING:EQ if
      source ASCII-WIRE
      source STR:LEN 0 ?do
         source STR:DATA i + c@ {: byte:n :}
         source WHOLE i byte REQUIRE-SCALAR
      loop
      source STR:LEN exit
   then
   encoding XML-ENCODING:UTF8 XML-ENCODING:EQ if
      bom
      begin dup source STR:LEN < while
         {: cursor:n :}
         source WHOLE cursor SCALAR-AT nip
      repeat
      drop source STR:LEN bom - exit
   then
   0 bom
   begin dup source STR:LEN < while
      {: total:n cursor:n :}
      source cursor encoding SRC-NEXT
      {: scalar:n next:n :}
      scalar SCALAR-WIDTH {: width:n :}
      width MAX-SIZE total - > if E-CAPACITY throw then
      total width + next
   repeat
   drop ;

\ Answers the encoding, the BOM's bytes and the lexical length.
: SRC-VALIDATE ( str -- encoding n n )
   {: source:str :}
   source SPAN-CHECK
   source SRC-DETECT {: encoding:encoding bom:n :}
   source bom encoding SRC-SCAN {: lexical:n :}
   encoding bom lexical ;

\ Set each checkpoint from mark on whose stride point lies at or before the
\ scalar boundary at lexical to original, that boundary's original offset;
\ answer the next checkpoint to set.
: SRC-MARKS ( ^header n n n -- n )
   {: state mark:n lexical:n original:n :}
   mark
   begin dup SRC-MARK-STRIDE * lexical <= while
      {: next:n :}
      original state HEADER-MARKS @ next 1- VEC:AT !
      next 1+
   repeat ;

: SRC-WRITE-UTF16 ( ^header -- )
   {: state :}
   state SRC-ORIGINAL$ {: original:str :}
   state SRC-ENCODING {: encoding:encoding :}
   state HEADER-LEXICAL @ STR:DATA {: output :}
   output state HEADER-BOM-LEN @ 1
   begin over original STR:LEN < while
      {: target cursor:n mark:n :}
      state mark target output - cursor SRC-MARKS {: marked:n :}
      original cursor encoding SRC-NEXT
      {: scalar:n next:n :}
      scalar target PUT-SCALAR next marked
   repeat
   {: target cursor:n mark:n :}
   state mark target output - cursor SRC-MARKS drop ;

: SRC-INITIALIZE ( ^header str encoding n n -- )
   {: state original:str encoding:encoding bom:n lexical:n :}
   original state HEADER-ORIGINAL !
   encoding state HEADER-ENCODING !
   bom state HEADER-BOM-LEN !
   encoding XML-ENCODING:UTF8 XML-ENCODING:EQ
   encoding XML-ENCODING:US-ASCII XML-ENCODING:EQ or if
      original STR:DATA bom + lexical >str state HEADER-LEXICAL ! exit
   then
   state HEADER-ALLOCATOR @ {: al:^ :}
   al lexical SRC-MARK-STRIDE / [type] n VEC:NEW state HEADER-MARKS !
   al lexical [type] u8 VEC:NEW {: copy:^ :}
   copy state HEADER-COPY !
   copy VEC:BYTES state HEADER-LEXICAL !
   state SRC-WRITE-UTF16 ;

: SRC-FREE ( ^header -- )
   {: state :}
   state HEADER-MARKS @ VEC:DELETE
   state HEADER-COPY @ VEC:DELETE
   state HEADER-ALLOCATOR @ state ALLOC:DELETE ;

public
\ Validates the whole wire encoding and scalar range before it takes storage;
\ it does not check well-formedness.
: SOURCE-INIT ( ^ALLOC:allocator str -- XML:source )
   {: al:^ original:str :}
   original SRC-VALIDATE {: encoding:encoding bom:n lexical:n :}
   al [type] header ALLOC:NEW {: state :}
   al state HEADER-ALLOCATOR !
   [: state original encoding bom lexical SRC-INITIALIZE ;] catch {: code:n :}
   code 0<> if state SRC-FREE code throw then
   state SRC-MINT ;

: SOURCE-CLOSE ( XML:source -- )
   SRC-ERASE SRC-FREE ;

\ A one-cell XML:source-owner for movable records.
: SOURCE-OWN ( XML:source -- source-owner )
   SRC-ERASE SOURCE-OWNER-MAKE ;

: OWNER-CLOSE ( source-owner -- )
   OWNER-STATE SRC-FREE ;

: OWNER-ORIGINAL$ ( source-owner -- str )
   OWNER-STATE SRC-ORIGINAL$ ;

: OWNER-LEXICAL$ ( source-owner -- str )
   OWNER-STATE SRC-LEXICAL$ ;

: OWNER-ENCODING ( source-owner -- encoding )
   OWNER-STATE HEADER-ENCODING @ ;

: OWNER-BOM$ ( source-owner -- str )
   OWNER-STATE SRC-BOM$ ;

\ Includes the BOM; SOURCE-BOM$ is that prefix (0, 2 or 3 bytes) and
\ SOURCE-LEXICAL$ drops only it. A reader over a source reports lexical UTF-8
\ offsets only.
: SOURCE-ORIGINAL$ ( XML:source -- XML:source str )
   SRC-STATE SRC-ORIGINAL$ ;

: SOURCE-LEXICAL$ ( XML:source -- XML:source str )
   SRC-STATE SRC-LEXICAL$ ;

: SOURCE-ENCODING ( XML:source -- XML:source encoding )
   SRC-STATE HEADER-ENCODING @ ;

: SOURCE-BOM$ ( XML:source -- XML:source str )
   SRC-STATE SRC-BOM$ ;

private
\ SOURCE-INIT validated every scalar; only continuation bytes split one.
: SRC-CONTINUATION? ( str n -- bool )
   {: lexical:str cursor:n :}
   cursor 0<> cursor lexical STR:LEN <> and if
      lexical STR:DATA cursor + c@ $C0 and $80 = exit
   then
   false ;


: SRC-MARK-AT ( ^header n -- n n )
   {: state endpoint:n :}
   state SRC-LEXICAL$ {: lexical:str :}
   endpoint SRC-MARK-STRIDE / {: mark:n :}
   mark 0= if 0 state HEADER-BOM-LEN @ exit then
   mark SRC-MARK-STRIDE *
   begin dup lexical swap SRC-CONTINUATION? while 1+ repeat
   state HEADER-MARKS @ mark 1- VEC:AT @ ;


: SRC-MAP ( ^header n -- n )
   {: state endpoint:n :}
   state SRC-LEXICAL$ {: lexical:str :}
   lexical endpoint SRC-CONTINUATION? if E-BOUNDARY throw then
   state SRC-ENCODING XML-ENCODING:UTF8 XML-ENCODING:EQ
   state SRC-ENCODING XML-ENCODING:US-ASCII XML-ENCODING:EQ or if
      state HEADER-BOM-LEN @ endpoint + exit
   then
   state endpoint SRC-MARK-AT
   begin over endpoint < while
      {: cursor:n original:n :}
      lexical WHOLE cursor SCALAR-AT {: scalar:n next:n :}
      original scalar $10000 < if 2 else 4 then +
      next swap
   repeat
   nip ;

: SRC-RANGE ( ^header n n -- n n )
   {: state start:n count:n :}
   state SRC-LEXICAL$ STR:LEN {: size:n :}
   start 0 < count 0 < or if E-RANGE throw then
   start size > if E-RANGE throw then
   count size start - > if E-RANGE throw then
   state start SRC-MAP {: first:n :}
   state start count + SRC-MAP first -
   first swap ;

: SRC-RANGE-ARGS ( XML:source off len -- XML:source ^header n n )
   {: start:off count:len :}
   SRC-STATE start OFF>N count LEN>N ;

public
\ Maps a lexical half-open range on scalar boundaries to the original offsets
\ EDIT:REPLACE takes, BOM included, in time independent of offset and source
\ size: constant for UTF-8 and US-ASCII, at most SRC-MARK-STRIDE bytes walked
\ for UTF-16. A lexical cut off a UTF-8 boundary is E-BOUNDARY. Edit flow:
\ SOURCE-INIT, drain a reader to EOF, SOURCE-RANGE, ENCODE, EDIT:INIT on
\ SOURCE-ORIGINAL$, EDIT:REPLACE, EDIT:WRITE, reopen.
: SOURCE-RANGE ( XML:source off len -- XML:source off len )
   SRC-RANGE-ARGS SRC-RANGE >LEN swap >OFF swap ;

: OWNER-RANGE ( source-owner off len -- off len )
   {: owner start:off count:len :}
   owner OWNER-STATE start OFF>N count LEN>N SRC-RANGE
   >LEN swap >OFF swap ;

private
: SRC-ENCODE-SIZE ( ^header str -- n )
   {: state input:str :}
   input SPAN-CHECK
   state SRC-ENCODING XML-ENCODING:US-ASCII XML-ENCODING:EQ if
      input ASCII-WIRE
      input STR:LEN 0 ?do
         input WHOLE i input STR:DATA i + c@ REQUIRE-SCALAR
      loop
      input STR:LEN exit
   then
   state SRC-ENCODING XML-ENCODING:UTF8 XML-ENCODING:EQ if
      0
      begin dup input STR:LEN < while
         {: cursor:n :}
         input WHOLE cursor SCALAR-AT nip
      repeat
      drop input STR:LEN exit
   then
   0 0
   begin dup input STR:LEN < while
      {: total:n cursor:n :}
      input WHOLE cursor SCALAR-AT {: scalar:n next:n :}
      scalar $10000 < if 2 else 4 then {: width:n :}
      width MAX-SIZE total - > if E-CAPACITY throw then
      total width + next
   repeat
   drop ;

: SRC-ENCODE-ARGS ( XML:source str -- XML:source ^header str )
   {: input:str :}
   SRC-STATE input ;

public
\ ENCODE appends replacement markup, already escaped, in the wire encoding
\ without a BOM, or nothing when it rejects; the text may lie in the vector.
: ENCODE-SIZE ( XML:source str -- XML:source n )
   SRC-ENCODE-ARGS SRC-ENCODE-SIZE ;

: OWNER-ENCODE-SIZE ( source-owner str -- n )
   {: owner input:str :}
   owner OWNER-STATE input SRC-ENCODE-SIZE ;

private
: SRC-PUT-UNIT ( n encoding ^u8 -- ^u8 )
   {: unit:n encoding:encoding output :}
   encoding XML-ENCODING:UTF16-LE XML-ENCODING:EQ if
      unit $FF and output PUT-BYTE
      unit 8 rshift swap PUT-BYTE
   else
      unit 8 rshift output PUT-BYTE
      unit $FF and swap PUT-BYTE
   then ;

: SRC-ENCODE-WRITE ( ^header str ^u8 -- )
   {: state input:str output :}
   state SRC-ENCODING {: encoding:encoding :}
   encoding XML-ENCODING:UTF8 XML-ENCODING:EQ
   encoding XML-ENCODING:US-ASCII XML-ENCODING:EQ or if
      input STR:DATA output input STR:LEN >LEN BYTE-COPY-LEN exit
   then
   output 0
   begin dup input STR:LEN < while
      {: target cursor:n :}
      input WHOLE cursor SCALAR-AT {: scalar:n next:n :}
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

: SRC-ENCODE-INTO ( ^header str ^VEC:vector<u8> -- )
   {: state input:str v:^ :}
   state input SRC-ENCODE-SIZE {: needed:n :}
   v VEC:COUNT {: at:n :}
   v at needed + input STR:DATA VEC:MAKE-ROOM {: src:^ :}
   v at needed + VEC:ENSURE
   state src input STR:LEN >str  v VEC:BYTES STR:DATA at +  SRC-ENCODE-WRITE ;

: SRC-INTO-ARGS ( XML:source str ^VEC:vector<u8> -- XML:source ^header str ^VEC:vector<u8> )
   {: input:str v:^ :}
   SRC-STATE input v ;

public
: ENCODE ( XML:source str ^VEC:vector<u8> -- XML:source )
   SRC-INTO-ARGS SRC-ENCODE-INTO ;

: OWNER-ENCODE ( source-owner str ^VEC:vector<u8> -- )
   {: owner input:str v:^ :}
   owner OWNER-STATE input v SRC-ENCODE-INTO ;

;package
