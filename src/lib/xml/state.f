\ state.f - XML reader state: INIT, INIT-INPUT, INIT-SOURCE, CLOSE, DISPOSE and the
\ accessors; part of package XML (src/lib/xml.f). Offsets are absolute bytes of the
\ stream, a BOM included. Open frames' names and live bindings are copied once into
\ scope storage, so releasing consumed bytes never revisits them.

require src/lib/xml/source.f
require src/lib/utf8-scalar.f
require src/lib/vector.f
require src/lib/allocator.f
require src/lib/siphash.f
require src/lib/entropy.f

package XML
public
DEFLINEAR XML:reader
ENUM kind DERIVE eq start end text comment pi cdata eof ;ENUM

private
STRUCTURE frame DERIVE addr
   FIELD name n
   FIELD name-u n
   FIELD ns-count n
   FIELD uri n
   FIELD uri-u n
;STRUCTURE

\ link is the older attribute in its bucket, and head the head of the bucket
\ its own index numbers.
STRUCTURE attribute DERIVE addr
   FIELD name n
   FIELD name-u n
   FIELD value n
   FIELD value-u n
   FIELD raw n
   FIELD raw-u n
   FIELD uri n
   FIELD uri-u n
   FIELD name-hash n
   FIELD link n
   FIELD head n
;STRUCTURE

\ link is the older binding in its prefix bucket, and head the head of the
\ bucket its own index numbers.
STRUCTURE binding DERIVE addr
   FIELD prefix n
   FIELD prefix-u n
   FIELD uri n
   FIELD uri-u n
   FIELD uri-hash n
   FIELD link n
   FIELD head n
;STRUCTURE

ENUM pending DERIVE eq none pop-next empty-end ;ENUM

\ The most bytes one refill asks an input reader's fill for.
4096 constant CHUNK

\ CLOSE releases an input reader's stream and its buffers, after a failed NEXT
\ too. Line and column are those of the window's first byte in the whole input.
STRUCTURE stream DERIVE addr
   FIELD fill [ ^n SPAN:span<u8> -- n ]
   FIELD ctx ^n
   FIELD eof n
   FIELD base n
   FIELD started n
   FIELD input ^VEC:vector<u8>
   FIELD token ^VEC:vector<u8>
   FIELD scope ^VEC:vector<u8>
   FIELD line n
   FIELD column n
;STRUCTURE

ENUM mode DERIVE eq legacy-utf8 source-utf8 source-utf16-le-bom
   source-utf16-be-bom source-utf16-le-decl source-utf16-be-decl input-utf8
   source-ascii legacy-ascii input-ascii
;ENUM

\ The reader's whole fixed state. The tables and an input reader's stream it
\ points to are storage the reader owns; sip is scratch for one hash.
public
STRUCTURE cursor OPAQUE DERIVE addr
   FIELD source ^u8
   FIELD source-len n
   FIELD position n
   FIELD open-depth n
   FIELD ns-count n
   FIELD attr-count n
   FIELD attr-index n
   FIELD raw-off n
   FIELD raw-len n
   FIELD token-kind kind
   FIELD name-off n
   FIELD name-len n
   FIELD value-off n
   FIELD value-len n
   FIELD uri-off n
   FIELD uri-len n
   FIELD pending pending
   FIELD root-count n
   FIELD failed bool
   FIELD source-mode mode
   FIELD stream ^stream
   FIELD head-columns n
   FIELD window-kind counting
   FIELD allocator ^ALLOC:allocator
   FIELD frames ^VEC:vector<frame>
   FIELD attrs ^VEC:vector<attribute>
   FIELD namespaces ^VEC:vector<binding>
   FIELD hash-k0 n
   FIELD hash-k1 n
   FIELD sip SIPHASH:state
;STRUCTURE

private
8 constant TABLE-START

: INPUT? ( ^cursor -- bool )
   CURSOR-SOURCE-MODE @ dup MODE-INPUT-UTF8 MODE-EQ
   swap MODE-INPUT-ASCII MODE-EQ or ;

: ABS-OFF ( ^cursor n -- n )
   {: state offset:n :}
   state INPUT? if state CURSOR-STREAM @ STREAM-BASE @ offset + else offset then ;

: INPUT-VEC ( ^stream -- ^VEC:vector<u8> ) STREAM-INPUT @ ;
: TOKEN-VEC ( ^stream -- ^VEC:vector<u8> ) STREAM-TOKEN @ ;
: SCOPE-VEC ( ^stream -- ^VEC:vector<u8> ) STREAM-SCOPE @ ;

: SCOPE$ ( ^cursor -- str )
   CURSOR-STREAM @ SCOPE-VEC VEC:SPAN SPAN:$ ;

\ A vector not yet made is NULL-PTR, which DELETE ignores.
: STREAM-CLOSE ( ^ALLOC:allocator ^stream -- )
   {: al:^ stream :}
   stream INPUT-VEC VEC:DELETE
   stream TOKEN-VEC VEC:DELETE
   stream SCOPE-VEC VEC:DELETE
   al stream ALLOC:DELETE ;

: STREAM-OPEN-VECS ( ^ALLOC:allocator ^stream -- )
   {: al:^ stream :}
   al CHUNK [type] u8 VEC:NEW stream STREAM-INPUT !
   al 0 [type] u8 VEC:NEW stream STREAM-TOKEN !
   stream TOKEN-VEC CHUNK VEC:RESERVE
   al 0 [type] u8 VEC:NEW stream STREAM-SCOPE ! ;

: STREAM-OPEN ( ^ALLOC:allocator ^n [ ^n SPAN:span<u8> -- n ]
                -- ^stream )
   {: al:^ ctx fill :}
   al [type] stream ALLOC:NEW {: stream :}
   fill stream STREAM-FILL !
   ctx stream STREAM-CTX !
   1 stream STREAM-LINE !
   1 stream STREAM-COLUMN !
   [: al stream STREAM-OPEN-VECS ;] catch {: code:n :}
   code 0<> if al stream STREAM-CLOSE code throw then
   stream ;

-1 constant XML-URI
-2 constant XMLNS-URI

\ A reader token is its cursor record; only these rows cross it.
LINEAR: MINT ( ^cursor -- XML:reader )
LINEAR: ERASE ( XML:reader -- ^cursor )

: STATE ( XML:reader -- XML:reader ^cursor )
   ERASE dup MINT swap ;

: FRAME ( ^cursor n -- ^frame )
   swap CURSOR-FRAMES @ swap VEC:AT ;

: ATTRIBUTE ( ^cursor n -- ^attribute )
   swap CURSOR-ATTRS @ swap VEC:AT ;

: NAMESPACE ( ^cursor n -- ^binding )
   swap CURSOR-NAMESPACES @ swap VEC:AT ;

\ Doubles the slots. Records move, so no caller keeps a record's address
\ across a growth; a failed growth leaves the table as it was.
: GROW ( ^VEC:vector<t> n -- )
   {: table want:n :}
   table VEC:COUNT {: cap:n :}
   want cap <= if exit then
   table cap 2 * TABLE-START max want max VEC:RESIZE ;

: FRAMES-GROW ( ^cursor n -- ) swap CURSOR-FRAMES @ swap GROW ;
: ATTRS-GROW ( ^cursor n -- ) swap CURSOR-ATTRS @ swap GROW ;
: NS-GROW ( ^cursor n -- ) swap CURSOR-NAMESPACES @ swap GROW ;

\ Empty every prefix bucket: src/lib/xml/names.f relinks the bindings.
: NS-HEADS-CLEAR ( ^cursor -- )
   {: state :}
   state CURSOR-NAMESPACES @ VEC:COUNT 0 ?do
      -1 state i NAMESPACE BINDING-HEAD !
   loop ;

: SOURCE$ ( ^cursor -- str )
   {: state :}
   state CURSOR-SOURCE @ state CURSOR-SOURCE-LEN @ >str ;

: UTF16-SOURCE? ( mode -- bool )
   dup MODE-SOURCE-UTF16-LE-BOM MODE-EQ over MODE-SOURCE-UTF16-BE-BOM MODE-EQ or
   over MODE-SOURCE-UTF16-LE-DECL MODE-EQ or
   swap MODE-SOURCE-UTF16-BE-DECL MODE-EQ or ;

\ An input reader's input follows the window until its fill has answered 0; an INIT
\ reader's window starts at line 1, column 1, in bytes.
: WINDOW-PLACE ( ^cursor -- place )
   {: state :}
   state INPUT? 0= if
      1 state CURSOR-HEAD-COLUMNS @ 1+ state CURSOR-WINDOW-KIND @ PLACE-MAKE exit
   then
   state CURSOR-STREAM @ {: stream :}
   stream STREAM-LINE @ stream STREAM-COLUMN @
   stream STREAM-EOF @ 0= if COUNTING-MORE else COUNTING-BYTES then PLACE-MAKE ;

\ Opens the text of a rejection at off in the window, up to its expected part.
: REJECT-HEAD ( ^cursor n -- ) {: state off:n :}
   state SOURCE$ state WINDOW-PLACE off HEAD-FROM ;

\ Writes the found part at off in the window and the fix, and throws code with
\ the text.
: REJECT-TAIL ( ^cursor n str n -- ) {: state off:n fs:str code:n :}
   state SOURCE$ state WINDOW-PLACE off fs code TAIL-FROM ;

\ Rejects at off in the window, placed in the whole input.
: REJECT ( ^cursor n str str n -- )
   {: state off:n es:str fs:str code:n :}
   state SOURCE$ state WINDOW-PLACE off es fs code REJECT-FROM ;

\ Rejects the first byte above $7F in the window from off on.
: ASCII-WINDOW ( ^cursor n -- )
   {: state off:n :}
   state SOURCE$ {: window:str :}
   window STR:DATA off + window STR:LEN off - >str HIGH-BYTE off + {: high:n :}
   high window STR:LEN < if
      state high ASCII-EXPECTED$ ASCII-FIX$ E-ENCODING REJECT
   then ;

: INPUT-REFILL ( ^cursor -- bool )
   {: state :}
   state CURSOR-STREAM @ {: stream :}
   stream STREAM-EOF @ 0<> if false exit then
   stream INPUT-VEC VEC:SPAN {: input :}
   stream STREAM-CTX @ input stream STREAM-FILL @ execute {: count:n :}
   count 0 < count CHUNK > or if E-RANGE throw then
   count 0= if 1 stream STREAM-EOF ! false exit then
   input SPAN:$ STR:DATA count >str stream TOKEN-VEC VEC:APPEND
   stream TOKEN-VEC VEC:SPAN SPAN:$
   dup STR:LEN state CURSOR-SOURCE-LEN ! STR:DATA state CURSOR-SOURCE !
   state CURSOR-SOURCE-MODE @ MODE-INPUT-ASCII MODE-EQ if
      state state CURSOR-SOURCE-LEN @ count - ASCII-WINDOW
   then
   true ;

: INPUT-ENSURE ( ^cursor n -- bool )
   {: state want:n :}
   want 0 < if E-RANGE throw then
   begin want state CURSOR-SOURCE-LEN @ > while
      state INPUT-REFILL 0= if false exit then
   repeat
   true ;

: AT ( ^cursor n -- n )
   {: state index:n :}
   state INPUT? if state index 1+ INPUT-ENSURE drop then
   state SOURCE$ state WINDOW-PLACE index BYTE@ ;

: EOF? ( ^cursor -- bool )
   {: state :}
   state INPUT? if state state CURSOR-POSITION @ 1+ INPUT-ENSURE drop then
   state CURSOR-POSITION @ state CURSOR-SOURCE-LEN @ = ;

: PEEK ( ^cursor -- n )
   dup CURSOR-POSITION @ AT ;

: ADVANCE ( ^cursor -- )
   CURSOR-POSITION 1 swap +! ;

\ Skips count bytes the caller has already matched, so the reader holds them.
: ADVANCE-N ( ^cursor n -- )
   swap CURSOR-POSITION +! ;

: SPAN$ ( ^cursor n n -- str )
   {: state start:n size:n :}
   start -2 < if
      state SCOPE$ STR:DATA start negate 3 - + size >str exit
   then
   state CURSOR-SOURCE @ start + size >str ;

: MATCH$ ( ^cursor str -- bool )
   {: state value:str :}
   value STR:LEN 0 ?do
      state INPUT? if
         state state CURSOR-POSITION @ i + 1+ INPUT-ENSURE 0= if
            false unloop exit
         then
      then
      state CURSOR-POSITION @ i + state CURSOR-SOURCE-LEN @ >= if
         false unloop exit
      then
      state CURSOR-SOURCE @ state CURSOR-POSITION @ i + + c@
      value STR:DATA i + c@ <> if false unloop exit then
   loop
   true ;

\ The expected text is the byte in backticks, written in the scope's three bytes.
: BYTE-SCOPE ( ^cursor n ^u8 NUM:alloc-byte-len -- )
   drop {: state byte:n buf:^ :}
   $60 buf c!  byte buf 1+ c!  $60 buf 2 + c!
   state state CURSOR-POSITION @ buf 3 >str
   s" write that byte here, or correct the markup before it" E-MALFORMED REJECT ;

: REQUIRE-BYTE ( ^cursor n -- )
   {: state byte:n :}
   state PEEK byte <> if
      state byte 3 MEM:BYTES-ALLOC-LEN [: BYTE-SCOPE ;] MEM:WITH-BYTES
   then
   state ADVANCE ;

: WHITE? ( ^cursor -- bool )
   dup EOF? if drop false exit then
   PEEK XML-SPACE? ;

: SKIP-WHITE ( ^cursor -- )
   begin dup WHITE? while dup ADVANCE repeat drop ;

\ Only fills the window: the read after it rejects what UTF8:NEXT does not decode.
: INPUT-SCALAR-ENSURE ( ^cursor -- )
   {: state :}
   state state CURSOR-POSITION @ 1+ INPUT-ENSURE 0= if exit then
   begin
      state SOURCE$ state CURSOR-POSITION @ UTF8:CUT?
   while
      state state CURSOR-SOURCE-LEN @ 1+ INPUT-ENSURE 0= if exit then
   repeat ;

: SCAN-SCALAR ( ^cursor -- n )
   {: state :}
   state INPUT? if state INPUT-SCALAR-ENSURE then
   state SOURCE$ state WINDOW-PLACE state CURSOR-POSITION @ SCALAR-AT
   state CURSOR-POSITION ! ;

: INPUT-NUMERIC-LOOKAHEAD ( ^cursor -- )
   {: state :}
   state state CURSOR-POSITION @ 3 + INPUT-ENSURE 0= if exit then
   state state CURSOR-POSITION @ 2 + AT $78 = if 0 3 $10 else 0 2 10 then
   begin
      {: value:n count:n radix:n :}
      state state CURSOR-POSITION @ count + 1+ INPUT-ENSURE 0= if exit then
      state state CURSOR-POSITION @ count + AT
      dup $3B = if drop exit then
      {: byte:n :}
      state SOURCE$ state WINDOW-PLACE state CURSOR-POSITION @ byte
      HEX-DIGIT {: digit:n :}
      state SOURCE$ state WINDOW-PLACE state CURSOR-POSITION @
      value digit radix NUMERIC-STEP
      count 1+ radix
   again ;

: INPUT-ENTITY-LOOKAHEAD ( ^cursor -- )
   {: state :}
   state state CURSOR-POSITION @ 2 + INPUT-ENSURE 0= if exit then
   state state CURSOR-POSITION @ 1+ AT $23 = if
      state INPUT-NUMERIC-LOOKAHEAD exit
   then
   2
   begin
      {: count:n :}
      count 7 > if exit then
      state state CURSOR-POSITION @ count + INPUT-ENSURE 0= if exit then
      state state CURSOR-POSITION @ count 1- + AT $3B = if exit then
      count 1+
   again ;

: SCAN-DECODE ( ^cursor n -- n n )
   {: state mode:n :}
   state INPUT? if
      state state CURSOR-POSITION @ 1+ INPUT-ENSURE drop
      mode DECODE-RAW <> state PEEK $26 = and if
         state INPUT-ENTITY-LOOKAHEAD
      else
         state INPUT-SCALAR-ENSURE
         state PEEK $D = if
            state state CURSOR-POSITION @ 2 + INPUT-ENSURE drop
         then
      then
   then
   state SOURCE$ state WINDOW-PLACE state CURSOR-POSITION @ mode
   DECODE-AT ;

\ An input reader keeps each open frame's name and each live binding's prefix and
\ URI in the scope buffer: SCOPE-REF appends them when the frame opens or the
\ binding is declared, SCOPE-CUT drops a frame's strings and its bindings' when it
\ pops. A reference below -2 is a scope offset.
: SCOPE-REF ( ^cursor n n -- n n )
   {: state start:n size:n :}
   state INPUT? 0= if start size exit then
   state CURSOR-STREAM @ SCOPE-VEC {: scope :}
   scope VEC:COUNT {: index:n :}
   state start size SPAN$ scope VEC:APPEND
   index negate 3 - size ;

: TOKEN-TRIM ( ^stream -- )
   {: stream :}
   stream TOKEN-VEC VEC:COUNT CHUNK max {: need:n :}
   stream TOKEN-VEC VEC:CAP 4 / need <= if exit then
   stream TOKEN-VEC need VEC:TRIM ;

: SCOPE-TRIM ( ^stream -- )
   {: stream :}
   stream SCOPE-VEC VEC:COUNT {: used:n :}
   used 16 max {: need:n :}
   stream SCOPE-VEC VEC:CAP 4 / need <= if exit then
   stream SCOPE-VEC need VEC:TRIM ;

: SCOPE-CUT ( ^cursor n -- )
   {: state ref:n :}
   state INPUT? 0= if exit then
   state CURSOR-STREAM @ {: stream :}
   stream SCOPE-VEC ref negate 3 - VEC:RESIZE
   stream SCOPE-TRIM ;

\ Before an event, drop the consumed bytes only when the next refill would not
\ fit or the buffer is four times what it keeps, and only once at least as many
\ bytes were consumed as remain: each move copies fewer bytes than were read
\ since the last one, so streaming costs time linear in the input.
: INPUT-REBASE ( ^cursor -- )
   {: state :}
   state CURSOR-STREAM @ {: stream :}
   state CURSOR-POSITION @ {: consumed:n :}
   state CURSOR-SOURCE-LEN @ consumed - {: tail:n :}
   consumed 0= consumed tail < or if exit then
   stream TOKEN-VEC VEC:CAP {: cap:n :}
   state CURSOR-SOURCE-LEN @ CHUNK + cap <=
   cap 4 / CHUNK tail max <= and if exit then
   state SOURCE$ state WINDOW-PLACE consumed PLACE-POSITION
   stream STREAM-COLUMN ! stream STREAM-LINE !
   state CURSOR-SOURCE @ consumed + tail >str
      stream TOKEN-VEC VEC:SPAN SPAN:COPY
   stream TOKEN-VEC tail VEC:RESIZE
   stream TOKEN-TRIM
   stream TOKEN-VEC VEC:SPAN SPAN:$
      dup STR:LEN state CURSOR-SOURCE-LEN ! STR:DATA state CURSOR-SOURCE !
   stream STREAM-BASE @ consumed + stream STREAM-BASE !
   0 state CURSOR-POSITION ! ;

: URI$ ( ^cursor n n -- str )
   {: state start:n size:n :}
   start XML-URI = if s" http://www.w3.org/XML/1998/namespace" exit then
   start XMLNS-URI = if s" http://www.w3.org/2000/xmlns/" exit then
   state start size SPAN$ ;

\ A value of bytes $20..$7F other than `&` decodes to itself.
: PLAIN-VALUE? ( str -- bool )
   {: source:str :}
   source STR:LEN 0 ?do
      source STR:DATA i + c@ {: byte:n :}
      byte $20 < byte $26 = or byte $7F > or if false unloop exit then
   loop
   true ;

: URI-INTO ( str ^VEC:vector<u8> -- )
   {: source:str v:^ :}
   source PLAIN-VALUE? 0= if
      source DECODE-ATTR v DECODE-INTO exit
   then
   source v VEC:APPEND ;

: CURRENT-FRAME ( ^cursor -- ^frame )
   dup CURSOR-OPEN-DEPTH @ 1- FRAME ;

\ Release the three tables and an input reader's stream, leaving the record owning
\ nothing and the mode MODE-LEGACY-UTF8.
: RECORD-FREE ( ^cursor -- )
   {: state :}
   state CURSOR-FRAMES @ VEC:DELETE
   NULL-PTR state CURSOR-FRAMES !
   state CURSOR-ATTRS @ VEC:DELETE
   NULL-PTR state CURSOR-ATTRS !
   state CURSOR-NAMESPACES @ VEC:DELETE
   NULL-PTR state CURSOR-NAMESPACES !
   state INPUT? if
      state CURSOR-ALLOCATOR @ state CURSOR-STREAM @
      MODE-LEGACY-UTF8 state CURSOR-SOURCE-MODE !
      NULL-PTR state CURSOR-STREAM !
      STREAM-CLOSE
   then ;


\ An abandoned record still holds its tables: release them, then zero every field but
\ sip, which each hash sets before use, so a failed build releases exactly what it made.
: INIT-STATE ( ^cursor -- )
   {: state :}
   state RECORD-FREE
   NULL-PTR state CURSOR-SOURCE !  0 state CURSOR-SOURCE-LEN !
   0 state CURSOR-POSITION !  0 state CURSOR-OPEN-DEPTH !
   0 state CURSOR-NS-COUNT !  0 state CURSOR-ATTR-COUNT !
   0 state CURSOR-ATTR-INDEX !  0 state CURSOR-RAW-OFF !
   0 state CURSOR-RAW-LEN !  XML-KIND:START state CURSOR-TOKEN-KIND !
   0 state CURSOR-NAME-OFF !  0 state CURSOR-NAME-LEN !
   0 state CURSOR-VALUE-OFF !  0 state CURSOR-VALUE-LEN !
   0 state CURSOR-URI-OFF !  0 state CURSOR-URI-LEN !
   PENDING-NONE state CURSOR-PENDING !  0 state CURSOR-ROOT-COUNT !
   false state CURSOR-FAILED !  MODE-LEGACY-UTF8 state CURSOR-SOURCE-MODE !
   NULL-PTR state CURSOR-STREAM !
   0 state CURSOR-HEAD-COLUMNS !
   COUNTING-BYTES state CURSOR-WINDOW-KIND !
   NULL-PTR state CURSOR-ALLOCATOR !
   NULL-PTR state CURSOR-FRAMES !  NULL-PTR state CURSOR-ATTRS !
   NULL-PTR state CURSOR-NAMESPACES !
   0 state CURSOR-HASH-K0 !  0 state CURSOR-HASH-K1 ! ;


: READER-TABLES ( ^cursor -- )
   {: state :}
   state CURSOR-HASH-K0 BYTE-VIEW CELL ENTROPY:FILL
   state CURSOR-HASH-K1 BYTE-VIEW CELL ENTROPY:FILL
   state CURSOR-ALLOCATOR @ {: al:^ :}
   al TABLE-START [type] frame VEC:NEW state CURSOR-FRAMES !
   al TABLE-START [type] attribute VEC:NEW state CURSOR-ATTRS !
   al TABLE-START [type] binding VEC:NEW state CURSOR-NAMESPACES !
   state NS-HEADS-CLEAR
   -1 state CURSOR-ATTR-INDEX !
   XML-KIND:EOF state CURSOR-TOKEN-KIND ! ;


\ Answer the reader a build body made, or release it and rethrow the body's code.
: BUILT ( ^cursor n -- XML:reader )
   {: state code:n :}
   code 0<> if state RECORD-FREE code throw then
   state MINT ;

: BOM ( ^cursor -- )
   {: state :}
   state s\" \xFF\xFE" MATCH$ state s\" \xFE\xFF" MATCH$ or
   state s\" <\z" MATCH$ or state s\" \z<" MATCH$ or if
      state 0 s" a document in UTF-8 or US-ASCII"
      s" save the document as UTF-8, or read UTF-16 with XML:INIT-SOURCE"
      E-ENCODING REJECT
   then
   state s\" \xEF\xBB\xBF" MATCH$ if state 3 ADVANCE-N then ;

: INIT-BODY ( ^cursor str -- ^cursor str )
   {: state source:str :}
   source STR:DATA state CURSOR-SOURCE !
   source STR:LEN state CURSOR-SOURCE-LEN !
   state READER-TABLES
   state BOM
   state source ;


\ The stream goes into the record before the tables, so a failed table step
\ leaves RECORD-FREE to release both.
: INPUT-BODY ( ^cursor ^n [ ^n SPAN:span<u8> -- n ] -- ^cursor ^n [ ^n SPAN:span<u8> -- n ] )
   {: state ctx fill :}
   state CURSOR-ALLOCATOR @ ctx fill STREAM-OPEN state CURSOR-STREAM !
   MODE-INPUT-UTF8 state CURSOR-SOURCE-MODE !
   state READER-TABLES
   state ctx fill ;

public
: INIT ( ^cursor ^ALLOC:allocator str -- XML:reader )
   {: record al:^ source:str :}
   source SPAN-CHECK
   record INIT-STATE
   al record CURSOR-ALLOCATOR !
   record source ['] INIT-BODY catch
   >r drop drop r> {: code:n :}
   record code BUILT ;

\ Reads UTF-8 or declared US-ASCII; the fill writes the span it is lent and answers a
\ count: 0 ends input, below 0 or past the span is E-RANGE. ctx and fill live until CLOSE.
: INIT-INPUT ( ^cursor ^ALLOC:allocator ^n [ ^n SPAN:span<u8> -- n ]
               -- XML:reader )
   {: record al:^ ctx fill :}
   record INIT-STATE
   al record CURSOR-ALLOCATOR !
   record ctx fill ['] INPUT-BODY catch
   >r 2drop drop r> {: code:n :}
   record code BUILT ;

private
: SOURCE-READER-MODE ( ^header -- mode )
   {: source-state :}
   source-state SRC-ENCODING {: encoding:encoding :}
   encoding XML-ENCODING:UTF8 XML-ENCODING:EQ if MODE-SOURCE-UTF8 exit then
   encoding XML-ENCODING:US-ASCII XML-ENCODING:EQ if MODE-SOURCE-ASCII exit then
   source-state HEADER-BOM-LEN @ 0= if
      encoding XML-ENCODING:UTF16-LE XML-ENCODING:EQ if
         MODE-SOURCE-UTF16-LE-DECL
      else
         MODE-SOURCE-UTF16-BE-DECL
      then
   else
      encoding XML-ENCODING:UTF16-LE XML-ENCODING:EQ if
         MODE-SOURCE-UTF16-LE-BOM
      else
         MODE-SOURCE-UTF16-BE-BOM
      then
   then ;

\ A source reader's window is the lexical text, after the byte order mark
\ that starts line 1: the mark's bytes in UTF-8, one unit in UTF-16.
: WINDOW-COLUMNS ( ^cursor ^header -- )
   {: state source-state :}
   source-state HEADER-BOM-LEN @ {: bom:n :}
   state CURSOR-SOURCE-MODE @ UTF16-SOURCE? if
      COUNTING-UNITS bom 2 /
   else
      COUNTING-BYTES bom
   then state CURSOR-HEAD-COLUMNS ! state CURSOR-WINDOW-KIND ! ;

: SOURCE-BODY ( ^cursor ^header -- ^cursor ^header )
   {: state source-state :}
   source-state SRC-LEXICAL$ {: lexical:str :}
   lexical STR:DATA state CURSOR-SOURCE !
   lexical STR:LEN state CURSOR-SOURCE-LEN !
   source-state SOURCE-READER-MODE state CURSOR-SOURCE-MODE !
   state source-state WINDOW-COLUMNS
   state READER-TABLES
   state source-state ;

: SOURCE-READER ( ^cursor ^header -- XML:reader )
   {: record source-state :}
   record INIT-STATE
   source-state HEADER-ALLOCATOR @ record CURSOR-ALLOCATOR !
   record source-state ['] SOURCE-BODY catch
   >r 2drop r> {: code:n :}
   record code BUILT ;

public
\ The source stays in place under the reader's inputs, so a caller that
\ catches a refusal still holds it to close.
: INIT-SOURCE ( XML:source ^cursor -- XML:source XML:reader )
   >r SRC-STATE r> swap SOURCE-READER ;

: OWNER-READER ( ^cursor source-owner -- XML:reader )
   OWNER-STATE SOURCE-READER ;

: CLOSE ( XML:reader -- )
   ERASE RECORD-FREE ;

\ Release what a record owns after a caller's throw abandoned its live reader.
\ A second DISPOSE, or one on a closed or zero-filled record, does nothing.
: DISPOSE ( ^cursor -- )
   RECORD-FREE ;

private
\ FAILED is true from NEXT's entry until it returns, so a NEXT that throws
\ leaves the reader refusing every read.
: LIVE ( ^cursor -- )
   CURSOR-FAILED @ if E-STATE throw then ;

: NAMED ( ^cursor -- )
   {: state :}
   state LIVE
   state CURSOR-NAME-LEN @ 0= if E-STATE throw then ;

\ Throws E-STATE unless the token is a start tag.
: START-ONLY ( ^cursor -- )
   CURSOR-TOKEN-KIND @ XML-KIND:START XML-KIND:EQ 0= if E-STATE throw then ;

: ACTIVE-ATTRIBUTE ( ^cursor -- ^attribute )
   {: state :}
   state LIVE
   state START-ONLY
   state CURSOR-ATTR-INDEX @ {: index:n :}
   index 0 < index state CURSOR-ATTR-COUNT @ >= or if E-STATE throw then
   state index ATTRIBUTE ;

: LOCAL-SPAN ( str -- str )
   {: name:str :}
   name STR:LEN 0 ?do
      name STR:DATA i + c@ $3A = if
         name STR:DATA i 1+ + name STR:LEN i 1+ - >str unloop exit
      then
   loop
   name ;

\ Keep the reader below throwing workers' arguments. Native catch restores
\ stack depth, so public locals could overwrite the reader's saved stack cell.
: RAW-STATE ( ^cursor -- off len )
   {: state :}
   state LIVE
   state state CURSOR-RAW-OFF @ ABS-OFF >OFF state CURSOR-RAW-LEN @ >LEN ;

: RAW$-STATE ( ^cursor -- str )
   {: state :}
   state LIVE
   state state CURSOR-RAW-OFF @ state CURSOR-RAW-LEN @ SPAN$ ;

: NAME$-STATE ( ^cursor -- str )
   {: state :}
   state NAMED
   state state CURSOR-NAME-OFF @ state CURSOR-NAME-LEN @ SPAN$ ;

: DECODE-ARGS ( XML:reader ^VEC:vector<u8> -- XML:reader ^cursor ^VEC:vector<u8> )
   {: v:^ :}
   STATE v ;

: URI-STATE ( ^cursor ^VEC:vector<u8> -- )
   {: state v:^ :}
   state NAMED
   state state CURSOR-URI-OFF @ state CURSOR-URI-LEN @ URI$
   v URI-INTO ;

: ATTR-RESET-STATE ( ^cursor -- )
   {: state :}
   state LIVE
   state START-ONLY
   -1 state CURSOR-ATTR-INDEX ! ;

: ATTR-NEXT-STATE ( ^cursor -- bool )
   {: state :}
   state LIVE
   state START-ONLY
   state CURSOR-ATTR-INDEX @ state CURSOR-ATTR-COUNT @ >= if false exit then
   1 state CURSOR-ATTR-INDEX +!
   state CURSOR-ATTR-INDEX @ state CURSOR-ATTR-COUNT @ < ;

: ATTR-RAW-STATE ( ^cursor -- off len )
   {: state :}
   state ACTIVE-ATTRIBUTE {: attr :}
   state attr ATTRIBUTE-RAW @ ABS-OFF >OFF attr ATTRIBUTE-RAW-U @ >LEN ;

: ATTR-NAME$-STATE ( ^cursor -- str )
   {: state :}
   state ACTIVE-ATTRIBUTE {: attr :}
   state attr ATTRIBUTE-NAME @ attr ATTRIBUTE-NAME-U @ SPAN$ ;

: ATTR-VALUE$-STATE ( ^cursor -- str )
   {: state :}
   state ACTIVE-ATTRIBUTE {: attr :}
   state attr ATTRIBUTE-VALUE @ attr ATTRIBUTE-VALUE-U @ SPAN$ ;

: ATTR-VALUE-STATE ( ^cursor -- off len )
   {: state :}
   state ACTIVE-ATTRIBUTE {: attr :}
   state attr ATTRIBUTE-VALUE @ ABS-OFF >OFF attr ATTRIBUTE-VALUE-U @ >LEN ;

: ATTR-URI-STATE ( ^cursor ^VEC:vector<u8> -- )
   {: state v:^ :}
   state ACTIVE-ATTRIBUTE {: attr :}
   state attr ATTRIBUTE-URI @ attr ATTRIBUTE-URI-U @ URI$
   v URI-INTO ;

: ATTR-TEXT-STATE ( ^cursor ^VEC:vector<u8> -- )
   {: state v:^ :}
   state ACTIVE-ATTRIBUTE {: attr :}
   state attr ATTRIBUTE-VALUE @ attr ATTRIBUTE-VALUE-U @ SPAN$
   DECODE-ATTR v DECODE-INTO ;

public
: KIND ( XML:reader -- XML:reader kind )
   STATE dup LIVE CURSOR-TOKEN-KIND @ ;

: RAW ( XML:reader -- XML:reader off len )
   STATE RAW-STATE ;

: RAW$ ( XML:reader -- XML:reader str )
   STATE RAW$-STATE ;

: NAME$ ( XML:reader -- XML:reader str )
   STATE NAME$-STATE ;

: LOCAL$ ( XML:reader -- XML:reader str )
   NAME$ LOCAL-SPAN ;

: DEPTH ( XML:reader -- XML:reader n )
   STATE dup LIVE CURSOR-OPEN-DEPTH @ ;

: URI ( XML:reader ^VEC:vector<u8> -- XML:reader )
   DECODE-ARGS URI-STATE ;

\ Attribute access works only on a start event, after ATTR-NEXT selects one and
\ before exhaustion, else E-STATE; xmlns declarations are attributes. ATTR-RAW
\ covers name, `=`, whitespace and quoted value; ATTR-VALUE omits the quotes.
: ATTR-RESET ( XML:reader -- XML:reader )
   STATE ATTR-RESET-STATE ;

: ATTR-NEXT ( XML:reader -- XML:reader bool )
   STATE ATTR-NEXT-STATE ;

: ATTR-RAW ( XML:reader -- XML:reader off len )
   STATE ATTR-RAW-STATE ;

: ATTR-NAME$ ( XML:reader -- XML:reader str )
   STATE ATTR-NAME$-STATE ;

: ATTR-LOCAL$ ( XML:reader -- XML:reader str )
   ATTR-NAME$ LOCAL-SPAN ;

: ATTR-VALUE$ ( XML:reader -- XML:reader str )
   STATE ATTR-VALUE$-STATE ;

: ATTR-VALUE ( XML:reader -- XML:reader off len )
   STATE ATTR-VALUE-STATE ;

: ATTR-URI ( XML:reader ^VEC:vector<u8> -- XML:reader )
   DECODE-ARGS ATTR-URI-STATE ;

: ATTR-TEXT ( XML:reader ^VEC:vector<u8> -- XML:reader )
   DECODE-ARGS ATTR-TEXT-STATE ;

;package
