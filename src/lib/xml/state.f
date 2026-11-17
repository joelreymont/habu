\ state.f - XML reader state: INIT, INIT-INPUT, INIT-SOURCE, CLOSE, DISPOSE and the
\ accessors; part of package XML (src/lib/xml.f). Offsets are absolute bytes of the
\ stream, a BOM included. Open frames' names and live bindings are copied once into
\ scope storage, so releasing consumed bytes never revisits them.

require src/lib/xml/source.f
require src/lib/byte-buffer.f
require src/lib/siphash.f
require src/lib/entropy.f

package XML
public
DEFLINEAR XML:reader
ENUM kind start end text comment pi cdata eof ;ENUM

private
CAST: KIND>N ( kind -- n )
\ Views the token kind cell, which holds only KIND>N values.
CAST: >KIND-CELL ( ^n -- ^kind )

\ The reader's whole fixed state. The tables and an input reader's stream it
\ points to are mappings the reader owns; sip is scratch for one hash.
public
STRUCTURE cursor OPAQUE DERIVE addr
   FIELD source ^u8
   FIELD source-len n
   FIELD position n
   FIELD frame-cap n
   FIELD open-depth n
   FIELD ns-count n
   FIELD attr-count n
   FIELD attr-index n
   FIELD raw-off n
   FIELD raw-len n
   FIELD token-kind n
   FIELD name-off n
   FIELD name-len n
   FIELD value-off n
   FIELD value-len n
   FIELD uri-off n
   FIELD uri-len n
   FIELD pending n
   FIELD root-count n
   FIELD failed n
   FIELD attr-cap n
   FIELD source-mode n
   FIELD wire-original ^u8
   FIELD wire-original-len n
   FIELD wire-storage ^u8
   FIELD wire-storage-len n
   FIELD ns-cap n
   FIELD frames ^n
   FIELD attrs ^n
   FIELD namespaces ^n
   FIELD hash-k0 n
   FIELD hash-k1 n
   FIELD sip SIPHASH:state
;STRUCTURE

\ The record's size in bytes, for a caller that allocates it as bytes and makes
\ it a record with EMPTY-CURSOR.
EXPORT CURSOR-BYTES

private
CAST: >STATE ( ^cursor -- ^n )
8 constant TABLE-START
5 constant FRAME-CELLS
\ An attribute is name, value and raw spans and URI, then its name hash, the
\ older attribute in its bucket, and the head of the bucket its own index
\ numbers. A binding is prefix and URI spans, then the URI hash, the older
\ binding in its prefix bucket and that head. src/lib/xml/names.f fills them.
11 constant ATTR-CELLS
8 cells constant ATTR-HASH
9 cells constant ATTR-LINK
10 cells constant ATTR-HEAD
7 constant NS-CELLS
4 cells constant NS-HASH
5 cells constant NS-LINK
6 cells constant NS-HEAD
NULL-PTR CURSOR-SOURCE NULL-PTR - CELL / constant SOURCE-IDX
NULL-PTR CURSOR-SOURCE-LEN NULL-PTR - constant SOURCE-LEN
NULL-PTR CURSOR-POSITION NULL-PTR - constant POSITION
NULL-PTR CURSOR-FRAME-CAP NULL-PTR - constant FRAME-CAP
NULL-PTR CURSOR-OPEN-DEPTH NULL-PTR - constant OPEN-DEPTH
NULL-PTR CURSOR-NS-COUNT NULL-PTR - constant NS-COUNT
NULL-PTR CURSOR-ATTR-COUNT NULL-PTR - constant ATTR-COUNT
NULL-PTR CURSOR-ATTR-INDEX NULL-PTR - constant ATTR-INDEX
NULL-PTR CURSOR-RAW-OFF NULL-PTR - constant RAW-OFF
NULL-PTR CURSOR-RAW-LEN NULL-PTR - constant RAW-LEN
NULL-PTR CURSOR-TOKEN-KIND NULL-PTR - constant TOKEN-KIND
NULL-PTR CURSOR-NAME-OFF NULL-PTR - constant NAME-OFF
NULL-PTR CURSOR-NAME-LEN NULL-PTR - constant NAME-LEN
NULL-PTR CURSOR-VALUE-OFF NULL-PTR - constant VALUE-OFF
NULL-PTR CURSOR-VALUE-LEN NULL-PTR - constant VALUE-LEN
NULL-PTR CURSOR-URI-OFF NULL-PTR - constant URI-OFF
NULL-PTR CURSOR-URI-LEN NULL-PTR - constant URI-LEN
NULL-PTR CURSOR-PENDING NULL-PTR - constant PENDING
NULL-PTR CURSOR-ROOT-COUNT NULL-PTR - constant ROOT-COUNT
NULL-PTR CURSOR-FAILED NULL-PTR - constant FAILED
NULL-PTR CURSOR-ATTR-CAP NULL-PTR - constant ATTR-CAP
NULL-PTR CURSOR-SOURCE-MODE NULL-PTR - constant SOURCE-MODE
NULL-PTR CURSOR-WIRE-ORIGINAL NULL-PTR - CELL / constant WIRE-ORIGINAL-IDX
NULL-PTR CURSOR-WIRE-ORIGINAL-LEN NULL-PTR - constant WIRE-ORIGINAL-LEN
NULL-PTR CURSOR-WIRE-STORAGE NULL-PTR - CELL / constant WIRE-STORAGE-IDX
NULL-PTR CURSOR-WIRE-STORAGE-LEN NULL-PTR - constant WIRE-STORAGE-LEN
NULL-PTR CURSOR-NS-CAP NULL-PTR - constant NS-CAP
NULL-PTR CURSOR-FRAMES NULL-PTR - CELL / constant FRAMES-IDX
NULL-PTR CURSOR-ATTRS NULL-PTR - CELL / constant ATTRS-IDX
NULL-PTR CURSOR-NAMESPACES NULL-PTR - CELL / constant NS-IDX
NULL-PTR CURSOR-HASH-K0 NULL-PTR - constant HASH-K0
NULL-PTR CURSOR-HASH-K1 NULL-PTR - constant HASH-K1
NULL-PTR CURSOR-SIP NULL-PTR - constant SIP-STATE

0 constant LEGACY-UTF8
1 constant SOURCE-UTF8
2 constant SOURCE-UTF16-LE-BOM
3 constant SOURCE-UTF16-BE-BOM
4 constant SOURCE-UTF16-LE-DECL
5 constant SOURCE-UTF16-BE-DECL
6 constant INPUT-UTF8
7 constant SOURCE-ASCII
8 constant LEGACY-ASCII
9 constant INPUT-ASCII

\ An input reader owns only its current lexical window and active scope strings.
\ Its metadata and byte buffers are released by CLOSE, including after a
\ failed NEXT. The caller retains the callback, context and input.
STRUCTURE stream DERIVE addr
   FIELD fill [ ^n SPAN:span<u8> -- n ]
   FIELD ctx ^n
   FIELD input ^u8
   FIELD input-cap n
   FIELD eof n
   FIELD base n
   FIELD started n
   FIELD token ^u8
   FIELD token-len n
   FIELD token-cap n
   FIELD scope ^u8
   FIELD scope-len n
   FIELD scope-cap n
;STRUCTURE

: INPUT? ( ^n -- bool )
   SOURCE-MODE + @ dup INPUT-UTF8 = swap INPUT-ASCII = or ;

CAST: >STREAM ( ^u8 -- ^stream )
CAST: STREAM>BYTES ( ^stream -- ^u8 )

: STREAM@ ( ^n -- ^stream )
   WIRE-STORAGE-IDX ptr-field @ >STREAM ;

: ABS-OFF ( ^n n -- n )
   {: state offset:n :}
   state INPUT? if state STREAM@ STREAM-BASE @ offset + else offset then ;

: TOKEN-BUF ( ^stream -- ^^u8 ) STREAM-TOKEN ;
: SCOPE-BUF ( ^stream -- ^^u8 ) STREAM-SCOPE ;

: BUF-LEN ( ^a -- n ) BUF:LEN@ BUF:BLEN>N ;
: BUF-CAP ( ^a -- n ) BUF:CAP@ BUF:BLEN>N ;

: SCOPE$ ( ^n -- ^u8 n )
   STREAM@ SCOPE-BUF BUF:SPAN$ BUF:BLEN>N ;

: STREAM-CLOSE ( ^stream -- )
   {: stream :}
   stream STREAM-TOKEN-CAP @ 0<> if stream TOKEN-BUF BUF:DISPOSE then
   stream STREAM-SCOPE-CAP @ 0<> if stream SCOPE-BUF BUF:DISPOSE then
   stream STREAM>BYTES STREAM-CELLS cells
      MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;

: STREAM-OPEN-BUFS ( ^stream n -- ^stream n )
   {: stream cap:n :}
   stream TOKEN-BUF cap BUF:N>BLEN BUF:INIT
   stream SCOPE-BUF 16 BUF:N>BLEN BUF:INIT
   stream cap ;

: STREAM-OPEN ( ^n [ ^n SPAN:span<u8> -- n ] SPAN:span<u8> -- ^stream )
   {: ctx fill input :}
   input SPAN:$ {: bytes cap:n :}
   cap 0= if E-CAPACITY throw then
   STREAM-CELLS MEM:CELLS-ALLOC-COUNT MEM:ALLOC-CELLS
      BYTE-VIEW >STREAM {: stream :}
   fill stream STREAM-FILL !
   ctx stream STREAM-CTX !
   bytes stream STREAM-INPUT !
   cap stream STREAM-INPUT-CAP !
   stream cap ['] STREAM-OPEN-BUFS catch
   >r 2drop r> {: code:n :}
   code 0<> if stream STREAM-CLOSE code throw then
   stream ;

1 constant POP-NEXT
2 constant EMPTY-END
-1 constant XML-URI
-2 constant XMLNS-URI

\ A reader token is its cursor record; only these rows cross it.
LINEAR: MINT ( ^n -- XML:reader )
LINEAR: ERASE ( XML:reader -- ^n )

: STATE ( XML:reader -- XML:reader ^n )
   ERASE dup MINT swap ;

: RECORD ( ^n n n n -- ^n )
   {: state table:n index:n size:n :}
   state table ptr-field @ index size * cells + ;

: FRAME ( ^n n -- ^n ) FRAMES-IDX swap FRAME-CELLS RECORD ;
: ATTRIBUTE ( ^n n -- ^n ) ATTRS-IDX swap ATTR-CELLS RECORD ;
: NAMESPACE ( ^n n -- ^n ) NS-IDX swap NS-CELLS RECORD ;

\ Release a table and leave the record owning none, so a second release is a
\ no-op.
: TABLE-RELEASE ( ^n n n n -- )
   {: state table:n cap-off:n size:n :}
   state cap-off + @ {: cap:n :}
   cap 0= if exit then
   state table ptr-field @ {: old:^ :}
   0 state cap-off + !
   NULL-PTR state table ptr-field !
   old cap size * MEM:CELLS-ALLOC-COUNT MEM:RELEASE-CELLS ;

\ Doubles into a fresh mapping. Records move, so no caller keeps a record's
\ address across a growth; a failed allocation leaves the table as it was.
: GROW ( ^n n n n n -- )
   {: state want:n table:n cap-off:n size:n :}
   state cap-off + @ {: cap:n :}
   want cap <= if exit then
   cap 2 * TABLE-START max want max {: fresh-cap:n :}
   fresh-cap size * MEM:CELLS-ALLOC-COUNT MEM:ALLOC-CELLS {: fresh:^ :}
   cap 0<> if
      state table ptr-field @ BYTE-VIEW fresh BYTE-VIEW cap size * cells BYTE-COPY
   then
   state table cap-off size TABLE-RELEASE
   fresh state table ptr-field !
   fresh-cap state cap-off + ! ;

: FRAMES-GROW ( ^n n -- ) FRAMES-IDX FRAME-CAP FRAME-CELLS GROW ;
: ATTRS-GROW ( ^n n -- ) ATTRS-IDX ATTR-CAP ATTR-CELLS GROW ;
: NS-GROW ( ^n n -- ) NS-IDX NS-CAP NS-CELLS GROW ;

\ Empty every prefix bucket: src/lib/xml/names.f relinks the bindings.
: NS-HEADS-CLEAR ( ^n -- )
   {: state :}
   state NS-CAP + @ 0 ?do -1 state i NAMESPACE NS-HEAD + ! loop ;

: SOURCE$ ( ^n -- ^u8 n )
   {: state :}
   state SOURCE-IDX ptr-field @ state SOURCE-LEN + @ ;

: INPUT-REFILL ( ^n -- bool )
   {: state :}
   state STREAM@ {: stream :}
   stream STREAM-EOF @ 0<> if false exit then
   stream STREAM-CTX @
   stream STREAM-INPUT @ stream STREAM-INPUT-CAP @ SPAN:MAKE
   stream STREAM-FILL @ execute {: count:n :}
   count 0 < count stream STREAM-INPUT-CAP @ > or if E-RANGE throw then
   count 0= if 1 stream STREAM-EOF ! false exit then
   state SOURCE-MODE + @ INPUT-ASCII = if
      stream STREAM-INPUT @ count ASCII-WIRE
   then
   stream STREAM-INPUT @ count BUF:N>BLEN stream TOKEN-BUF BUF:APPEND-SPAN
   stream TOKEN-BUF BUF:SPAN$ BUF:BLEN>N
   state SOURCE-LEN + ! state SOURCE-IDX ptr-field !
   true ;

: INPUT-ENSURE ( ^n n -- bool )
   {: state want:n :}
   want 0 < if E-RANGE throw then
   begin want state SOURCE-LEN + @ > while
      state INPUT-REFILL 0= if false exit then
   repeat
   true ;

: AT ( ^n n -- n )
   {: state index:n :}
   state INPUT? if state index 1+ INPUT-ENSURE drop then
   state SOURCE$ index BYTE@ ;

: EOF? ( ^n -- bool )
   {: state :}
   state INPUT? if state state POSITION + @ 1+ INPUT-ENSURE drop then
   state POSITION + @ state SOURCE-LEN + @ = ;

: PEEK ( ^n -- n )
   dup POSITION + @ AT ;

: ADVANCE ( ^n -- )
   POSITION + 1 swap +! ;

: ADVANCE-N ( ^n n -- )
   {: state count:n :}
   state INPUT? if
      state state POSITION + @ count + INPUT-ENSURE drop
   then
   count state SOURCE-LEN + @ state POSITION + @ - > if
      E-TRUNCATED throw
   then
   count state POSITION + +! ;

: SPAN$ ( ^n n n -- ^u8 n )
   {: state start:n size:n :}
   start -2 < if
      state SCOPE$ drop start negate 3 - + size exit
   then
   state SOURCE-IDX ptr-field @ start + size ;

: MATCH$ ( ^n ^u8 n -- bool )
   {: state value size:n :}
   size 0 ?do
      state INPUT? if
         state state POSITION + @ i + 1+ INPUT-ENSURE 0= if
            false unloop exit
         then
      then
      state POSITION + @ i + state SOURCE-LEN + @ >= if
         false unloop exit
      then
      state SOURCE-IDX ptr-field @ state POSITION + @ i + + c@
      value i + c@ <> if false unloop exit then
   loop
   true ;

: REQUIRE-BYTE ( ^n n -- )
   {: state byte:n :}
   state PEEK byte <> if E-MALFORMED throw then
   state ADVANCE ;

: WHITE? ( ^n -- bool )
   dup EOF? if drop false exit then
   PEEK XML-SPACE? ;

: SKIP-WHITE ( ^n -- )
   begin dup WHITE? while dup ADVANCE repeat drop ;

: INPUT-SCALAR-ENSURE ( ^n -- )
   {: state :}
   state state POSITION + @ 1+ INPUT-ENSURE 0= if exit then
   state PEEK {: lead:n :}
   lead $80 < if exit then
   lead $C2 < if E-UTF8 throw then
   lead $E0 < if 2 else
      lead $F0 < if 3 else
         lead $F5 < if 4 else E-UTF8 throw then
      then
   then {: width:n :}
   width 1 ?do
      state state POSITION + @ i + 1+ INPUT-ENSURE 0= if unloop exit then
      state state POSITION + @ i + AT {: byte:n :}
      byte $C0 and $80 <> if E-UTF8 throw then
      i 1 = if
         lead $E0 = byte $A0 < and
         lead $ED = byte $A0 >= and or
         lead $F0 = byte $90 < and or
         lead $F4 = byte $90 >= and or if E-UTF8 throw then
      then
   loop ;

: SCAN-SCALAR ( ^n -- n )
   {: state :}
   state INPUT? if state INPUT-SCALAR-ENSURE then
   state SOURCE$ state POSITION + @ SCALAR-AT
   state POSITION + ! ;

: INPUT-NUMERIC-LOOKAHEAD ( ^n -- )
   {: state :}
   state state POSITION + @ 3 + INPUT-ENSURE 0= if exit then
   state state POSITION + @ 2 + AT $78 = if 0 3 $10 else 0 2 10 then
   begin
      {: value:n count:n radix:n :}
      state state POSITION + @ count + 1+ INPUT-ENSURE 0= if exit then
      state state POSITION + @ count + AT
      dup $3B = if drop exit then
      HEX-DIGIT {: digit:n :}
      value digit radix NUMERIC-STEP count 1+ radix
   again ;

: INPUT-ENTITY-LOOKAHEAD ( ^n -- )
   {: state :}
   state state POSITION + @ 2 + INPUT-ENSURE 0= if exit then
   state state POSITION + @ 1+ AT $23 = if
      state INPUT-NUMERIC-LOOKAHEAD exit
   then
   2
   begin
      {: count:n :}
      count 7 > if exit then
      state state POSITION + @ count + INPUT-ENSURE 0= if exit then
      state state POSITION + @ count 1- + AT $3B = if exit then
      count 1+
   again ;

: SCAN-DECODE ( ^n n -- n n )
   {: state mode:n :}
   state INPUT? if
      state state POSITION + @ 1+ INPUT-ENSURE drop
      mode DECODE-RAW <> state PEEK $26 = and if
         state INPUT-ENTITY-LOOKAHEAD
      else
         state INPUT-SCALAR-ENSURE
         state PEEK $D = if
            state state POSITION + @ 2 + INPUT-ENSURE drop
         then
      then
   then
   state SOURCE$ state POSITION + @ mode DECODE-AT ;

\ An input reader keeps each open frame's name and each live binding's prefix and
\ URI in the scope buffer: SCOPE-REF appends them when the frame opens or the
\ binding is declared, SCOPE-CUT drops a frame's strings and its bindings' when it
\ pops. A reference below -2 is a scope offset.
: SCOPE-REF ( ^n n n -- n n )
   {: state start:n size:n :}
   state INPUT? 0= if start size exit then
   state STREAM@ SCOPE-BUF {: scope :}
   scope BUF-LEN {: index:n :}
   state start size SPAN$ BUF:N>BLEN scope BUF:APPEND-SPAN
   index negate 3 - size ;

: TOKEN-TRIM ( ^stream -- )
   {: stream :}
   stream TOKEN-BUF BUF-LEN {: used:n :}
   stream STREAM-INPUT-CAP @
   used max 1 max {: need:n :}
   stream TOKEN-BUF BUF-CAP 4 / need <= if exit then
   need MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN {: fresh :}
   stream TOKEN-BUF BUF:SPAN$ BUF:BLEN>N fresh SPAN:COPY
   stream TOKEN-BUF BUF:DISPOSE
   fresh SPAN:$ {: bytes cap:n :}
   bytes stream STREAM-TOKEN !
   used stream STREAM-TOKEN-LEN !
   cap stream STREAM-TOKEN-CAP ! ;

: SCOPE-TRIM ( ^stream -- )
   {: stream :}
   stream SCOPE-BUF BUF-LEN {: used:n :}
   used 16 max {: need:n :}
   stream SCOPE-BUF BUF-CAP 4 / need <= if exit then
   need MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN {: fresh :}
   stream SCOPE-BUF BUF:SPAN$ BUF:BLEN>N fresh SPAN:COPY
   stream SCOPE-BUF BUF:DISPOSE
   fresh SPAN:$ {: bytes cap:n :}
   bytes stream STREAM-SCOPE !
   used stream STREAM-SCOPE-LEN !
   cap stream STREAM-SCOPE-CAP ! ;

: SCOPE-CUT ( ^n n -- )
   {: state ref:n :}
   state INPUT? 0= if exit then
   state STREAM@ {: stream :}
   ref negate 3 - stream STREAM-SCOPE-LEN !
   stream SCOPE-TRIM ;

\ Before an event, drop the consumed bytes only when the next refill would not
\ fit or the buffer is four times what it keeps, and only once at least as many
\ bytes were consumed as remain: each move copies fewer bytes than were read
\ since the last one, so streaming costs time linear in the input.
: INPUT-REBASE ( ^n -- )
   {: state :}
   state STREAM@ {: stream :}
   state POSITION + @ {: consumed:n :}
   state SOURCE-LEN + @ consumed - {: tail:n :}
   consumed 0= consumed tail < or if exit then
   stream TOKEN-BUF BUF-CAP {: cap:n :}
   stream STREAM-INPUT-CAP @ {: chunk:n :}
   state SOURCE-LEN + @ chunk + cap <=
   cap 4 / chunk tail max <= and if exit then
   state SOURCE-IDX ptr-field @ consumed + tail BUF:N>BLEN
      stream TOKEN-BUF BUF:REPLACE
   stream TOKEN-TRIM
   stream TOKEN-BUF BUF:SPAN$ BUF:BLEN>N
      state SOURCE-LEN + ! state SOURCE-IDX ptr-field !
   stream STREAM-BASE @ consumed + stream STREAM-BASE !
   0 state POSITION + ! ;

: URI$ ( ^n n n -- ^u8 n )
   {: state start:n size:n :}
   start XML-URI = if s" http://www.w3.org/XML/1998/namespace" exit then
   start XMLNS-URI = if s" http://www.w3.org/2000/xmlns/" exit then
   state start size SPAN$ ;

\ A value of bytes $20..$7F other than `&` decodes to itself.
: PLAIN-VALUE? ( ^u8 n -- bool )
   {: source size:n :}
   size 0 ?do
      source i + c@ {: byte:n :}
      byte $20 < byte $26 = or byte $7F > or if false unloop exit then
   loop
   true ;

\ Decode a URI into destination, copying a plain one directly, with
\ DECODE-INTO's refusals in its order.
: URI-INTO ( ^u8 n ^u8 n -- n )
   {: source size:n destination cap:n :}
   source size PLAIN-VALUE? 0= if
      source size DECODE-ATTR destination cap DECODE-INTO exit
   then
   destination cap SPAN-CHECK
   size cap > if E-CAPACITY throw then
   source size destination cap OVERLAP? if E-ALIAS throw then
   source destination size BYTE-COPY
   size ;

: CURRENT-FRAME ( ^n -- ^n )
   dup OPEN-DEPTH + @ 1- FRAME ;

\ Release the three tables and an input reader's stream, leaving the record owning
\ nothing and the mode LEGACY-UTF8.
: RECORD-FREE ( ^n -- )
   {: state :}
   state FRAMES-IDX FRAME-CAP FRAME-CELLS TABLE-RELEASE
   state ATTRS-IDX ATTR-CAP ATTR-CELLS TABLE-RELEASE
   state NS-IDX NS-CAP NS-CELLS TABLE-RELEASE
   state INPUT? if
      state STREAM@
      LEGACY-UTF8 state SOURCE-MODE + !
      NULL-PTR state WIRE-STORAGE-IDX ptr-field !
      STREAM-CLOSE
   then ;


\ Refuse bytes that overlap the record: the reader writes it on every event.
: RECORD-CHECK ( ^n ^u8 n -- )
   {: state bytes size:n :}
   state BYTE-VIEW CURSOR-BYTES bytes size OVERLAP? if E-ALIAS throw then ;


\ A record a caller's throw abandoned still holds its tables: release them,
\ then zero the record, so a failed build releases exactly what it made.
: INIT-STATE ( ^n -- )
   {: state :}
   state RECORD-FREE
   CURSOR-CELLS 0 ?do 0 state i cells + ! loop ;


: READER-TABLES ( ^n -- )
   {: state :}
   state HASH-K0 + BYTE-VIEW 2 cells ENTROPY:FILL
   state 1 FRAMES-GROW
   state 1 ATTRS-GROW
   state 1 NS-GROW
   state NS-HEADS-CLEAR
   -1 state ATTR-INDEX + !
   XML-KIND:EOF KIND>N state TOKEN-KIND + ! ;


\ Answer the reader a build body made, or release it and rethrow the body's code.
: BUILT ( ^n n -- XML:reader )
   {: state code:n :}
   code 0<> if state RECORD-FREE code throw then
   state MINT ;

: BOM ( ^n -- )
   {: state :}
   state s\" \xFF\xFE" MATCH$ state s\" \xFE\xFF" MATCH$ or if
      E-ENCODING throw
   then
   state s\" <\z" MATCH$ state s\" \z<" MATCH$ or if
      E-ENCODING throw
   then
   state s\" \xEF\xBB\xBF" MATCH$ if state 3 ADVANCE-N then ;

: INIT-BODY ( ^n ^u8 n -- ^n ^u8 n )
   {: state source size:n :}
   source state SOURCE-IDX ptr-field !
   size state SOURCE-LEN + !
   state READER-TABLES
   state BOM
   state source size ;


\ The stream goes into the record before the tables, so a failed table step
\ leaves RECORD-FREE to release both.
: INPUT-BODY ( ^n ^n [ ^n SPAN:span<u8> -- n ] SPAN:span<u8> -- ^n ^n [ ^n SPAN:span<u8> -- n ] SPAN:span<u8> )
   {: state ctx fill input :}
   ctx fill input STREAM-OPEN
   STREAM>BYTES state WIRE-STORAGE-IDX ptr-field !
   INPUT-UTF8 state SOURCE-MODE + !
   state READER-TABLES
   state ctx fill input ;

public
: INIT ( ^cursor ^u8 n -- XML:reader )
   {: record source size:n :}
   source size SPAN-CHECK
   record >STATE {: state :}
   state source size RECORD-CHECK
   state INIT-STATE
   state source size ['] INIT-BODY catch
   >r 2drop drop r> {: code:n :}
   state code BUILT ;

\ Reads bounded UTF-8 or declared US-ASCII through a callback that fills up to the
\ chunk span's reach and answers its count: zero ends input, a negative count or one
\ past the reach is E-RANGE. The context, callback and chunk span stay live through
\ CLOSE. A long tag, comment or CDATA grows the event buffer.
: INIT-INPUT ( ^cursor ^n [ ^n SPAN:span<u8> -- n ] SPAN:span<u8> -- XML:reader )
   {: record ctx fill input :}
   input SPAN:$ SPAN-CHECK
   record >STATE {: state :}
   state input SPAN:$ RECORD-CHECK
   state INIT-STATE
   state ctx fill input ['] INPUT-BODY catch
   >r 2drop 2drop r> {: code:n :}
   state code BUILT ;

private
: SOURCE-READER-MODE ( ^n -- n )
   {: source-state :}
   source-state SRC-ENCODING-N {: encoding:n :}
   encoding XML-ENCODING:UTF8 ENCODING>N = if SOURCE-UTF8 exit then
   encoding XML-ENCODING:US-ASCII ENCODING>N = if SOURCE-ASCII exit then
   source-state SRC-BOM-LEN + @ 0= if
      encoding XML-ENCODING:UTF16-LE ENCODING>N = if
         SOURCE-UTF16-LE-DECL
      else
         SOURCE-UTF16-BE-DECL
      then
   else
      encoding XML-ENCODING:UTF16-LE ENCODING>N = if
         SOURCE-UTF16-LE-BOM
      else
         SOURCE-UTF16-BE-BOM
      then
   then ;

: SOURCE-BODY ( ^n ^n -- ^n ^n )
   {: state source-state :}
   source-state SRC-LEXICAL$ {: lexical size:n :}
   lexical state SOURCE-IDX ptr-field !
   size state SOURCE-LEN + !
   source-state SOURCE-READER-MODE state SOURCE-MODE + !
   source-state SRC-ORIGINAL$ {: original original-len:n :}
   original state WIRE-ORIGINAL-IDX ptr-field !
   original-len state WIRE-ORIGINAL-LEN + !
   source-state BYTE-VIEW state WIRE-STORAGE-IDX ptr-field !
   source-state SRC-STORAGE-LEN + @ state WIRE-STORAGE-LEN + !
   state READER-TABLES
   state source-state ;


: SOURCE-READER ( ^cursor ^n -- XML:reader )
   {: record source-state :}
   source-state SRC-LEXICAL$ SPAN-CHECK
   record >STATE {: state :}
   state source-state SRC-LEXICAL$ RECORD-CHECK
   state source-state SRC-ORIGINAL$ RECORD-CHECK
   state source-state BYTE-VIEW source-state SRC-STORAGE-LEN + @ RECORD-CHECK
   state INIT-STATE
   state source-state ['] SOURCE-BODY catch
   >r 2drop r> {: code:n :}
   state code BUILT ;

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
   >STATE RECORD-FREE ;

private
CAST: BYTES>CURSOR ( ^u8 -- ^cursor )

public
\ A record over the first CURSOR-BYTES of a caller's bytes, zero-filled so it
\ owns nothing; a shorter span is E-CAPACITY. A live record there leaks its tables.
: EMPTY-CURSOR ( ^u8 n -- ^cursor )
   {: bytes size:n :}
   bytes size SPAN-CHECK
   size CURSOR-BYTES < if E-CAPACITY throw then
   CURSOR-BYTES 0 ?do 0 bytes i + c! loop
   bytes BYTES>CURSOR ;

private
: LIVE ( ^n -- )
   FAILED + @ 0<> if E-STATE throw then ;

: NAMED ( ^n -- )
   {: state :}
   state LIVE
   state NAME-LEN + @ 0= if E-STATE throw then ;

: ACTIVE-ATTRIBUTE ( ^n -- ^n )
   {: state :}
   state LIVE
   state TOKEN-KIND + @ XML-KIND:START KIND>N <> if E-STATE throw then
   state ATTR-INDEX + @ {: index:n :}
   index 0 < index state ATTR-COUNT + @ >= or if E-STATE throw then
   state index ATTRIBUTE ;

: LOCAL-SPAN ( ^u8 n -- ^u8 n )
   {: name size:n :}
   size 0 ?do
      name i + c@ $3A = if
         name i 1+ + size i 1+ - unloop exit
      then
   loop
   name size ;

: OUTPUT-CHECK ( ^n ^u8 n -- )
   {: state destination cap:n :}
   destination cap SPAN-CHECK
   state SOURCE$ destination cap OVERLAP? if E-ALIAS throw then
   state destination cap RECORD-CHECK
   state WIRE-ORIGINAL-IDX ptr-field @ state WIRE-ORIGINAL-LEN + @
   destination cap OVERLAP? if E-ALIAS throw then
   state WIRE-STORAGE-IDX ptr-field @ state WIRE-STORAGE-LEN + @
   destination cap OVERLAP? if E-ALIAS throw then
   state INPUT? if
      state STREAM@ {: stream :}
      stream STREAM-INPUT @ stream STREAM-INPUT-CAP @
      destination cap OVERLAP? if E-ALIAS throw then
      stream STREAM>BYTES STREAM-CELLS cells
      destination cap OVERLAP? if E-ALIAS throw then
      stream STREAM-TOKEN @ stream STREAM-TOKEN-CAP @
      destination cap OVERLAP? if E-ALIAS throw then
      stream STREAM-SCOPE @ stream STREAM-SCOPE-CAP @
      destination cap OVERLAP? if E-ALIAS throw then
   then ;

\ Keep the reader below throwing workers' arguments. Native catch restores
\ stack depth, so public locals could overwrite the reader's saved stack cell.
: RAW-STATE ( ^n -- off len )
   {: state :}
   state LIVE
   state state RAW-OFF + @ ABS-OFF >OFF state RAW-LEN + @ >LEN ;

: RAW$-STATE ( ^n -- ^u8 n )
   {: state :}
   state LIVE
   state state RAW-OFF + @ state RAW-LEN + @ SPAN$ ;

: NAME$-STATE ( ^n -- ^u8 n )
   {: state :}
   state NAMED
   state state NAME-OFF + @ state NAME-LEN + @ SPAN$ ;

: DECODE-ARGS ( XML:reader ^u8 n -- XML:reader ^n ^u8 n )
   {: destination cap:n :}
   STATE destination cap ;

: URI-STATE ( ^n ^u8 n -- n )
   {: state destination cap:n :}
   state NAMED
   state destination cap OUTPUT-CHECK
   state state URI-OFF + @ state URI-LEN + @ URI$
   destination cap URI-INTO ;

: ATTR-RESET-STATE ( ^n -- )
   {: state :}
   state LIVE
   state TOKEN-KIND + @ XML-KIND:START KIND>N <> if E-STATE throw then
   -1 state ATTR-INDEX + ! ;

: ATTR-NEXT-STATE ( ^n -- bool )
   {: state :}
   state LIVE
   state TOKEN-KIND + @ XML-KIND:START KIND>N <> if E-STATE throw then
   state ATTR-INDEX + @ state ATTR-COUNT + @ >= if false exit then
   1 state ATTR-INDEX + +!
   state ATTR-INDEX + @ state ATTR-COUNT + @ < ;

: ATTR-RAW-STATE ( ^n -- off len )
   {: state :}
   state ACTIVE-ATTRIBUTE {: attr :}
   state attr 4 cells + @ ABS-OFF >OFF attr 5 cells + @ >LEN ;

: ATTR-NAME$-STATE ( ^n -- ^u8 n )
   {: state :}
   state ACTIVE-ATTRIBUTE {: attr :}
   state attr @ attr CELL + @ SPAN$ ;

: ATTR-VALUE$-STATE ( ^n -- ^u8 n )
   {: state :}
   state ACTIVE-ATTRIBUTE {: attr :}
   state attr 2 cells + @ attr 3 cells + @ SPAN$ ;

: ATTR-VALUE-STATE ( ^n -- off len )
   {: state :}
   state ACTIVE-ATTRIBUTE {: attr :}
   state attr 2 cells + @ ABS-OFF >OFF attr 3 cells + @ >LEN ;

: ATTR-URI-STATE ( ^n ^u8 n -- n )
   {: state destination cap:n :}
   state ACTIVE-ATTRIBUTE {: attr :}
   state destination cap OUTPUT-CHECK
   state attr 6 cells + @ attr 7 cells + @ URI$
   destination cap URI-INTO ;

: ATTR-TEXT-STATE ( ^n ^u8 n -- n )
   {: state destination cap:n :}
   state ACTIVE-ATTRIBUTE {: attr :}
   state destination cap OUTPUT-CHECK
   state attr 2 cells + @ attr 3 cells + @ SPAN$
   DECODE-ATTR destination cap DECODE-INTO ;

public
: KIND ( XML:reader -- XML:reader kind )
   STATE dup LIVE TOKEN-KIND + >KIND-CELL @ ;

: RAW ( XML:reader -- XML:reader off len )
   STATE RAW-STATE ;

: RAW$ ( XML:reader -- XML:reader ^u8 n )
   STATE RAW$-STATE ;

: NAME$ ( XML:reader -- XML:reader ^u8 n )
   STATE NAME$-STATE ;

: LOCAL$ ( XML:reader -- XML:reader ^u8 n )
   NAME$ LOCAL-SPAN ;

: DEPTH ( XML:reader -- XML:reader n )
   STATE dup LIVE OPEN-DEPTH + @ ;

: URI ( XML:reader ^u8 n -- XML:reader n )
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

: ATTR-NAME$ ( XML:reader -- XML:reader ^u8 n )
   STATE ATTR-NAME$-STATE ;

: ATTR-LOCAL$ ( XML:reader -- XML:reader ^u8 n )
   ATTR-NAME$ LOCAL-SPAN ;

: ATTR-VALUE$ ( XML:reader -- XML:reader ^u8 n )
   STATE ATTR-VALUE$-STATE ;

: ATTR-VALUE ( XML:reader -- XML:reader off len )
   STATE ATTR-VALUE-STATE ;

: ATTR-URI ( XML:reader ^u8 n -- XML:reader n )
   DECODE-ARGS ATTR-URI-STATE ;

: ATTR-TEXT ( XML:reader ^u8 n -- XML:reader n )
   DECODE-ARGS ATTR-TEXT-STATE ;

;package
