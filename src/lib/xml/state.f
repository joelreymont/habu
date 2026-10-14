require src/lib/xml/source.f
require src/lib/byte-buffer.f

package XML
public
DEFLINEAR XML:reader
ENUM kind start end text comment pi cdata eof ;ENUM

private
CAST: KIND>N ( kind -- n )
CAST: >KIND ( n -- kind )

26 constant HEADER-CELLS
5 constant FRAME-CELLS
8 constant ATTR-CELLS
4 constant NS-CELLS
FRAME-CELLS ATTR-CELLS + NS-CELLS + constant CAP-CELLS
0 constant SOURCE-IDX
1 cells constant SOURCE-LEN
2 cells constant POSITION
3 cells constant CAPACITY
4 cells constant OPEN-DEPTH
5 cells constant NS-COUNT
6 cells constant ATTR-COUNT
7 cells constant ATTR-INDEX
8 cells constant RAW-OFF
9 cells constant RAW-LEN
10 cells constant TOKEN-KIND
11 cells constant NAME-OFF
12 cells constant NAME-LEN
13 cells constant VALUE-OFF
14 cells constant VALUE-LEN
15 cells constant URI-OFF
16 cells constant URI-LEN
17 cells constant PENDING
18 cells constant ROOT-COUNT
19 cells constant FAILED
20 cells constant STORAGE-LEN
21 cells constant SOURCE-MODE
22 constant WIRE-ORIGINAL-IDX
23 cells constant WIRE-ORIGINAL-LEN
24 constant WIRE-STORAGE-IDX
25 cells constant WIRE-STORAGE-LEN

0 constant LEGACY-UTF8
1 constant SOURCE-UTF8
2 constant SOURCE-UTF16-LE-BOM
3 constant SOURCE-UTF16-BE-BOM
4 constant SOURCE-UTF16-LE-DECL
5 constant SOURCE-UTF16-BE-DECL
6 constant INPUT-UTF8

\ An input reader owns only its current lexical window and active scope strings.
\ Its metadata and byte buffers are released by CLOSE, including after a
\ failed NEXT. The caller retains the callback, context, input and parser store.
STRUCTURE stream DERIVE addr
   FIELD fill [ ptr n SPAN:span<u8> -- n ]
   FIELD ctx ptr n
   FIELD input ptr u8
   FIELD input-cap n
   FIELD eof n
   FIELD base n
   FIELD started n
   FIELD token ptr u8
   FIELD token-len n
   FIELD token-cap n
   FIELD scope ptr u8
   FIELD scope-len n
   FIELD scope-cap n
   FIELD spare ptr u8
   FIELD spare-len n
   FIELD spare-cap n
;STRUCTURE

: INPUT? ( ptr n -- bool ) SOURCE-MODE + @ INPUT-UTF8 = ;

CAST: >STREAM ( ptr u8 -- ptr stream )
CAST: STREAM>BYTES ( ptr stream -- ptr u8 )

: STREAM@ ( ptr n -- ptr stream )
   WIRE-STORAGE-IDX ptr-field @ >STREAM ;

: ABS-OFF ( ptr n n -- n )
   {: state offset:n :}
   state INPUT? if state STREAM@ STREAM-BASE @ offset + else offset then ;

: TOKEN-BUF ( ptr stream -- ptr ptr u8 ) STREAM-TOKEN ;
: SCOPE-BUF ( ptr stream -- ptr ptr u8 ) STREAM-SCOPE ;
: SPARE-BUF ( ptr stream -- ptr ptr u8 ) STREAM-SPARE ;

: BUF-LEN ( ptr a -- n ) BUF:LEN@ BUF:BLEN>N ;
: BUF-CAP ( ptr a -- n ) BUF:CAP@ BUF:BLEN>N ;

: SCOPE$ ( ptr n -- ptr u8 n )
   STREAM@ SCOPE-BUF BUF:SPAN$ BUF:BLEN>N ;

: STREAM-CLOSE ( ptr stream -- )
   {: stream :}
   stream STREAM-TOKEN-CAP @ 0<> if stream TOKEN-BUF BUF:DISPOSE then
   stream STREAM-SCOPE-CAP @ 0<> if stream SCOPE-BUF BUF:DISPOSE then
   stream STREAM-SPARE-CAP @ 0<> if stream SPARE-BUF BUF:DISPOSE then
   stream STREAM>BYTES STREAM-CELLS cells
      MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;

: STREAM-OPEN-BUFS ( ptr stream n -- ptr stream n )
   {: stream cap:n :}
   stream TOKEN-BUF cap BUF:N>BLEN BUF:INIT
   stream SCOPE-BUF 16 BUF:N>BLEN BUF:INIT
   stream SPARE-BUF 16 BUF:N>BLEN BUF:INIT
   stream cap ;

: STREAM-OPEN ( ptr n [ ptr n SPAN:span<u8> -- n ] SPAN:span<u8> -- ptr stream )
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

\ A reader token is the caller's validated storage; only these rows cross it.
LINEAR: MINT ( ptr n -- XML:reader )
LINEAR: ERASE ( XML:reader -- ptr n )

: STATE ( XML:reader -- XML:reader ptr n )
   ERASE dup MINT swap ;

: CAP-CHECK ( n -- )
   dup 0 < if E-CAPACITY throw then
   MAX-SIZE HEADER-CELLS cells - CAP-CELLS cells / > if
      E-CAPACITY throw
   then ;

public
: STORAGE-BYTES ( n -- n )
   dup CAP-CHECK
   CAP-CELLS * HEADER-CELLS + cells ;

private
: FRAME ( ptr n n -- ptr n )
   {: state index:n :}
   state HEADER-CELLS index FRAME-CELLS * + cells + ;

: ATTRIBUTE ( ptr n n -- ptr n )
   {: state index:n :}
   state HEADER-CELLS state CAPACITY + @ FRAME-CELLS * +
   index ATTR-CELLS * + cells + ;

: NAMESPACE ( ptr n n -- ptr n )
   {: state index:n :}
   state HEADER-CELLS state CAPACITY + @ FRAME-CELLS ATTR-CELLS + * +
   index NS-CELLS * + cells + ;

: SOURCE$ ( ptr n -- ptr u8 n )
   {: state :}
   state SOURCE-IDX ptr-field @ state SOURCE-LEN + @ ;

: INPUT-REFILL ( ptr n -- bool )
   {: state :}
   state STREAM@ {: stream :}
   stream STREAM-EOF @ 0<> if false exit then
   stream STREAM-CTX @
   stream STREAM-INPUT @ stream STREAM-INPUT-CAP @ SPAN:MAKE
   stream STREAM-FILL @ execute {: count:n :}
   count 0 < count stream STREAM-INPUT-CAP @ > or if E-RANGE throw then
   count 0= if 1 stream STREAM-EOF ! false exit then
   stream STREAM-INPUT @ count BUF:N>BLEN stream TOKEN-BUF BUF:APPEND-SPAN
   stream TOKEN-BUF BUF:SPAN$ BUF:BLEN>N
   state SOURCE-LEN + ! state SOURCE-IDX ptr-field !
   true ;

: INPUT-ENSURE ( ptr n n -- bool )
   {: state want:n :}
   want 0 < if E-RANGE throw then
   begin want state SOURCE-LEN + @ > while
      state INPUT-REFILL 0= if false exit then
   repeat
   true ;

: AT ( ptr n n -- n )
   {: state index:n :}
   state INPUT? if state index 1+ INPUT-ENSURE drop then
   state SOURCE$ index BYTE@ ;

: EOF? ( ptr n -- bool )
   {: state :}
   state INPUT? if state state POSITION + @ 1+ INPUT-ENSURE drop then
   state POSITION + @ state SOURCE-LEN + @ = ;

: PEEK ( ptr n -- n )
   dup POSITION + @ AT ;

: ADVANCE ( ptr n -- )
   POSITION + 1 swap +! ;

: ADVANCE-N ( ptr n n -- )
   {: state count:n :}
   state INPUT? if
      state state POSITION + @ count + INPUT-ENSURE drop
   then
   count state SOURCE-LEN + @ state POSITION + @ - > if
      E-TRUNCATED throw
   then
   count state POSITION + +! ;

: SPAN$ ( ptr n n n -- ptr u8 n )
   {: state start:n size:n :}
   start -2 < if
      state SCOPE$ drop start negate 3 - + size exit
   then
   state SOURCE-IDX ptr-field @ start + size ;

: MATCH$ ( ptr n ptr u8 n -- bool )
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

: REQUIRE-BYTE ( ptr n n -- )
   {: state byte:n :}
   state PEEK byte <> if E-MALFORMED throw then
   state ADVANCE ;

: WHITE? ( ptr n -- bool )
   dup EOF? if drop false exit then
   PEEK XML-SPACE? ;

: SKIP-WHITE ( ptr n -- )
   begin dup WHITE? while dup ADVANCE repeat drop ;

: INPUT-SCALAR-ENSURE ( ptr n -- )
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

: SCAN-SCALAR ( ptr n -- n )
   {: state :}
   state INPUT? if state INPUT-SCALAR-ENSURE then
   state SOURCE$ state POSITION + @ SCALAR-AT
   state POSITION + ! ;

: INPUT-NUMERIC-LOOKAHEAD ( ptr n -- )
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

: INPUT-ENTITY-LOOKAHEAD ( ptr n -- )
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

: SCAN-DECODE ( ptr n n -- n n )
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

\ Negative references below -2 point into the active-scope buffer. Rebuild it
\ from the live frames before dropping consumed lexical bytes.
: SAVE-REF ( ptr n n n -- n )
   {: state start:n size:n :}
   start XML-URI = start XMLNS-URI = or if start exit then
   state start size SPAN$ BUF:N>BLEN
   state STREAM@ SPARE-BUF {: spare :}
   spare BUF-LEN {: index:n :}
   spare BUF:APPEND-SPAN
   index negate 3 - ;

: SAVE-FRAME-REFS ( ptr n n -- )
   {: state index:n :}
   state index FRAME {: frame :}
   state frame @ frame CELL + @ SAVE-REF frame !
   state frame 3 cells + @ frame 4 cells + @ SAVE-REF
      frame 3 cells + ! ;

: SAVE-NS-REFS ( ptr n n -- )
   {: state index:n :}
   state index NAMESPACE {: binding :}
   state binding @ binding CELL + @ SAVE-REF binding !
   state binding 2 cells + @ binding 3 cells + @ SAVE-REF
      binding 2 cells + ! ;

: SWAP-SCOPES ( ptr stream -- )
   {: stream :}
   stream STREAM-SCOPE @ {: old-ptr :}
   stream STREAM-SCOPE-LEN @ {: old-len:n :}
   stream STREAM-SCOPE-CAP @ {: old-cap:n :}
   stream STREAM-SPARE @ stream STREAM-SCOPE !
   stream STREAM-SPARE-LEN @ stream STREAM-SCOPE-LEN !
   stream STREAM-SPARE-CAP @ stream STREAM-SCOPE-CAP !
   old-ptr stream STREAM-SPARE !
   old-len stream STREAM-SPARE-LEN !
   old-cap stream STREAM-SPARE-CAP ! ;

: TOKEN-TRIM ( ptr stream -- )
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

: SCOPE-TRIM ( ptr stream -- )
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

: SPARE-TRIM ( ptr stream -- )
   {: stream :}
   stream SPARE-BUF BUF-CAP 4 / 16 <= if exit then
   stream SPARE-BUF BUF:DISPOSE
   stream SPARE-BUF 16 BUF:N>BLEN BUF:INIT ;

: INPUT-REBASE ( ptr n -- )
   {: state :}
   state STREAM@ {: stream :}
   stream SPARE-BUF BUF:CLEAR
   state OPEN-DEPTH + @ 0 ?do state i SAVE-FRAME-REFS loop
   state NS-COUNT + @ 0 ?do state i SAVE-NS-REFS loop
   stream SWAP-SCOPES
   stream SCOPE-TRIM
   stream SPARE-TRIM
   state POSITION + @ {: consumed:n :}
   state SOURCE-LEN + @ consumed - {: tail:n :}
   state SOURCE-IDX ptr-field @ consumed + tail BUF:N>BLEN
      stream TOKEN-BUF BUF:REPLACE
   stream TOKEN-TRIM
   stream TOKEN-BUF BUF:SPAN$ BUF:BLEN>N
      state SOURCE-LEN + ! state SOURCE-IDX ptr-field !
   stream STREAM-BASE @ consumed + stream STREAM-BASE !
   0 state POSITION + ! ;

: URI$ ( ptr n n n -- ptr u8 n )
   {: state start:n size:n :}
   start XML-URI = if s" http://www.w3.org/XML/1998/namespace" exit then
   start XMLNS-URI = if s" http://www.w3.org/2000/xmlns/" exit then
   state start size SPAN$ ;

: CURRENT-FRAME ( ptr n -- ptr n )
   dup OPEN-DEPTH + @ 1- FRAME ;

: STORAGE-CHECK ( ptr n n ptr u8 n -- )
   {: storage cap:n source size:n :}
   storage BYTE-VIEW cap SPAN-CHECK
   storage BYTE-VIEW BYTE-ADDRESS CELL 1- and 0<> if E-STORAGE throw then
   cap 1 STORAGE-BYTES < if E-CAPACITY throw then
   source size SPAN-CHECK
   storage BYTE-VIEW cap source size OVERLAP? if E-ALIAS throw then ;

: INIT-STATE ( ptr n n ptr u8 n -- )
   {: storage cap:n source size:n :}
   HEADER-CELLS 0 ?do 0 storage i cells + ! loop
   source storage SOURCE-IDX ptr-field !
   size storage SOURCE-LEN + !
   cap HEADER-CELLS cells - CAP-CELLS cells / storage CAPACITY + !
   cap storage STORAGE-LEN + !
   -1 storage ATTR-INDEX + !
   XML-KIND:EOF KIND>N storage TOKEN-KIND + ! ;

: BOM ( ptr n -- )
   {: state :}
   state s\" \xFF\xFE" MATCH$ state s\" \xFE\xFF" MATCH$ or if
      E-ENCODING throw
   then
   state s\" <\z" MATCH$ state s\" \z<" MATCH$ or if
      E-ENCODING throw
   then
   state s\" \xEF\xBB\xBF" MATCH$ if state 3 ADVANCE-N then ;

public
: INIT ( ptr n n ptr u8 n -- XML:reader )
   {: storage cap:n source size:n :}
   storage cap source size STORAGE-CHECK
   storage cap source size INIT-STATE
   storage BOM
   storage MINT ;

: INIT-INPUT ( ptr n [ ptr n SPAN:span<u8> -- n ] SPAN:span<u8> ptr n n -- XML:reader )
   {: ctx fill input storage cap:n :}
   storage cap NULL-PTR 0 STORAGE-CHECK
   input SPAN:$ {: bytes size:n :}
   bytes size SPAN-CHECK
   storage BYTE-VIEW cap bytes size OVERLAP? if E-ALIAS throw then
   ctx fill input STREAM-OPEN {: stream :}
   storage cap NULL-PTR 0 INIT-STATE
   INPUT-UTF8 storage SOURCE-MODE + !
   stream STREAM>BYTES storage WIRE-STORAGE-IDX ptr-field !
   storage MINT ;

private
: SOURCE-READER-MODE ( ptr n -- n )
   {: source-state :}
   source-state SRC-ENCODING-N {: encoding:n :}
   encoding XML-ENCODING:UTF8 ENCODING>N = if SOURCE-UTF8 exit then
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

: SOURCE-READER ( ptr n ptr n n -- XML:reader )
   {: source-state storage cap:n :}
   source-state SRC-LEXICAL$ {: lexical size:n :}
   storage cap lexical size STORAGE-CHECK
   source-state SRC-ORIGINAL$ storage BYTE-VIEW cap OVERLAP? if
      E-ALIAS throw
   then
   source-state BYTE-VIEW source-state SRC-STORAGE-LEN + @
   storage BYTE-VIEW cap OVERLAP? if E-ALIAS throw then
   storage cap lexical size INIT-STATE
   source-state SOURCE-READER-MODE storage SOURCE-MODE + !
   source-state SRC-ORIGINAL$ {: original original-len:n :}
   original storage WIRE-ORIGINAL-IDX ptr-field !
   original-len storage WIRE-ORIGINAL-LEN + !
   source-state BYTE-VIEW storage WIRE-STORAGE-IDX ptr-field !
   source-state SRC-STORAGE-LEN + @ storage WIRE-STORAGE-LEN + !
   storage MINT ;

: SOURCE-READER-ARGS ( XML:source ptr n n -- XML:source ptr n ptr n n )
   {: storage cap:n :}
   SRC-STATE storage cap ;

public
: INIT-SOURCE ( XML:source ptr n n -- XML:source XML:reader )
   SOURCE-READER-ARGS SOURCE-READER ;

: OWNER-READER ( source-owner ptr n n -- XML:reader )
   {: owner storage cap:n :}
   owner OWNER-STATE storage cap SOURCE-READER ;

: CLOSE ( XML:reader -- )
   ERASE dup INPUT? if dup STREAM@ STREAM-CLOSE then drop ;

private
: LIVE ( ptr n -- )
   FAILED + @ 0<> if E-STATE throw then ;

: NAMED ( ptr n -- )
   {: state :}
   state LIVE
   state NAME-LEN + @ 0= if E-STATE throw then ;

: ACTIVE-ATTRIBUTE ( ptr n -- ptr n )
   {: state :}
   state LIVE
   state TOKEN-KIND + @ XML-KIND:START KIND>N <> if E-STATE throw then
   state ATTR-INDEX + @ {: index:n :}
   index 0 < index state ATTR-COUNT + @ >= or if E-STATE throw then
   state index ATTRIBUTE ;

: LOCAL-SPAN ( ptr u8 n -- ptr u8 n )
   {: name size:n :}
   size 0 ?do
      name i + c@ $3A = if
         name i 1+ + size i 1+ - unloop exit
      then
   loop
   name size ;

: OUTPUT-CHECK ( ptr n ptr u8 n -- )
   {: state destination cap:n :}
   destination cap SPAN-CHECK
   state SOURCE$ destination cap OVERLAP? if E-ALIAS throw then
   state BYTE-VIEW state STORAGE-LEN + @
   destination cap OVERLAP? if E-ALIAS throw then
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
      stream STREAM-SPARE @ stream STREAM-SPARE-CAP @
      destination cap OVERLAP? if E-ALIAS throw then
   then ;

\ Keep the reader below throwing workers' arguments. Native catch restores
\ stack depth, so public locals could overwrite the reader's saved stack cell.
: RAW-STATE ( ptr n -- off len )
   {: state :}
   state LIVE
   state state RAW-OFF + @ ABS-OFF >OFF state RAW-LEN + @ >LEN ;

: RAW$-STATE ( ptr n -- ptr u8 n )
   {: state :}
   state LIVE
   state state RAW-OFF + @ state RAW-LEN + @ SPAN$ ;

: NAME$-STATE ( ptr n -- ptr u8 n )
   {: state :}
   state NAMED
   state state NAME-OFF + @ state NAME-LEN + @ SPAN$ ;

: DECODE-ARGS ( XML:reader ptr u8 n -- XML:reader ptr n ptr u8 n )
   {: destination cap:n :}
   STATE destination cap ;

: URI-STATE ( ptr n ptr u8 n -- n )
   {: state destination cap:n :}
   state NAMED
   state destination cap OUTPUT-CHECK
   state state URI-OFF + @ state URI-LEN + @ URI$
   DECODE-ATTR destination cap DECODE-INTO ;

: ATTR-RESET-STATE ( ptr n -- )
   {: state :}
   state LIVE
   state TOKEN-KIND + @ XML-KIND:START KIND>N <> if E-STATE throw then
   -1 state ATTR-INDEX + ! ;

: ATTR-NEXT-STATE ( ptr n -- bool )
   {: state :}
   state LIVE
   state TOKEN-KIND + @ XML-KIND:START KIND>N <> if E-STATE throw then
   state ATTR-INDEX + @ state ATTR-COUNT + @ >= if false exit then
   1 state ATTR-INDEX + +!
   state ATTR-INDEX + @ state ATTR-COUNT + @ < ;

: ATTR-RAW-STATE ( ptr n -- off len )
   {: state :}
   state ACTIVE-ATTRIBUTE {: attr :}
   state attr 4 cells + @ ABS-OFF >OFF attr 5 cells + @ >LEN ;

: ATTR-NAME$-STATE ( ptr n -- ptr u8 n )
   {: state :}
   state ACTIVE-ATTRIBUTE {: attr :}
   state attr @ attr CELL + @ SPAN$ ;

: ATTR-VALUE$-STATE ( ptr n -- ptr u8 n )
   {: state :}
   state ACTIVE-ATTRIBUTE {: attr :}
   state attr 2 cells + @ attr 3 cells + @ SPAN$ ;

: ATTR-VALUE-STATE ( ptr n -- off len )
   {: state :}
   state ACTIVE-ATTRIBUTE {: attr :}
   state attr 2 cells + @ ABS-OFF >OFF attr 3 cells + @ >LEN ;

: ATTR-URI-STATE ( ptr n ptr u8 n -- n )
   {: state destination cap:n :}
   state ACTIVE-ATTRIBUTE {: attr :}
   state destination cap OUTPUT-CHECK
   state attr 6 cells + @ attr 7 cells + @ URI$
   DECODE-ATTR destination cap DECODE-INTO ;

: ATTR-TEXT-STATE ( ptr n ptr u8 n -- n )
   {: state destination cap:n :}
   state ACTIVE-ATTRIBUTE {: attr :}
   state destination cap OUTPUT-CHECK
   state attr 2 cells + @ attr 3 cells + @ SPAN$
   DECODE-ATTR destination cap DECODE-INTO ;

public
: KIND ( XML:reader -- XML:reader kind )
   STATE dup LIVE TOKEN-KIND + @ >KIND ;

: RAW ( XML:reader -- XML:reader off len )
   STATE RAW-STATE ;

: RAW$ ( XML:reader -- XML:reader ptr u8 n )
   STATE RAW$-STATE ;

: NAME$ ( XML:reader -- XML:reader ptr u8 n )
   STATE NAME$-STATE ;

: LOCAL$ ( XML:reader -- XML:reader ptr u8 n )
   NAME$ LOCAL-SPAN ;

: DEPTH ( XML:reader -- XML:reader n )
   STATE dup LIVE OPEN-DEPTH + @ ;

: URI ( XML:reader ptr u8 n -- XML:reader n )
   DECODE-ARGS URI-STATE ;

: ATTR-RESET ( XML:reader -- XML:reader )
   STATE ATTR-RESET-STATE ;

: ATTR-NEXT ( XML:reader -- XML:reader bool )
   STATE ATTR-NEXT-STATE ;

: ATTR-RAW ( XML:reader -- XML:reader off len )
   STATE ATTR-RAW-STATE ;

: ATTR-NAME$ ( XML:reader -- XML:reader ptr u8 n )
   STATE ATTR-NAME$-STATE ;

: ATTR-LOCAL$ ( XML:reader -- XML:reader ptr u8 n )
   ATTR-NAME$ LOCAL-SPAN ;

: ATTR-VALUE$ ( XML:reader -- XML:reader ptr u8 n )
   STATE ATTR-VALUE$-STATE ;

: ATTR-VALUE ( XML:reader -- XML:reader off len )
   STATE ATTR-VALUE-STATE ;

: ATTR-URI ( XML:reader ptr u8 n -- XML:reader n )
   DECODE-ARGS ATTR-URI-STATE ;

: ATTR-TEXT ( XML:reader ptr u8 n -- XML:reader n )
   DECODE-ARGS ATTR-TEXT-STATE ;

;package
