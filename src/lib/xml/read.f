split-str
\ read.f - the XML event scanner: NEXT, CONTENT and TEXT.

require src/lib/xml/names.f
require src/lib/unicode/visible.f

package XML
private
: EVENT-RESET ( ^n -- )
   {: state :}
   state POSITION + @ state RAW-OFF + !
   0 state RAW-LEN + !
   0 state NAME-LEN + !
   0 state VALUE-LEN + !
   0 state URI-OFF + !
   0 state URI-LEN + !
   0 state ATTR-COUNT + !
   -1 state ATTR-INDEX + ! ;

: EVENT-FINISH ( ^n kind -- )
   {: state token:kind :}
   state POSITION + @ state RAW-OFF + @ - state RAW-LEN + !
   token KIND>N state TOKEN-KIND + ! ;

: VALUE-BEGIN ( ^n -- )
   dup POSITION + @ swap VALUE-OFF + ! ;

: VALUE-END ( ^n -- )
   {: state :}
   state POSITION + @ state VALUE-OFF + @ - state VALUE-LEN + ! ;

: SCAN-ELEMENT-NAME ( ^n -- )
   {: state :}
   state SCAN-NAME {: start:n size:n :}
   state start size QNAME-CHECK
   start state NAME-OFF + !
   size state NAME-LEN + ! ;

: OPEN-END? ( ^n -- bool )
   {: state :}
   state PEEK $3E = state s" />" MATCH$ or ;

: SCAN-OPEN-ATTRIBUTES ( ^n -- bool )
   {: state :}
   begin state OPEN-END? 0= while
      state WHITE? 0= if
         state state POSITION + @
         s" white space, `>` or `/>` after the element name or an attribute"
         s" separate attributes with white space, and end the start tag with `>` or `/>`"
         E-MALFORMED REJECT
      then
      state SKIP-WHITE
      state OPEN-END? 0= if state SCAN-ATTRIBUTE then
   repeat
   state PEEK $2F = {: empty:bool :}
   empty if state ADVANCE then
   state $3E REQUIRE-BYTE
   empty ;

: PUSH-CHECK ( ^n -- )
   {: state :}
   state OPEN-DEPTH + @ 0= if
      state ROOT-COUNT + @ 0<> if
         state state POSITION + @
         s" white space, a comment or a processing instruction after the root element"
         s" wrap the elements in one root element"
         E-MALFORMED REJECT
      then
      1 state ROOT-COUNT + !
   then
   state state OPEN-DEPTH + @ 1+ FRAMES-GROW ;

: SAVE-FRAME ( ^n -- )
   {: state :}
   state state OPEN-DEPTH + @ FRAME {: frame :}
   state state NAME-OFF + @ state NAME-LEN + @ SCOPE-REF
   frame FRAME-NAME-U + ! frame FRAME-NAME + !
   state NS-COUNT + @ frame FRAME-NS-COUNT + ! ;

: PUSH-FRAME ( ^n -- )
   {: state :}
   state state OPEN-DEPTH + @ FRAME {: frame :}
   state URI-OFF + @ frame FRAME-URI + !
   state URI-LEN + @ frame FRAME-URI-U + !
   1 state OPEN-DEPTH + +! ;

: READ-START ( ^n -- )
   {: state :}
   state PUSH-CHECK
   state ADVANCE
   state SCAN-ELEMENT-NAME
   state SCAN-OPEN-ATTRIBUTES {: empty:bool :}
   state SAVE-FRAME
   state RESOLVE-ELEMENT
   state PUSH-FRAME
   empty if EMPTY-END state PENDING + ! then
   state XML-KIND:START EVENT-FINISH ;

\ The expected text `</NAME>` names the open element, cut as a found token is.
: END-TAG-SCOPE ( ^n n str n str n ^u8 NUM:alloc-byte-len -- )
   drop {: state off:n fa:^ fu:n code:n name:^ len:n form:n buf:^ :}
   buf 0 s" `</" PUT over + name len UNICODE-VISIBLE:PUT over -
   form FOUND-CUT = if CUT$ PUT then
   s" >`" PUT {: size:n :} drop
   state off buf size fa fu code REJECT ;

\ Rejects the reader at off, expecting the open element's end tag.
: END-TAG-REJECT ( ^n n str n -- )
   {: state off:n fa:^ fu:n code:n :}
   state CURRENT-FRAME {: frame :}
   state frame FRAME-NAME + @ frame FRAME-NAME-U + @ SPAN$ {: name:^ u:n :}
   name u CAPPED {: len:n form:n :}
   name len UNICODE-VISIBLE:LEN {: shown:n :}
   state off fa fu code name len form
   form FOUND-CUT = if shown CUT$ nip + else shown then 5 + MEM:BYTES-ALLOC-LEN
   [: END-TAG-SCOPE ;] MEM:WITH-BYTES ;

: MATCH-END-NAME ( ^n -- )
   {: state :}
   state CURRENT-FRAME {: frame :}
   state frame FRAME-NAME + @ frame FRAME-NAME-U + @ SPAN$
   state state NAME-OFF + @ state NAME-LEN + @ SPAN$ BYTES= 0= if
      state state NAME-OFF + @
      s" close elements in the reverse of the order they were opened"
      E-MALFORMED END-TAG-REJECT
   then
   frame FRAME-URI + @ state URI-OFF + !
   frame FRAME-URI-U + @ state URI-LEN + ! ;

: READ-END ( ^n -- )
   {: state :}
   state OPEN-DEPTH + @ 0= if
      state state POSITION + @ s" an end tag only after its start tag"
      s" remove the end tag, or write its start tag before it"
      E-MALFORMED REJECT
   then
   state 2 ADVANCE-N
   state SCAN-ELEMENT-NAME
   state MATCH-END-NAME
   state SKIP-WHITE
   state $3E REQUIRE-BYTE
   POP-NEXT state PENDING + !
   state XML-KIND:END EVENT-FINISH ;

: POP-FRAME ( ^n -- )
   {: state :}
   state CURRENT-FRAME {: frame :}
   state frame FRAME-NS-COUNT + @ POP-BINDINGS
   state frame FRAME-NAME + @ SCOPE-CUT
   -1 state OPEN-DEPTH + +!
   0 state PENDING + ! ;

: READ-EMPTY-END ( ^n -- )
   {: state :}
   state CURRENT-FRAME {: frame :}
   frame FRAME-NAME + @ state NAME-OFF + !
   frame FRAME-NAME-U + @ state NAME-LEN + !
   frame FRAME-URI + @ state URI-OFF + !
   frame FRAME-URI-U + @ state URI-LEN + !
   POP-NEXT state PENDING + !
   state XML-KIND:END EVENT-FINISH ;

: TEXT-CONTINUES? ( ^n -- bool )
   {: state :}
   state INPUT? if
      state POSITION + @ state VALUE-OFF + @ -
      state STREAM@ STREAM-INPUT-CAP @ >= if false exit then
   then
   state EOF? if false exit then
   state PEEK $3C <> ;

: TEXT-SCALAR ( ^n -- )
   {: state :}
   state s" ]]>" MATCH$ if
      state state POSITION + @ s" text without `]]>`"
      s" write `]]&gt;` for a literal `]]>` in text" E-MALFORMED REJECT
   then
   state OPEN-DEPTH + @ 0= if
      state PEEK XML-SPACE? 0= if
         state state POSITION + @ s" only white space outside the root element"
         s" move the text inside the root element, or remove it"
         E-MALFORMED REJECT
      then
   then
   state DECODE-TEXT SCAN-DECODE
   state POSITION + ! drop ;

: READ-TEXT ( ^n -- )
   {: state :}
   state VALUE-BEGIN
   begin state TEXT-CONTINUES? while state TEXT-SCALAR repeat
   state VALUE-END
   state XML-KIND:TEXT EVENT-FINISH ;

: READ-COMMENT ( ^n -- )
   {: state :}
   state 4 ADVANCE-N
   state VALUE-BEGIN
   begin state s" -->" MATCH$ 0= while
      state s" --" MATCH$ if
         state state POSITION + @ s" `-->` or comment text without `--`"
         s" remove `--` from the comment, or end the comment with `-->`"
         E-MALFORMED REJECT
      then
      state SCAN-SCALAR drop
   repeat
   state VALUE-END
   state 3 ADVANCE-N
   state XML-KIND:COMMENT EVENT-FINISH ;

: READ-CDATA ( ^n -- )
   {: state :}
   state OPEN-DEPTH + @ 0= if
      state state POSITION + @ s" a CDATA section only inside the root element"
      s" move the CDATA section inside the root element, or remove it"
      E-MALFORMED REJECT
   then
   state 9 ADVANCE-N
   state VALUE-BEGIN
   begin state s" ]]>" MATCH$ 0= while state SCAN-SCALAR drop repeat
   state VALUE-END
   state 3 ADVANCE-N
   state XML-KIND:CDATA EVENT-FINISH ;

\ Whether the declaration starts the document; a plain reader's may follow a BOM.
: DECLARATION-FIRST? ( ^n -- bool )
   {: state :}
   state INPUT? if
      state state RAW-OFF + @ ABS-OFF
      state STREAM@ STREAM-STARTED @ 1- = exit
   then
   state RAW-OFF + @ 0= if true exit then
   state SOURCE-MODE + @ LEGACY-UTF8 <> if false exit then
   state RAW-OFF + @ 3 = if
      state 0 3 SPAN$ s\" \xEF\xBB\xBF" BYTES= exit
   then
   false ;

: DECLARATION-POSITION ( ^n -- )
   {: state :}
   state DECLARATION-FIRST? 0= if
      state state RAW-OFF + @ s" `<?xml` only at the start of the document"
      s" move the XML declaration to the start of the document, or remove it"
      E-MALFORMED REJECT
   then ;

: DECLARATION-NAME? ( ^n n str -- bool )
   {: state index:n name size:n :}
   state state index ATTRIBUTE name size NAME-MATCH? ;

: DECLARATION-VALUE$ ( ^n n -- str )
   {: state index:n :}
   state state index ATTRIBUTE ATTRIBUTE-VALUE$ ;

: DECLARATION-NAME-OFF ( ^n n -- n )
   ATTRIBUTE ATTRIBUTE-NAME + @ ;

: DECLARATION-VALUE-OFF ( ^n n -- n )
   ATTRIBUTE ATTRIBUTE-VALUE + @ ;

: VERSION-CHECK ( ^n -- )
   {: state :}
   state ATTR-COUNT + @ 0= if
      state state POSITION + @ s\" `version=\q1.0\q`"
      s\" write `version=\q1.0\q` after `<?xml`" E-MALFORMED REJECT
   then
   state 0 s" version" DECLARATION-NAME? 0= if
      state state 0 DECLARATION-NAME-OFF s" `version` as the first attribute"
      s\" write `version=\q1.0\q` before the declaration's other attributes"
      E-MALFORMED REJECT
   then
   state 0 DECLARATION-VALUE$ s" 1.0" BYTES= 0= if
      state state 0 DECLARATION-VALUE-OFF s" `1.0`"
      s\" declare `version=\q1.0\q`: the reader reads XML 1.0" E-ENCODING REJECT
   then ;

: ENCODING-REQUIRED? ( ^n -- bool )
   SOURCE-MODE + @ dup SOURCE-ASCII = over SOURCE-UTF16-LE-DECL = or
   swap SOURCE-UTF16-BE-DECL = or ;

: UTF8-OR-ASCII? ( ^n -- bool )
   {: state :}
   state 1 DECLARATION-VALUE$ s" UTF-8" FOLD-EQUAL? if true exit then
   state 1 DECLARATION-VALUE$ s" US-ASCII" FOLD-EQUAL? 0= if
      false exit
   then
   state INPUT? if
      state STREAM@ STREAM-STARTED @ 1 <>
   else
      state SOURCE$ s\" \xEF\xBB\xBF" SRC-STARTS?
   then if
      state state 1 DECLARATION-VALUE-OFF
      s" `UTF-8` after a UTF-8 byte order mark"
      s\" declare `encoding=\qUTF-8\q`, or remove the byte order mark"
      E-ENCODING REJECT
   then
   state 0 ASCII-WINDOW
   state INPUT? if INPUT-ASCII else LEGACY-ASCII then
   state SOURCE-MODE + !
   true ;

\ Only a UTF-16 document without a byte order mark reaches this: a US-ASCII
\ source reader is one whose declaration names its encoding.
: ENCODING-MISSING ( ^n n -- )
   {: state off:n :}
   state off s" `encoding` after `version`"
   s\" declare `encoding=\qUTF-16LE\q` or `encoding=\qUTF-16BE\q`, as the document's bytes are"
   E-ENCODING REJECT ;

\ The expected text and the fix of a declared encoding the reader's mode does
\ not take.
: ENCODING-TEXTS ( n -- str str ) {: mode:n :}
   mode SOURCE-UTF16-LE-BOM = mode SOURCE-UTF16-BE-BOM = or if
      s" `UTF-16`, as the byte order mark shows"
      s\" declare `encoding=\qUTF-16\q`" exit
   then
   mode SOURCE-UTF16-LE-DECL = if
      s" `UTF-16LE`, the encoding of the document's bytes"
      s\" declare `encoding=\qUTF-16LE\q`" exit
   then
   mode SOURCE-UTF16-BE-DECL = if
      s" `UTF-16BE`, the encoding of the document's bytes"
      s\" declare `encoding=\qUTF-16BE\q`" exit
   then
   s" `UTF-8` or `US-ASCII`"
   s\" save the document as UTF-8 and declare `encoding=\qUTF-8\q`" ;

: ENCODING-CHECK ( ^n -- n )
   {: state :}
   state ATTR-COUNT + @ 1 = if
      state ENCODING-REQUIRED? if state state POSITION + @ ENCODING-MISSING then
      1 exit
   then
   state 1 s" encoding" DECLARATION-NAME? 0= if
      state ENCODING-REQUIRED? if
         state state 1 DECLARATION-NAME-OFF ENCODING-MISSING
      then
      1 exit
   then
   state SOURCE-MODE + @ case
      SOURCE-UTF16-LE-BOM of
         state 1 DECLARATION-VALUE$ s" UTF-16" FOLD-EQUAL? endof
      SOURCE-UTF16-BE-BOM of
         state 1 DECLARATION-VALUE$ s" UTF-16" FOLD-EQUAL? endof
      SOURCE-UTF16-LE-DECL of
         state 1 DECLARATION-VALUE$ s" UTF-16LE" FOLD-EQUAL? endof
      SOURCE-UTF16-BE-DECL of
         state 1 DECLARATION-VALUE$ s" UTF-16BE" FOLD-EQUAL? endof
      SOURCE-ASCII of
         state 1 DECLARATION-VALUE$ s" US-ASCII" FOLD-EQUAL? endof
      SOURCE-UTF8 of
         state 1 DECLARATION-VALUE$ s" UTF-8" FOLD-EQUAL? endof
      state UTF8-OR-ASCII? swap
   endcase
   0= if
      state state 1 DECLARATION-VALUE-OFF
      state SOURCE-MODE + @ ENCODING-TEXTS E-ENCODING REJECT
   then
   2 ;

: DECLARATION-START? ( ^n -- bool )
   {: state :}
   state s" <?xml" MATCH$ 0= if false exit then
   state SOURCE-LEN + @ 6 < if false exit then
   state 5 AT XML-SPACE? ;

: BOMLESS-DECLARATION ( ^n -- )
   {: state :}
   state SOURCE-MODE + @ SOURCE-UTF16-LE-DECL =
   state SOURCE-MODE + @ SOURCE-UTF16-BE-DECL = or
   state POSITION + @ 0= and 0= if exit then
   state DECLARATION-START? 0= if
      state state POSITION + @
      s" an XML declaration, which a UTF-16 document without a byte order mark begins with"
      s\" begin the document with `<?xml version=\q1.0\q encoding=\qUTF-16LE\q?>` or its `UTF-16BE` form, or with a byte order mark"
      E-ENCODING REJECT
   then ;

: STANDALONE-CHECK ( ^n n -- )
   {: state index:n :}
   state ATTR-COUNT + @ index = if exit then
   state ATTR-COUNT + @ index 1+ <> if
      state state index 1+ DECLARATION-NAME-OFF
      s" `?>` to end the XML declaration"
      s" write only `version`, `encoding` and `standalone`, in that order"
      E-MALFORMED REJECT
   then
   state index s" standalone" DECLARATION-NAME? 0= if
      state state index DECLARATION-NAME-OFF
      s" `standalone` as the declaration's last attribute"
      s" correct the attribute's name, or remove the attribute"
      E-MALFORMED REJECT
   then
   state index DECLARATION-VALUE$ {: value size:n :}
   value size s" yes" BYTES= value size s" no" BYTES= or 0= if
      state state index DECLARATION-VALUE-OFF
      s" `yes` or `no`" s" write `yes` or `no` as `standalone`'s value"
      E-MALFORMED REJECT
   then ;

: READ-DECLARATION ( ^n -- )
   {: state :}
   state DECLARATION-POSITION
   state state NAME-OFF + @ state NAME-LEN + @ SPAN$ s" xml" BYTES= 0= if
      state state NAME-OFF + @ s" `xml` in lower case"
      s" begin the XML declaration with `<?xml`" E-MALFORMED REJECT
   then
   state POSITION + @ state VALUE-OFF + !
   begin state s" ?>" MATCH$ 0= while
      state EOF? if
         state state POSITION + @ s" `?>` to end the XML declaration"
         s" end the declaration with `?>`" E-TRUNCATED REJECT
      then
      state WHITE? 0= if
         state state POSITION + @ s" white space or `?>`"
         s" separate the declaration's attributes with a space"
         E-MALFORMED REJECT
      then
      state SKIP-WHITE
      state s" ?>" MATCH$ 0= if state SCAN-ATTRIBUTE then
   repeat
   state VERSION-CHECK
   state state ENCODING-CHECK STANDALONE-CHECK
   state VALUE-END ;

: READ-PI-CONTENT ( ^n -- )
   {: state :}
   state EOF? if
      state state POSITION + @
      s" `?>` to end the processing instruction"
      s" end the processing instruction with `?>`" E-TRUNCATED REJECT
   then
   state s" ?>" MATCH$ 0= if
      state WHITE? 0= if
         state state POSITION + @ s" white space or `?>` after the target"
         s" put a space between the target and the instruction's content"
         E-MALFORMED REJECT
      then
      state SKIP-WHITE
   then
   state VALUE-BEGIN
   begin state s" ?>" MATCH$ 0= while state SCAN-SCALAR drop repeat
   state VALUE-END ;

: READ-PI ( ^n -- )
   {: state :}
   state 2 ADVANCE-N
   state SCAN-NAME
   state NAME-LEN + ! state NAME-OFF + !
   state state NAME-OFF + @ state NAME-LEN + @ SPAN$
   s" xml" FOLD-EQUAL? if
      state READ-DECLARATION
   else
      state READ-PI-CONTENT
   then
   state 2 ADVANCE-N
   state XML-KIND:PI EVENT-FINISH ;

: READ-MARKUP ( ^n -- )
   {: state :}
   state s" </" MATCH$ if state READ-END exit then
   state s" <!--" MATCH$ if state READ-COMMENT exit then
   state s" <![CDATA[" MATCH$ if state READ-CDATA exit then
   state s" <?" MATCH$ if state READ-PI exit then
   state s" <!DOCTYPE" MATCH$ if
      state state POSITION + @
      s" an element, a comment or a processing instruction"
      s" remove the `<!DOCTYPE`: the reader reads no DTD" E-DTD REJECT
   then
   state s" <!" MATCH$ if
      state state POSITION + @ s" `<!--` or `<![CDATA[`"
      s" write `<!--` for a comment or `<![CDATA[` for character data"
      E-MALFORMED REJECT
   then
   state READ-START ;

: READ-EOF ( ^n -- )
   {: state :}
   state OPEN-DEPTH + @ 0<> if
      state state SOURCE-LEN + @
      s" write the rest of the document, closing each open element"
      E-TRUNCATED END-TAG-REJECT
   then
   state ROOT-COUNT + @ 1 <> if
      state state SOURCE-LEN + @ s" a root element"
      s" write one root element that holds the document's content"
      E-MALFORMED REJECT
   then
   state XML-KIND:EOF EVENT-FINISH ;

: READ-EVENT ( ^n -- )
   {: state :}
   state PENDING + @ POP-NEXT = if state POP-FRAME then
   state INPUT? if
      state STREAM@ {: stream :}
      stream STREAM-STARTED @ 0= if
         state BOM
         state POSITION + @ 1+ stream STREAM-STARTED !
      then
      state INPUT-REBASE
   then
   state EVENT-RESET
   state BOMLESS-DECLARATION
   state PENDING + @ EMPTY-END = if state READ-EMPTY-END exit then
   state EOF? if state READ-EOF exit then
   state PEEK $3C = if state READ-MARKUP else state READ-TEXT then ;

: NEXT-STATE ( ^n -- kind )
   {: state :}
   state LIVE
   ST-FAILED state FAILED + !
   state READ-EVENT
   ST-LIVE state FAILED + !
   state TOKEN-KIND + >KIND-CELL @ ;

: TEXT-MODE ( ^n -- n )
   TOKEN-KIND + >KIND-CELL @ MATCH kind
      start OF E-STATE throw ENDOF
      end OF E-STATE throw ENDOF
      text OF DECODE-TEXT ENDOF
      comment OF DECODE-RAW ENDOF
      pi OF DECODE-RAW ENDOF
      cdata OF DECODE-RAW ENDOF
      eof OF E-STATE throw ENDOF
   ;MATCH ;

: CONTENT-STATE ( ^n -- off len )
   {: state :}
   state LIVE
   state TEXT-MODE drop
   state state VALUE-OFF + @ ABS-OFF >OFF state VALUE-LEN + @ >LEN ;

: TEXT-STATE ( ^n ^u8 n -- n )
   {: state destination cap:n :}
   state LIVE
   state TEXT-MODE {: mode:n :}
   state destination cap OUTPUT-CHECK
   state state VALUE-OFF + @ state VALUE-LEN + @ SPAN$
   mode destination cap DECODE-INTO ;

public
: NEXT ( XML:reader -- XML:reader kind )
   STATE NEXT-STATE ;

: CONTENT ( XML:reader -- XML:reader off len )
   STATE CONTENT-STATE ;

: TEXT ( XML:reader ^u8 n -- XML:reader n )
   DECODE-ARGS TEXT-STATE ;

;package
