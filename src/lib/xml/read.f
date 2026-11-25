\ read.f - the XML event scanner: NEXT, CONTENT and TEXT.

require src/lib/xml/names.f
require src/lib/reason.f
require src/lib/vector.f
require src/lib/xml/reject.f
require src/lib/xml/scalar.f
require src/lib/xml/source.f
require src/lib/xml/state.f

package XML
private
: EVENT-RESET ( ^cursor -- )
   {: state :}
   state CURSOR-POSITION @ state CURSOR-RAW-OFF !
   0 state CURSOR-RAW-LEN !
   0 state CURSOR-NAME-LEN !
   0 state CURSOR-VALUE-LEN !
   0 state CURSOR-URI-OFF !
   0 state CURSOR-URI-LEN !
   0 state CURSOR-ATTR-COUNT !
   -1 state CURSOR-ATTR-INDEX ! ;

: EVENT-FINISH ( ^cursor kind -- )
   {: state token:kind :}
   state CURSOR-POSITION @ state CURSOR-RAW-OFF @ - state CURSOR-RAW-LEN !
   token state CURSOR-TOKEN-KIND ! ;

: VALUE-BEGIN ( ^cursor -- )
   dup CURSOR-POSITION @ swap CURSOR-VALUE-OFF ! ;

: VALUE-END ( ^cursor -- )
   {: state :}
   state CURSOR-POSITION @ state CURSOR-VALUE-OFF @ - state CURSOR-VALUE-LEN ! ;

: SCAN-ELEMENT-NAME ( ^cursor -- )
   {: state :}
   state SCAN-NAME {: start:n size:n :}
   state start size QNAME-CHECK
   start state CURSOR-NAME-OFF !
   size state CURSOR-NAME-LEN ! ;

: OPEN-END? ( ^cursor -- bool )
   {: state :}
   state PEEK $3E = state s" />" MATCH$ or ;

: SCAN-OPEN-ATTRIBUTES ( ^cursor -- bool )
   {: state :}
   begin state OPEN-END? 0= while
      state WHITE? 0= if
         state state CURSOR-POSITION @
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

: PUSH-CHECK ( ^cursor -- )
   {: state :}
   state CURSOR-OPEN-DEPTH @ 0= if
      state CURSOR-ROOT-COUNT @ 0<> if
         state state CURSOR-POSITION @
         s" white space, a comment or a processing instruction after the root element"
         s" wrap the elements in one root element"
         E-MALFORMED REJECT
      then
      1 state CURSOR-ROOT-COUNT !
   then
   state state CURSOR-OPEN-DEPTH @ 1+ FRAMES-GROW ;

: SAVE-FRAME ( ^cursor -- )
   {: state :}
   state state CURSOR-OPEN-DEPTH @ FRAME {: frame :}
   state state CURSOR-NAME-OFF @ state CURSOR-NAME-LEN @ SCOPE-REF
   frame FRAME-NAME-U ! frame FRAME-NAME !
   state CURSOR-NS-COUNT @ frame FRAME-NS-COUNT ! ;

: PUSH-FRAME ( ^cursor -- )
   {: state :}
   state state CURSOR-OPEN-DEPTH @ FRAME {: frame :}
   state CURSOR-URI-OFF @ frame FRAME-URI !
   state CURSOR-URI-LEN @ frame FRAME-URI-U !
   1 state CURSOR-OPEN-DEPTH +! ;

: READ-START ( ^cursor -- )
   {: state :}
   state PUSH-CHECK
   state ADVANCE
   state SCAN-ELEMENT-NAME
   state SCAN-OPEN-ATTRIBUTES {: empty:bool :}
   state SAVE-FRAME
   state RESOLVE-ELEMENT
   state PUSH-FRAME
   empty if PENDING-EMPTY-END state CURSOR-PENDING ! then
   state XML-KIND:START EVENT-FINISH ;

\ Rejects the reader at off, expecting the open element's end tag.
\ The expected text `</NAME>` names the open element, cut as a found token is.
: END-TAG-REJECT ( ^cursor n str n -- )
   {: state off:n fs:str code:n :}
   state CURRENT-FRAME {: frame :}
   state frame FRAME-NAME @ frame FRAME-NAME-U @ SPAN$ {: name:str :}
   name CAPPED {: len:n form:n :}
   state off REJECT-HEAD
   s" `</" REASON:APPEND
   name STR:DATA len >str REASON:VISIBLE
   form FOUND-CUT = if s" ..." REASON:APPEND then
   s" >`" REASON:APPEND
   state off fs code REJECT-TAIL ;

: MATCH-END-NAME ( ^cursor -- )
   {: state :}
   state CURRENT-FRAME {: frame :}
   state frame FRAME-NAME @ frame FRAME-NAME-U @ SPAN$
   state state CURSOR-NAME-OFF @ state CURSOR-NAME-LEN @ SPAN$ BYTES= 0= if
      state state CURSOR-NAME-OFF @
      s" close elements in the reverse of the order they were opened"
      E-MALFORMED END-TAG-REJECT
   then
   frame FRAME-URI @ state CURSOR-URI-OFF !
   frame FRAME-URI-U @ state CURSOR-URI-LEN ! ;

: READ-END ( ^cursor -- )
   {: state :}
   state CURSOR-OPEN-DEPTH @ 0= if
      state state CURSOR-POSITION @ s" an end tag only after its start tag"
      s" remove the end tag, or write its start tag before it"
      E-MALFORMED REJECT
   then
   state 2 ADVANCE-N
   state SCAN-ELEMENT-NAME
   state MATCH-END-NAME
   state SKIP-WHITE
   state $3E REQUIRE-BYTE
   PENDING-POP-NEXT state CURSOR-PENDING !
   state XML-KIND:END EVENT-FINISH ;

: POP-FRAME ( ^cursor -- )
   {: state :}
   state CURRENT-FRAME {: frame :}
   state frame FRAME-NS-COUNT @ POP-BINDINGS
   state frame FRAME-NAME @ SCOPE-CUT
   -1 state CURSOR-OPEN-DEPTH +!
   PENDING-NONE state CURSOR-PENDING ! ;

: READ-EMPTY-END ( ^cursor -- )
   {: state :}
   state CURRENT-FRAME {: frame :}
   frame FRAME-NAME @ state CURSOR-NAME-OFF !
   frame FRAME-NAME-U @ state CURSOR-NAME-LEN !
   frame FRAME-URI @ state CURSOR-URI-OFF !
   frame FRAME-URI-U @ state CURSOR-URI-LEN !
   PENDING-POP-NEXT state CURSOR-PENDING !
   state XML-KIND:END EVENT-FINISH ;

: TEXT-CONTINUES? ( ^cursor -- bool )
   {: state :}
   state INPUT? if
      state CURSOR-POSITION @ state CURSOR-VALUE-OFF @ -
      CHUNK >= if false exit then
   then
   state EOF? if false exit then
   state PEEK $3C <> ;

: TEXT-SCALAR ( ^cursor -- )
   {: state :}
   state s" ]]>" MATCH$ if
      state state CURSOR-POSITION @ s" text without `]]>`"
      s" write `]]&gt;` for a literal `]]>` in text" E-MALFORMED REJECT
   then
   state CURSOR-OPEN-DEPTH @ 0= if
      state PEEK XML-SPACE? 0= if
         state state CURSOR-POSITION @ s" only white space outside the root element"
         s" move the text inside the root element, or remove it"
         E-MALFORMED REJECT
      then
   then
   state DECODE-TEXT SCAN-DECODE
   state CURSOR-POSITION ! drop ;

: READ-TEXT ( ^cursor -- )
   {: state :}
   state VALUE-BEGIN
   begin state TEXT-CONTINUES? while state TEXT-SCALAR repeat
   state VALUE-END
   state XML-KIND:TEXT EVENT-FINISH ;

: READ-COMMENT ( ^cursor -- )
   {: state :}
   state 4 ADVANCE-N
   state VALUE-BEGIN
   begin state s" -->" MATCH$ 0= while
      state s" --" MATCH$ if
         state state CURSOR-POSITION @ s" `-->` or comment text without `--`"
         s" remove `--` from the comment, or end the comment with `-->`"
         E-MALFORMED REJECT
      then
      state SCAN-SCALAR drop
   repeat
   state VALUE-END
   state 3 ADVANCE-N
   state XML-KIND:COMMENT EVENT-FINISH ;

: READ-CDATA ( ^cursor -- )
   {: state :}
   state CURSOR-OPEN-DEPTH @ 0= if
      state state CURSOR-POSITION @ s" a CDATA section only inside the root element"
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
: DECLARATION-FIRST? ( ^cursor -- bool )
   {: state :}
   state INPUT? if
      state state CURSOR-RAW-OFF @ ABS-OFF
      state CURSOR-STREAM @ STREAM-STARTED @ 1- = exit
   then
   state CURSOR-RAW-OFF @ 0= if true exit then
   state CURSOR-SOURCE-MODE @ MODE-LEGACY-UTF8 MODE-EQ 0= if false exit then
   state CURSOR-RAW-OFF @ 3 = if
      state 0 3 SPAN$ s\" \xEF\xBB\xBF" BYTES= exit
   then
   false ;

: DECLARATION-POSITION ( ^cursor -- )
   {: state :}
   state DECLARATION-FIRST? 0= if
      state state CURSOR-RAW-OFF @ s" `<?xml` only at the start of the document"
      s" move the XML declaration to the start of the document, or remove it"
      E-MALFORMED REJECT
   then ;

: DECLARATION-NAME? ( ^cursor n str -- bool )
   {: state index:n name:str :}
   state state index ATTRIBUTE name NAME-MATCH? ;

: DECLARATION-VALUE$ ( ^cursor n -- str )
   {: state index:n :}
   state state index ATTRIBUTE ATTRIBUTE-VALUE$ ;

: DECLARATION-NAME-OFF ( ^cursor n -- n )
   ATTRIBUTE ATTRIBUTE-NAME @ ;

: DECLARATION-VALUE-OFF ( ^cursor n -- n )
   ATTRIBUTE ATTRIBUTE-VALUE @ ;

: VERSION-CHECK ( ^cursor -- )
   {: state :}
   state CURSOR-ATTR-COUNT @ 0= if
      state state CURSOR-POSITION @ s\" `version=\q1.0\q`"
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

: ENCODING-REQUIRED? ( ^cursor -- bool )
   CURSOR-SOURCE-MODE @ dup MODE-SOURCE-ASCII MODE-EQ
   over MODE-SOURCE-UTF16-LE-DECL MODE-EQ or
   swap MODE-SOURCE-UTF16-BE-DECL MODE-EQ or ;

: UTF8-OR-ASCII? ( ^cursor -- bool )
   {: state :}
   state 1 DECLARATION-VALUE$ s" UTF-8" FOLD-EQUAL? if true exit then
   state 1 DECLARATION-VALUE$ s" US-ASCII" FOLD-EQUAL? 0= if
      false exit
   then
   state INPUT? if
      state CURSOR-STREAM @ STREAM-STARTED @ 1 <>
   else
      state SOURCE$ s\" \xEF\xBB\xBF" SRC-STARTS?
   then if
      state state 1 DECLARATION-VALUE-OFF
      s" `UTF-8` after a UTF-8 byte order mark"
      s\" declare `encoding=\qUTF-8\q`, or remove the byte order mark"
      E-ENCODING REJECT
   then
   state 0 ASCII-WINDOW
   state INPUT? if MODE-INPUT-ASCII else MODE-LEGACY-ASCII then
   state CURSOR-SOURCE-MODE !
   true ;

\ Only a UTF-16 document without a byte order mark reaches this: a US-ASCII
\ source reader is one whose declaration names its encoding.
: ENCODING-MISSING ( ^cursor n -- )
   {: state off:n :}
   state off s" `encoding` after `version`"
   s\" declare `encoding=\qUTF-16LE\q` or `encoding=\qUTF-16BE\q`, as the document's bytes are"
   E-ENCODING REJECT ;

\ The expected text and the fix of a declared encoding the reader's mode does
\ not take.
: ENCODING-TEXTS ( mode -- str str ) {: mode:mode :}
   mode MODE-SOURCE-UTF16-LE-BOM MODE-EQ
   mode MODE-SOURCE-UTF16-BE-BOM MODE-EQ or if
      s" `UTF-16`, as the byte order mark shows"
      s\" declare `encoding=\qUTF-16\q`" exit
   then
   mode MODE-SOURCE-UTF16-LE-DECL MODE-EQ if
      s" `UTF-16LE`, the encoding of the document's bytes"
      s\" declare `encoding=\qUTF-16LE\q`" exit
   then
   mode MODE-SOURCE-UTF16-BE-DECL MODE-EQ if
      s" `UTF-16BE`, the encoding of the document's bytes"
      s\" declare `encoding=\qUTF-16BE\q`" exit
   then
   s" `UTF-8` or `US-ASCII`"
   s\" save the document as UTF-8 and declare `encoding=\qUTF-8\q`" ;

: ENCODING-CHECK ( ^cursor -- n )
   {: state :}
   state CURSOR-ATTR-COUNT @ 1 = if
      state ENCODING-REQUIRED? if state state CURSOR-POSITION @ ENCODING-MISSING then
      1 exit
   then
   state 1 s" encoding" DECLARATION-NAME? 0= if
      state ENCODING-REQUIRED? if
         state state 1 DECLARATION-NAME-OFF ENCODING-MISSING
      then
      1 exit
   then
   state CURSOR-SOURCE-MODE @ MATCH mode
      legacy-utf8 OF state UTF8-OR-ASCII? ENDOF
      source-utf8 OF
         state 1 DECLARATION-VALUE$ s" UTF-8" FOLD-EQUAL? ENDOF
      source-utf16-le-bom OF
         state 1 DECLARATION-VALUE$ s" UTF-16" FOLD-EQUAL? ENDOF
      source-utf16-be-bom OF
         state 1 DECLARATION-VALUE$ s" UTF-16" FOLD-EQUAL? ENDOF
      source-utf16-le-decl OF
         state 1 DECLARATION-VALUE$ s" UTF-16LE" FOLD-EQUAL? ENDOF
      source-utf16-be-decl OF
         state 1 DECLARATION-VALUE$ s" UTF-16BE" FOLD-EQUAL? ENDOF
      input-utf8 OF state UTF8-OR-ASCII? ENDOF
      source-ascii OF
         state 1 DECLARATION-VALUE$ s" US-ASCII" FOLD-EQUAL? ENDOF
      legacy-ascii OF state UTF8-OR-ASCII? ENDOF
      input-ascii OF state UTF8-OR-ASCII? ENDOF
   ;MATCH
   0= if
      state state 1 DECLARATION-VALUE-OFF
      state CURSOR-SOURCE-MODE @ ENCODING-TEXTS E-ENCODING REJECT
   then
   2 ;

: DECLARATION-START? ( ^cursor -- bool )
   {: state :}
   state s" <?xml" MATCH$ 0= if false exit then
   state CURSOR-SOURCE-LEN @ 6 < if false exit then
   state 5 AT XML-SPACE? ;

: BOMLESS-DECLARATION ( ^cursor -- )
   {: state :}
   state CURSOR-SOURCE-MODE @ MODE-SOURCE-UTF16-LE-DECL MODE-EQ
   state CURSOR-SOURCE-MODE @ MODE-SOURCE-UTF16-BE-DECL MODE-EQ or
   state CURSOR-POSITION @ 0= and 0= if exit then
   state DECLARATION-START? 0= if
      state state CURSOR-POSITION @
      s" an XML declaration, which a UTF-16 document without a byte order mark begins with"
      s\" begin the document with `<?xml version=\q1.0\q encoding=\qUTF-16LE\q?>` or its `UTF-16BE` form, or with a byte order mark"
      E-ENCODING REJECT
   then ;

: STANDALONE-CHECK ( ^cursor n -- )
   {: state index:n :}
   state CURSOR-ATTR-COUNT @ index = if exit then
   state CURSOR-ATTR-COUNT @ index 1+ <> if
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
   state index DECLARATION-VALUE$ {: value:str :}
   value s" yes" BYTES= value s" no" BYTES= or 0= if
      state state index DECLARATION-VALUE-OFF
      s" `yes` or `no`" s" write `yes` or `no` as `standalone`'s value"
      E-MALFORMED REJECT
   then ;

: READ-DECLARATION ( ^cursor -- )
   {: state :}
   state DECLARATION-POSITION
   state state CURSOR-NAME-OFF @ state CURSOR-NAME-LEN @ SPAN$ s" xml" BYTES= 0= if
      state state CURSOR-NAME-OFF @ s" `xml` in lower case"
      s" begin the XML declaration with `<?xml`" E-MALFORMED REJECT
   then
   state CURSOR-POSITION @ state CURSOR-VALUE-OFF !
   begin state s" ?>" MATCH$ 0= while
      state EOF? if
         state state CURSOR-POSITION @ s" `?>` to end the XML declaration"
         s" end the declaration with `?>`" E-TRUNCATED REJECT
      then
      state WHITE? 0= if
         state state CURSOR-POSITION @ s" white space or `?>`"
         s" separate the declaration's attributes with a space"
         E-MALFORMED REJECT
      then
      state SKIP-WHITE
      state s" ?>" MATCH$ 0= if state SCAN-ATTRIBUTE then
   repeat
   state VERSION-CHECK
   state state ENCODING-CHECK STANDALONE-CHECK
   state VALUE-END ;

: READ-PI-CONTENT ( ^cursor -- )
   {: state :}
   state EOF? if
      state state CURSOR-POSITION @
      s" `?>` to end the processing instruction"
      s" end the processing instruction with `?>`" E-TRUNCATED REJECT
   then
   state s" ?>" MATCH$ 0= if
      state WHITE? 0= if
         state state CURSOR-POSITION @ s" white space or `?>` after the target"
         s" put a space between the target and the instruction's content"
         E-MALFORMED REJECT
      then
      state SKIP-WHITE
   then
   state VALUE-BEGIN
   begin state s" ?>" MATCH$ 0= while state SCAN-SCALAR drop repeat
   state VALUE-END ;

: READ-PI ( ^cursor -- )
   {: state :}
   state 2 ADVANCE-N
   state SCAN-NAME
   state CURSOR-NAME-LEN ! state CURSOR-NAME-OFF !
   state state CURSOR-NAME-OFF @ state CURSOR-NAME-LEN @ SPAN$
   s" xml" FOLD-EQUAL? if
      state READ-DECLARATION
   else
      state READ-PI-CONTENT
   then
   state 2 ADVANCE-N
   state XML-KIND:PI EVENT-FINISH ;

: READ-MARKUP ( ^cursor -- )
   {: state :}
   state s" </" MATCH$ if state READ-END exit then
   state s" <!--" MATCH$ if state READ-COMMENT exit then
   state s" <![CDATA[" MATCH$ if state READ-CDATA exit then
   state s" <?" MATCH$ if state READ-PI exit then
   state s" <!DOCTYPE" MATCH$ if
      state state CURSOR-POSITION @
      s" an element, a comment or a processing instruction"
      s" remove the `<!DOCTYPE`: the reader reads no DTD" E-DTD REJECT
   then
   state s" <!" MATCH$ if
      state state CURSOR-POSITION @ s" `<!--` or `<![CDATA[`"
      s" write `<!--` for a comment or `<![CDATA[` for character data"
      E-MALFORMED REJECT
   then
   state READ-START ;

: READ-EOF ( ^cursor -- )
   {: state :}
   state CURSOR-OPEN-DEPTH @ 0<> if
      state state CURSOR-SOURCE-LEN @
      s" write the rest of the document, closing each open element"
      E-TRUNCATED END-TAG-REJECT
   then
   state CURSOR-ROOT-COUNT @ 1 <> if
      state state CURSOR-SOURCE-LEN @ s" a root element"
      s" write one root element that holds the document's content"
      E-MALFORMED REJECT
   then
   state XML-KIND:EOF EVENT-FINISH ;

: READ-EVENT ( ^cursor -- )
   {: state :}
   state CURSOR-PENDING @ PENDING-POP-NEXT PENDING-EQ if
      state POP-FRAME
   then
   state INPUT? if
      state CURSOR-STREAM @ {: stream :}
      stream STREAM-STARTED @ 0= if
         state BOM
         state CURSOR-POSITION @ 1+ stream STREAM-STARTED !
      then
      state INPUT-REBASE
   then
   state EVENT-RESET
   state BOMLESS-DECLARATION
   state CURSOR-PENDING @ PENDING-EMPTY-END PENDING-EQ if
      state READ-EMPTY-END exit
   then
   state EOF? if state READ-EOF exit then
   state PEEK $3C = if state READ-MARKUP else state READ-TEXT then ;

: NEXT-STATE ( ^cursor -- kind )
   {: state :}
   state LIVE
   true state CURSOR-FAILED !
   state READ-EVENT
   false state CURSOR-FAILED !
   state CURSOR-TOKEN-KIND @ ;

: TEXT-MODE ( ^cursor -- n )
   CURSOR-TOKEN-KIND @ MATCH kind
      start OF E-STATE throw ENDOF
      end OF E-STATE throw ENDOF
      text OF DECODE-TEXT ENDOF
      comment OF DECODE-RAW ENDOF
      pi OF DECODE-RAW ENDOF
      cdata OF DECODE-RAW ENDOF
      eof OF E-STATE throw ENDOF
   ;MATCH ;

: CONTENT-STATE ( ^cursor -- off len )
   {: state :}
   state LIVE
   state TEXT-MODE drop
   state state CURSOR-VALUE-OFF @ ABS-OFF >OFF state CURSOR-VALUE-LEN @ >LEN ;

: TEXT-STATE ( ^cursor ^VEC:vector<u8> -- )
   {: state v:^ :}
   state LIVE
   state TEXT-MODE {: mode:n :}
   state state CURSOR-VALUE-OFF @ state CURSOR-VALUE-LEN @ SPAN$
   mode v DECODE-INTO ;

public
: NEXT ( XML:reader -- XML:reader kind )
   STATE NEXT-STATE ;

: CONTENT ( XML:reader -- XML:reader off len )
   STATE CONTENT-STATE ;

: TEXT ( XML:reader ^VEC:vector<u8> -- XML:reader )
   DECODE-ARGS TEXT-STATE ;

;package
