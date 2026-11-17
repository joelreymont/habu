\ read.f - the XML event scanner: NEXT, CONTENT and TEXT.

require src/lib/xml/names.f

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
      state WHITE? 0= if E-MALFORMED throw then
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
      state ROOT-COUNT + @ 0<> if E-MALFORMED throw then
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

: MATCH-END-NAME ( ^n -- )
   {: state :}
   state CURRENT-FRAME {: frame :}
   state frame FRAME-NAME + @ frame FRAME-NAME-U + @ SPAN$
   state state NAME-OFF + @ state NAME-LEN + @ SPAN$ BYTES= 0= if
      E-MALFORMED throw
   then
   frame FRAME-URI + @ state URI-OFF + !
   frame FRAME-URI-U + @ state URI-LEN + ! ;

: READ-END ( ^n -- )
   {: state :}
   state OPEN-DEPTH + @ 0= if E-MALFORMED throw then
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
   state s" ]]>" MATCH$ if E-MALFORMED throw then
   state OPEN-DEPTH + @ 0= if
      state PEEK XML-SPACE? 0= if E-MALFORMED throw then
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
      state s" --" MATCH$ if E-MALFORMED throw then
      state SCAN-SCALAR drop
   repeat
   state VALUE-END
   state 3 ADVANCE-N
   state XML-KIND:COMMENT EVENT-FINISH ;

: READ-CDATA ( ^n -- )
   {: state :}
   state OPEN-DEPTH + @ 0= if E-MALFORMED throw then
   state 9 ADVANCE-N
   state VALUE-BEGIN
   begin state s" ]]>" MATCH$ 0= while state SCAN-SCALAR drop repeat
   state VALUE-END
   state 3 ADVANCE-N
   state XML-KIND:CDATA EVENT-FINISH ;

: DECLARATION-POSITION ( ^n -- )
   {: state :}
   state INPUT? if
      state state RAW-OFF + @ ABS-OFF
      state STREAM@ STREAM-STARTED @ 1- = if exit then
      E-MALFORMED throw
   then
   state SOURCE-MODE + @ LEGACY-UTF8 <> if
      state RAW-OFF + @ 0= if exit then
      E-MALFORMED throw
   then
   state RAW-OFF + @ 0= if exit then
   state RAW-OFF + @ 3 = if
      state 0 3 SPAN$ s\" \xEF\xBB\xBF" BYTES= if exit then
   then
   E-MALFORMED throw ;

: DECLARATION-NAME? ( ^n n ^u8 n -- bool )
   {: state index:n name size:n :}
   state state index ATTRIBUTE name size NAME-MATCH? ;

: DECLARATION-VALUE$ ( ^n n -- ^u8 n )
   {: state index:n :}
   state state index ATTRIBUTE ATTRIBUTE-VALUE$ ;

: VERSION-CHECK ( ^n -- )
   {: state :}
   state ATTR-COUNT + @ 0= if E-MALFORMED throw then
   state 0 s" version" DECLARATION-NAME? 0= if E-MALFORMED throw then
   state 0 DECLARATION-VALUE$ s" 1.0" BYTES= 0= if E-ENCODING throw then ;

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
      state STREAM@ STREAM-STARTED @ 1 <> if E-ENCODING throw then
   else
      state SOURCE$ s\" \xEF\xBB\xBF" SRC-STARTS? if E-ENCODING throw then
   then
   state SOURCE$ ASCII-WIRE
   state INPUT? if INPUT-ASCII else LEGACY-ASCII then
   state SOURCE-MODE + !
   true ;

: ENCODING-CHECK ( ^n -- n )
   {: state :}
   state ATTR-COUNT + @ 1 = if
      state ENCODING-REQUIRED? if E-ENCODING throw then
      1 exit
   then
   state 1 s" encoding" DECLARATION-NAME? 0= if
      state ENCODING-REQUIRED? if E-ENCODING throw then
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
      E-ENCODING throw
   then
   2 ;

: BOMLESS-DECLARATION ( ^n -- )
   {: state :}
   state SOURCE-MODE + @ SOURCE-UTF16-LE-DECL =
   state SOURCE-MODE + @ SOURCE-UTF16-BE-DECL = or
   state POSITION + @ 0= and 0= if exit then
   state s" <?xml" MATCH$ 0= if E-ENCODING throw then
   state SOURCE-LEN + @ 6 < if E-ENCODING throw then
   state 5 AT XML-SPACE? 0= if E-ENCODING throw then ;

: STANDALONE-CHECK ( ^n n -- )
   {: state index:n :}
   state ATTR-COUNT + @ index = if exit then
   state ATTR-COUNT + @ index 1+ <> if E-MALFORMED throw then
   state index s" standalone" DECLARATION-NAME? 0= if E-MALFORMED throw then
   state index DECLARATION-VALUE$ {: value size:n :}
   value size s" yes" BYTES= value size s" no" BYTES= or 0= if
      E-MALFORMED throw
   then ;

: READ-DECLARATION ( ^n -- )
   {: state :}
   state DECLARATION-POSITION
   state state NAME-OFF + @ state NAME-LEN + @ SPAN$ s" xml" BYTES= 0= if
      E-MALFORMED throw
   then
   state POSITION + @ state VALUE-OFF + !
   begin state s" ?>" MATCH$ 0= while
      state EOF? if E-TRUNCATED throw then
      state WHITE? 0= if E-MALFORMED throw then
      state SKIP-WHITE
      state s" ?>" MATCH$ 0= if state SCAN-ATTRIBUTE then
   repeat
   state VERSION-CHECK
   state state ENCODING-CHECK STANDALONE-CHECK
   state VALUE-END ;

: READ-PI-CONTENT ( ^n -- )
   {: state :}
   state EOF? if E-TRUNCATED throw then
   state s" ?>" MATCH$ 0= if
      state WHITE? 0= if E-MALFORMED throw then
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
   state s" <!DOCTYPE" MATCH$ if E-DTD throw then
   state s" <!" MATCH$ if E-MALFORMED throw then
   state READ-START ;

: READ-EOF ( ^n -- )
   {: state :}
   state OPEN-DEPTH + @ 0<> if E-TRUNCATED throw then
   state ROOT-COUNT + @ 1 <> if E-MALFORMED throw then
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
   1 state FAILED + !
   state READ-EVENT
   0 state FAILED + !
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
