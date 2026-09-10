require lib/xml/names.f

package XML
private
: EVENT-RESET ( ptr n -- )
   {: state :}
   state POSITION + @ state RAW-OFF + !
   0 state RAW-LEN + !
   0 state NAME-LEN + !
   0 state VALUE-LEN + !
   0 state URI-OFF + !
   0 state URI-LEN + !
   0 state ATTR-COUNT + !
   -1 state ATTR-INDEX + ! ;

: EVENT-FINISH ( ptr n kind -- )
   {: state token:kind :}
   state POSITION + @ state RAW-OFF + @ - state RAW-LEN + !
   token KIND>N state TOKEN-KIND + ! ;

: VALUE-BEGIN ( ptr n -- )
   dup POSITION + @ swap VALUE-OFF + ! ;

: VALUE-END ( ptr n -- )
   {: state :}
   state POSITION + @ state VALUE-OFF + @ - state VALUE-LEN + ! ;

: SCAN-ELEMENT-NAME ( ptr n -- )
   {: state :}
   state SCAN-NAME {: start:n size:n :}
   state start size QNAME-CHECK
   start state NAME-OFF + !
   size state NAME-LEN + ! ;

: OPEN-END? ( ptr n -- bool )
   {: state :}
   state PEEK $3E = state s" />" MATCH$ or ;

: SCAN-OPEN-ATTRIBUTES ( ptr n -- bool )
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

: PUSH-CHECK ( ptr n -- )
   {: state :}
   state OPEN-DEPTH + @ state CAPACITY + @ >= if E-DEPTH throw then
   state OPEN-DEPTH + @ 0= if
      state ROOT-COUNT + @ 0<> if E-MALFORMED throw then
      1 state ROOT-COUNT + !
   then ;

: SAVE-FRAME ( ptr n -- )
   {: state :}
   state state OPEN-DEPTH + @ FRAME {: frame :}
   state NAME-OFF + @ frame !
   state NAME-LEN + @ frame CELL + !
   state NS-COUNT + @ frame 2 cells + ! ;

: PUSH-FRAME ( ptr n -- )
   {: state :}
   state state OPEN-DEPTH + @ FRAME {: frame :}
   state URI-OFF + @ frame 3 cells + !
   state URI-LEN + @ frame 4 cells + !
   1 state OPEN-DEPTH + +! ;

: READ-START ( ptr n -- )
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

: MATCH-END-NAME ( ptr n -- )
   {: state :}
   state CURRENT-FRAME {: frame :}
   state frame @ frame CELL + @ SPAN$
   state state NAME-OFF + @ state NAME-LEN + @ SPAN$ BYTES= 0= if
      E-MALFORMED throw
   then
   frame 3 cells + @ state URI-OFF + !
   frame 4 cells + @ state URI-LEN + ! ;

: READ-END ( ptr n -- )
   {: state :}
   state OPEN-DEPTH + @ 0= if E-MALFORMED throw then
   state 2 ADVANCE-N
   state SCAN-ELEMENT-NAME
   state MATCH-END-NAME
   state SKIP-WHITE
   state $3E REQUIRE-BYTE
   POP-NEXT state PENDING + !
   state XML-KIND:END EVENT-FINISH ;

: POP-FRAME ( ptr n -- )
   {: state :}
   state CURRENT-FRAME 2 cells + @ state NS-COUNT + !
   -1 state OPEN-DEPTH + +!
   0 state PENDING + ! ;

: READ-EMPTY-END ( ptr n -- )
   {: state :}
   state CURRENT-FRAME {: frame :}
   frame @ state NAME-OFF + !
   frame CELL + @ state NAME-LEN + !
   frame 3 cells + @ state URI-OFF + !
   frame 4 cells + @ state URI-LEN + !
   POP-NEXT state PENDING + !
   state XML-KIND:END EVENT-FINISH ;

: TEXT-CONTINUES? ( ptr n -- bool )
   {: state :}
   state EOF? if false exit then
   state PEEK $3C <> ;

: TEXT-SCALAR ( ptr n -- )
   {: state :}
   state s" ]]>" MATCH$ if E-MALFORMED throw then
   state OPEN-DEPTH + @ 0= if
      state PEEK XML-SPACE? 0= if E-MALFORMED throw then
   then
   state SOURCE$ state POSITION + @ DECODE-TEXT DECODE-AT
   state POSITION + ! drop ;

: READ-TEXT ( ptr n -- )
   {: state :}
   state VALUE-BEGIN
   begin state TEXT-CONTINUES? while state TEXT-SCALAR repeat
   state VALUE-END
   state XML-KIND:TEXT EVENT-FINISH ;

: READ-COMMENT ( ptr n -- )
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

: READ-CDATA ( ptr n -- )
   {: state :}
   state OPEN-DEPTH + @ 0= if E-MALFORMED throw then
   state 9 ADVANCE-N
   state VALUE-BEGIN
   begin state s" ]]>" MATCH$ 0= while state SCAN-SCALAR drop repeat
   state VALUE-END
   state 3 ADVANCE-N
   state XML-KIND:CDATA EVENT-FINISH ;

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

: DECLARATION-POSITION ( ptr n -- )
   {: state :}
   state RAW-OFF + @ 0= if exit then
   state RAW-OFF + @ 3 = if
      state 0 3 SPAN$ s\" \xEF\xBB\xBF" BYTES= if exit then
   then
   E-MALFORMED throw ;

: DECLARATION-NAME? ( ptr n n ptr u8 n -- bool )
   {: state index:n name size:n :}
   state state index ATTRIBUTE name size NAME-MATCH? ;

: DECLARATION-VALUE$ ( ptr n n -- ptr u8 n )
   {: state index:n :}
   state state index ATTRIBUTE ATTRIBUTE-VALUE$ ;

: VERSION-CHECK ( ptr n -- )
   {: state :}
   state ATTR-COUNT + @ 0= if E-MALFORMED throw then
   state 0 s" version" DECLARATION-NAME? 0= if E-MALFORMED throw then
   state 0 DECLARATION-VALUE$ s" 1.0" BYTES= 0= if E-ENCODING throw then ;

: ENCODING-CHECK ( ptr n -- n )
   {: state :}
   state ATTR-COUNT + @ 1 = if 1 exit then
   state 1 s" encoding" DECLARATION-NAME? 0= if 1 exit then
   state 1 DECLARATION-VALUE$ s" UTF-8" FOLD-EQUAL? 0= if
      E-ENCODING throw
   then
   2 ;

: STANDALONE-CHECK ( ptr n n -- )
   {: state index:n :}
   state ATTR-COUNT + @ index = if exit then
   state ATTR-COUNT + @ index 1+ <> if E-MALFORMED throw then
   state index s" standalone" DECLARATION-NAME? 0= if E-MALFORMED throw then
   state index DECLARATION-VALUE$ {: value size:n :}
   value size s" yes" BYTES= value size s" no" BYTES= or 0= if
      E-MALFORMED throw
   then ;

: READ-DECLARATION ( ptr n -- )
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

: READ-PI-CONTENT ( ptr n -- )
   {: state :}
   state EOF? if E-TRUNCATED throw then
   state s" ?>" MATCH$ 0= if
      state WHITE? 0= if E-MALFORMED throw then
      state SKIP-WHITE
   then
   state VALUE-BEGIN
   begin state s" ?>" MATCH$ 0= while state SCAN-SCALAR drop repeat
   state VALUE-END ;

: READ-PI ( ptr n -- )
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

: READ-MARKUP ( ptr n -- )
   {: state :}
   state s" </" MATCH$ if state READ-END exit then
   state s" <!--" MATCH$ if state READ-COMMENT exit then
   state s" <![CDATA[" MATCH$ if state READ-CDATA exit then
   state s" <?" MATCH$ if state READ-PI exit then
   state s" <!DOCTYPE" MATCH$ if E-DTD throw then
   state s" <!" MATCH$ if E-MALFORMED throw then
   state READ-START ;

: READ-EOF ( ptr n -- )
   {: state :}
   state OPEN-DEPTH + @ 0<> if E-TRUNCATED throw then
   state ROOT-COUNT + @ 1 <> if E-MALFORMED throw then
   state XML-KIND:EOF EVENT-FINISH ;

: READ-EVENT ( ptr n -- )
   {: state :}
   state PENDING + @ POP-NEXT = if state POP-FRAME then
   state EVENT-RESET
   state PENDING + @ EMPTY-END = if state READ-EMPTY-END exit then
   state EOF? if state READ-EOF exit then
   state PEEK $3C = if state READ-MARKUP else state READ-TEXT then ;

public
: NEXT ( XML:reader -- XML:reader kind )
   STATE {: state :}
   state LIVE
   1 state FAILED + !
   state READ-EVENT
   0 state FAILED + !
   state TOKEN-KIND + @ >KIND ;

private
: TEXT-MODE ( ptr n -- n )
   TOKEN-KIND + @ >KIND MATCH kind
      start OF E-STATE throw ENDOF
      end OF E-STATE throw ENDOF
      text OF DECODE-TEXT ENDOF
      comment OF DECODE-RAW ENDOF
      pi OF DECODE-RAW ENDOF
      cdata OF DECODE-RAW ENDOF
      eof OF E-STATE throw ENDOF
   ;MATCH ;

public
: CONTENT ( XML:reader -- XML:reader off len )
   STATE {: state :}
   state LIVE
   state TEXT-MODE drop
   state VALUE-OFF + @ >OFF state VALUE-LEN + @ >LEN ;

: TEXT ( XML:reader ptr u8 n -- XML:reader n )
   {: destination cap:n :}
   STATE {: state :}
   state LIVE
   state TEXT-MODE {: mode:n :}
   state destination cap OUTPUT-CHECK
   state state VALUE-OFF + @ state VALUE-LEN + @ SPAN$
   mode destination cap DECODE-INTO ;

;package
