\ string-test.f - focused tests for checked stdlib string helpers.
\ Run: bin/hb --load src/lib/string-test.f

require src/lib/error.f
require src/lib/string.f
require src/lib/string-option.f
require src/lib/test/guard-page.f
require src/lib/fmt.f
require src/core/exit-status.f

64 constant STR-TEST-BUF-LEN
44 constant STR-TEST-COMMA
45 constant STR-TEST-DASH
65 constant STR-TEST-A-CHAR

variable STR-TEST-N
variable STR-TEST-FAIL
TYPED-VARIABLE STR-TEST-SPLIT-A ^u8
variable STR-TEST-SPLIT-U
variable STR-TEST-SPLIT-NEXT
variable STR-TEST-SPLIT-OK
variable STR-TEST-BYTE-VIEW-CELL
create STR-TEST-BUF STR-TEST-BUF-LEN allot
create STR-TEST-LEFT-WS
   STR:TAB c, STR:LF c, 97 c, 98 c, 99 c,
create STR-TEST-RIGHT-WS
   97 c, 98 c, 99 c, STR:TAB c, STR:LF c,
create STR-TEST-BYTEPLUS
   65 c, 66 c, 67 c,
create STR-TEST-ORDER-LOW
   0 c, 127 c,
create STR-TEST-ORDER-HIGH
   0 c, 128 c,
16 BUFFER: STR-TEST-BUF2
variable STR-TEST-BUF2-LEN

: STR-ASSERT ( bool -- )
   STR-TEST-N @ 1+ STR-TEST-N !
   0= if
      s" string-test: assertion " type STR-TEST-N @ FMT:.INT s"  failed" type cr
      STR-TEST-FAIL @ 1+ STR-TEST-FAIL !
   then ;

: STR-ASSERT= ( n n -- )
   = STR-ASSERT ;

: STR-ASSERT$ ( str str -- )
   STR:= STR-ASSERT ;

: STR-CHECK-REJECTS ( str -- )
   CHECK-QUIET-CANDIDATE! 0 STR-ASSERT= ;

: STR-SPLIT-CHECK ( str n n str n bool -- ) {: s:str sep start exp:str exp-next exp-ok :}
   s sep start STR:SPLIT-NEXT
   STR-TEST-SPLIT-OK !
   STR-TEST-SPLIT-NEXT !
   dup STR:LEN STR-TEST-SPLIT-U !
   STR:DATA STR-TEST-SPLIT-A !
   STR-TEST-SPLIT-A @ STR-TEST-SPLIT-U @ >str exp STR-ASSERT$
   STR-TEST-SPLIT-NEXT @ exp-next STR-ASSERT=
   exp-ok if
      STR-TEST-SPLIT-OK @ 0= 0= STR-ASSERT
   else
      STR-TEST-SPLIT-OK @ 0= STR-ASSERT
   then ;

: STR-PARSE-CHECK ( str n bool -- ) {: s:str want ok :}
   s STR:>NUMBER? MATCH option
     none OF ok 0= STR-ASSERT ENDOF
     some OF ok STR-ASSERT want STR-ASSERT= ENDOF
   ;MATCH ;

: STR-TEST-SB-OVERFLOW ( -- )
   SB-RESET
   SB-CAP 0 ?do STR-TEST-A-CHAR SB-APPEND-C loop
   STR-TEST-A-CHAR SB-APPEND-C ;

: STR-TEST-SB-APPEND-LEN ( n -- ) {: u:n :}
   s" x" STR:DATA u >str SB-APPEND ;

: STR-TEST-BUILDER ( -- )
   SB-RESET
   s" ab" SB-APPEND
   99 SB-APPEND-C
   SB$ s" abc" STR-ASSERT$
   SB-RESET
   s" alpha" SB-APPEND
   STR-TEST-DASH SB-APPEND-C
   s" beta" SB-APPEND
   SB$ s" alpha-beta" STR-ASSERT$
   [: STR-TEST-SB-OVERFLOW ;] catch E-STR-CAPACITY STR-ASSERT=
   SB-RESET
   s" ab" SB-APPEND
   [: SB-CAP 1- STR-TEST-SB-APPEND-LEN ;] catch E-STR-CAPACITY STR-ASSERT=
   SB$ s" ab" STR-ASSERT$
   STR-TEST-BYTE-VIEW-CELL BYTE-VIEW
      STR-TEST-BYTE-VIEW-CELL 0 STRUCT-BYTE+ = STR-ASSERT
   STR-TEST-A-CHAR STR-TEST-BYTE-VIEW-CELL BYTE-VIEW c!
   STR-TEST-BYTE-VIEW-CELL BYTE-VIEW c@ STR-TEST-A-CHAR STR-ASSERT=
   STR-TEST-BYTEPLUS 1 BYTE+ c@ STR-TEST-A-CHAR 1+ STR-ASSERT=
   STR-TEST-BYTEPLUS 2 BYTE@ STR-TEST-A-CHAR 2 + STR-ASSERT=
   s" BAD-BYTE-VIEW-LOAD ( ^a -- n ) BYTE-VIEW @" STR-CHECK-REJECTS
   s" BAD-BYTE-VIEW-STORE ( n ^a -- ) BYTE-VIEW !" STR-CHECK-REJECTS
   s" BAD-BYTE-VIEW-WIDEN ( ^a -- ^cell ) BYTE-VIEW" STR-CHECK-REJECTS
   s" BAD-BYTE-PLUS-RESULT ( ^u8 n -- n ) BYTE+" STR-CHECK-REJECTS
   s" BAD-BYTE-AT-RESULT ( ^u8 n -- ^u8 ) BYTE@" STR-CHECK-REJECTS
   s" BAD-BYTE-COPY-LEN ( ^u8 ^u8 off -- ) BYTE-COPY-LEN" STR-CHECK-REJECTS
   s" BAD-SB-APPEND-LEN ( ^u8 off -- ) SB-APPEND" STR-CHECK-REJECTS
   SB-RESET ;

: STR-TEST-BUF-OVERFLOW ( -- )
   15 >LEN STR-TEST-BUF2-LEN !
   s" xx" STR-TEST-BUF2 16 STR-TEST-BUF2-LEN BUF-APPEND ;

: STR-TEST-BUFFER ( -- )
   STR-TEST-BUF2-LEN BUF-RESET
   s" hi" STR-TEST-BUF2 16 STR-TEST-BUF2-LEN BUF-APPEND
   33 STR-TEST-BUF2 16 STR-TEST-BUF2-LEN BUF-APPEND-C
   STR-TEST-BUF2 STR-TEST-BUF2-LEN BUF-LEN@ >str s" hi!" STR-ASSERT$
   [: STR-TEST-BUF-OVERFLOW ;] catch E-STR-CAPACITY STR-ASSERT=
   s" BAD-BUF-RESET ( ^n -- ) BUF-RESET" STR-CHECK-REJECTS
   s" BAD-BUF-LEN@ ( ^n -- n ) BUF-LEN@" STR-CHECK-REJECTS
   s" BAD-BUF-APPEND-LEN ( ^u8 off ^u8 len ^len -- ) BUF-APPEND-LEN" STR-CHECK-REJECTS ;

: STR-TEST-SPLIT ( -- )
   s" a,b,c" STR-TEST-COMMA 0 s" a" 2 STR:TRUE STR-SPLIT-CHECK
   s" a,b,c" STR-TEST-COMMA 2 s" b" 4 STR:TRUE STR-SPLIT-CHECK
   s" a,b,c" STR-TEST-COMMA 4 s" c" 6 STR:TRUE STR-SPLIT-CHECK
   s" a,b,c" STR-TEST-COMMA 6 s" " 6 STR:FALSE STR-SPLIT-CHECK
   s" ,a,," STR-TEST-COMMA 0 s" " 1 STR:TRUE STR-SPLIT-CHECK
   s" ,a,," STR-TEST-COMMA 1 s" a" 3 STR:TRUE STR-SPLIT-CHECK
   s" ,a,," STR-TEST-COMMA 3 s" " 4 STR:TRUE STR-SPLIT-CHECK
   s" ,a,," STR-TEST-COMMA 4 s" " 5 STR:TRUE STR-SPLIT-CHECK
   s" " STR-TEST-COMMA 0 s" " 1 STR:TRUE STR-SPLIT-CHECK
   s" " STR-TEST-COMMA 1 s" " 1 STR:FALSE STR-SPLIT-CHECK ;

: STR-TEST-PARSE ( -- )
   s" 12345" 12345 STR:TRUE STR-PARSE-CHECK
   s" -456" -456 STR:TRUE STR-PARSE-CHECK
   s" +77" 77 STR:TRUE STR-PARSE-CHECK
   s" " 0 STR:FALSE STR-PARSE-CHECK
   s" abc" 0 STR:FALSE STR-PARSE-CHECK
   s" 12x" 0 STR:FALSE STR-PARSE-CHECK
   s" 9223372036854775807" STR:MAX-I64 STR:TRUE STR-PARSE-CHECK
   s" -9223372036854775808" STR:MIN-I64 STR:TRUE STR-PARSE-CHECK
   s" 9223372036854775808" 0 STR:FALSE STR-PARSE-CHECK
   s" -9223372036854775809" 0 STR:FALSE STR-PARSE-CHECK ;

: STR-PARSE-SOME ( str n [ str -- option<n> ] -- ) {: s:str want:n q :}
   s q execute MATCH option
     none OF 0 0= 0= ENDOF
     some OF want = ENDOF
   ;MATCH STR-ASSERT ;
: STR-PARSE-NONE ( str [ str -- option<n> ] -- ) {: s:str q :}
   s q execute MATCH option
     none OF 0 0= ENDOF
     some OF drop 0 0= 0= ENDOF
   ;MATCH STR-ASSERT ;
: STR-TEST-PARSE-OPTION ( -- )
   s" 42" 42 [: STR:PARSE-POS ;] STR-PARSE-SOME
   s" 0" 0 [: STR:PARSE-POS ;] STR-PARSE-SOME
   s" abc" [: STR:PARSE-POS ;] STR-PARSE-NONE
   s" 9223372036854775808" [: STR:PARSE-POS ;] STR-PARSE-NONE
   s" 456" -456 [: STR:PARSE-NEG ;] STR-PARSE-SOME
   s" 9223372036854775808" STR:MIN-I64 [: STR:PARSE-NEG ;] STR-PARSE-SOME
   s" 4x6" [: STR:PARSE-NEG ;] STR-PARSE-NONE
   s" 9223372036854775809" [: STR:PARSE-NEG ;] STR-PARSE-NONE
   s" 77" 77 [: STR:>NUMBER? ;] STR-PARSE-SOME
   s" nope" [: STR:>NUMBER? ;] STR-PARSE-NONE
   s" 0" 0 [: STR:HEX>NUMBER? ;] STR-PARSE-SOME
   s" ff" 255 [: STR:HEX>NUMBER? ;] STR-PARSE-SOME
   s" FF" 255 [: STR:HEX>NUMBER? ;] STR-PARSE-SOME
   s" 4008000000000000" $4008000000000000 [: STR:HEX>NUMBER? ;] STR-PARSE-SOME
   s" FFFFFFFFFFFFFFFF" -1 [: STR:HEX>NUMBER? ;] STR-PARSE-SOME
   s" 00000000000000001" [: STR:HEX>NUMBER? ;] STR-PARSE-NONE
   s" " [: STR:HEX>NUMBER? ;] STR-PARSE-NONE
   s" 0x10" [: STR:HEX>NUMBER? ;] STR-PARSE-NONE
   s" -1" [: STR:HEX>NUMBER? ;] STR-PARSE-NONE
   s" 1G" [: STR:HEX>NUMBER? ;] STR-PARSE-NONE ;

: STR-TEST-HEX-DIGIT ( -- )
   [char] 0 STR:HEX-DIGIT-VALUE 0 STR-ASSERT=
   [char] 9 STR:HEX-DIGIT-VALUE 9 STR-ASSERT=
   [char] A STR:HEX-DIGIT-VALUE 10 STR-ASSERT=
   [char] F STR:HEX-DIGIT-VALUE 15 STR-ASSERT=
   [char] a STR:HEX-DIGIT-VALUE 10 STR-ASSERT=
   [char] f STR:HEX-DIGIT-VALUE 15 STR-ASSERT=
   [char] 0 STR:HEX-DIGIT? STR-ASSERT
   [char] 9 STR:HEX-DIGIT? STR-ASSERT
   [char] A STR:HEX-DIGIT? STR-ASSERT
   [char] F STR:HEX-DIGIT? STR-ASSERT
   [char] a STR:HEX-DIGIT? STR-ASSERT
   [char] f STR:HEX-DIGIT? STR-ASSERT
   [char] / STR:HEX-DIGIT? 0= STR-ASSERT
   [char] : STR:HEX-DIGIT? 0= STR-ASSERT
   [char] @ STR:HEX-DIGIT? 0= STR-ASSERT
   [char] G STR:HEX-DIGIT? 0= STR-ASSERT
   [char] ` STR:HEX-DIGIT? 0= STR-ASSERT
   [char] g STR:HEX-DIGIT? 0= STR-ASSERT ;

: STR-TEST-ALNUM ( -- )
   [char] 0 STR:ALNUM? STR-ASSERT
   [char] 9 STR:ALNUM? STR-ASSERT
   [char] A STR:ALNUM? STR-ASSERT
   [char] Z STR:ALNUM? STR-ASSERT
   [char] a STR:ALNUM? STR-ASSERT
   [char] z STR:ALNUM? STR-ASSERT
   [char] / STR:ALNUM? 0= STR-ASSERT
   [char] : STR:ALNUM? 0= STR-ASSERT
   [char] @ STR:ALNUM? 0= STR-ASSERT
   [char] [ STR:ALNUM? 0= STR-ASSERT
   [char] ` STR:ALNUM? 0= STR-ASSERT
   [char] { STR:ALNUM? 0= STR-ASSERT
   STR:SPACE STR:ALNUM? 0= STR-ASSERT
   0 STR:ALNUM? 0= STR-ASSERT
   127 STR:ALNUM? 0= STR-ASSERT
   200 STR:ALNUM? 0= STR-ASSERT ;

: STR-IDX>N ( option<idx> -- n )
   MATCH option
     none OF -1 ENDOF
     some OF IDX>N ENDOF
   ;MATCH ;

: STR-INDEX-OF>N ( str n -- n ) STR:INDEX-OF STR-IDX>N ;
: STR-FIND-SUB>N ( str str -- n ) STR:FIND-SUB STR-IDX>N ;
: STR-FIND-SUB-FROM>N ( str str n -- n ) STR:FIND-SUB-FROM STR-IDX>N ;

: STR-TEST-FIND-SUB-OPTION ( -- )
   s" abcdef" s" bcd" STR:FIND-SUB MATCH option
     none OF STR:FALSE ENDOF
     some OF IDX>N 1 = ENDOF
   ;MATCH STR-ASSERT
   s" abcdef" s" z" STR:FIND-SUB MATCH option
     none OF STR:TRUE ENDOF
     some OF drop STR:FALSE ENDOF
   ;MATCH STR-ASSERT ;

\ The end of readable memory: a word that reads a byte of a span it should have
\ rejected faults here instead of answering.
: STR-TEST-EDGE ( -- ^u8 )
   0 0 GUARD-PAGE:TAIL ;

\ Equality must stay inside both spans, at every alignment and through NUL.
\ The guarded span ends at unreadable memory; the other is independently
\ aligned. A difference at each byte must remain visible after preceding NULs.
3 cells BUFFER: STR-TEST-EQ-BUF

: STR-TEST-EQ-SPANS ( -- )
   3 cells 0 ?do 0 STR-TEST-EQ-BUF i + c! loop
   2 cells 2 + 0 ?do
      i {: u:n :}
      u 0 GUARD-PAGE:TAIL {: a:^ :}
      CELL 0 ?do
         STR-TEST-EQ-BUF i + {: b:^ :}
         a u >str b u >str STR:= STR-ASSERT
         u 0 ?do
            128 b i + c!
            a u >str b u >str STR:= 0= STR-ASSERT
            0 b i + c!
         loop
      loop
   loop
   STR-TEST-EDGE 0 >str STR-TEST-EDGE 1 >str STR:= 0= STR-ASSERT
   STR-TEST-EDGE 1 >str STR-TEST-EDGE 0 >str STR:= 0= STR-ASSERT ;

: STR-TEST-ORDER ( -- )
   s" " s" " STR:< 0= STR-ASSERT
   s" " s" a" STR:< STR-ASSERT
   s" a" s" " STR:< 0= STR-ASSERT
   s" ab" s" ab" STR:< 0= STR-ASSERT
   s" ab" s" abc" STR:< STR-ASSERT
   s" abc" s" ab" STR:< 0= STR-ASSERT
   s" azzz" s" ba" STR:< STR-ASSERT
   s" ba" s" azzz" STR:< 0= STR-ASSERT
   STR-TEST-ORDER-LOW 2 >str STR-TEST-ORDER-HIGH 2 >str STR:< STR-ASSERT
   STR-TEST-ORDER-HIGH 2 >str STR-TEST-ORDER-LOW 2 >str STR:< 0= STR-ASSERT ;

: STR-TEST-INDEX-OF-OPTION ( -- )
   s" abcdef" 100 STR:INDEX-OF MATCH option
     none OF STR:FALSE ENDOF
     some OF IDX>N 3 = ENDOF
   ;MATCH STR-ASSERT
   s" abcdef" 120 STR:INDEX-OF MATCH option
     none OF STR:TRUE ENDOF
     some OF drop STR:FALSE ENDOF
   ;MATCH STR-ASSERT ;

s" abc" s" abc" STR:= STR-ASSERT
s" " s" " STR:= STR-ASSERT
s" abc" s" abd" STR:= 0= STR-ASSERT
s" abc" s" abcd" STR:= 0= STR-ASSERT
$41 STR:ASCII-LOWER $61 STR-ASSERT=
$5A STR:ASCII-LOWER $7A STR-ASSERT=
$7A STR:ASCII-LOWER $7A STR-ASSERT=
$61 STR:ASCII-UPPER $41 STR-ASSERT=
$7A STR:ASCII-UPPER $5A STR-ASSERT=
$5A STR:ASCII-UPPER $5A STR-ASSERT=
s" AbC" s" aBc" STR:=CI STR-ASSERT
s" AbCd" s" aBce" STR:=CI 0= STR-ASSERT
s" abcdef" s" abc" STR:STARTS-WITH? STR-ASSERT
s" abcdef" s" " STR:STARTS-WITH? STR-ASSERT
s" abcdef" s" abd" STR:STARTS-WITH? 0= STR-ASSERT
s" abc" s" abcdef" STR:STARTS-WITH? 0= STR-ASSERT
s" abcdef" s" def" STR:ENDS-WITH? STR-ASSERT
s" abcdef" s" " STR:ENDS-WITH? STR-ASSERT
s" abcdef" s" cef" STR:ENDS-WITH? 0= STR-ASSERT
s" abc" s" abcdef" STR:ENDS-WITH? 0= STR-ASSERT
s" abcdef" s" bcd" STR-FIND-SUB>N 1 STR-ASSERT=
s" abcabc" s" abc" STR-FIND-SUB>N 0 STR-ASSERT=
s" abcdef" s" z" STR-FIND-SUB>N -1 STR-ASSERT=
s" abcdef" s" " STR-FIND-SUB>N 0 STR-ASSERT=
s" abcabc" s" abc" 3 STR-FIND-SUB-FROM>N 3 STR-ASSERT=
s" abcabc" s" abc" 4 STR-FIND-SUB-FROM>N -1 STR-ASSERT=
s" abcabc" s" bc" 2 STR-FIND-SUB-FROM>N 4 STR-ASSERT=
s" abcabc" s" " 2 STR-FIND-SUB-FROM>N 2 STR-ASSERT=
s" abc" s" " 3 STR-FIND-SUB-FROM>N 3 STR-ASSERT=
s" abc" s" c" 3 STR-FIND-SUB-FROM>N -1 STR-ASSERT=
s" abc" s" " 4 STR-FIND-SUB-FROM>N -1 STR-ASSERT=
s" abc" s" a" -1 STR-FIND-SUB-FROM>N -1 STR-ASSERT=
STR-TEST-EDGE 0 >str s" a" 5 STR-FIND-SUB-FROM>N -1 STR-ASSERT=
s" abcdef" s" cde" STR:CONTAINS? STR-ASSERT
s" abcdef" s" " STR:CONTAINS? STR-ASSERT
s" abcdef" s" xyz" STR:CONTAINS? 0= STR-ASSERT
s" abcdef" 100 STR-INDEX-OF>N 3 STR-ASSERT=
s" abcdef" 97 STR-INDEX-OF>N 0 STR-ASSERT=
s" abcdef" 120 STR-INDEX-OF>N -1 STR-ASSERT=
s" banana" 97 STR:COUNT-CHAR 3 STR-ASSERT=
s" " 97 STR:COUNT-CHAR 0 STR-ASSERT=
s" banana" 120 STR:COUNT-CHAR 0 STR-ASSERT=
s" abcabc" s" abc" STR:COUNT-SUB 2 STR-ASSERT=
s" aaaa" s" aa" STR:COUNT-SUB 2 STR-ASSERT=
s" aaa" s" aa" STR:COUNT-SUB 1 STR-ASSERT=
s" xabab" s" ab" STR:COUNT-SUB 2 STR-ASSERT=
s" abcdef" s" xyz" STR:COUNT-SUB 0 STR-ASSERT=
s" abcdef" s" " STR:COUNT-SUB 0 STR-ASSERT=
s" " s" a" STR:COUNT-SUB 0 STR-ASSERT=
s" ab" s" abc" STR:COUNT-SUB 0 STR-ASSERT=
s"    abc" STR:LTRIM s" abc" STR-ASSERT$
STR-TEST-LEFT-WS 5 >str STR:LTRIM s" abc" STR-ASSERT$
s" abc   " STR:RTRIM s" abc" STR-ASSERT$
STR-TEST-RIGHT-WS 5 >str STR:RTRIM s" abc" STR-ASSERT$
s"    abc   " STR:TRIM s" abc" STR-ASSERT$
s"   " STR:TRIM STR:LEN 0 STR-ASSERT=
s" " STR:LTRIM STR:LEN 0 STR-ASSERT=
s" " STR:RTRIM STR:LEN 0 STR-ASSERT=
s" copy" STR:DATA STR-TEST-BUF 4 BYTE-COPY
STR-TEST-BUF 4 >str s" copy" STR-ASSERT$
STR-TEST-BUF STR-TEST-BUF 0 STR-LEN BYTE-COPY-LEN
STR-TEST-BUF STR-TEST-BUF 0 BYTE-COPY
STR-TEST-BUILDER
STR-TEST-BUFFER
STR-TEST-SPLIT
STR-TEST-PARSE
STR-TEST-PARSE-OPTION
STR-TEST-HEX-DIGIT
STR-TEST-ALNUM
STR-TEST-INDEX-OF-OPTION
STR-TEST-FIND-SUB-OPTION
STR-TEST-EQ-SPANS
STR-TEST-ORDER

false [if] \ off until dot habu-add-the-checks-a49d054d
: STR-TEST-LEN-NEG ( -- )
   -1 STR-LEN drop ;

: STR-TEST-OFF-NEG ( -- )
   -1 STR-OFF drop ;

: STR-TEST-COUNT-NEG ( -- )
   -1 STR-COUNT drop ;

: STR-TEST-BUILDER-NEGATIVE ( -- )
   [: STR-TEST-LEN-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR-TEST-OFF-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR-TEST-COUNT-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   SB-RESET                                         \ a negative length would move the cursor back
   s" ab" SB-APPEND
   [: -1 STR-TEST-SB-APPEND-LEN ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR:MIN-I64 STR-TEST-SB-APPEND-LEN ;] catch E-STR-BOUNDS STR-ASSERT=
   SB$ s" ab" STR-ASSERT$ ;

: STR-TEST-BUF-BAD-C ( -- )
   300 STR-TEST-BUF2 16 STR-TEST-BUF2-LEN BUF-APPEND-C ;

: STR-TEST-BUF-BAD-LEN ( -- )
   -1 STR-TEST-BUF2-LEN !
   s" x" STR-TEST-BUF2 16 STR-TEST-BUF2-LEN BUF-APPEND ;

: STR-TEST-BUFFER-NEGATIVE ( -- )
   [: STR-TEST-BUF-BAD-C ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR-TEST-BUF-BAD-LEN ;] catch E-STR-BOUNDS STR-ASSERT= ;

: STR-TEST-FIND-SUB-NEG ( n -- ) {: v:n :}
   s" abc" s" x" drop v STR-FIND-SUB>N drop ;

: STR-TEST-FIND-SUB-NEGATIVE ( -- )
   [: -1 STR-TEST-FIND-SUB-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR:MIN-I64 STR-TEST-FIND-SUB-NEG ;] catch E-STR-BOUNDS STR-ASSERT= ;

: STR-TEST-FIND-SUB-HAY ( n n -- ) {: u:n v:n :}
   s" abc" drop u s" abc" drop v STR-FIND-SUB>N drop ;

: STR-TEST-EQ-NEG ( n n -- ) {: u:n v:n :}
   s" abc" drop u s" abc" drop v STR:= drop ;

: STR-TEST-EQ-CI-NEG ( n n -- ) {: u:n v:n :}
   s" abc" drop u s" abc" drop v STR:=CI drop ;

: STR-TEST-STARTS-NEG ( n n -- ) {: u:n v:n :}
   s" abc" drop u s" abc" drop v STR:STARTS-WITH? drop ;

: STR-TEST-ENDS-NEG ( n n -- ) {: u:n v:n :}
   s" abc" drop u s" abc" drop v STR:ENDS-WITH? drop ;

\ Caught: equal negative lengths comparing true, a negative needle matching
\ every haystack, a negative haystack holding the empty needle at 0.
: STR-TEST-COMPARE-NEGATIVE ( -- )
   [: -1 1 STR-TEST-FIND-SUB-HAY ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR:MIN-I64 0 STR-TEST-FIND-SUB-HAY ;] catch E-STR-BOUNDS STR-ASSERT=
   [: -1 -1 STR-TEST-EQ-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR:MIN-I64 STR:MIN-I64 STR-TEST-EQ-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: 3 -1 STR-TEST-EQ-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR:MIN-I64 3 STR-TEST-EQ-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: -1 -1 STR-TEST-EQ-CI-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR:MIN-I64 STR:MIN-I64 STR-TEST-EQ-CI-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: 3 -1 STR-TEST-EQ-CI-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR:MIN-I64 3 STR-TEST-EQ-CI-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: 3 -1 STR-TEST-STARTS-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: 3 STR:MIN-I64 STR-TEST-STARTS-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: -1 1 STR-TEST-STARTS-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR:MIN-I64 0 STR-TEST-STARTS-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: 3 -1 STR-TEST-ENDS-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: 3 STR:MIN-I64 STR-TEST-ENDS-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: -1 1 STR-TEST-ENDS-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR:MIN-I64 0 STR-TEST-ENDS-NEG ;] catch E-STR-BOUNDS STR-ASSERT= ;

: STR-TEST-ORDER-LEFT-NEG ( n -- ) {: u:n :}
   STR-TEST-EDGE u s" a" STR:< drop ;

: STR-TEST-ORDER-RIGHT-NEG ( n -- ) {: v:n :}
   s" a" STR-TEST-EDGE v STR:< drop ;

\ Invalid lengths throw before any byte is read.
: STR-TEST-ORDER-NEGATIVE ( -- )
   [: -1 STR-TEST-ORDER-LEFT-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR:MIN-I64 STR-TEST-ORDER-LEFT-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: -1 STR-TEST-ORDER-RIGHT-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR:MIN-I64 STR-TEST-ORDER-RIGHT-NEG ;] catch E-STR-BOUNDS STR-ASSERT= ;

\ A sign and one digit ending at the edge, so a length that wraps past the sign
\ scans on into the inaccessible page.
: STR-TEST-SIGNED ( n -- ^u8 ) {: sign:n :}
   2 STR:ZERO 1+ GUARD-PAGE:TAIL {: a:^ :}
   sign a c!
   a ;

: STR-TEST-DROP-N ( option<n> -- )
   MATCH option
     none OF ENDOF
     some OF drop ENDOF
   ;MATCH ;

: STR-TEST-INDEX-NEG ( n -- ) {: u:n :}
   STR-TEST-EDGE u STR-TEST-A-CHAR STR-INDEX-OF>N drop ;

: STR-TEST-COUNT-CHAR-NEG ( n -- ) {: u:n :}
   STR-TEST-EDGE u STR-TEST-A-CHAR STR:COUNT-CHAR drop ;

: STR-TEST-COUNT-SUB-NEG ( n -- ) {: u:n :}
   STR-TEST-EDGE u s" a" STR:COUNT-SUB drop ;

: STR-TEST-COUNT-SUB-NEEDLE-NEG ( n -- ) {: v:n :}
   s" abc" STR-TEST-EDGE v STR:COUNT-SUB drop ;

: STR-TEST-LTRIM-NEG ( n -- ) {: u:n :}
   STR-TEST-EDGE u STR:LTRIM 2drop ;

: STR-TEST-RTRIM-NEG ( n -- ) {: u:n :}
   STR-TEST-EDGE u STR:RTRIM 2drop ;

: STR-TEST-TRIM-NEG ( n -- ) {: u:n :}
   STR-TEST-EDGE u STR:TRIM 2drop ;

: STR-TEST-SPLIT-NEG ( n -- ) {: u:n :}
   STR-TEST-EDGE u STR-TEST-COMMA 0 STR:SPLIT-NEXT 2drop 2drop ;

: STR-TEST-DIGITS-NEG ( n -- ) {: u:n :}
   STR-TEST-EDGE u STR:DIGITS? drop ;

: STR-TEST-DIGITS<=-LEFT ( n -- ) {: u:n :}
   STR-TEST-EDGE u s" 1" STR:DIGITS<= drop ;

: STR-TEST-DIGITS<=-RIGHT ( n -- ) {: v:n :}
   s" 1" STR-TEST-EDGE v STR:DIGITS<= drop ;

: STR-TEST-PARSE-POS-NEG ( n -- ) {: u:n :}
   STR-TEST-EDGE u STR:PARSE-POS STR-TEST-DROP-N ;

: STR-TEST-PARSE-NEG-NEG ( n -- ) {: u:n :}
   STR-TEST-EDGE u STR:PARSE-NEG STR-TEST-DROP-N ;

: STR-TEST-NUMBER-NEG ( n -- ) {: u:n :}
   STR-TEST-EDGE u STR:>NUMBER? STR-TEST-DROP-N ;

: STR-TEST-SIGNED-NEG ( n n -- ) {: sign:n u:n :}
   sign STR-TEST-SIGNED u STR:>NUMBER? STR-TEST-DROP-N ;

\ Each span ends at the inaccessible page, so a read before the length check
\ faults. With no rejection, STR:INDEX-OF, STR:COUNT-CHAR and STR:SPLIT-NEXT answer NONE, 0 and no
\ more fields, the trims an empty string, STR:DIGITS? and STR:DIGITS<= true, and
\ the parsers SOME 0.
: STR-TEST-SCAN-NEGATIVE ( -- )
   [: -1 STR-TEST-INDEX-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR:MIN-I64 STR-TEST-INDEX-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: -1 STR-TEST-COUNT-CHAR-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR:MIN-I64 STR-TEST-COUNT-CHAR-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: -1 STR-TEST-COUNT-SUB-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR:MIN-I64 STR-TEST-COUNT-SUB-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: -1 STR-TEST-COUNT-SUB-NEEDLE-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: -1 STR-TEST-LTRIM-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR:MIN-I64 STR-TEST-LTRIM-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: -1 STR-TEST-RTRIM-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR:MIN-I64 STR-TEST-RTRIM-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: -1 STR-TEST-TRIM-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR:MIN-I64 STR-TEST-TRIM-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: -1 STR-TEST-SPLIT-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR:MIN-I64 STR-TEST-SPLIT-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: -1 STR-TEST-DIGITS-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR:MIN-I64 STR-TEST-DIGITS-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: -1 STR-TEST-DIGITS<=-LEFT ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR:MIN-I64 STR-TEST-DIGITS<=-LEFT ;] catch E-STR-BOUNDS STR-ASSERT=
   [: -1 STR-TEST-DIGITS<=-RIGHT ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR:MIN-I64 STR-TEST-DIGITS<=-RIGHT ;] catch E-STR-BOUNDS STR-ASSERT=
   [: -1 STR-TEST-PARSE-POS-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR:MIN-I64 STR-TEST-PARSE-POS-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: -1 STR-TEST-PARSE-NEG-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR:MIN-I64 STR-TEST-PARSE-NEG-NEG ;] catch E-STR-BOUNDS STR-ASSERT= ;

\ Caught: STR:>NUMBER? reading the first byte of a negative span, answering
\ SOME 0 for -1 after a sign, and at the minimum cell `u 1-` wrapping to the
\ maximum and the digit scan running into the inaccessible page.
: STR-TEST-NUMBER-NEGATIVE ( -- )
   [: STR:MINUS -1 STR-TEST-SIGNED-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR:PLUS -1 STR-TEST-SIGNED-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR:MINUS STR:MIN-I64 STR-TEST-SIGNED-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR:PLUS STR:MIN-I64 STR-TEST-SIGNED-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: -1 STR-TEST-NUMBER-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR:MIN-I64 STR-TEST-NUMBER-NEG ;] catch E-STR-BOUNDS STR-ASSERT= ;

STR-TEST-BUILDER-NEGATIVE
STR-TEST-BUFFER-NEGATIVE
STR-TEST-FIND-SUB-NEGATIVE
STR-TEST-COMPARE-NEGATIVE
STR-TEST-ORDER-NEGATIVE
STR-TEST-SCAN-NEGATIVE
STR-TEST-NUMBER-NEGATIVE
[then]

: STR-TEST-REPORT ( -- )
   STR-TEST-FAIL @ 0= if s" string-test: ok" type cr exit then
   s" string-test: " type STR-TEST-FAIL @ FMT:.INT s"  failure(s)" type cr
   s" string-test: failures" EXIT-STATUS:FAILURE die ;

STR-TEST-REPORT
