\ string-test.f - focused tests for checked stdlib string helpers.
\ Run: cat lib/errors.f lib/string.f lib/string-test.f | bin/hb

64 constant STR-TEST-BUF-LEN
1 constant STR-TEST-EX-FAIL
44 constant STR-TEST-COMMA
45 constant STR-TEST-DASH
65 constant STR-TEST-A-CHAR

variable STR-TEST-N
variable STR-TEST-FAIL
variable STR-TEST-SPLIT-A
variable STR-TEST-SPLIT-U
variable STR-TEST-SPLIT-NEXT
variable STR-TEST-SPLIT-OK
variable STR-TEST-PARSE-N
variable STR-TEST-PARSE-OK
create STR-TEST-BUF STR-TEST-BUF-LEN allot
create STR-TEST-LEFT-WS
   STR-TAB c, STR-LF c, 97 c, 98 c, 99 c,
create STR-TEST-RIGHT-WS
   97 c, 98 c, 99 c, STR-TAB c, STR-LF c,

: STR-ASSERT ( bool -- )
   STR-TEST-N @ 1+ STR-TEST-N !
   0= if
      s" string-test: assertion " type STR-TEST-N @ . s"  failed" type cr
      STR-TEST-FAIL @ 1+ STR-TEST-FAIL !
   then ;

: STR-ASSERT= ( n n -- )
   = STR-ASSERT ;

: STR-ASSERT$ ( ptr u8 n ptr u8 n -- )
   STR= STR-ASSERT ;

TRUSTED: STR-CHECK-REJECTS ( ptr u8 n -- )
   DIAGXT @ >r
   0 DIAGXT !
   CHECK! 0 STR-ASSERT=
   r> DIAGXT ! ;

: STR-SPLIT-CHECK ( ptr u8 n n n ptr u8 n n bool -- ) {: a:ptr u sep start exp:ptr exp-u exp-next exp-ok :}
   a u sep start SPLIT-NEXT
   STR-TEST-SPLIT-OK !
   STR-TEST-SPLIT-NEXT !
   STR-TEST-SPLIT-U !
   STR-TEST-SPLIT-A !
   STR-TEST-SPLIT-A @ STR-TEST-SPLIT-U @ exp exp-u STR-ASSERT$
   STR-TEST-SPLIT-NEXT @ exp-next STR-ASSERT=
   exp-ok if
      STR-TEST-SPLIT-OK @ 0= 0= STR-ASSERT
   else
      STR-TEST-SPLIT-OK @ 0= STR-ASSERT
   then ;

: STR-PARSE-CHECK ( ptr u8 n n bool -- ) {: a:ptr u want ok :}
   a u STR>NUMBER?
   STR-TEST-PARSE-OK !
   STR-TEST-PARSE-N !
   ok if
      STR-TEST-PARSE-OK @ 0= 0= STR-ASSERT
   else
      STR-TEST-PARSE-OK @ 0= STR-ASSERT
   then
   STR-TEST-PARSE-N @ want STR-ASSERT= ;

: STR-TEST-SB-OVERFLOW ( -- )
   SB-RESET
   SB-CAP 0 ?do STR-TEST-A-CHAR SB-APPEND-C loop
   STR-TEST-A-CHAR SB-APPEND-C ;

: STR-TEST-LEN-NEG ( -- )
   -1 STR-LEN drop ;

: STR-TEST-OFF-NEG ( -- )
   -1 STR-OFF drop ;

: STR-TEST-COUNT-NEG ( -- )
   -1 STR-COUNT drop ;

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
   [: STR-TEST-LEN-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR-TEST-OFF-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR-TEST-COUNT-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   s" BAD-BYTE-COPY-LEN ( ptr u8 ptr u8 off -- ) BYTE-COPY-LEN" STR-CHECK-REJECTS
   s" BAD-SB-APPEND-LEN ( ptr u8 off -- ) SB-APPEND-LEN" STR-CHECK-REJECTS
   SB-RESET ;

: STR-TEST-SPLIT ( -- )
   s" a,b,c" STR-TEST-COMMA 0 s" a" 2 STR-TRUE STR-SPLIT-CHECK
   s" a,b,c" STR-TEST-COMMA 2 s" b" 4 STR-TRUE STR-SPLIT-CHECK
   s" a,b,c" STR-TEST-COMMA 4 s" c" 6 STR-TRUE STR-SPLIT-CHECK
   s" a,b,c" STR-TEST-COMMA 6 s" " 6 STR-FALSE STR-SPLIT-CHECK
   s" ,a,," STR-TEST-COMMA 0 s" " 1 STR-TRUE STR-SPLIT-CHECK
   s" ,a,," STR-TEST-COMMA 1 s" a" 3 STR-TRUE STR-SPLIT-CHECK
   s" ,a,," STR-TEST-COMMA 3 s" " 4 STR-TRUE STR-SPLIT-CHECK
   s" ,a,," STR-TEST-COMMA 4 s" " 5 STR-TRUE STR-SPLIT-CHECK
   s" " STR-TEST-COMMA 0 s" " 1 STR-TRUE STR-SPLIT-CHECK
   s" " STR-TEST-COMMA 1 s" " 1 STR-FALSE STR-SPLIT-CHECK ;

: STR-TEST-PARSE ( -- )
   s" 12345" 12345 STR-TRUE STR-PARSE-CHECK
   s" -456" -456 STR-TRUE STR-PARSE-CHECK
   s" +77" 77 STR-TRUE STR-PARSE-CHECK
   s" " 0 STR-FALSE STR-PARSE-CHECK
   s" abc" 0 STR-FALSE STR-PARSE-CHECK
   s" 12x" 0 STR-FALSE STR-PARSE-CHECK
   s" 9223372036854775807" STR-MAX-I64 STR-TRUE STR-PARSE-CHECK
   s" -9223372036854775808" STR-MIN-I64 STR-TRUE STR-PARSE-CHECK
   s" 9223372036854775808" 0 STR-FALSE STR-PARSE-CHECK
   s" -9223372036854775809" 0 STR-FALSE STR-PARSE-CHECK ;

s" abc" s" abc" STR= STR-ASSERT
s" " s" " STR= STR-ASSERT
s" abc" s" abd" STR= 0= STR-ASSERT
s" abc" s" abcd" STR= 0= STR-ASSERT
$41 ASCII-LOWER $61 STR-ASSERT=
$5A ASCII-LOWER $7A STR-ASSERT=
$7A ASCII-LOWER $7A STR-ASSERT=
$61 ASCII-UPPER $41 STR-ASSERT=
$7A ASCII-UPPER $5A STR-ASSERT=
$5A ASCII-UPPER $5A STR-ASSERT=
s" AbC" s" aBc" STR=CI STR-ASSERT
s" AbCd" s" aBce" STR=CI 0= STR-ASSERT
s" abcdef" s" abc" STARTS-WITH? STR-ASSERT
s" abcdef" s" " STARTS-WITH? STR-ASSERT
s" abcdef" s" abd" STARTS-WITH? 0= STR-ASSERT
s" abc" s" abcdef" STARTS-WITH? 0= STR-ASSERT
s" abcdef" s" def" ENDS-WITH? STR-ASSERT
s" abcdef" s" " ENDS-WITH? STR-ASSERT
s" abcdef" s" cef" ENDS-WITH? 0= STR-ASSERT
s" abc" s" abcdef" ENDS-WITH? 0= STR-ASSERT
s" abcdef" s" bcd" FIND-SUB 1 STR-ASSERT=
s" abcabc" s" abc" FIND-SUB 0 STR-ASSERT=
s" abcdef" s" z" FIND-SUB -1 STR-ASSERT=
s" abcdef" s" " FIND-SUB 0 STR-ASSERT=
s" abcdef" s" cde" CONTAINS? STR-ASSERT
s" abcdef" s" " CONTAINS? STR-ASSERT
s" abcdef" s" xyz" CONTAINS? 0= STR-ASSERT
s" abcdef" 100 INDEX-OF 3 STR-ASSERT=
s" abcdef" 97 INDEX-OF 0 STR-ASSERT=
s" abcdef" 120 INDEX-OF -1 STR-ASSERT=
s" banana" 97 COUNT-CHAR 3 STR-ASSERT=
s" " 97 COUNT-CHAR 0 STR-ASSERT=
s" banana" 120 COUNT-CHAR 0 STR-ASSERT=
s"    abc" LTRIM s" abc" STR-ASSERT$
STR-TEST-LEFT-WS 5 LTRIM s" abc" STR-ASSERT$
s" abc   " RTRIM s" abc" STR-ASSERT$
STR-TEST-RIGHT-WS 5 RTRIM s" abc" STR-ASSERT$
s"    abc   " TRIM s" abc" STR-ASSERT$
s"   " TRIM nip 0 STR-ASSERT=
s" " LTRIM nip 0 STR-ASSERT=
s" " RTRIM nip 0 STR-ASSERT=
s" copy" drop STR-TEST-BUF 4 BYTE-COPY
STR-TEST-BUF 4 s" copy" STR-ASSERT$
STR-TEST-BUF 0 STR-TEST-BUF 0 STR-LEN BYTE-COPY-LEN
STR-TEST-BUF 0 STR-TEST-BUF 0 BYTE-COPY
STR-TEST-BUILDER
STR-TEST-SPLIT
STR-TEST-PARSE

: STR-TEST-REPORT ( -- )
   STR-TEST-FAIL @ 0= if s" string-test: ok" type cr exit then
   STR-TEST-FAIL @ . s" string-test: failures" type cr
   s" string-test: failures" STR-TEST-EX-FAIL die ;

STR-TEST-REPORT
