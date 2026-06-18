\ string-test.f - focused tests for checked stdlib string helpers.
\ Run: cat lib/string.f lib/string-test.f | bin/hb

64 constant STR-TEST-BUF-LEN
1 constant STR-TEST-EX-FAIL

variable STR-TEST-N
variable STR-TEST-FAIL
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
STR-TEST-BUF 0 STR-TEST-BUF 0 BYTE-COPY

: STR-TEST-REPORT ( -- )
   STR-TEST-FAIL @ 0= if s" string-test: ok" type cr exit then
   STR-TEST-FAIL @ . s" string-test: failures" type cr
   s" string-test: failures" STR-TEST-EX-FAIL die ;

STR-TEST-REPORT
