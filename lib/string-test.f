\ string-test.f - focused tests for checked stdlib string helpers.
\ Run: cat lib/errors.f lib/string.f lib/string-test.f | bin/hb

require lib/errors.f
require lib/string.f
require test/checker-assert.f

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
variable STR-TEST-BYTE-VIEW-CELL
create STR-TEST-BUF STR-TEST-BUF-LEN allot
create STR-TEST-LEFT-WS
   STR-TAB c, STR-LF c, 97 c, 98 c, 99 c,
create STR-TEST-RIGHT-WS
   97 c, 98 c, 99 c, STR-TAB c, STR-LF c,
create STR-TEST-BYTEPLUS
   65 c, 66 c, 67 c,
16 BUFFER: STR-TEST-BUF2
variable STR-TEST-BUF2-LEN

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

: STR-CHECK-REJECTS ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 STR-ASSERT= ;

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
   a u STR>NUMBER? MATCH option
     none OF ok 0= STR-ASSERT ENDOF
     some OF ok STR-ASSERT want STR-ASSERT= ENDOF
   ;MATCH ;

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
   STR-TEST-BYTE-VIEW-CELL BYTE-VIEW
      STR-TEST-BYTE-VIEW-CELL 0 STRUCT-BYTE+ = STR-ASSERT
   STR-TEST-A-CHAR STR-TEST-BYTE-VIEW-CELL BYTE-VIEW c!
   STR-TEST-BYTE-VIEW-CELL BYTE-VIEW c@ STR-TEST-A-CHAR STR-ASSERT=
   STR-TEST-BYTEPLUS 1 BYTE+ c@ STR-TEST-A-CHAR 1+ STR-ASSERT=
   STR-TEST-BYTEPLUS 2 BYTE@ STR-TEST-A-CHAR 2 + STR-ASSERT=
   s" BAD-BYTE-VIEW-LOAD ( ptr a -- n ) BYTE-VIEW @" STR-CHECK-REJECTS
   s" BAD-BYTE-VIEW-STORE ( n ptr a -- ) BYTE-VIEW !" STR-CHECK-REJECTS
   s" BAD-BYTE-VIEW-WIDEN ( ptr a -- ptr cell ) BYTE-VIEW" STR-CHECK-REJECTS
   s" BAD-BYTE-PLUS-RESULT ( ptr u8 n -- n ) BYTE+" STR-CHECK-REJECTS
   s" BAD-BYTE-AT-RESULT ( ptr u8 n -- ptr u8 ) BYTE@" STR-CHECK-REJECTS
   s" BAD-BYTE-COPY-LEN ( ptr u8 ptr u8 off -- ) BYTE-COPY-LEN" STR-CHECK-REJECTS
   s" BAD-SB-APPEND-LEN ( ptr u8 off -- ) SB-APPEND-LEN" STR-CHECK-REJECTS
   SB-RESET ;

: STR-TEST-BUF-OVERFLOW ( -- )
   15 >LEN STR-TEST-BUF2-LEN !
   s" xx" STR-TEST-BUF2 16 STR-TEST-BUF2-LEN BUF-APPEND ;

: STR-TEST-BUF-BAD-C ( -- )
   300 STR-TEST-BUF2 16 STR-TEST-BUF2-LEN BUF-APPEND-C ;

: STR-TEST-BUF-BAD-LEN ( -- )
   -1 STR-TEST-BUF2-LEN !
   s" x" STR-TEST-BUF2 16 STR-TEST-BUF2-LEN BUF-APPEND ;

: STR-TEST-BUFFER ( -- )
   STR-TEST-BUF2-LEN BUF-RESET
   s" hi" STR-TEST-BUF2 16 STR-TEST-BUF2-LEN BUF-APPEND
   33 STR-TEST-BUF2 16 STR-TEST-BUF2-LEN BUF-APPEND-C
   STR-TEST-BUF2 STR-TEST-BUF2-LEN BUF-LEN@ s" hi!" STR-ASSERT$
   [: STR-TEST-BUF-OVERFLOW ;] catch E-STR-CAPACITY STR-ASSERT=
   [: STR-TEST-BUF-BAD-C ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR-TEST-BUF-BAD-LEN ;] catch E-STR-BOUNDS STR-ASSERT=
   s" BAD-BUF-RESET ( ptr n -- ) BUF-RESET" STR-CHECK-REJECTS
   s" BAD-BUF-LEN@ ( ptr n -- n ) BUF-LEN@" STR-CHECK-REJECTS
   s" BAD-BUF-APPEND-LEN ( ptr u8 off ptr u8 len ptr len -- ) BUF-APPEND-LEN" STR-CHECK-REJECTS ;

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

\ switchover wave A: STR-PARSE-POS/NEG and STR>NUMBER? return option<n>
\ (SOME parsed value, else NONE).
\ typed-local-lint: allow-bare-local - q keeps the parser quotation effect from the stack signature.
: STR-PARSE-SOME ( ptr u8 n n [ ptr u8 n -- option<n> ] -- ) {: a:ptr u:n want:n q :}
   a u q execute MATCH option
     none OF 0 0= 0= ENDOF                          \ none -> fail (expected a value)
     some OF want = ENDOF
   ;MATCH STR-ASSERT ;
\ typed-local-lint: allow-bare-local - q keeps the parser quotation effect from the stack signature.
: STR-PARSE-NONE ( ptr u8 n [ ptr u8 n -- option<n> ] -- ) {: a:ptr u:n q :}
   a u q execute MATCH option
     none OF 0 0= ENDOF                             \ none -> pass
     some OF drop 0 0= 0= ENDOF
   ;MATCH STR-ASSERT ;
: STR-TEST-PARSE-OPTION ( -- )
   s" 42" 42 [: STR-PARSE-POS ;] STR-PARSE-SOME
   s" 0" 0 [: STR-PARSE-POS ;] STR-PARSE-SOME
   s" abc" [: STR-PARSE-POS ;] STR-PARSE-NONE
   s" 9223372036854775808" [: STR-PARSE-POS ;] STR-PARSE-NONE
   s" 456" -456 [: STR-PARSE-NEG ;] STR-PARSE-SOME
   s" 9223372036854775808" STR-MIN-I64 [: STR-PARSE-NEG ;] STR-PARSE-SOME
   s" 4x6" [: STR-PARSE-NEG ;] STR-PARSE-NONE
   s" 9223372036854775809" [: STR-PARSE-NEG ;] STR-PARSE-NONE
   s" 77" 77 [: STR>NUMBER? ;] STR-PARSE-SOME
   s" nope" [: STR>NUMBER? ;] STR-PARSE-NONE ;

\ switchover wave A: INDEX-OF / FIND-SUB return option<idx> (SOME index, else NONE).
: STR-INDEX-OF>N ( ptr u8 n n -- n )                \ test re-wrap: found index, -1 when absent
   INDEX-OF MATCH option
     none OF -1 ENDOF
     some OF IDX>N ENDOF
   ;MATCH ;

: STR-FIND-SUB>N ( ptr u8 n ptr u8 n -- n )         \ test re-wrap: found offset, -1 when absent
   FIND-SUB MATCH option
     none OF -1 ENDOF
     some OF IDX>N ENDOF
   ;MATCH ;

: STR-TEST-FIND-SUB-OPTION ( -- )                   \ direct both-branch option assertions
   s" abcdef" s" bcd" FIND-SUB MATCH option
     none OF STR-FALSE ENDOF
     some OF IDX>N 1 = ENDOF
   ;MATCH STR-ASSERT
   s" abcdef" s" z" FIND-SUB MATCH option
     none OF STR-TRUE ENDOF
     some OF drop STR-FALSE ENDOF
   ;MATCH STR-ASSERT ;

: STR-TEST-INDEX-OF-OPTION ( -- )                   \ direct both-branch option assertions
   s" abcdef" 100 INDEX-OF MATCH option
     none OF STR-FALSE ENDOF
     some OF IDX>N 3 = ENDOF
   ;MATCH STR-ASSERT
   s" abcdef" 120 INDEX-OF MATCH option
     none OF STR-TRUE ENDOF
     some OF drop STR-FALSE ENDOF
   ;MATCH STR-ASSERT ;

\ ---- B5.5 typed STR surface over CAD-NUM roles --------------------------------
\ White-box role readers: reopen the unsealed CAD-NUM package to project a role's
\ raw cell for scalar assertions, the pattern lib/vector-test.f and
\ lib/memory-test.f use. (STR's own BYTE-LEN>N/BYTE-OFF>N stay STR-private.)
package CAD-NUM
public
: STR-T-BL>RAW ( CAD-NUM:byte-len -- n ) BYTE-LEN>N ;
: STR-T-BO>RAW ( CAD-NUM:byte-off -- n ) BYTE-OFF>N ;
: STR-T-IC>RAW ( CAD-NUM:item-count -- n ) ITEM-COUNT>N ;
: STR-T-IX>RAW ( CAD-NUM:index -- n ) INDEX>N ;
;package

variable STR-T-SPLIT-A
variable STR-T-SPLIT-U
variable STR-T-SPLIT-NEXT
variable STR-T-SPLIT-OK

: STR-T-FIND ( ptr u8 n ptr u8 n -- n ) {: a:ptr u:n b:ptr v:n :}   \ typed find; -1 when NONE
   a u STR:LENGTH b v STR:LENGTH STR:FIND-SUB
   MATCH option
      none OF -1 ENDOF
      some OF CAD-NUM:STR-T-IX>RAW ENDOF
   ;MATCH ;
: STR-T-INDEX ( ptr u8 n n -- n ) {: a:ptr u:n c:n :}              \ typed byte index; -1 when NONE
   a u STR:LENGTH c STR:INDEX-OF
   MATCH option
      none OF -1 ENDOF
      some OF CAD-NUM:STR-T-IX>RAW ENDOF
   ;MATCH ;

: STR-T-SPLIT-CHECK ( ptr u8 n n n ptr u8 n n bool -- )
   {: a:ptr u:n sep:n start:n exp:ptr exp-u:n exp-next:n exp-ok:bool :}
   a u STR:LENGTH sep start STR:OFFSET STR:SPLIT-NEXT   \ ( sub-ptr byte-len byte-off bool )
   STR-T-SPLIT-OK !
   CAD-NUM:STR-T-BO>RAW STR-T-SPLIT-NEXT !
   CAD-NUM:STR-T-BL>RAW STR-T-SPLIT-U !
   STR-T-SPLIT-A !
   STR-T-SPLIT-A @ STR-T-SPLIT-U @ exp exp-u STR-ASSERT$
   STR-T-SPLIT-NEXT @ exp-next STR-ASSERT=
   exp-ok if
      STR-T-SPLIT-OK @ 0= 0= STR-ASSERT
   else
      STR-T-SPLIT-OK @ 0= STR-ASSERT
   then ;

: STR-TEST-TYPED-SCALAR ( -- )                          \ zero + positive roles round-trip
   0 STR:LENGTH CAD-NUM:STR-T-BL>RAW 0 STR-ASSERT=
   5 STR:LENGTH CAD-NUM:STR-T-BL>RAW 5 STR-ASSERT=
   0 STR:OFFSET CAD-NUM:STR-T-BO>RAW 0 STR-ASSERT=
   7 STR:OFFSET CAD-NUM:STR-T-BO>RAW 7 STR-ASSERT=
   0 STR:COUNT CAD-NUM:STR-T-IC>RAW 0 STR-ASSERT=
   3 STR:COUNT CAD-NUM:STR-T-IC>RAW 3 STR-ASSERT= ;

: STR-TEST-TYPED-FIND ( -- )                            \ first/last/not-found/empty-needle
   s" abcdef" s" bcd" STR-T-FIND 1 STR-ASSERT=
   s" abcabc" s" abc" STR-T-FIND 0 STR-ASSERT=
   s" abcdef" s" def" STR-T-FIND 3 STR-ASSERT=
   s" abcdef" s" z" STR-T-FIND -1 STR-ASSERT=
   s" abcdef" s" " STR-T-FIND 0 STR-ASSERT=
   s" " s" " STR-T-FIND 0 STR-ASSERT=
   s" abcdef" 100 STR-T-INDEX 3 STR-ASSERT=
   s" abcdef" 97 STR-T-INDEX 0 STR-ASSERT=
   s" abcdef" 102 STR-T-INDEX 5 STR-ASSERT=
   s" abcdef" 120 STR-T-INDEX -1 STR-ASSERT= ;

: STR-TEST-TYPED-SPLIT ( -- )                           \ byte-identical to the raw SPLIT-NEXT
   s" a,b,c" STR-TEST-COMMA 0 s" a" 2 STR-TRUE STR-T-SPLIT-CHECK
   s" a,b,c" STR-TEST-COMMA 2 s" b" 4 STR-TRUE STR-T-SPLIT-CHECK
   s" a,b,c" STR-TEST-COMMA 4 s" c" 6 STR-TRUE STR-T-SPLIT-CHECK
   s" a,b,c" STR-TEST-COMMA 6 s" " 6 STR-FALSE STR-T-SPLIT-CHECK
   s" " STR-TEST-COMMA 0 s" " 1 STR-TRUE STR-T-SPLIT-CHECK
   s" " STR-TEST-COMMA 1 s" " 1 STR-FALSE STR-T-SPLIT-CHECK ;

: STR-TEST-TYPED-BUFFER ( -- )                          \ append + read length as a byte-len role
   STR-TEST-BUF2-LEN STR:BUF-RESET
   s" hi" STR:LENGTH STR-TEST-BUF2 16 STR:LENGTH STR-TEST-BUF2-LEN STR:BUF-APPEND
   STR-TEST-BUF2 STR-TEST-BUF2-LEN STR:BUF-LEN@ CAD-NUM:STR-T-BL>RAW s" hi" STR-ASSERT$ ;

: STR-TEST-TYPED-BUF-OVERFLOW ( -- )
   15 >LEN STR-TEST-BUF2-LEN !
   s" xx" STR:LENGTH STR-TEST-BUF2 16 STR:LENGTH STR-TEST-BUF2-LEN STR:BUF-APPEND ;

: STR-TEST-TYPED-SPLIT-OVERFLOW ( -- )                  \ one-past-end offset advance overflows
   s" x" drop STR-MAX-I64 STR:LENGTH STR-TEST-COMMA 0 STR:OFFSET STR:SPLIT-NEXT 2drop 2drop ;

: STR-TEST-TYPED-LEN-NEG ( -- )   -1 STR:LENGTH drop ;
: STR-TEST-TYPED-OFF-NEG ( -- )   -1 STR:OFFSET drop ;
: STR-TEST-TYPED-COUNT-NEG ( -- ) -1 STR:COUNT drop ;

: STR-TEST-TYPED ( -- )
   STR-TEST-TYPED-SCALAR
   STR-TEST-TYPED-FIND
   STR-TEST-TYPED-SPLIT
   STR-TEST-TYPED-BUFFER
   [: STR-TEST-TYPED-BUF-OVERFLOW ;] catch E-STR-CAPACITY STR-ASSERT=
   [: STR-TEST-TYPED-SPLIT-OVERFLOW ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR-TEST-TYPED-LEN-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR-TEST-TYPED-OFF-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   [: STR-TEST-TYPED-COUNT-NEG ;] catch E-STR-BOUNDS STR-ASSERT=
   \ ---- static role-flow: resolving positives (-1) --------------------------------
   s" SOK-LEN ( n -- CAD-NUM:byte-len ) STR:LENGTH" CHECK-QUIET-CANDIDATE! -1 STR-ASSERT=
   s" SOK-FIND ( ptr u8 CAD-NUM:byte-len ptr u8 CAD-NUM:byte-len -- option<CAD-NUM:index> ) STR:FIND-SUB" CHECK-QUIET-CANDIDATE! -1 STR-ASSERT=
   s" SOK-INDEX ( ptr u8 CAD-NUM:byte-len n -- option<CAD-NUM:index> ) STR:INDEX-OF" CHECK-QUIET-CANDIDATE! -1 STR-ASSERT=
   s" SOK-SPLIT ( ptr u8 CAD-NUM:byte-len n CAD-NUM:byte-off -- ptr u8 CAD-NUM:byte-len CAD-NUM:byte-off bool ) STR:SPLIT-NEXT" CHECK-QUIET-CANDIDATE! -1 STR-ASSERT=
   \ ---- static role-flow: length/index/offset swaps + raw-n reject (0) ------------
   s" SBAD-OFF-FOR-LEN ( ptr u8 CAD-NUM:byte-off ptr u8 CAD-NUM:byte-len -- option<CAD-NUM:index> ) STR:FIND-SUB" STR-CHECK-REJECTS
   s" SBAD-LEN-FOR-OFF ( ptr u8 CAD-NUM:byte-len n CAD-NUM:byte-len -- ptr u8 CAD-NUM:byte-len CAD-NUM:byte-off bool ) STR:SPLIT-NEXT" STR-CHECK-REJECTS
   s" SBAD-RAW-LEN ( ptr u8 n ptr u8 CAD-NUM:byte-len -- option<CAD-NUM:index> ) STR:FIND-SUB" STR-CHECK-REJECTS
   s" SBAD-RAW-OFF ( ptr u8 CAD-NUM:byte-len n n -- ptr u8 CAD-NUM:byte-len CAD-NUM:byte-off bool ) STR:SPLIT-NEXT" STR-CHECK-REJECTS
   s" SBAD-OFF-FOR-INDEX ( ptr u8 CAD-NUM:byte-off n -- option<CAD-NUM:index> ) STR:INDEX-OF" STR-CHECK-REJECTS
   s" SBAD-FIND-OUT ( ptr u8 CAD-NUM:byte-len ptr u8 CAD-NUM:byte-len -- option<CAD-NUM:byte-off> ) STR:FIND-SUB" STR-CHECK-REJECTS ;

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
s" abcdef" s" bcd" STR-FIND-SUB>N 1 STR-ASSERT=
s" abcabc" s" abc" STR-FIND-SUB>N 0 STR-ASSERT=
s" abcdef" s" z" STR-FIND-SUB>N -1 STR-ASSERT=
s" abcdef" s" " STR-FIND-SUB>N 0 STR-ASSERT=
s" abcdef" s" cde" CONTAINS? STR-ASSERT
s" abcdef" s" " CONTAINS? STR-ASSERT
s" abcdef" s" xyz" CONTAINS? 0= STR-ASSERT
s" abcdef" 100 STR-INDEX-OF>N 3 STR-ASSERT=
s" abcdef" 97 STR-INDEX-OF>N 0 STR-ASSERT=
s" abcdef" 120 STR-INDEX-OF>N -1 STR-ASSERT=
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
STR-TEST-BUFFER
STR-TEST-SPLIT
STR-TEST-PARSE
STR-TEST-PARSE-OPTION
STR-TEST-INDEX-OF-OPTION
STR-TEST-FIND-SUB-OPTION
STR-TEST-TYPED

: STR-TEST-REPORT ( -- )
   STR-TEST-FAIL @ 0= if s" string-test: ok" type cr exit then
   STR-TEST-FAIL @ . s" string-test: failures" type cr
   s" string-test: failures" STR-TEST-EX-FAIL die ;

STR-TEST-REPORT
