\ json-test.f - focused tests for tools/json.f.
\ Run: cat tools/json.f tools/json-test.f | bin/hb

0 set-check

variable TEST-N
: ASSERT  \ ( f -- )
   0= IF s" json-test failed at assertion " type TEST-N @ . cr 1 die THEN
   TEST-N @ 1+ TEST-N ! ;

: ASSERT=  \ ( got want -- )
   = ASSERT ;

: ASSERT$  \ ( a u b v -- )
   JSON-STR= ASSERT ;

$1000 constant T-CAP
$9000 constant T-LARGE-BODY
create TBUF T-CAP allot
variable TLEN
variable T-LARGE-A
variable T-LARGE-CAP
variable T-LARGE-LEN

: T-CLEAR  \ ( -- )
   0 TLEN ! ;

: T+C  \ ( c -- )
   TLEN @ 1+ T-CAP > IF s" json-test: fixture buffer full" 1 die THEN
   TBUF TLEN @ + c!
   TLEN @ 1+ TLEN ! ;

: T+  \ ( a u -- )
   {: a u :}
   TLEN @ u + T-CAP > IF s" json-test: fixture buffer full" 1 die THEN
   a u TBUF TLEN @ + JSON-COPY
   TLEN @ u + TLEN ! ;

: T$  \ ( -- a u )
   TBUF TLEN @ ;

: T-LARGE-RESET  \ ( -- )
   T-LARGE-BODY 32 + JSON-ALLOC-STR-PTR T-LARGE-A !
   T-LARGE-BODY 32 + T-LARGE-CAP !
   0 T-LARGE-LEN ! ;

: T-LARGE+C  \ ( c -- )
   T-LARGE-LEN @ 1+ T-LARGE-CAP @ > IF s" json-test: large buffer full" 1 die THEN
   T-LARGE-A @ T-LARGE-LEN @ + c!
   T-LARGE-LEN @ 1+ T-LARGE-LEN ! ;

: T-LARGE+  \ ( a u -- )
   {: a u :}
   T-LARGE-LEN @ u + T-LARGE-CAP @ > IF s" json-test: large buffer full" 1 die THEN
   a u T-LARGE-A @ T-LARGE-LEN @ + JSON-COPY
   T-LARGE-LEN @ u + T-LARGE-LEN ! ;

: T-LARGE$  \ ( -- a u )
   T-LARGE-A @ T-LARGE-LEN @ ;

variable TRY-A
variable TRY-U
: TRY-PARSE  \ ( -- node )
   TRY-A @ TRY-U @ JSON-PARSE ;

: PARSE-CODE  \ ( a u -- code )
   TRY-U ! TRY-A !
   [: TRY-PARSE ;] catch
   dup 0= IF drop drop 0 exit THEN ;

variable TA
variable TU
variable ROOT
variable NODE

: ASSERT-BYTE  \ ( a idx c -- )
   {: a idx c :}
   a idx + c@ c ASSERT= ;

: ADD-DQ$  \ ( a u -- )
   J-DQ T+C T+ J-DQ T+C ;

: ADD-LARGE-DQ$  \ ( a u -- )
   J-DQ T-LARGE+C T-LARGE+ J-DQ T-LARGE+C ;

: BUILD-NESTED  \ ( -- a u )
   T-CLEAR
   J-LBRACE T+C
   s" schema_version" ADD-DQ$ J-COLON T+C s" 1" T+
   J-COMMA T+C s" word" ADD-DQ$ J-COLON T+C s" SQ" ADD-DQ$
   J-COMMA T+C s" ok" ADD-DQ$ J-COLON T+C s" true" T+
   J-COMMA T+C s" nothing" ADD-DQ$ J-COLON T+C s" null" T+
   J-COMMA T+C s" nested" ADD-DQ$ J-COLON T+C
   J-LBRACE T+C
   s" arr" ADD-DQ$ J-COLON T+C
   J-LBRACK T+C
   s" -12" T+
   J-COMMA T+C s" 3.5e+2" T+
   J-COMMA T+C J-DQ T+C
   s" line\nquote" T+
   J-BACKSLASH T+C J-DQ T+C
   s" slash" T+
   J-BACKSLASH T+C J-BACKSLASH T+C
   J-DQ T+C
   J-COMMA T+C J-LBRACE T+C s" unicode" ADD-DQ$ J-COLON T+C J-DQ T+C s" \u03A9" T+ J-DQ T+C J-RBRACE T+C
   J-RBRACK T+C
   J-RBRACE T+C
   J-RBRACE T+C
   T$ ;

: TEST-NESTED  \ ( -- )
   BUILD-NESTED JSON-PARSE ROOT !
   ROOT @ JSON-KIND J-OBJ ASSERT=
   ROOT @ s" schema_version" JSON-GET dup JSON-KIND J-NUM ASSERT=
   JSON-NUMBER$ s" 1" ASSERT$
   ROOT @ s" word" JSON-GET JSON-STRING$ s" SQ" ASSERT$
   ROOT @ s" ok" JSON-GET JSON-BOOL@ ASSERT
   ROOT @ s" nothing" JSON-GET JSON-NULL? ASSERT
   ROOT @ s" missing" JSON-GET -1 ASSERT=
   ROOT @ s" nested" JSON-GET s" arr" JSON-GET NODE !
   NODE @ JSON-KIND J-ARR ASSERT=
   NODE @ JSON-COUNT 4 ASSERT=
   NODE @ 0 JSON-ARR@ JSON-NUMBER$ s" -12" ASSERT$
   NODE @ 1 JSON-ARR@ JSON-NUMBER$ s" 3.5e+2" ASSERT$
   NODE @ 2 JSON-ARR@ JSON-STRING$ TU ! TA !
   TU @ 17 ASSERT=
   TA @ 4 J-LF ASSERT-BYTE
   TA @ 10 J-DQ ASSERT-BYTE
   TA @ 16 J-BACKSLASH ASSERT-BYTE
   NODE @ 3 JSON-ARR@ s" unicode" JSON-GET JSON-STRING$ TU ! TA !
   TU @ 2 ASSERT=
   TA @ 0 $CE ASSERT-BYTE
   TA @ 1 $A9 ASSERT-BYTE ;

: TEST-WRITE-ROUNDTRIP  \ ( -- )
   BUILD-NESTED JSON-PARSE ROOT !
   ROOT @ JSON-WRITE JSON-PARSE ROOT !
   ROOT @ s" nested" JSON-GET s" arr" JSON-GET 2 JSON-ARR@ JSON-STRING$ TU ! TA !
   TU @ 17 ASSERT=
   TA @ 10 J-DQ ASSERT-BYTE
   JSONW-RESET
   JSONW-OBJECT-START
   s" msg" JSONW-KEY
   s" a" JSONW-STRING
   JSONW-COMMA
   s" n" JSONW-KEY
   s" -7.25e-1" JSONW-RAW
   JSONW-OBJECT-END
   JSON-OUT-BUF JSON-OUT-LEN @ JSON-PARSE ROOT !
   ROOT @ s" msg" JSON-GET JSON-STRING$ s" a" ASSERT$
   ROOT @ s" n" JSON-GET JSON-NUMBER$ s" -7.25e-1" ASSERT$ ;

: BUILD-LARGE-STRING  \ ( -- a u )
   T-LARGE-RESET
   J-LBRACE T-LARGE+C
   s" big" ADD-LARGE-DQ$ J-COLON T-LARGE+C J-DQ T-LARGE+C
   0 begin dup T-LARGE-BODY < while
      97 T-LARGE+C
      1+
   repeat drop
   J-DQ T-LARGE+C J-RBRACE T-LARGE+C
   T-LARGE$ ;

: TEST-LARGE-STRING  \ ( -- )
   BUILD-LARGE-STRING JSON-PARSE ROOT !
   ROOT @ s" big" JSON-GET JSON-STRING$ TU ! TA !
   TU @ T-LARGE-BODY ASSERT=
   TA @ 0 97 ASSERT-BYTE
   TA @ T-LARGE-BODY 1- 97 ASSERT-BYTE
   JSON-STR-CAP JSON-STR-BOOT-CAP > ASSERT ;

: BUILD-LEADING-ZERO  \ ( -- a u )
   T-CLEAR
   J-LBRACE T+C s" n" ADD-DQ$ J-COLON T+C s" 01" T+ J-RBRACE T+C
   T$ ;

: BUILD-TRAILING-COMMA  \ ( -- a u )
   T-CLEAR
   J-LBRACK T+C s" 1" T+ J-COMMA T+C J-RBRACK T+C
   T$ ;

: BUILD-BAD-ESCAPE  \ ( -- a u )
   T-CLEAR
   J-DQ T+C s" \q" T+ J-DQ T+C
   T$ ;

: TEST-STRICT-ERRORS  \ ( -- )
   BUILD-LEADING-ZERO PARSE-CODE E-JSON-SYNTAX ASSERT=
   JSON-ERROR$ s" json: leading zero" ASSERT$
   BUILD-TRAILING-COMMA PARSE-CODE E-JSON-SYNTAX ASSERT=
   BUILD-BAD-ESCAPE PARSE-CODE E-JSON-SYNTAX ASSERT=
   s" prose not json" PARSE-CODE E-JSON-SYNTAX ASSERT= ;

: ADD-LINE  \ ( -- )
   J-LF T+C ;

: BUILD-JSONL  \ ( -- a u )
   T-CLEAR
   s" prose before json" T+ ADD-LINE
   J-LBRACE T+C s" a" ADD-DQ$ J-COLON T+C s" 1" T+ J-RBRACE T+C ADD-LINE
   J-LBRACE T+C s" bad" ADD-DQ$ J-COLON T+C J-RBRACK T+C J-RBRACE T+C ADD-LINE
   J-LBRACK T+C s" 1" T+ J-RBRACK T+C ADD-LINE
   J-SP T+C J-LBRACE T+C s" b" ADD-DQ$ J-COLON T+C s" true" T+ J-RBRACE T+C J-SP T+C ADD-LINE
   T$ ;

: TEST-JSONL  \ ( -- )
   BUILD-JSONL JSONL-START-SKIP
   JSONL-NEXT-OBJECT ROOT !
   ROOT @ -1 <> ASSERT
   ROOT @ s" a" JSON-GET JSON-NUMBER$ s" 1" ASSERT$
   JSONL-NEXT-OBJECT ROOT !
   ROOT @ -1 <> ASSERT
   ROOT @ s" b" JSON-GET JSON-BOOL@ ASSERT
   JSONL-NEXT-OBJECT -1 ASSERT=
   JSONL-SKIPPED 3 ASSERT= ;

: TEST-JSONL-BLANK-ROWS  \ ( -- )
   T-CLEAR
   ADD-LINE
   J-SP T+C J-TAB T+C ADD-LINE
   J-LBRACE T+C s" a" ADD-DQ$ J-COLON T+C s" 1" T+ J-RBRACE T+C ADD-LINE
   T$ JSONL-START-STRICT
   JSONL-NEXT-OBJECT ROOT !
   ROOT @ s" a" JSON-GET JSON-NUMBER$ s" 1" ASSERT$
   JSONL-NEXT-OBJECT -1 ASSERT=
   JSONL-SKIPPED 2 ASSERT= ;

: JSONL-STRICT-BAD  \ ( -- node )
   BUILD-JSONL JSONL-START-STRICT
   JSONL-NEXT-OBJECT ;

: JSONL-SKIP-BAD  \ ( -- node )
   BUILD-JSONL JSONL-START-SKIP
   JSONL-NEXT-OBJECT ;

: TEST-JSONL-MODES  \ ( -- )
   [: JSONL-STRICT-BAD ;] catch E-JSON-SYNTAX ASSERT=
   [: JSONL-SKIP-BAD ;] catch 0 ASSERT=
   drop
   JSONL-SKIPPED 1 ASSERT=
   JSONL-NEXT-OBJECT ROOT !
   ROOT @ s" b" JSON-GET JSON-BOOL@ ASSERT
   JSONL-NEXT-OBJECT -1 ASSERT=
   JSONL-SKIPPED 3 ASSERT= ;

: JSON-TEST  \ ( -- )
   1 TEST-N !
   TEST-NESTED
   TEST-WRITE-ROUNDTRIP
   TEST-LARGE-STRING
   TEST-STRICT-ERRORS
   TEST-JSONL
   TEST-JSONL-BLANK-ROWS
   TEST-JSONL-MODES
   s" json-test: ok (" type TEST-N @ 1- . s"  assertions)" type cr ;

JSON-TEST
