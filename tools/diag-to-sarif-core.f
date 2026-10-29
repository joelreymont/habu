\ diag-to-sarif-core.f - convert Habu diagnostic JSONL to SARIF 2.1.0.

require src/lib/errors.f
require src/lib/fs.f
require src/lib/memory.f
require src/lib/jsonl.f
require src/lib/json-write.f
require src/lib/json-copy.f
require src/lib/string.f

65 constant SARIF-E-DATA
74 constant SARIF-E-IO

$40000 constant SARIF-IN-CAP
$100 constant SARIF-MAX-RULES
$2000 constant SARIF-RULE-BUF-CAP
$1000 constant SARIF-TEXT-CAP
$8000 constant SARIF-QUOTE-CAP

10 constant SARIF-LF
44 constant SARIF-COMMA
45 constant SARIF-MINUS
58 constant SARIF-COLON
91 constant SARIF-LBRACK
93 constant SARIF-RBRACK
123 constant SARIF-LBRACE
125 constant SARIF-RBRACE

create SARIF-IN-BUF SARIF-IN-CAP allot
create SARIF-RULE-BUF SARIF-RULE-BUF-CAP allot
create SARIF-RULE-OFF SARIF-MAX-RULES cells allot
create SARIF-RULE-LEN SARIF-MAX-RULES cells allot
create SARIF-TEXT-BUF SARIF-TEXT-CAP allot
create SARIF-ONE 1 allot
create SARIF-QUOTE SARIF-QUOTE-CAP allot
create SARIF-DECODE SARIF-IN-CAP allot
TYPED-VARIABLE SARIF-READER JR:cursor
TYPED-VARIABLE SARIF-W JSON-WRITE:writer

variable SARIF-IN-LEN
variable SARIF-RULE-N
variable SARIF-RULE-END
variable SARIF-LINE-I
variable SARIF-LINE-START
TYPED-VARIABLE SARIF-LINE-A ptr u8
variable SARIF-LINE-U
variable SARIF-FIELD?
variable SARIF-RD
variable SARIF-FD
variable SARIF-NV
variable SARIF-NP
variable SARIF-NEG?
variable SARIF-ROW-OBJECT
variable SARIF-RESULT-N

: SARIF-TRUE ( -- bool )
   0 0= ;

: SARIF-FALSE ( -- bool )
   SARIF-TRUE 0= ;

: SARIF-LINE-A@ ( -- ptr u8 )
   SARIF-LINE-A @ ;

: SARIF-LINE-A! ( ptr u8 -- )
   SARIF-LINE-A ! ;

: SARIF-FAIL ( ptr u8 n n -- )
   die ;

: SARIF-WRITE ( n ptr u8 n -- )
   {: fd:n a:ptr u:n :}
   u 0= if exit then
   fd a u write u <> if s" diag-to-sarif: write failed" SARIF-E-IO SARIF-FAIL then ;

: SARIF-OUT ( ptr u8 n -- )
   1 -rot SARIF-WRITE ;

: SARIF-C ( n -- )
   SARIF-ONE c!
   1 SARIF-ONE 1 SARIF-WRITE ;

: SARIF-STRING ( ptr u8 n -- )
   {: a:ptr u:n :}
   SARIF-W SARIF-QUOTE SARIF-QUOTE-CAP JSON-WRITE:OPEN
   a u JSON-WRITE:STRING JSON-WRITE:$ SARIF-OUT ;

: SARIF-KEY ( ptr u8 n -- )
   SARIF-STRING
   SARIF-COLON SARIF-C ;

: SARIF-I64 ( n -- )
   {: value:n :}
   SARIF-W SARIF-QUOTE SARIF-QUOTE-CAP JSON-WRITE:OPEN
   value JSON-WRITE:INT JSON-WRITE:$ SARIF-OUT ;

: SARIF-DIGIT ( n -- n )
   dup 48 < if drop -1 exit then
   dup 57 > if drop -1 exit then
   48 - ;

: SARIF>S ( ptr u8 n -- n )
   {: a:ptr u:n :}
   u 0= if s" diag-to-sarif: expected integer" SARIF-E-DATA SARIF-FAIL then
   0 SARIF-NV !
   0 SARIF-NP !
   0 SARIF-NEG? !
   a c@ SARIF-MINUS = if
      -1 SARIF-NEG? !
      1 SARIF-NP !
      u 1 = if s" diag-to-sarif: expected integer" SARIF-E-DATA SARIF-FAIL then
   then
   begin SARIF-NP @ u < while
      a SARIF-NP @ + c@ SARIF-DIGIT dup 0< if drop s" diag-to-sarif: expected integer" SARIF-E-DATA SARIF-FAIL then
      SARIF-NV @ 10 * + SARIF-NV !
      SARIF-NP @ 1+ SARIF-NP !
   repeat
   SARIF-NEG? @ if SARIF-NV @ negate else SARIF-NV @ then ;

: SARIF-COPY-TEXT ( ptr u8 n -- ptr u8 n )
   {: a:ptr u:n :}
   u SARIF-TEXT-CAP > if s" diag-to-sarif: text too large" SARIF-E-DATA SARIF-FAIL then
   a SARIF-TEXT-BUF u BYTE-COPY
   SARIF-TEXT-BUF u ;

: SARIF-GET ( ptr u8 n -- JR:reader bool )
   {: key:ptr ku:n :}
   SARIF-READER SARIF-LINE-A@ SARIF-LINE-U @ JR:INIT
   JR:NEXT drop
   key ku JR:FIND-KEY ;

: SARIF-NONZERO-NUM? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   0 begin dup u < while
      dup a + c@ dup 49 >= swap 57 <= and if drop SARIF-TRUE exit then
      1+
   repeat drop SARIF-FALSE ;

: SARIF-NODE-TEXT? ( JR:reader -- ptr u8 n bool )
   JR:TOKEN {: kind:n :}
   kind JR:T-STR = if
      SARIF-DECODE SARIF-IN-CAP JR:STR {: u:n :} JR:CLOSE
      SARIF-DECODE u u 0 > exit
   then
   kind JR:T-INT = kind JR:T-FLOAT = or if
      JR:SPAN$ {: a:ptr u:n :} JR:CLOSE
      a u a u SARIF-NONZERO-NUM? exit
   then
   kind JR:T-TRUE = if JR:CLOSE s" True" SARIF-TRUE exit then
   kind JR:T-FALSE = if JR:CLOSE s" False" SARIF-FALSE exit then
   kind JR:T-NULL = if JR:CLOSE s" None" SARIF-FALSE exit then
   kind JR:T-OBJ = kind JR:T-ARR = or if
      SARIF-W SARIF-QUOTE SARIF-QUOTE-CAP JSON-WRITE:OPEN
      SARIF-DECODE SARIF-IN-CAP JSON-COPY:VALUE
      swap JR:CLOSE
      JSON-WRITE:$ SARIF-COPY-TEXT {: a:ptr u:n :}
      a u u 2 > exit
   then
   JR:CLOSE E-JR-STATE throw ;

: SARIF-NODE-TEXT$ ( JR:reader -- ptr u8 n )
   SARIF-NODE-TEXT? drop ;

: SARIF-TRUTHY-GET ( ptr u8 n -- ptr u8 n bool )
   SARIF-GET 0= if JR:CLOSE SARIF-DECODE 0 SARIF-FALSE exit then
   SARIF-NODE-TEXT? ;

: SARIF-MESSAGE$ ( -- ptr u8 n )
   s" suggestion" SARIF-TRUTHY-GET if exit then 2drop
   s" reason" SARIF-TRUTHY-GET if exit then 2drop
   s" verdict" SARIF-TRUTHY-GET if exit then 2drop
   s" code" SARIF-TRUTHY-GET if exit then 2drop
   s" Habu diagnostic" ;

: SARIF-CODE$ ( -- ptr u8 n )
   s" code" SARIF-GET 0= if JR:CLOSE s" HABU-DIAGNOSTIC" exit then
   SARIF-NODE-TEXT$ ;

: SARIF-FILE$ ( -- ptr u8 n )
   s" file" SARIF-GET 0= if JR:CLOSE s" <input>" exit then
   SARIF-NODE-TEXT$ ;

: SARIF-NODE>I64 ( JR:reader -- n )
   JR:TOKEN {: kind:n :}
   kind JR:T-INT = kind JR:T-FLOAT = or if
      JR:SPAN$ {: a:ptr u:n :} JR:CLOSE a u SARIF>S exit
   then
   kind JR:T-STR = if
      SARIF-DECODE SARIF-IN-CAP JR:STR {: u:n :} JR:CLOSE
      SARIF-DECODE u SARIF>S exit
   then
   JR:CLOSE s" diag-to-sarif: expected integer" SARIF-E-DATA SARIF-FAIL ;

: SARIF-EMIT-NODE-OR-NULL ( JR:reader bool -- )
   0= if JR:CLOSE s" null" SARIF-OUT exit then
   SARIF-W SARIF-QUOTE SARIF-QUOTE-CAP JSON-WRITE:OPEN
   SARIF-DECODE SARIF-IN-CAP JSON-COPY:VALUE
   swap JR:CLOSE JSON-WRITE:$ SARIF-OUT ;

: SARIF-EMIT-GET ( ptr u8 n -- )
   SARIF-GET SARIF-EMIT-NODE-OR-NULL ;

: SARIF-FIELDS-START ( -- )
   0 SARIF-FIELD? ! ;

: SARIF-NEXT-FIELD ( -- )
   SARIF-FIELD? @ if SARIF-COMMA SARIF-C then
   -1 SARIF-FIELD? ! ;

: SARIF-REGION-FIELD ( ptr u8 n ptr u8 n -- )
   {: src-a:ptr src-u:n out-a:ptr out-u:n :}
   src-a src-u SARIF-GET 0= if JR:CLOSE exit then
   SARIF-NEXT-FIELD
   out-a out-u SARIF-KEY
   SARIF-NODE>I64 SARIF-I64 ;

: SARIF-REGION-LEN ( -- )
   s" byte_start" SARIF-GET 0= if JR:CLOSE exit then
   SARIF-NODE>I64 {: start:n :}
   s" byte_end" SARIF-GET 0= if JR:CLOSE exit then
   SARIF-NODE>I64 start -
   dup 0< if drop 0 then
   SARIF-NEXT-FIELD
   s" byteLength" SARIF-KEY
   SARIF-I64 ;

: SARIF-EMIT-REGION ( -- )
   SARIF-LBRACE SARIF-C
   SARIF-FIELDS-START
   s" line" s" startLine" SARIF-REGION-FIELD
   s" column" s" startColumn" SARIF-REGION-FIELD
   s" byte_start" s" byteOffset" SARIF-REGION-FIELD
   SARIF-REGION-LEN
   SARIF-RBRACE SARIF-C ;

: SARIF-EMIT-PROPERTIES ( -- )
   SARIF-LBRACE SARIF-C
   s" schema_version" SARIF-KEY s" schema_version" SARIF-EMIT-GET
   SARIF-COMMA SARIF-C
   s" word" SARIF-KEY s" word" SARIF-EMIT-GET
   SARIF-COMMA SARIF-C
   s" token" SARIF-KEY s" token" SARIF-EMIT-GET
   SARIF-COMMA SARIF-C
   s" verdict" SARIF-KEY s" verdict" SARIF-EMIT-GET
   SARIF-RBRACE SARIF-C ;

: SARIF-EMIT-MESSAGE ( -- )
   SARIF-LBRACE SARIF-C
   s" text" SARIF-KEY SARIF-MESSAGE$ SARIF-STRING
   SARIF-RBRACE SARIF-C ;

: SARIF-EMIT-ARTIFACT ( -- )
   SARIF-LBRACE SARIF-C
   s" uri" SARIF-KEY SARIF-FILE$ SARIF-STRING
   SARIF-RBRACE SARIF-C ;

: SARIF-EMIT-PHYSICAL ( -- )
   SARIF-LBRACE SARIF-C
   s" artifactLocation" SARIF-KEY SARIF-EMIT-ARTIFACT
   SARIF-COMMA SARIF-C
   s" region" SARIF-KEY SARIF-EMIT-REGION
   SARIF-RBRACE SARIF-C ;

: SARIF-EMIT-LOCATION ( -- )
   SARIF-LBRACE SARIF-C
   s" physicalLocation" SARIF-KEY SARIF-EMIT-PHYSICAL
   SARIF-RBRACE SARIF-C ;

: SARIF-EMIT-LOCATIONS ( -- )
   SARIF-LBRACK SARIF-C
   SARIF-EMIT-LOCATION
   SARIF-RBRACK SARIF-C ;

: SARIF-EMIT-RESULT ( -- )
   SARIF-LBRACE SARIF-C
   s" ruleId" SARIF-KEY SARIF-CODE$ SARIF-STRING
   SARIF-COMMA SARIF-C
   s" level" SARIF-KEY s" error" SARIF-STRING
   SARIF-COMMA SARIF-C
   s" message" SARIF-KEY SARIF-EMIT-MESSAGE
   SARIF-COMMA SARIF-C
   s" properties" SARIF-KEY SARIF-EMIT-PROPERTIES
   SARIF-COMMA SARIF-C
   s" locations" SARIF-KEY SARIF-EMIT-LOCATIONS
   SARIF-RBRACE SARIF-C ;

: SARIF-RULE$ ( n -- ptr u8 n )
   {: idx:n :}
   SARIF-RULE-BUF idx cells SARIF-RULE-OFF + @ +
   idx cells SARIF-RULE-LEN + @ ;

: SARIF-RULE-FIND ( ptr u8 n -- n )
   {: a:ptr u:n :}
   0 begin dup SARIF-RULE-N @ < while
      dup SARIF-RULE$ a u STR= if exit then
      1+
   repeat drop -1 ;

: SARIF-RULE-ADD ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u SARIF-RULE-FIND 0 >= if exit then
   SARIF-RULE-N @ SARIF-MAX-RULES >= if s" diag-to-sarif: too many rules" SARIF-E-DATA SARIF-FAIL then
   SARIF-RULE-END @ u + SARIF-RULE-BUF-CAP > if s" diag-to-sarif: rule buffer full" SARIF-E-DATA SARIF-FAIL then
   SARIF-RULE-END @ SARIF-RULE-N @ cells SARIF-RULE-OFF + !
   u SARIF-RULE-N @ cells SARIF-RULE-LEN + !
   a SARIF-RULE-BUF SARIF-RULE-END @ + u BYTE-COPY
   SARIF-RULE-END @ u + SARIF-RULE-END !
   SARIF-RULE-N @ 1+ SARIF-RULE-N ! ;

: SARIF-STR< ( ptr u8 n ptr u8 n -- bool )
   {: a:ptr u:n b:ptr v:n :}
   0 begin dup u < over v < and while
      dup a + c@ over b + c@
      2dup < if 2drop drop SARIF-TRUE exit then
      > if drop SARIF-FALSE exit then
      1+
   repeat drop
   u v < ;

: SARIF-RULE>? ( n n -- bool )
   {: lhs:n rhs:n :}
   rhs SARIF-RULE$ lhs SARIF-RULE$ SARIF-STR< ;

: SARIF-CELL-SWAP ( ptr n ptr n -- )
   {: a:ptr b:ptr :}
   a @ b @
   a !
   b ! ;

: SARIF-RULE-SWAP ( n n -- )
   {: lhs:n rhs:n :}
   lhs cells SARIF-RULE-OFF + rhs cells SARIF-RULE-OFF + SARIF-CELL-SWAP
   lhs cells SARIF-RULE-LEN + rhs cells SARIF-RULE-LEN + SARIF-CELL-SWAP ;

: SARIF-SORT-RULES ( -- )
   0 begin dup SARIF-RULE-N @ < while
      0 begin dup SARIF-RULE-N @ 1- < while
         dup dup 1+ SARIF-RULE>? if dup dup 1+ SARIF-RULE-SWAP then
         1+
      repeat drop
      1+
   repeat drop ;

: SARIF-EMIT-RULE ( n -- )
   {: idx:n :}
   SARIF-LBRACE SARIF-C
   s" id" SARIF-KEY idx SARIF-RULE$ SARIF-STRING
   SARIF-COMMA SARIF-C
   s" name" SARIF-KEY idx SARIF-RULE$ SARIF-STRING
   SARIF-COMMA SARIF-C
   s" shortDescription" SARIF-KEY
      SARIF-LBRACE SARIF-C
      s" text" SARIF-KEY idx SARIF-RULE$ SARIF-STRING
      SARIF-RBRACE SARIF-C
   SARIF-RBRACE SARIF-C ;

: SARIF-EMIT-RULES ( -- )
   SARIF-LBRACK SARIF-C
   0 begin dup SARIF-RULE-N @ < while
      dup 0 > if SARIF-COMMA SARIF-C then
      dup SARIF-EMIT-RULE
      1+
   repeat drop
   SARIF-RBRACK SARIF-C ;

: SARIF-IN-C+ ( n -- )
   SARIF-IN-LEN @ 1+ SARIF-IN-CAP > if s" diag-to-sarif: input too large" SARIF-E-DATA SARIF-FAIL then
   SARIF-IN-BUF SARIF-IN-LEN @ + c!
   SARIF-IN-LEN @ 1+ SARIF-IN-LEN ! ;

: SARIF-IN+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   SARIF-IN-LEN @ u + SARIF-IN-CAP > if s" diag-to-sarif: input too large" SARIF-E-DATA SARIF-FAIL then
   a SARIF-IN-BUF SARIF-IN-LEN @ + u BYTE-COPY
   SARIF-IN-LEN @ u + SARIF-IN-LEN ! ;

: SARIF-LAST-LF? ( -- bool )
   SARIF-IN-LEN @ 0= if SARIF-TRUE exit then
   SARIF-IN-BUF SARIF-IN-LEN @ 1- + c@ SARIF-LF = ;

: SARIF-APPEND-SOURCE ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u SARIF-IN+
   u 0 > if SARIF-LAST-LF? 0= if SARIF-LF SARIF-IN-C+ then then ;

: SARIF-READ-FD ( n -- )
   {: fd:n :}
   begin
      SARIF-IN-LEN @ SARIF-IN-CAP >= if s" diag-to-sarif: input too large" SARIF-E-DATA SARIF-FAIL then
      fd SARIF-IN-BUF SARIF-IN-LEN @ + SARIF-IN-CAP SARIF-IN-LEN @ - read SARIF-RD !
      SARIF-RD @ 0 >
   while
      SARIF-IN-LEN @ SARIF-RD @ + SARIF-IN-LEN !
   repeat
   SARIF-RD @ 0< if s" diag-to-sarif: read failed" SARIF-E-IO SARIF-FAIL then
   SARIF-IN-LEN @ 0 > if SARIF-LAST-LF? 0= if SARIF-LF SARIF-IN-C+ then then ;

: SARIF-READ-STDIN ( -- )
   0 SARIF-READ-FD ;

: SARIF-LOAD-PATH ( n -- )
   SCRIPT-ARGV$ FS:>PATHZ 0 0 open SARIF-FD !
   SARIF-FD @ 0< if s" diag-to-sarif: cannot open input" SARIF-E-IO SARIF-FAIL then
   SARIF-FD @ SARIF-READ-FD
   SARIF-FD @ close ;

: SARIF-LOAD-FILE ( ptr u8 n -- )
   FS:>PATHZ 0 0 open SARIF-FD !
   SARIF-FD @ 0< if s" diag-to-sarif: cannot open input" SARIF-E-IO SARIF-FAIL then
   SARIF-FD @ SARIF-READ-FD
   SARIF-FD @ close ;

: SARIF-LOAD-INPUT ( -- )
   0 SARIF-IN-LEN !
   SCRIPT-ARGC 0= if SARIF-READ-STDIN exit then
   0 begin dup SCRIPT-ARGC < while
      dup SARIF-LOAD-PATH
      1+
   repeat drop ;

: SARIF-LINES-START ( -- )
   0 SARIF-LINE-I ! ;

: SARIF-NEXT-LINE ( -- bool )
   SARIF-LINE-I @ SARIF-IN-LEN @ >= if SARIF-FALSE exit then
   SARIF-LINE-I @ SARIF-LINE-START !
   begin SARIF-LINE-I @ SARIF-IN-LEN @ < while
      SARIF-IN-BUF SARIF-LINE-I @ + c@ SARIF-LF = if
         SARIF-IN-BUF SARIF-LINE-START @ +
         SARIF-LINE-I @ SARIF-LINE-START @ - TRIM
         SARIF-LINE-U ! SARIF-LINE-A!
         SARIF-LINE-I @ 1+ SARIF-LINE-I !
         SARIF-TRUE exit
      then
      SARIF-LINE-I @ 1+ SARIF-LINE-I !
   repeat
   SARIF-IN-BUF SARIF-LINE-START @ +
   SARIF-IN-LEN @ SARIF-LINE-START @ - TRIM
   SARIF-LINE-U ! SARIF-LINE-A!
   SARIF-IN-LEN @ SARIF-LINE-I !
   SARIF-TRUE ;

: SARIF-PARSE-RAW ( -- )
   SARIF-READER SARIF-LINE-A@ SARIF-LINE-U @ JR:INIT
   JR:NEXT {: kind:n :}
   begin JR:NEXT dup JR:T-END <> while drop repeat drop
   JR:CLOSE
   kind JR:T-OBJ = SARIF-ROW-OBJECT ! ;

\ JR's codes for text that is not JSON; any other code, such as a refused
\ growth of the reader's nesting stack, is not the row's fault and is thrown.
: SARIF-SYNTAX? ( n -- bool )
   {: code:n :}
   code E-JR-COMMA >= code E-JR-MALFORMED <= and ;

: SARIF-PARSE-LINE ( -- )
   [: SARIF-PARSE-RAW ;] catch
   dup 0<> if
      dup SARIF-SYNTAX? 0= if throw then
      s" diag-to-sarif: invalid JSON" SARIF-E-DATA SARIF-FAIL
   then drop
   SARIF-ROW-OBJECT @ 0= if
      s" diag-to-sarif: expected JSON object" SARIF-E-DATA SARIF-FAIL
   then ;

: SARIF-COLLECT-RULES ( -- )
   0 SARIF-RULE-N !
   0 SARIF-RULE-END !
   SARIF-LINES-START
   begin SARIF-NEXT-LINE while
      SARIF-LINE-U @ 0 > if
         SARIF-PARSE-LINE SARIF-CODE$ SARIF-RULE-ADD
      then
   repeat
   SARIF-SORT-RULES ;

: SARIF-EMIT-RESULTS ( -- )
   SARIF-LBRACK SARIF-C
   0 SARIF-RESULT-N !
   SARIF-LINES-START
   begin SARIF-NEXT-LINE while
      SARIF-LINE-U @ 0 > if
         SARIF-RESULT-N @ 0 > if SARIF-COMMA SARIF-C then
         SARIF-PARSE-LINE SARIF-EMIT-RESULT
         SARIF-RESULT-N @ 1+ SARIF-RESULT-N !
      then
   repeat
   SARIF-RBRACK SARIF-C ;

: SARIF-EMIT-DOC ( -- )
   SARIF-LBRACE SARIF-C
   s" $schema" SARIF-KEY s" https://json.schemastore.org/sarif-2.1.0.json" SARIF-STRING
   SARIF-COMMA SARIF-C
   s" version" SARIF-KEY s" 2.1.0" SARIF-STRING
   SARIF-COMMA SARIF-C
   s" runs" SARIF-KEY
      SARIF-LBRACK SARIF-C
      SARIF-LBRACE SARIF-C
      s" tool" SARIF-KEY
         SARIF-LBRACE SARIF-C
         s" driver" SARIF-KEY
            SARIF-LBRACE SARIF-C
            s" name" SARIF-KEY s" habu" SARIF-STRING
            SARIF-COMMA SARIF-C
            s" informationUri" SARIF-KEY s" https://github.com/joelreymont/habu" SARIF-STRING
            SARIF-COMMA SARIF-C
            s" rules" SARIF-KEY SARIF-EMIT-RULES
            SARIF-RBRACE SARIF-C
         SARIF-RBRACE SARIF-C
      SARIF-COMMA SARIF-C
      s" results" SARIF-KEY SARIF-EMIT-RESULTS
      SARIF-RBRACE SARIF-C
      SARIF-RBRACK SARIF-C
   SARIF-RBRACE SARIF-C
   SARIF-LF SARIF-C ;

: SARIF-MAIN ( -- )
   SARIF-LOAD-INPUT
   SARIF-COLLECT-RULES
   SARIF-EMIT-DOC ;

: SARIF-FILE ( ptr u8 n -- )
   0 SARIF-IN-LEN !
   SARIF-LOAD-FILE
   SARIF-COLLECT-RULES
   SARIF-EMIT-DOC ;
