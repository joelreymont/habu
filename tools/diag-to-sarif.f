\ diag-to-sarif.f - convert Habu diagnostic JSONL to SARIF 2.1.0.
\ Load after lib/errors.f, lib/memory.f, and tools/json.f, then run with bin/hb.

65 constant SARIF-E-DATA
74 constant SARIF-E-IO

$40000 constant SARIF-IN-CAP
$400 constant SARIF-PATH-CAP
$100 constant SARIF-MAX-RULES
$2000 constant SARIF-RULE-BUF-CAP
$1000 constant SARIF-TEXT-CAP
32 constant SARIF-NUM-CAP

create SARIF-IN-BUF SARIF-IN-CAP allot
create SARIF-PATH-BUF SARIF-PATH-CAP allot
create SARIF-RULE-BUF SARIF-RULE-BUF-CAP allot
create SARIF-RULE-OFF SARIF-MAX-RULES cells allot
create SARIF-RULE-LEN SARIF-MAX-RULES cells allot
create SARIF-TEXT-BUF SARIF-TEXT-CAP allot
create SARIF-NUM-BUF SARIF-NUM-CAP allot
create SARIF-ONE 1 allot

variable SARIF-IN-LEN
variable SARIF-RULE-N
variable SARIF-RULE-END
variable SARIF-LINE-I
variable SARIF-LINE-N
variable SARIF-LINE-START
variable SARIF-LINE-A
variable SARIF-LINE-U
variable SARIF-FIELD?
variable SARIF-RD
variable SARIF-FD
variable SARIF-NV
variable SARIF-NP
variable SARIF-NEG?
variable SARIF-PARSE-NODE

: SARIF-TRUE ( -- bool )
   0 0= ;

: SARIF-FALSE ( -- bool )
   SARIF-TRUE 0= ;

: SARIF-LINE-A-FIELD ( -- ptr ptr u8 )
   SARIF-LINE-A 0 ptr-field ;

: SARIF-LINE-A@ ( -- ptr u8 )
   SARIF-LINE-A-FIELD @ ;

: SARIF-LINE-A! ( ptr u8 -- )
   SARIF-LINE-A-FIELD ! ;

: SARIF-FAIL ( ptr u8 n n -- )
   die ;

: SARIF-WRITE ( n ptr u8 n -- )
   {: fd a u :}
   u 0= if exit then
   fd a u write u <> if s" diag-to-sarif: write failed" SARIF-E-IO SARIF-FAIL then ;

: SARIF-OUT ( ptr u8 n -- )
   1 -rot SARIF-WRITE ;

: SARIF-C ( n -- )
   SARIF-ONE c!
   1 SARIF-ONE 1 SARIF-WRITE ;

: SARIF-STRING ( ptr u8 n -- )
   JSONW-RESET
   JSONW-STRING
   JSON-OUT-BUF JSON-OUT-LEN @ SARIF-OUT ;

: SARIF-KEY ( ptr u8 n -- )
   SARIF-STRING
   J-COLON SARIF-C ;

: SARIF-U64 ( u -- )
   SARIF-NV !
   SARIF-NV @ 0= if 48 SARIF-C exit then
   0 SARIF-NP !
   begin SARIF-NV @ 0 > while
      SARIF-NV @ 10 mod 48 + SARIF-NUM-BUF SARIF-NP @ + c!
      SARIF-NV @ 10 / SARIF-NV !
      SARIF-NP @ 1+ SARIF-NP !
   repeat
   begin SARIF-NP @ 0 > while
      SARIF-NP @ 1- SARIF-NP !
      SARIF-NUM-BUF SARIF-NP @ + c@ SARIF-C
   repeat ;

: SARIF-I64 ( n -- )
   dup 0< if J-MINUS SARIF-C negate then
   SARIF-U64 ;

: SARIF-DIGIT ( n -- n )
   dup 48 < if drop -1 exit then
   dup 57 > if drop -1 exit then
   48 - ;

: SARIF>S ( ptr u8 n -- n )
   {: a u :}
   u 0= if s" diag-to-sarif: expected integer" SARIF-E-DATA SARIF-FAIL then
   0 SARIF-NV !
   0 SARIF-NP !
   0 SARIF-NEG? !
   a c@ J-MINUS = if
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
   {: a u :}
   u SARIF-TEXT-CAP > if s" diag-to-sarif: text too large" SARIF-E-DATA SARIF-FAIL then
   a u SARIF-TEXT-BUF JSON-COPY
   SARIF-TEXT-BUF u ;

: SARIF-NODE-TEXT$ ( n -- ptr u8 n )
   {: node :}
   node JSON-KIND J-STR = if node JSON-STRING$ exit then
   node JSON-KIND J-NUM = if node JSON-NUMBER$ exit then
   node JSON-KIND J-BOOL = if
      node JSON-BOOL@ if s" True" else s" False" then exit
   then
   node JSON-KIND J-NULL = if s" None" exit then
   node JSON-WRITE SARIF-COPY-TEXT ;

: SARIF-NONZERO-NUM? ( ptr u8 n -- bool )
   {: a u :}
   0 begin dup u < while
      dup a + c@ dup 49 >= swap 57 <= and if drop SARIF-TRUE exit then
      1+
   repeat drop SARIF-FALSE ;

: SARIF-TRUTHY? ( n -- bool )
   {: node :}
   node JSON-KIND J-NULL = if SARIF-FALSE exit then
   node JSON-KIND J-BOOL = if node JSON-BOOL@ exit then
   node JSON-KIND J-STR = if node JSON-STRING$ nip 0 > exit then
   node JSON-KIND J-NUM = if node JSON-NUMBER$ SARIF-NONZERO-NUM? exit then
   node JSON-COUNT 0 > ;

: SARIF-TRUTHY-GET ( n ptr u8 n -- n )
   {: obj a u :}
   obj a u JSON-GET dup -1 = if exit then
   dup SARIF-TRUTHY? if exit then
   drop -1 ;

: SARIF-MESSAGE$ ( n -- ptr u8 n )
   {: obj :}
   obj s" suggestion" SARIF-TRUTHY-GET dup -1 <> if SARIF-NODE-TEXT$ exit then drop
   obj s" reason" SARIF-TRUTHY-GET dup -1 <> if SARIF-NODE-TEXT$ exit then drop
   obj s" verdict" SARIF-TRUTHY-GET dup -1 <> if SARIF-NODE-TEXT$ exit then drop
   obj s" code" SARIF-TRUTHY-GET dup -1 <> if SARIF-NODE-TEXT$ exit then drop
   s" Habu diagnostic" ;

: SARIF-CODE$ ( n -- ptr u8 n )
   s" code" JSON-GET dup -1 = if drop s" HABU-DIAGNOSTIC" exit then
   SARIF-NODE-TEXT$ ;

: SARIF-FILE$ ( n -- ptr u8 n )
   s" file" JSON-GET dup -1 = if drop s" <input>" exit then
   SARIF-NODE-TEXT$ ;

: SARIF-NODE>I64 ( n -- n )
   {: node :}
   node JSON-KIND J-NUM = if node JSON-NUMBER$ SARIF>S exit then
   node JSON-KIND J-STR = if node JSON-STRING$ SARIF>S exit then
   s" diag-to-sarif: expected integer" SARIF-E-DATA SARIF-FAIL ;

: SARIF-EMIT-NODE-OR-NULL ( n -- )
   dup -1 = if drop s" null" SARIF-OUT exit then
   JSON-WRITE SARIF-OUT ;

: SARIF-EMIT-GET ( n ptr u8 n -- )
   JSON-GET SARIF-EMIT-NODE-OR-NULL ;

: SARIF-FIELDS-START ( -- )
   0 SARIF-FIELD? ! ;

: SARIF-NEXT-FIELD ( -- )
   SARIF-FIELD? @ if J-COMMA SARIF-C then
   -1 SARIF-FIELD? ! ;

: SARIF-REGION-FIELD ( n ptr u8 n ptr u8 n -- )
   {: obj src-a src-u out-a out-u :}
   obj src-a src-u JSON-GET dup -1 = if drop exit then
   SARIF-NEXT-FIELD
   out-a out-u SARIF-KEY
   SARIF-NODE>I64 SARIF-I64 ;

: SARIF-REGION-LEN ( n -- )
   {: obj :}
   obj s" byte_start" JSON-GET dup -1 = if drop exit then
   SARIF-NODE>I64
   obj s" byte_end" JSON-GET dup -1 = if drop drop exit then
   SARIF-NODE>I64 swap -
   dup 0< if drop 0 then
   SARIF-NEXT-FIELD
   s" byteLength" SARIF-KEY
   SARIF-I64 ;

: SARIF-EMIT-REGION ( n -- )
   {: obj :}
   J-LBRACE SARIF-C
   SARIF-FIELDS-START
   obj s" line" s" startLine" SARIF-REGION-FIELD
   obj s" column" s" startColumn" SARIF-REGION-FIELD
   obj s" byte_start" s" byteOffset" SARIF-REGION-FIELD
   obj SARIF-REGION-LEN
   J-RBRACE SARIF-C ;

: SARIF-EMIT-PROPERTIES ( n -- )
   {: obj :}
   J-LBRACE SARIF-C
   s" schema_version" SARIF-KEY obj s" schema_version" SARIF-EMIT-GET
   J-COMMA SARIF-C
   s" word" SARIF-KEY obj s" word" SARIF-EMIT-GET
   J-COMMA SARIF-C
   s" token" SARIF-KEY obj s" token" SARIF-EMIT-GET
   J-COMMA SARIF-C
   s" verdict" SARIF-KEY obj s" verdict" SARIF-EMIT-GET
   J-RBRACE SARIF-C ;

: SARIF-EMIT-RESULT ( n -- )
   {: obj :}
   J-LBRACE SARIF-C
   s" ruleId" SARIF-KEY obj SARIF-CODE$ SARIF-STRING
   J-COMMA SARIF-C
   s" level" SARIF-KEY s" error" SARIF-STRING
   J-COMMA SARIF-C
   s" message" SARIF-KEY
      J-LBRACE SARIF-C
      s" text" SARIF-KEY obj SARIF-MESSAGE$ SARIF-STRING
      J-RBRACE SARIF-C
   J-COMMA SARIF-C
   s" properties" SARIF-KEY obj SARIF-EMIT-PROPERTIES
   J-COMMA SARIF-C
   s" locations" SARIF-KEY
      J-LBRACK SARIF-C
      J-LBRACE SARIF-C
      s" physicalLocation" SARIF-KEY
         J-LBRACE SARIF-C
         s" artifactLocation" SARIF-KEY
            J-LBRACE SARIF-C
            s" uri" SARIF-KEY obj SARIF-FILE$ SARIF-STRING
            J-RBRACE SARIF-C
         J-COMMA SARIF-C
         s" region" SARIF-KEY obj SARIF-EMIT-REGION
         J-RBRACE SARIF-C
      J-RBRACE SARIF-C
      J-RBRACK SARIF-C
   J-RBRACE SARIF-C ;

: SARIF-RULE$ ( n -- ptr u8 n )
   {: idx :}
   SARIF-RULE-BUF idx cells SARIF-RULE-OFF + @ +
   idx cells SARIF-RULE-LEN + @ ;

: SARIF-RULE-FIND ( ptr u8 n -- n )
   {: a u :}
   0 begin dup SARIF-RULE-N @ < while
      dup SARIF-RULE$ a u JSON-STR= if exit then
      1+
   repeat drop -1 ;

: SARIF-RULE-ADD ( ptr u8 n -- )
   {: a u :}
   a u SARIF-RULE-FIND 0 >= if exit then
   SARIF-RULE-N @ SARIF-MAX-RULES >= if s" diag-to-sarif: too many rules" SARIF-E-DATA SARIF-FAIL then
   SARIF-RULE-END @ u + SARIF-RULE-BUF-CAP > if s" diag-to-sarif: rule buffer full" SARIF-E-DATA SARIF-FAIL then
   SARIF-RULE-END @ SARIF-RULE-N @ cells SARIF-RULE-OFF + !
   u SARIF-RULE-N @ cells SARIF-RULE-LEN + !
   a u SARIF-RULE-BUF SARIF-RULE-END @ + JSON-COPY
   SARIF-RULE-END @ u + SARIF-RULE-END !
   SARIF-RULE-N @ 1+ SARIF-RULE-N ! ;

: SARIF-STR< ( ptr u8 n ptr u8 n -- bool )
   {: a u b v :}
   0 begin dup u < over v < and while
      dup a + c@ over b + c@
      2dup < if 2drop drop SARIF-TRUE exit then
      > if drop SARIF-FALSE exit then
      1+
   repeat drop
   u v < ;

: SARIF-RULE>? ( n n -- bool )
   {: lhs rhs :}
   rhs SARIF-RULE$ lhs SARIF-RULE$ SARIF-STR< ;

: SARIF-CELL-SWAP ( ptr n ptr n -- )
   {: a b :}
   a @ b @
   a !
   b ! ;

: SARIF-RULE-SWAP ( n n -- )
   {: lhs rhs :}
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
   {: idx :}
   J-LBRACE SARIF-C
   s" id" SARIF-KEY idx SARIF-RULE$ SARIF-STRING
   J-COMMA SARIF-C
   s" name" SARIF-KEY idx SARIF-RULE$ SARIF-STRING
   J-COMMA SARIF-C
   s" shortDescription" SARIF-KEY
      J-LBRACE SARIF-C
      s" text" SARIF-KEY idx SARIF-RULE$ SARIF-STRING
      J-RBRACE SARIF-C
   J-RBRACE SARIF-C ;

: SARIF-EMIT-RULES ( -- )
   J-LBRACK SARIF-C
   0 begin dup SARIF-RULE-N @ < while
      dup 0 > if J-COMMA SARIF-C then
      dup SARIF-EMIT-RULE
      1+
   repeat drop
   J-RBRACK SARIF-C ;

: SARIF-IN-C+ ( n -- )
   SARIF-IN-LEN @ 1+ SARIF-IN-CAP > if s" diag-to-sarif: input too large" SARIF-E-DATA SARIF-FAIL then
   SARIF-IN-BUF SARIF-IN-LEN @ + c!
   SARIF-IN-LEN @ 1+ SARIF-IN-LEN ! ;

: SARIF-IN+ ( ptr u8 n -- )
   {: a u :}
   SARIF-IN-LEN @ u + SARIF-IN-CAP > if s" diag-to-sarif: input too large" SARIF-E-DATA SARIF-FAIL then
   a u SARIF-IN-BUF SARIF-IN-LEN @ + JSON-COPY
   SARIF-IN-LEN @ u + SARIF-IN-LEN ! ;

: SARIF-LAST-LF? ( -- bool )
   SARIF-IN-LEN @ 0= if SARIF-TRUE exit then
   SARIF-IN-BUF SARIF-IN-LEN @ 1- + c@ J-LF = ;

: SARIF-COPY-BYTES {: a:ptr dst:ptr u :} ( ptr u8 ptr u8 n -- )
   0 begin dup u < while
      dup a + c@ over dst + c!
      1+
   repeat drop ;

: SARIF-PATHZ {: a:ptr u :} ( ptr u8 n -- ptr u8 )
   u 1+ SARIF-PATH-CAP > if s" diag-to-sarif: path too long" SARIF-E-IO SARIF-FAIL then
   a SARIF-PATH-BUF u SARIF-COPY-BYTES
   0 SARIF-PATH-BUF u + c!
   SARIF-PATH-BUF ;

: SARIF-APPEND-SOURCE ( ptr u8 n -- )
   {: a u :}
   a u SARIF-IN+
   u 0 > if SARIF-LAST-LF? 0= if J-LF SARIF-IN-C+ then then ;

: SARIF-READ-FD ( n -- )
   {: fd :}
   begin
      SARIF-IN-LEN @ SARIF-IN-CAP >= if s" diag-to-sarif: input too large" SARIF-E-DATA SARIF-FAIL then
      fd SARIF-IN-BUF SARIF-IN-LEN @ + SARIF-IN-CAP SARIF-IN-LEN @ - read SARIF-RD !
      SARIF-RD @ 0 >
   while
      SARIF-IN-LEN @ SARIF-RD @ + SARIF-IN-LEN !
   repeat
   SARIF-RD @ 0< if s" diag-to-sarif: read failed" SARIF-E-IO SARIF-FAIL then
   SARIF-IN-LEN @ 0 > if SARIF-LAST-LF? 0= if J-LF SARIF-IN-C+ then then ;

: SARIF-READ-STDIN ( -- )
   0 SARIF-READ-FD ;

: SARIF-LOAD-PATH ( n -- )
   SCRIPT-ARGV$ SARIF-PATHZ 0 0 open SARIF-FD !
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
   0 SARIF-LINE-I !
   0 SARIF-LINE-N ! ;

: SARIF-NEXT-LINE ( -- bool )
   SARIF-LINE-I @ SARIF-IN-LEN @ >= if SARIF-FALSE exit then
   SARIF-LINE-I @ SARIF-LINE-START !
   begin SARIF-LINE-I @ SARIF-IN-LEN @ < while
      SARIF-IN-BUF SARIF-LINE-I @ + c@ J-LF = if
         SARIF-IN-BUF SARIF-LINE-START @ +
         SARIF-LINE-I @ SARIF-LINE-START @ - JSON-TRIM
         SARIF-LINE-U ! SARIF-LINE-A!
         SARIF-LINE-I @ 1+ SARIF-LINE-I !
         SARIF-LINE-N @ 1+ SARIF-LINE-N !
         SARIF-TRUE exit
      then
      SARIF-LINE-I @ 1+ SARIF-LINE-I !
   repeat
   SARIF-IN-BUF SARIF-LINE-START @ +
   SARIF-IN-LEN @ SARIF-LINE-START @ - JSON-TRIM
   SARIF-LINE-U ! SARIF-LINE-A!
   SARIF-IN-LEN @ SARIF-LINE-I !
   SARIF-LINE-N @ 1+ SARIF-LINE-N !
   SARIF-TRUE ;

: SARIF-PARSE-RAW ( -- )
   SARIF-LINE-A@ SARIF-LINE-U @ JSON-PARSE SARIF-PARSE-NODE ! ;

: SARIF-PARSE-LINE ( -- n )
   [: SARIF-PARSE-RAW ;] catch
   dup 0= if drop else s" diag-to-sarif: invalid JSON" SARIF-E-DATA SARIF-FAIL then
   SARIF-PARSE-NODE @
   dup JSON-KIND J-OBJ <> if s" diag-to-sarif: expected JSON object" SARIF-E-DATA SARIF-FAIL then ;

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
   J-LBRACK SARIF-C
   SARIF-FIELDS-START
   SARIF-LINES-START
   begin SARIF-NEXT-LINE while
      SARIF-LINE-U @ 0 > if
         SARIF-NEXT-FIELD
         SARIF-PARSE-LINE SARIF-EMIT-RESULT
      then
   repeat
   J-RBRACK SARIF-C ;

: SARIF-EMIT-DOC ( -- )
   J-LBRACE SARIF-C
   s" $schema" SARIF-KEY s" https://json.schemastore.org/sarif-2.1.0.json" SARIF-STRING
   J-COMMA SARIF-C
   s" version" SARIF-KEY s" 2.1.0" SARIF-STRING
   J-COMMA SARIF-C
   s" runs" SARIF-KEY
      J-LBRACK SARIF-C
      J-LBRACE SARIF-C
      s" tool" SARIF-KEY
         J-LBRACE SARIF-C
         s" driver" SARIF-KEY
            J-LBRACE SARIF-C
            s" name" SARIF-KEY s" habu" SARIF-STRING
            J-COMMA SARIF-C
            s" informationUri" SARIF-KEY s" https://github.com/joelreymont/habu" SARIF-STRING
            J-COMMA SARIF-C
            s" rules" SARIF-KEY SARIF-EMIT-RULES
            J-RBRACE SARIF-C
         J-RBRACE SARIF-C
      J-COMMA SARIF-C
      s" results" SARIF-KEY SARIF-EMIT-RESULTS
      J-RBRACE SARIF-C
      J-RBRACK SARIF-C
   J-RBRACE SARIF-C
   J-LF SARIF-C ;

: SARIF-MAIN ( -- )
   SARIF-LOAD-INPUT
   SARIF-COLLECT-RULES
   SARIF-EMIT-DOC ;

SARIF-MAIN
