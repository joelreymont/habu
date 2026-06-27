\ gate-json-assert-core.f - native JSON assertion library for gate tests.
\ Load after lib/errors.f, lib/memory.f, and tools/json.f.

$8000 constant GJA-IN-CAP
$1000 constant GJA-SRC-CAP
$400 constant GJA-PATH-CAP
$100 constant GJA-LINE-MAX

create GJA-IN GJA-IN-CAP allot
create GJA-SRC GJA-SRC-CAP allot
create GJA-PATH GJA-PATH-CAP allot
create GJA-LINE-A GJA-LINE-MAX cells allot
create GJA-LINE-U GJA-LINE-MAX cells allot

variable GJA-FD
variable GJA-RD
variable GJA-LEN
variable GJA-ROOT
variable GJA-ARR
variable GJA-I
variable GJA-J
variable GJA-N
variable GJA-OK1
variable GJA-OK2
variable GJA-IDX
variable GJA-LINE
variable GJA-COL
variable GJA-LAST-NL
variable GJA-SRC-LEN
variable GJA-LINE#
variable GJA-LS
variable GJA-LX
variable GJA-STENCILS
variable GJA-PADDING
variable GJA-DIRECT

: GJA-TRUE ( -- bool )
   0 0= ;

: GJA-FALSE ( -- bool )
   GJA-TRUE 0= ;

: GJA-LINE-A-FIELD ( n -- ptr ptr u8 )
   cells GJA-LINE-A + 0 ptr-field ;

: GJA-LINE-A@ ( n -- ptr u8 )
   GJA-LINE-A-FIELD @ ;

: GJA-LINE-A! ( ptr u8 n -- )
   GJA-LINE-A-FIELD ! ;

: GJA-FAIL ( ptr u8 n -- )
   1 die ;

: GJA-USAGE ( -- )
   s" usage: tools/gate-json-assert.f MODE FILE [ARG]" GJA-FAIL ;

: GJA-ARGC= ( n -- ) {: n :}
   SCRIPT-ARGC n <> IF GJA-USAGE THEN ;

: GJA-COPY ( ptr u8 ptr u8 n -- )
   {: a:ptr dst:ptr u :}
   0 begin dup u < while
      dup a + c@ over dst + c!
      1+
   repeat drop ;

: GJA-PATHZ ( ptr u8 n -- ptr u8 )
   {: a:ptr u :}
   u 1+ GJA-PATH-CAP > IF s" path too long" GJA-FAIL THEN
   a GJA-PATH u GJA-COPY
   0 GJA-PATH u + c!
   GJA-PATH ;

: GJA-BYTES= ( ptr u8 n ptr u8 n -- bool )
   {: a:ptr u b:ptr v :}
   u v <> IF GJA-FALSE exit THEN
   0 begin dup u < while
      dup a + c@ over b + c@ <> IF drop GJA-FALSE exit THEN
      1+
   repeat drop GJA-TRUE ;

: GJA-CMD? ( ptr u8 n -- bool ) {: a:ptr u :}
   0 SCRIPT-ARGV$ a u GJA-BYTES= ;

: GJA-SUGGEST-ROW ( ptr u8 n ptr u8 n ptr u8 n -- ptr u8 n bool )
   {: class:ptr classu key:ptr keyu suggestion:ptr suggestionu :}
   class classu key keyu GJA-BYTES= IF suggestion suggestionu GJA-TRUE exit THEN
   class classu GJA-FALSE ;

: GJA-LINE! ( ptr u8 n n -- )
   {: a:ptr u k :}
   a k GJA-LINE-A!
   u k cells GJA-LINE-U + ! ;

: GJA-LINE$ ( n -- ptr u8 n )
   dup GJA-LINE-A@
   swap cells GJA-LINE-U + @ ;

: GJA-LINE+ ( ptr u8 n -- )
   GJA-LINE# @ GJA-LINE-MAX >= IF s" too many JSON lines" GJA-FAIL THEN
   dup 0 > IF
      2dup + 1- c@ 13 = IF 1- THEN
   THEN
   GJA-LINE# @ GJA-LINE!
   GJA-LINE# @ 1+ GJA-LINE# ! ;

: GJA-SPLIT-LINES ( ptr u8 n -- )
   {: a:ptr u :}
   0 GJA-LINE# !
   0 GJA-LS !
   0 GJA-LX !
   begin GJA-LX @ u < while
      a GJA-LX @ + c@ 10 = IF
         a GJA-LS @ + GJA-LX @ GJA-LS @ - GJA-LINE+
         GJA-LX @ 1+ GJA-LS !
      THEN
      GJA-LX @ 1+ GJA-LX !
   repeat
   GJA-LS @ u < IF
      a GJA-LS @ + u GJA-LS @ - GJA-LINE+
   THEN ;

: GJA-U? ( ptr u8 n -- n bool )
   {: a:ptr u :}
   u 0= IF 0 GJA-FALSE exit THEN
   0 GJA-N !
   0 begin dup u < while
      dup a + c@ dup 48 < over 57 > or IF drop drop 0 GJA-FALSE exit THEN
      48 - GJA-N @ 10 * + GJA-N !
      1+
   repeat drop GJA-N @ GJA-TRUE ;

: GJA-READ ( ptr u8 n -- ptr u8 n )
   GJA-PATHZ 0 0 open GJA-FD !
   GJA-FD @ 0 < IF s" cannot open JSON fixture" GJA-FAIL THEN
   0 GJA-LEN !
   begin
      GJA-LEN @ GJA-IN-CAP >= IF
         GJA-FD @ close
         s" JSON fixture too large" GJA-FAIL
      THEN
      GJA-FD @ GJA-IN GJA-LEN @ + GJA-IN-CAP GJA-LEN @ - read GJA-RD !
      GJA-RD @ 0 >
   while
      GJA-LEN @ GJA-RD @ + GJA-LEN !
   repeat
   GJA-RD @ 0 < IF
      GJA-FD @ close
      s" JSON fixture read failed" GJA-FAIL
   THEN
   GJA-FD @ close
   GJA-IN GJA-LEN @ ;

: GJA-READ-SRC ( ptr u8 n -- )
   GJA-PATHZ 0 0 open GJA-FD !
   GJA-FD @ 0 < IF s" cannot open source fixture" GJA-FAIL THEN
   0 GJA-SRC-LEN !
   begin
      GJA-SRC-LEN @ GJA-SRC-CAP >= IF
         GJA-FD @ close
         s" source fixture too large" GJA-FAIL
      THEN
      GJA-FD @ GJA-SRC GJA-SRC-LEN @ + GJA-SRC-CAP GJA-SRC-LEN @ - read GJA-RD !
      GJA-RD @ 0 >
   while
      GJA-SRC-LEN @ GJA-RD @ + GJA-SRC-LEN !
   repeat
   GJA-RD @ 0 < IF
      GJA-FD @ close
      s" source fixture read failed" GJA-FAIL
   THEN
   GJA-FD @ close ;

: GJA-PARSE-FILE ( ptr u8 n -- n )
   GJA-READ JSON-PARSE ;

: GJA-REQ ( n ptr u8 n -- n )
   JSON-GET dup -1 = IF s" missing JSON field" GJA-FAIL THEN ;

: GJA-OBJ ( n -- )
   JSON-KIND J-OBJ <> IF s" expected JSON object" GJA-FAIL THEN ;

: GJA-ARR-KIND ( n -- )
   JSON-KIND J-ARR <> IF s" expected JSON array" GJA-FAIL THEN ;

: GJA-INT ( n -- n )
   dup JSON-KIND J-NUM <> IF drop s" expected JSON integer" GJA-FAIL THEN
   JSON-NUMBER$ GJA-U? 0= IF drop s" invalid JSON integer" GJA-FAIL THEN ;

: GJA-STR= ( n ptr u8 n -- bool )
   {: node want:ptr wantu :}
   node JSON-KIND J-STR <> IF GJA-FALSE exit THEN
   node JSON-STRING$ want wantu GJA-BYTES= ;

: GJA-ASSERT-STR ( n ptr u8 n -- )
   GJA-STR= 0= IF s" unexpected JSON string" GJA-FAIL THEN ;

: GJA-NONEMPTY-STR ( n -- )
   dup JSON-KIND J-STR <> IF drop s" expected JSON string" GJA-FAIL THEN
   JSON-STRING$ nip 0= IF s" expected nonempty JSON string" GJA-FAIL THEN ;

: GJA-REQ-STRF ( n ptr u8 n -- )
   GJA-REQ dup JSON-KIND J-STR <> IF drop s" expected JSON string" GJA-FAIL THEN
   drop ;

: GJA-REQ-INTF ( n ptr u8 n -- )
   GJA-REQ GJA-INT drop ;

: GJA-NULL-OR-STR ( n -- )
   dup JSON-KIND J-NULL = IF drop exit THEN
   dup JSON-KIND J-STR <> IF drop s" expected JSON string or null" GJA-FAIL THEN
   drop ;

: GJA-REQ-NULL-OR-STRF ( n ptr u8 n -- )
   GJA-REQ GJA-NULL-OR-STR ;

: GJA-SCHEMA1 ( n -- )
   s" schema_version" GJA-REQ GJA-INT 1 <> IF s" schema_version is not 1" GJA-FAIL THEN ;

: GJA-LINE-STARTS-OBJECT ( ptr u8 n -- )
   dup 0= IF 2drop s" empty JSON line" GJA-FAIL THEN
   over c@ 123 <> IF 2drop s" JSON line does not start with object" GJA-FAIL THEN
   2drop ;

: GJA-JSON-LINES-SCHEMA ( ptr u8 n -- )
   GJA-READ GJA-SPLIT-LINES
   GJA-LINE# @ 0= IF s" no JSON lines" GJA-FAIL THEN
   0 GJA-I !
   begin GJA-I @ GJA-LINE# @ < while
      GJA-I @ GJA-LINE$ 2dup GJA-LINE-STARTS-OBJECT
      JSON-PARSE dup GJA-OBJ GJA-SCHEMA1
      GJA-I @ 1+ GJA-I !
   repeat ;

: GJA-JSON-ONE-SCHEMA ( ptr u8 n -- )
   GJA-READ GJA-SPLIT-LINES
   GJA-LINE# @ 1 <> IF s" expected one JSON line" GJA-FAIL THEN
   0 GJA-LINE$ 2dup GJA-LINE-STARTS-OBJECT
   JSON-PARSE dup GJA-OBJ GJA-SCHEMA1 ;

: GJA-FIRST-JSON ( ptr u8 n -- n )
   GJA-READ GJA-SPLIT-LINES
   GJA-LINE# @ 0= IF s" no JSON lines" GJA-FAIL THEN
   0 GJA-LINE$ 2dup GJA-LINE-STARTS-OBJECT
   JSON-PARSE dup GJA-OBJ ;

: GJA-LINE-JSON ( n -- n )
   GJA-LINE$ 2dup GJA-LINE-STARTS-OBJECT
   JSON-PARSE dup GJA-OBJ ;

: GJA-DIAG-WORD? ( n ptr u8 n -- bool ) {: root word:ptr wordu :}
   root s" word" GJA-REQ word wordu GJA-STR= ;

: GJA-FIND-WORD ( ptr u8 n ptr u8 n -- n ) {: json:ptr jsonu word:ptr wordu :}
   json jsonu GJA-READ GJA-SPLIT-LINES
   GJA-LINE# @ 0= IF s" no JSON lines" GJA-FAIL THEN
   0 GJA-I !
   begin GJA-I @ GJA-LINE# @ < while
      GJA-I @ GJA-LINE-JSON dup word wordu GJA-DIAG-WORD? IF exit THEN
      drop
      GJA-I @ 1+ GJA-I !
   repeat
   s" missing JSON diagnostic word" GJA-FAIL ;

: GJA-SRC-LINE-COL ( n -- )
   GJA-IDX !
   1 GJA-LINE !
   -1 GJA-LAST-NL !
   0 GJA-I !
   begin GJA-I @ GJA-IDX @ < while
      GJA-SRC GJA-I @ + c@ 10 = IF
         GJA-LINE @ 1+ GJA-LINE !
         GJA-I @ GJA-LAST-NL !
      THEN
      GJA-I @ 1+ GJA-I !
   repeat
   GJA-IDX @ GJA-LAST-NL @ - GJA-COL ! ;

: GJA-ASSERT-INT-FIELD ( n ptr u8 n n -- )
   {: root key:ptr keyu want :}
   root key keyu GJA-REQ GJA-INT want <> IF s" unexpected JSON integer field" GJA-FAIL THEN ;

: GJA-DIAG-FILE-ORIGIN ( ptr u8 n ptr u8 n -- )
   {: json:ptr jsonu src:ptr srcu :}
   src srcu GJA-READ-SRC
   -1 GJA-IDX !
   0 GJA-I !
   begin GJA-I @ 3 + GJA-SRC-LEN @ <= while
      GJA-SRC GJA-I @ + 3 s" dup" GJA-BYTES= IF
         GJA-I @ GJA-IDX !
         GJA-SRC-LEN @ GJA-I !
      THEN
      GJA-I @ 1+ GJA-I !
   repeat
   GJA-IDX @ 0 < IF s" cannot find dup in source fixture" GJA-FAIL THEN
   GJA-IDX @ GJA-SRC-LINE-COL
   json jsonu GJA-FIRST-JSON GJA-ROOT !
   GJA-ROOT @ s" schema_version" 1 GJA-ASSERT-INT-FIELD
   GJA-ROOT @ s" file" GJA-REQ src srcu GJA-ASSERT-STR
   GJA-ROOT @ s" line" GJA-LINE @ GJA-ASSERT-INT-FIELD
   GJA-ROOT @ s" column" GJA-COL @ GJA-ASSERT-INT-FIELD
   GJA-ROOT @ s" byte_start" GJA-IDX @ GJA-ASSERT-INT-FIELD
   GJA-ROOT @ s" byte_end" GJA-IDX @ 3 + GJA-ASSERT-INT-FIELD ;

: GJA-SUGGEST-FOR ( ptr u8 n -- ptr u8 n )
   s" remove_producer" s" Remove an extra producer or drop the surplus value."
   GJA-SUGGEST-ROW IF exit THEN
   s" add_producer" s" Add the missing producer or stop consuming a required value."
   GJA-SUGGEST-ROW IF exit THEN
   s" fix_type" s" Change the body so produced types match the signature."
   GJA-SUGGEST-ROW IF exit THEN
   s" fix_return_stack" s" Balance return-stack transfers before the definition exits."
   GJA-SUGGEST-ROW IF exit THEN
   s" trusted_boundary_required" s" Move this compiler or runtime boundary behind audited TRUST."
   GJA-SUGGEST-ROW IF exit THEN
   s" factor_local_shape" s" Move locals to a live top-level path or factor a helper."
   GJA-SUGGEST-ROW IF exit THEN
   s" remove_dead_code" s" Remove tokens after the terminating control word, or move the work before it."
   GJA-SUGGEST-ROW IF exit THEN
   s" fix_signature_syntax" s" Repair the stack-effect comment syntax, including --."
   GJA-SUGGEST-ROW IF exit THEN
   s" fix_signature_type" s" Use a known stack-signature type or a single-letter type variable."
   GJA-SUGGEST-ROW IF exit THEN
   s" rewrite_uncheckable" s" Rewrite with modeled words or isolate an audited primitive."
   GJA-SUGGEST-ROW IF exit THEN
   s" unknown_rejection" s" Inspect the token, signature, and raw stack evidence."
   GJA-SUGGEST-ROW IF exit THEN
   2drop s" unknown repair class in suggestion assertion" GJA-FAIL ;

: GJA-DIAG-CLASS-SUGGEST ( n ptr u8 n -- ) {: root class:ptr classu :}
   root s" repair_class" GJA-REQ
   class classu GJA-ASSERT-STR
   root s" suggestion" GJA-REQ dup GJA-NONEMPTY-STR
   class classu GJA-SUGGEST-FOR GJA-ASSERT-STR ;

: GJA-DIAG-REPAIR-CLASS ( ptr u8 n ptr u8 n -- )
   {: json:ptr jsonu class:ptr classu :}
   json jsonu GJA-FIRST-JSON GJA-ROOT !
   GJA-ROOT @ class classu GJA-DIAG-CLASS-SUGGEST ;

: GJA-DIAG-WORD-REPAIR-CLASS ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: json:ptr jsonu word:ptr wordu class:ptr classu :}
   json jsonu word wordu GJA-FIND-WORD GJA-ROOT !
   GJA-ROOT @ class classu GJA-DIAG-CLASS-SUGGEST ;

: GJA-REPAIR-PACKET ( ptr u8 n ptr u8 n -- )
   {: json:ptr jsonu class:ptr classu :}
   json jsonu GJA-FIRST-JSON GJA-ROOT !
   GJA-ROOT @ s" schema_version" 1 GJA-ASSERT-INT-FIELD
   GJA-ROOT @ s" kind" GJA-REQ s" habu_repair_packet" GJA-ASSERT-STR
   GJA-ROOT @ s" repair_class" GJA-REQ
   class classu GJA-ASSERT-STR
   GJA-ROOT @ s" diagnostic_count" GJA-REQ GJA-INT 0 <= IF s" invalid diagnostic_count" GJA-FAIL THEN
   GJA-ROOT @ s" word" GJA-REQ GJA-NONEMPTY-STR
   GJA-ROOT @ s" token" GJA-REQ GJA-NONEMPTY-STR
   GJA-ROOT @ s" token_index" GJA-REQ-INTF
   GJA-ROOT @ s" file" GJA-REQ GJA-NONEMPTY-STR
   GJA-ROOT @ s" line" GJA-REQ-INTF
   GJA-ROOT @ s" column" GJA-REQ-INTF
   GJA-ROOT @ s" byte_start" GJA-REQ-INTF
   GJA-ROOT @ s" byte_end" GJA-REQ-INTF
   GJA-ROOT @ s" declared_effect" GJA-REQ-NULL-OR-STRF
   GJA-ROOT @ s" declared_effect_source" GJA-REQ-NULL-OR-STRF
   GJA-ROOT @ s" inferred_effect" GJA-REQ GJA-NONEMPTY-STR
   GJA-ROOT @ s" return_stack" GJA-REQ GJA-OBJ
   GJA-ROOT @ s" code" GJA-REQ GJA-NONEMPTY-STR
   GJA-ROOT @ s" suggestion" GJA-REQ dup GJA-NONEMPTY-STR
   class classu GJA-SUGGEST-FOR GJA-ASSERT-STR
   GJA-ROOT @ s" source_excerpt" GJA-REQ GJA-NONEMPTY-STR
   GJA-ROOT @ s" instruction" GJA-REQ
   s" Fix the definition so it certifies. Output only corrected Habu code." GJA-ASSERT-STR ;

: GJA-DIAG-COMMON ( n -- )
   dup GJA-SCHEMA1
   dup s" code" GJA-REQ GJA-NONEMPTY-STR
   dup s" repair_class" GJA-REQ GJA-NONEMPTY-STR
   dup s" word" GJA-REQ GJA-NONEMPTY-STR
   dup s" token" GJA-REQ GJA-NONEMPTY-STR
   dup s" token_index" GJA-REQ-INTF
   dup s" file" GJA-REQ GJA-NONEMPTY-STR
   dup s" line" GJA-REQ-INTF
   dup s" column" GJA-REQ-INTF
   dup s" byte_start" GJA-REQ-INTF
   dup s" byte_end" GJA-REQ-INTF
   dup s" definition_source" GJA-REQ GJA-NONEMPTY-STR
   dup s" suggestion" GJA-REQ GJA-NONEMPTY-STR
   dup s" return_stack" GJA-REQ dup GJA-OBJ
   dup s" expected" GJA-REQ-STRF
   s" actual" GJA-REQ-STRF
   drop ;

: GJA-DIAG-DSTACK ( n -- )
   dup s" expected" GJA-REQ-STRF
   s" actual" GJA-REQ-STRF ;

: GJA-DIAG-RETURN-STACK ( ptr u8 n ptr u8 n ptr u8 n -- ) {: json:ptr jsonu exp:ptr expu act:ptr actu :}
   json jsonu GJA-FIRST-JSON GJA-ROOT !
   GJA-ROOT @ GJA-DIAG-COMMON
   GJA-ROOT @ s" repair_class" GJA-REQ s" fix_return_stack" GJA-ASSERT-STR
   GJA-ROOT @ s" return_stack" GJA-REQ dup GJA-OBJ
   dup s" expected" GJA-REQ exp expu GJA-ASSERT-STR
   s" actual" GJA-REQ act actu GJA-ASSERT-STR ;

: GJA-DIAG-ROW-EFFECT ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- ) {: json:ptr jsonu src:ptr srcu exp:ptr expu act:ptr actu class:ptr classu :}
   json jsonu GJA-FIRST-JSON GJA-ROOT !
   GJA-ROOT @ GJA-DIAG-COMMON
   GJA-ROOT @ s" declared_effect_source" GJA-REQ src srcu GJA-ASSERT-STR
   GJA-ROOT @ s" expected" GJA-REQ exp expu GJA-ASSERT-STR
   GJA-ROOT @ s" actual" GJA-REQ act actu GJA-ASSERT-STR
   GJA-ROOT @ s" repair_class" GJA-REQ class classu GJA-ASSERT-STR ;

: GJA-DIAG-WORD-RETURN-STACK ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: json:ptr jsonu word:ptr wordu exp:ptr expu act:ptr actu :}
   json jsonu word wordu GJA-FIND-WORD GJA-ROOT !
   GJA-ROOT @ GJA-DIAG-COMMON
   GJA-ROOT @ s" repair_class" GJA-REQ s" fix_return_stack" GJA-ASSERT-STR
   GJA-ROOT @ s" return_stack" GJA-REQ dup GJA-OBJ
   dup s" expected" GJA-REQ exp expu GJA-ASSERT-STR
   s" actual" GJA-REQ act actu GJA-ASSERT-STR ;

: GJA-DIAG-WORD-ROW-EFFECT ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: json:ptr jsonu word:ptr wordu src:ptr srcu exp:ptr expu act:ptr actu class:ptr classu :}
   json jsonu word wordu GJA-FIND-WORD GJA-ROOT !
   GJA-ROOT @ GJA-DIAG-COMMON
   GJA-ROOT @ s" declared_effect_source" GJA-REQ src srcu GJA-ASSERT-STR
   GJA-ROOT @ s" expected" GJA-REQ exp expu GJA-ASSERT-STR
   GJA-ROOT @ s" actual" GJA-REQ act actu GJA-ASSERT-STR
   GJA-ROOT @ s" repair_class" GJA-REQ
   class classu GJA-ASSERT-STR ;

: GJA-ALL-ROW0 ( n -- )
   dup GJA-DIAG-COMMON
   dup s" word" GJA-REQ s" bad1" GJA-ASSERT-STR
   dup s" code" GJA-REQ s" E-MISMATCH" GJA-ASSERT-STR
   dup s" repair_class" GJA-REQ s" remove_producer" GJA-ASSERT-STR
   GJA-DIAG-DSTACK ;

: GJA-ALL-ROW1 ( n -- )
   dup GJA-DIAG-COMMON
   dup s" word" GJA-REQ s" bad2" GJA-ASSERT-STR
   dup s" code" GJA-REQ s" E-REJECTED" GJA-ASSERT-STR
   dup s" repair_class" GJA-REQ s" fix_return_stack" GJA-ASSERT-STR
   s" return_stack" GJA-REQ s" actual" GJA-REQ GJA-NONEMPTY-STR ;

: GJA-ALL-ERRORS ( ptr u8 n -- )
   GJA-READ GJA-SPLIT-LINES
   GJA-LINE# @ 2 <> IF s" expected two all-errors diagnostics" GJA-FAIL THEN
   0 GJA-LINE$ 2dup GJA-LINE-STARTS-OBJECT JSON-PARSE dup GJA-OBJ GJA-ALL-ROW0
   1 GJA-LINE$ 2dup GJA-LINE-STARTS-OBJECT JSON-PARSE dup GJA-OBJ GJA-ALL-ROW1 ;

: GJA-START-LINE-OK ( n -- )
   s" locations" GJA-REQ dup GJA-ARR-KIND
   0 JSON-ARR@
   s" physicalLocation" GJA-REQ
   s" region" GJA-REQ
   s" startLine" GJA-REQ GJA-INT 0 <= IF s" SARIF result missing startLine" GJA-FAIL THEN ;

: GJA-SARIF ( ptr u8 n -- )
   GJA-PARSE-FILE GJA-ROOT !
   GJA-ROOT @ s" version" GJA-REQ s" 2.1.0" GJA-ASSERT-STR
   GJA-ROOT @ s" runs" GJA-REQ dup GJA-ARR-KIND 0 JSON-ARR@
   s" results" GJA-REQ dup GJA-ARR-KIND GJA-ARR !
   GJA-ARR @ JSON-COUNT 2 <> IF s" SARIF result count mismatch" GJA-FAIL THEN
   0 GJA-I !
   begin GJA-I @ GJA-ARR @ JSON-COUNT < while
      GJA-ARR @ GJA-I @ JSON-ARR@ GJA-START-LINE-OK
      GJA-I @ 1+ GJA-I !
   repeat ;

: GJA-CHECK-PUBLIC-ITEM ( n -- )
   {: item :}
   item s" word" GJA-REQ s" SQUARE" GJA-STR= IF
      item s" signature" GJA-REQ s" (i64 -- i64)" GJA-ASSERT-STR
      -1 GJA-OK1 !
      exit
   THEN
   item s" word" GJA-REQ s" APPLY" GJA-STR= IF
      item s" signature" GJA-REQ s" (i64 [ i64 -- i64 ] -- i64)" GJA-ASSERT-STR
      -1 GJA-OK2 !
      exit
   THEN ;

: GJA-PUBLIC-SIGNATURES ( ptr u8 n -- )
   0 GJA-OK1 ! 0 GJA-OK2 !
   GJA-PARSE-FILE GJA-ROOT !
   GJA-ROOT @ GJA-SCHEMA1
   GJA-ROOT @ s" definitions" GJA-REQ dup GJA-ARR-KIND GJA-ARR !
   0 GJA-I !
   begin GJA-I @ GJA-ARR @ JSON-COUNT < while
      GJA-ARR @ GJA-I @ JSON-ARR@ GJA-CHECK-PUBLIC-ITEM
      GJA-I @ 1+ GJA-I !
   repeat
   GJA-OK1 @ 0= IF s" missing SQUARE public signature" GJA-FAIL THEN
   GJA-OK2 @ 0= IF s" missing APPLY public signature" GJA-FAIL THEN ;

: GJA-AOT-COMMON ( n -- )
   dup GJA-SCHEMA1
   dup s" file_bytes" GJA-REQ GJA-INT 0 <= IF s" AOT report file_bytes invalid" GJA-FAIL THEN
   dup s" patched_call_stencils" GJA-REQ GJA-INT GJA-STENCILS !
   dup s" padding_bytes" GJA-REQ GJA-INT GJA-PADDING !
   s" direct_bl_instructions" GJA-REQ GJA-INT GJA-DIRECT ! ;

: GJA-AOT-STRIPPED ( ptr u8 n -- )
   GJA-PARSE-FILE GJA-AOT-COMMON
   GJA-PADDING @ GJA-STENCILS @ 12 * <> IF s" AOT padding relation mismatch" GJA-FAIL THEN
   GJA-STENCILS @ 0 <> IF s" AOT report found patched stencils" GJA-FAIL THEN
   GJA-DIRECT @ 0 <= IF s" AOT report missing direct BL" GJA-FAIL THEN ;

: GJA-AOT-COMPACT ( ptr u8 n -- )
   GJA-PARSE-FILE GJA-AOT-COMMON
   GJA-STENCILS @ 0 <> IF s" compact AOT report found patched stencils" GJA-FAIL THEN
   GJA-PADDING @ 0 <> IF s" compact AOT report found padding" GJA-FAIL THEN
   GJA-DIRECT @ 3 < IF s" compact AOT report missing direct BLs" GJA-FAIL THEN ;

: GJA-DISPATCH-ONE-FILE-ROW ( ptr u8 n [ ptr u8 n -- ] -- bool )
   {: cmd:ptr cmdu q :}
   cmd cmdu GJA-CMD? IF
      2 GJA-ARGC=
      1 SCRIPT-ARGV$ q execute
      GJA-TRUE exit
   THEN
   GJA-FALSE ;

: GJA-DISPATCH-TWO-ARG-ROW ( ptr u8 n [ ptr u8 n ptr u8 n -- ] -- bool )
   {: cmd:ptr cmdu q :}
   cmd cmdu GJA-CMD? IF
      3 GJA-ARGC=
      1 SCRIPT-ARGV$ 2 SCRIPT-ARGV$ q execute
      GJA-TRUE exit
   THEN
   GJA-FALSE ;

: GJA-DISPATCH-THREE-ARG-ROW ( ptr u8 n [ ptr u8 n ptr u8 n ptr u8 n -- ] -- bool )
   {: cmd:ptr cmdu q :}
   cmd cmdu GJA-CMD? IF
      4 GJA-ARGC=
      1 SCRIPT-ARGV$ 2 SCRIPT-ARGV$ 3 SCRIPT-ARGV$ q execute
      GJA-TRUE exit
   THEN
   GJA-FALSE ;

: GJA-DISPATCH-FIVE-ARG-ROW ( ptr u8 n [ ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- ] -- bool )
   {: cmd:ptr cmdu q :}
   cmd cmdu GJA-CMD? IF
      6 GJA-ARGC=
      1 SCRIPT-ARGV$ 2 SCRIPT-ARGV$ 3 SCRIPT-ARGV$ 4 SCRIPT-ARGV$ 5 SCRIPT-ARGV$
      q execute
      GJA-TRUE exit
   THEN
   GJA-FALSE ;

: GJA-DISPATCH-ONE-FILE ( -- bool )
   s" json-lines-schema" [: GJA-JSON-LINES-SCHEMA ;]
   GJA-DISPATCH-ONE-FILE-ROW IF GJA-TRUE exit THEN
   s" json-one-schema" [: GJA-JSON-ONE-SCHEMA ;]
   GJA-DISPATCH-ONE-FILE-ROW IF GJA-TRUE exit THEN
   s" all-errors" [: GJA-ALL-ERRORS ;]
   GJA-DISPATCH-ONE-FILE-ROW IF GJA-TRUE exit THEN
   s" sarif" [: GJA-SARIF ;]
   GJA-DISPATCH-ONE-FILE-ROW IF GJA-TRUE exit THEN
   s" public-signatures" [: GJA-PUBLIC-SIGNATURES ;]
   GJA-DISPATCH-ONE-FILE-ROW IF GJA-TRUE exit THEN
   s" aot-stripped" [: GJA-AOT-STRIPPED ;]
   GJA-DISPATCH-ONE-FILE-ROW IF GJA-TRUE exit THEN
   s" aot-compact" [: GJA-AOT-COMPACT ;]
   GJA-DISPATCH-ONE-FILE-ROW IF GJA-TRUE exit THEN
   GJA-FALSE ;

: GJA-DISPATCH-TWO-ARG ( -- bool )
   s" diag-file-origin" [: GJA-DIAG-FILE-ORIGIN ;]
   GJA-DISPATCH-TWO-ARG-ROW IF GJA-TRUE exit THEN
   s" diag-repair-class" [: GJA-DIAG-REPAIR-CLASS ;]
   GJA-DISPATCH-TWO-ARG-ROW IF GJA-TRUE exit THEN
   s" repair-packet" [: GJA-REPAIR-PACKET ;]
   GJA-DISPATCH-TWO-ARG-ROW IF GJA-TRUE exit THEN
   GJA-FALSE ;

: GJA-DISPATCH-RETURN-STACK ( -- bool )
   s" diag-return-stack" [: GJA-DIAG-RETURN-STACK ;]
   GJA-DISPATCH-THREE-ARG-ROW IF GJA-TRUE exit THEN
   GJA-FALSE ;

: GJA-DISPATCH-ROW-EFFECT ( -- bool )
   s" diag-row-effect" [: GJA-DIAG-ROW-EFFECT ;]
   GJA-DISPATCH-FIVE-ARG-ROW IF GJA-TRUE exit THEN
   GJA-FALSE ;

: GJA-DISPATCH ( -- )
   SCRIPT-ARGC 2 < IF GJA-USAGE THEN
   GJA-DISPATCH-ONE-FILE IF exit THEN
   GJA-DISPATCH-TWO-ARG IF exit THEN
   GJA-DISPATCH-RETURN-STACK IF exit THEN
   GJA-DISPATCH-ROW-EFFECT IF exit THEN
   GJA-USAGE ;
