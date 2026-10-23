\ gate-json-assert-core.f - native JSON assertion library for gate tests.

require src/lib/errors.f
require src/lib/memory.f
require src/lib/span.f
require src/lib/json-read.f
require tools/diag-code.f

$8000 constant GJA-IN-CAP
$1000 constant GJA-SRC-CAP
PATH-CAP 1 + constant GJA-PATH-CAP
$100 constant GJA-LINE-MAX

create GJA-IN GJA-IN-CAP allot
create GJA-SRC GJA-SRC-CAP allot
create GJA-PATH GJA-PATH-CAP allot
create GJA-JR JR:STORAGE-BYTES allot
create GJA-STR-BUF GJA-IN-CAP allot
create GJA-CLASS-BUF GJA-IN-CAP allot
GJA-LINE-MAX TYPED-BUFFER GJA-LINE-A ptr u8   \ one line start per captured line
create GJA-LINE-U GJA-LINE-MAX cells allot

variable GJA-FD
variable GJA-RD
variable GJA-LEN
TYPED-VARIABLE GJA-ROOT SPAN:span<u8>
TYPED-VARIABLE GJA-ARR SPAN:span<u8>
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

: GJA-LINE-A@ ( n -- ptr u8 )
   GJA-LINE-A @ ;

: GJA-LINE-A! ( ptr u8 n -- )
   GJA-LINE-A ! ;

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

: GJA-MAP-ROW ( ptr u8 n ptr u8 n ptr u8 n -- ptr u8 n bool )
   {: in:ptr inu key:ptr keyu value:ptr valueu :}
   in inu key keyu GJA-BYTES= IF value valueu GJA-TRUE exit THEN
   in inu GJA-FALSE ;

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

: GJA-OPEN ( SPAN:span<u8> -- JR:reader )
   SPAN:$ GJA-JR JR:STORAGE-BYTES 2swap JR:INIT JR:NEXT drop ;

\ Turn the current token into its borrowed source value, including quotes for
\ strings. A container is walked to its close; a scalar keeps the cursor.
: GJA-VALUE ( JR:reader -- JR:reader SPAN:span<u8> )
   JR:TOKEN {: kind:n :}
   kind JR:T-OBJ = kind JR:T-ARR = or if JR:VALUE-SPAN$ SPAN:MAKE exit then
   JR:SPAN$ {: a:ptr u:n :}
   kind JR:T-STR = if a 1- u 2 + SPAN:MAKE exit then
   a u SPAN:MAKE ;

: GJA-PARSE ( ptr u8 n -- SPAN:span<u8> )
   {: a:ptr u:n :}
   GJA-JR JR:STORAGE-BYTES a u JR:INIT JR:NEXT drop
   JR:SKIP-VALUE JR:NEXT JR:T-END <> if E-JR-TRAILING throw then JR:CLOSE
   a u SPAN:MAKE ;

: GJA-PARSE-FILE ( ptr u8 n -- SPAN:span<u8> )
   GJA-READ GJA-PARSE ;

: GJA-KIND ( SPAN:span<u8> -- n )
   GJA-OPEN JR:TOKEN swap JR:CLOSE ;

: GJA-FIELD? ( SPAN:span<u8> ptr u8 n -- SPAN:span<u8> bool )
   {: value:SPAN:span<u8> key:ptr keyu:n :}
   value GJA-OPEN key keyu JR:FIND-KEY
   if GJA-VALUE swap JR:CLOSE true exit then
   JR:CLOSE value false ;

: GJA-REQ ( SPAN:span<u8> ptr u8 n -- SPAN:span<u8> )
   GJA-FIELD? 0= if drop s" missing JSON field" GJA-FAIL then ;

: GJA-ARR@ ( SPAN:span<u8> n -- SPAN:span<u8> )
   {: arr:SPAN:span<u8> idx:n :}
   idx 0 < if s" missing JSON array item" GJA-FAIL then
   arr GJA-OPEN JR:TOKEN JR:T-ARR <> if JR:CLOSE s" expected JSON array" GJA-FAIL then
   idx 0 ?do JR:NEXT dup JR:T-ARR-END = if drop JR:CLOSE s" missing JSON array item" GJA-FAIL then
      drop JR:SKIP-VALUE
   loop
   JR:NEXT dup JR:T-ARR-END = if drop JR:CLOSE s" missing JSON array item" GJA-FAIL then
   drop GJA-VALUE swap JR:CLOSE ;

: GJA-COUNT ( SPAN:span<u8> -- n )
   GJA-OPEN JR:TOKEN JR:T-ARR <> if JR:CLOSE s" expected JSON array" GJA-FAIL then
   0 GJA-N !
   begin JR:NEXT dup JR:T-ARR-END <> while
      drop JR:SKIP-VALUE GJA-N @ 1+ GJA-N !
   repeat drop JR:CLOSE GJA-N @ ;

: GJA-OBJ ( SPAN:span<u8> -- )
   GJA-KIND JR:T-OBJ <> IF s" expected JSON object" GJA-FAIL THEN ;

: GJA-ARR-KIND ( SPAN:span<u8> -- )
   GJA-KIND JR:T-ARR <> IF s" expected JSON array" GJA-FAIL THEN ;

: GJA-INT ( SPAN:span<u8> -- n )
   GJA-OPEN JR:TOKEN JR:T-INT <> if JR:CLOSE s" expected JSON integer" GJA-FAIL then
   JR:INT {: value:n :} JR:CLOSE
   value 0 < if s" expected unsigned JSON integer" GJA-FAIL then value ;

: GJA-SIGNED-INT ( SPAN:span<u8> -- )
   GJA-OPEN JR:TOKEN JR:T-INT <> if JR:CLOSE s" expected JSON integer" GJA-FAIL then
   JR:INT drop JR:CLOSE ;

: GJA-STR= ( SPAN:span<u8> ptr u8 n -- bool )
   {: value:SPAN:span<u8> want:ptr wantu:n :}
   value GJA-OPEN JR:TOKEN JR:T-STR <> if JR:CLOSE GJA-FALSE exit then
   want wantu JR:STR-EQ? swap JR:CLOSE ;

: GJA-ASSERT-STR ( SPAN:span<u8> ptr u8 n -- )
   GJA-STR= 0= IF s" unexpected JSON string" GJA-FAIL THEN ;

: GJA-STRING-INTO ( SPAN:span<u8> ptr u8 n -- ptr u8 n )
   {: value:SPAN:span<u8> dst:ptr cap:n :}
   value GJA-OPEN JR:TOKEN JR:T-STR <> if JR:CLOSE s" expected JSON string" GJA-FAIL then
   dst cap JR:STR {: u:n :} JR:CLOSE dst u ;

: GJA-STRING$ ( SPAN:span<u8> -- ptr u8 n )
   GJA-STR-BUF GJA-IN-CAP GJA-STRING-INTO ;

: GJA-NONEMPTY-STR ( SPAN:span<u8> -- )
   GJA-STRING$ nip 0= IF s" expected nonempty JSON string" GJA-FAIL THEN ;

: GJA-REQ-STRF ( SPAN:span<u8> ptr u8 n -- )
   GJA-REQ GJA-STRING$ 2drop ;

: GJA-REQ-INTF ( SPAN:span<u8> ptr u8 n -- )
   GJA-REQ GJA-INT drop ;

: GJA-NULL-OR-STR ( SPAN:span<u8> -- )
   dup GJA-KIND JR:T-NULL = IF drop exit THEN
   GJA-KIND JR:T-STR <> IF s" expected JSON string or null" GJA-FAIL THEN ;

: GJA-REQ-NULL-OR-STRF ( SPAN:span<u8> ptr u8 n -- )
   GJA-REQ GJA-NULL-OR-STR ;

: GJA-SCHEMA1 ( SPAN:span<u8> -- )
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
      GJA-PARSE dup GJA-OBJ GJA-SCHEMA1
      GJA-I @ 1+ GJA-I !
   repeat ;

: GJA-JSON-ONE-SCHEMA ( ptr u8 n -- )
   GJA-READ GJA-SPLIT-LINES
   GJA-LINE# @ 1 <> IF s" expected one JSON line" GJA-FAIL THEN
   0 GJA-LINE$ 2dup GJA-LINE-STARTS-OBJECT
   GJA-PARSE dup GJA-OBJ GJA-SCHEMA1 ;

: GJA-FIRST-JSON ( ptr u8 n -- SPAN:span<u8> )
   GJA-READ GJA-SPLIT-LINES
   GJA-LINE# @ 0= IF s" no JSON lines" GJA-FAIL THEN
   0 GJA-LINE$ 2dup GJA-LINE-STARTS-OBJECT
   GJA-PARSE dup GJA-OBJ ;

: GJA-LINE-JSON ( n -- SPAN:span<u8> )
   GJA-LINE$ 2dup GJA-LINE-STARTS-OBJECT
   GJA-PARSE dup GJA-OBJ ;

: GJA-DIAG-WORD? ( SPAN:span<u8> ptr u8 n -- bool ) {: root:SPAN:span<u8> word:ptr wordu :}
   root s" word" GJA-REQ word wordu GJA-STR= ;

: GJA-FIND-WORD ( ptr u8 n ptr u8 n -- SPAN:span<u8> ) {: json:ptr jsonu word:ptr wordu :}
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

: GJA-ASSERT-INT-FIELD ( SPAN:span<u8> ptr u8 n n -- )
   {: root:SPAN:span<u8> key:ptr keyu want :}
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

: GJA-DIAG-CLASS-FIELDS ( SPAN:span<u8> ptr u8 n -- ) {: root:SPAN:span<u8> class:ptr classu :}
   root s" repair_class" GJA-REQ
   class classu GJA-ASSERT-STR
   root s" suggestion" GJA-REQ GJA-NONEMPTY-STR ;

: GJA-DIAG-REPAIR-CLASS ( ptr u8 n ptr u8 n -- )
   {: json:ptr jsonu class:ptr classu :}
   json jsonu GJA-FIRST-JSON GJA-ROOT !
   GJA-ROOT @ class classu GJA-DIAG-CLASS-FIELDS ;

: GJA-DIAG-WORD-REPAIR-CLASS ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: json:ptr jsonu word:ptr wordu class:ptr classu :}
   json jsonu word wordu GJA-FIND-WORD GJA-ROOT !
   GJA-ROOT @ class classu GJA-DIAG-CLASS-FIELDS ;

: GJA-HAS? ( SPAN:span<u8> ptr u8 n -- bool )
   GJA-FIELD? {: value:SPAN:span<u8> found:bool :} found ;

: GJA-NO-FIELD ( SPAN:span<u8> ptr u8 n -- )
   GJA-HAS? IF s" unexpected JSON field" GJA-FAIL THEN ;

\ The fields only a definition carries.
: GJA-NO-DEF-FIELDS ( SPAN:span<u8> -- )
   dup s" token_index" GJA-NO-FIELD
   dup s" definition_source" GJA-NO-FIELD
   dup s" declared_effect" GJA-NO-FIELD
   dup s" declared_effect_source" GJA-NO-FIELD
   dup s" inferred_effect" GJA-NO-FIELD
   dup s" expected" GJA-NO-FIELD
   dup s" actual" GJA-NO-FIELD
   dup s" return_stack" GJA-NO-FIELD
   s" source_excerpt" GJA-NO-FIELD ;

\ None of a definition's own fields.
: GJA-NO-DEF ( SPAN:span<u8> -- )
   dup s" word" GJA-NO-FIELD
   GJA-NO-DEF-FIELDS ;

: GJA-NO-PLACE ( SPAN:span<u8> -- )
   dup s" line" GJA-NO-FIELD
   dup s" column" GJA-NO-FIELD
   dup s" byte_start" GJA-NO-FIELD
   s" byte_end" GJA-NO-FIELD ;

: GJA-DECL-NO-DEF ( SPAN:span<u8> -- )
   GJA-NO-DEF ;

\ A record's code names its shape, the repair classes it may carry and the
\ field a refused record adds (tools/diag-code.f).
: GJA-CODE$ ( SPAN:span<u8> -- ptr u8 n )
   s" code" GJA-REQ dup GJA-NONEMPTY-STR GJA-STRING$ ;

\ A declaration diagnostic carries all four position fields when its token
\ locates in the file, and none when it does not.
: GJA-DECL-POSITION ( SPAN:span<u8> -- )
   dup s" line" GJA-HAS? IF
      dup s" line" GJA-REQ-INTF
      dup s" column" GJA-REQ-INTF
      dup s" byte_start" GJA-REQ-INTF
      s" byte_end" GJA-REQ-INTF
   ELSE
      GJA-NO-PLACE
   THEN ;

: GJA-SHAPE ( SPAN:span<u8> -- DIAG-CODE:shape )
   GJA-CODE$ DIAG-CODE:SHAPE ;

: GJA-THROW? ( SPAN:span<u8> -- bool )
   GJA-CODE$ s" E-STATEMENT-THROW" GJA-BYTES= ;

: GJA-SPAN-FIELDS ( SPAN:span<u8> -- ) {: root:SPAN:span<u8> :}
   root GJA-NO-DEF
   root s" token" GJA-REQ GJA-NONEMPTY-STR
   root s" line" GJA-REQ-INTF
   root s" column" GJA-REQ-INTF
   root s" byte_start" GJA-REQ-INTF
   root s" byte_end" GJA-REQ-INTF ;

: GJA-INPUT-FIELDS ( SPAN:span<u8> -- ) {: root:SPAN:span<u8> :}
   root GJA-NO-DEF
   root s" token" GJA-NO-FIELD
   root s" byte_start" GJA-NO-FIELD
   root s" byte_end" GJA-NO-FIELD
   root s" line" GJA-REQ-INTF
   root s" column" GJA-REQ-INTF ;

\ The used packages a token resolves in: a non-empty array of package names.
: GJA-PACKAGES ( SPAN:span<u8> -- ) {: arr:SPAN:span<u8> :}
   arr GJA-ARR-KIND
   arr GJA-COUNT 0= IF s" empty used_packages" GJA-FAIL THEN
   arr GJA-COUNT 0 ?DO arr i GJA-ARR@ GJA-NONEMPTY-STR LOOP ;

\ A using refusal names the token, placed when it locates in the file, and
\ each used package it resolves in, with no definition field or throw code.
: GJA-USING-FIELDS ( SPAN:span<u8> -- ) {: root:SPAN:span<u8> :}
   root GJA-NO-DEF
   root s" throw_code" GJA-NO-FIELD
   root s" token" GJA-REQ GJA-NONEMPTY-STR
   root GJA-DECL-POSITION
   root s" used_packages" GJA-REQ GJA-PACKAGES ;

\ A refused checker record names the token it would have described, with the
\ field its code adds, and has no place for that token. The field is text, and
\ empty only as the signature of a row that stored none.
: GJA-RECORD-FIELDS ( SPAN:span<u8> -- ) {: root:SPAN:span<u8> :}
   root GJA-NO-DEF
   root GJA-DECL-POSITION
   root s" throw_code" GJA-NO-FIELD
   root s" token" GJA-REQ GJA-NONEMPTY-STR
   root GJA-CODE$ DIAG-CODE:EVIDENCE {: key:ptr keyu:n :}
   keyu 0= IF exit THEN
   root GJA-CODE$ s" E-BAD-STORED-SIGNATURE" GJA-BYTES= IF root key keyu GJA-REQ-STRF exit THEN
   root key keyu GJA-REQ GJA-NONEMPTY-STR ;

: GJA-REPAIR-HEAD ( SPAN:span<u8> ptr u8 n -- ) {: root:SPAN:span<u8> class:ptr classu:n :}
   root s" schema_version" 1 GJA-ASSERT-INT-FIELD
   root s" kind" GJA-REQ s" habu_repair_packet" GJA-ASSERT-STR
   root s" diagnostic_count" GJA-REQ GJA-INT 0 <= IF s" invalid diagnostic_count" GJA-FAIL THEN
   root s" code" GJA-REQ GJA-NONEMPTY-STR
   root s" file" GJA-REQ GJA-NONEMPTY-STR
   root class classu GJA-DIAG-CLASS-FIELDS ;

: GJA-REPAIR-DEF ( SPAN:span<u8> -- ) {: root:SPAN:span<u8> :}
   root s" word" GJA-REQ GJA-NONEMPTY-STR
   root s" token" GJA-REQ GJA-NONEMPTY-STR
   root s" token_index" GJA-REQ-INTF
   root s" line" GJA-REQ-INTF
   root s" column" GJA-REQ-INTF
   root s" byte_start" GJA-REQ-INTF
   root s" byte_end" GJA-REQ-INTF
   root s" declared_effect" GJA-REQ-NULL-OR-STRF
   root s" declared_effect_source" GJA-REQ-NULL-OR-STRF
   root s" inferred_effect" GJA-REQ GJA-NONEMPTY-STR
   root s" family" GJA-REQ-NULL-OR-STRF
   root s" reason" GJA-REQ-NULL-OR-STRF
   root s" return_stack" GJA-REQ GJA-OBJ
   root s" source_excerpt" GJA-REQ GJA-NONEMPTY-STR
   root s" instruction" GJA-REQ
   s" Fix the definition so it certifies. Output only corrected Habu code." GJA-ASSERT-STR ;

: GJA-REPAIR-DECL ( SPAN:span<u8> -- ) {: root:SPAN:span<u8> :}
   root GJA-DECL-NO-DEF
   root GJA-NO-PLACE
   root s" decl" GJA-REQ GJA-NONEMPTY-STR
   root s" family" GJA-REQ-STRF
   root s" token" GJA-REQ-STRF
   root s" reason" GJA-REQ GJA-NONEMPTY-STR
   root s" instruction" GJA-REQ
   s" Repair the type-family declaration. Output only corrected Habu code." GJA-ASSERT-STR ;

: GJA-REPAIR-THROW-CODE ( SPAN:span<u8> -- ) {: root:SPAN:span<u8> :}  \ null unless a statement threw
   root s" throw_code" GJA-REQ {: node:SPAN:span<u8> :}
   root GJA-THROW? IF node GJA-SIGNED-INT exit THEN
   node GJA-KIND JR:T-NULL <> IF s" expected JSON null" GJA-FAIL THEN ;

: GJA-REPAIR-SPAN ( SPAN:span<u8> -- ) {: root:SPAN:span<u8> :}
   root GJA-SPAN-FIELDS
   root GJA-REPAIR-THROW-CODE
   root s" instruction" GJA-REQ
   s" Fix the source at this token so it checks. Output only corrected Habu code." GJA-ASSERT-STR ;

: GJA-REPAIR-INPUT ( SPAN:span<u8> -- ) {: root:SPAN:span<u8> :}
   root GJA-INPUT-FIELDS
   root s" instruction" GJA-REQ
   s" Rebuild bin/hb to check this source; no code change answers this diagnostic." GJA-ASSERT-STR ;

: GJA-REPAIR-RECORD ( SPAN:span<u8> -- ) {: root:SPAN:span<u8> :}
   root GJA-RECORD-FIELDS
   root s" instruction" GJA-REQ
   s" Fix the statement that names this token so it loads. Output only corrected Habu code." GJA-ASSERT-STR ;

: GJA-REPAIR-USING ( SPAN:span<u8> -- ) {: root:SPAN:span<u8> :}
   root GJA-USING-FIELDS
   root s" instruction" GJA-REQ
   s" Qualify this token as PKG:WORD for the package word meant, or rename the collision. Output only corrected Habu code." GJA-ASSERT-STR ;

\ The repair class a storage record's reason takes: the renderer's eleven
\ reasons under their class, empty for a reason the renderer never writes.
: GJA-STORAGE-CLASS$ ( ptr u8 n -- ptr u8 n )
   s" unknown type" s" fix_storage_type" GJA-MAP-ROW IF exit THEN
   s" malformed type" s" fix_storage_type" GJA-MAP-ROW IF exit THEN
   s" type this definer cannot store" s" fix_storage_type" GJA-MAP-ROW IF exit THEN
   s" scheme in a stored type" s" fix_storage_type" GJA-MAP-ROW IF exit THEN
   s" no type for" s" fix_storage_type" GJA-MAP-ROW IF exit THEN
   s" more than one ':' in name" s" fix_storage_name" GJA-MAP-ROW IF exit THEN
   s" name in a sealed package" s" fix_storage_name" GJA-MAP-ROW IF exit THEN
   s" no name for" s" fix_storage_name" GJA-MAP-ROW IF exit THEN
   s" count outside the buffer's extent" s" fix_storage_count" GJA-MAP-ROW IF exit THEN
   s" count resolves to no ( -- n ) word" s" fix_storage_count" GJA-MAP-ROW IF exit THEN
   s" no count for" s" fix_storage_count" GJA-MAP-ROW IF exit THEN
   drop 0 ;

\ A storage record's class follows its reason, and its reason is one the
\ renderer writes.
: GJA-STORAGE-CLASS ( SPAN:span<u8> -- ) {: root:SPAN:span<u8> :}
   root s" reason" GJA-REQ GJA-STRING$ GJA-STORAGE-CLASS$ {: want:ptr wantu:n :}
   wantu 0= IF s" storage reason is not one the renderer writes" GJA-FAIL THEN
   root s" repair_class" GJA-REQ want wantu GJA-STR= 0= IF
      s" storage repair class does not follow its reason" GJA-FAIL
   THEN ;

\ A storage declaration its definer refuses (src/core/render.f STGR-JSON) names
\ the declared word, the refused token and the reason, under the class that
\ reason takes. Only a refusal the pre-pass read carries the token's place, so
\ it has all four place fields or none.
: GJA-STORAGE-FIELDS ( SPAN:span<u8> -- ) {: root:SPAN:span<u8> :}
   root GJA-NO-DEF-FIELDS
   root s" word" GJA-REQ GJA-NONEMPTY-STR
   root s" token" GJA-REQ GJA-NONEMPTY-STR
   root s" reason" GJA-REQ GJA-NONEMPTY-STR
   root GJA-STORAGE-CLASS
   root s" line" GJA-HAS? 0= IF root GJA-NO-PLACE exit THEN
   root s" line" GJA-REQ-INTF
   root s" column" GJA-REQ-INTF
   root s" byte_start" GJA-REQ-INTF
   root s" byte_end" GJA-REQ-INTF ;

: GJA-REPAIR-STORAGE ( SPAN:span<u8> -- ) {: root:SPAN:span<u8> :}
   root GJA-STORAGE-FIELDS
   root s" instruction" GJA-REQ
   s" Fix the storage declaration so its definer accepts it. Output only corrected Habu code." GJA-ASSERT-STR ;

: GJA-REPAIR-SHAPE ( SPAN:span<u8> -- ) {: root:SPAN:span<u8> :}
   root GJA-SHAPE MATCH DIAG-CODE:shape
      definition OF root GJA-REPAIR-DEF ENDOF
      declaration OF root GJA-REPAIR-DECL ENDOF
      storage OF root GJA-REPAIR-STORAGE ENDOF
      source-span OF root GJA-REPAIR-SPAN ENDOF
      deferral OF s" a deferral has no repair packet" GJA-FAIL ENDOF
      input OF root GJA-REPAIR-INPUT ENDOF
      record OF root GJA-REPAIR-RECORD ENDOF
      warning OF s" a warning has no repair packet" GJA-FAIL ENDOF
      using-refusal OF root GJA-REPAIR-USING ENDOF
   ;MATCH ;

: GJA-REPAIR-PACKET ( ptr u8 n ptr u8 n -- )
   {: json:ptr jsonu class:ptr classu :}
   json jsonu GJA-FIRST-JSON GJA-ROOT !
   GJA-ROOT @ class classu GJA-REPAIR-HEAD
   GJA-ROOT @ GJA-REPAIR-SHAPE ;

: GJA-DIAG-HEAD ( SPAN:span<u8> -- )
   dup GJA-SCHEMA1
   dup s" code" GJA-REQ GJA-NONEMPTY-STR
   dup s" repair_class" GJA-REQ GJA-NONEMPTY-STR
   dup s" verdict" GJA-REQ GJA-NONEMPTY-STR
   dup s" file" GJA-REQ GJA-NONEMPTY-STR
   s" suggestion" GJA-REQ GJA-NONEMPTY-STR ;

: GJA-DIAG-COMMON ( SPAN:span<u8> -- )
   dup GJA-DIAG-HEAD
   dup s" word" GJA-REQ GJA-NONEMPTY-STR
   dup s" token" GJA-REQ GJA-NONEMPTY-STR
   dup s" token_index" GJA-REQ-INTF
   dup s" line" GJA-REQ-INTF
   dup s" column" GJA-REQ-INTF
   dup s" byte_start" GJA-REQ-INTF
   dup s" byte_end" GJA-REQ-INTF
   dup s" definition_source" GJA-REQ GJA-NONEMPTY-STR
   dup s" declared_effect" GJA-REQ GJA-NONEMPTY-STR
   dup s" declared_effect_source" GJA-REQ GJA-NONEMPTY-STR
   dup s" inferred_effect" GJA-REQ GJA-NONEMPTY-STR
   dup s" return_stack" GJA-REQ dup GJA-OBJ
   dup s" expected" GJA-REQ-STRF
   s" actual" GJA-REQ-STRF
   drop ;

: GJA-DIAG-DECL ( SPAN:span<u8> -- ) {: root:SPAN:span<u8> :}
   root GJA-DIAG-HEAD
   root GJA-DECL-NO-DEF
   root GJA-DECL-POSITION
   root s" decl" GJA-REQ GJA-NONEMPTY-STR
   root s" family" GJA-REQ-STRF
   root s" token" GJA-REQ-STRF
   root s" reason" GJA-REQ GJA-NONEMPTY-STR
   root s" verdict" GJA-REQ s" rejected" GJA-STR= 0= IF
      s" declaration verdict is not rejected" GJA-FAIL
   THEN ;

: GJA-DIAG-VERDICT ( SPAN:span<u8> -- )
   s" verdict" GJA-REQ
   dup s" rejected" GJA-STR= IF drop exit THEN
   s" uncheckable" GJA-STR= 0= IF s" unexpected checker verdict" GJA-FAIL THEN ;

: GJA-DIAG-STORAGE ( SPAN:span<u8> -- ) {: root:SPAN:span<u8> :}
   root GJA-DIAG-HEAD
   root GJA-STORAGE-FIELDS
   root s" verdict" GJA-REQ s" rejected" GJA-STR= 0= IF
      s" storage verdict is not rejected" GJA-FAIL
   THEN ;

: GJA-DIAG-THROW-CODE ( SPAN:span<u8> -- ) {: root:SPAN:span<u8> :}  \ only a statement throw has one
   root GJA-THROW? IF root s" throw_code" GJA-REQ GJA-SIGNED-INT exit THEN
   root s" throw_code" GJA-NO-FIELD ;

: GJA-DIAG-SPAN ( SPAN:span<u8> -- ) {: root:SPAN:span<u8> :}
   root GJA-DIAG-HEAD
   root GJA-SPAN-FIELDS
   root GJA-DIAG-THROW-CODE
   root s" verdict" GJA-REQ s" rejected" GJA-STR= 0= IF
      s" span verdict is not rejected" GJA-FAIL
   THEN ;

\ A record's repair class is one its code names and has a suggestion.
: GJA-DIAG-CLASS ( SPAN:span<u8> -- ) {: root:SPAN:span<u8> :}
   root GJA-CODE$ {: code:ptr codeu:n :}
   root s" repair_class" GJA-REQ GJA-CLASS-BUF GJA-IN-CAP GJA-STRING-INTO
   {: class:ptr classu:n :}
   code codeu class classu DIAG-CODE:ADMITS? 0= IF
      s" repair class is not one its code names" GJA-FAIL
   THEN
   root class classu GJA-DIAG-CLASS-FIELDS ;

\ A deferral is no refusal: the source pre-pass left the stretch that opens at
\ its token to the run. It is placed as a span is, under the verdict deferred
\ and the repair class its code names.
: GJA-DIAG-DEFERRAL ( SPAN:span<u8> -- ) {: root:SPAN:span<u8> :}
   root GJA-DIAG-HEAD
   root GJA-SPAN-FIELDS
   root s" throw_code" GJA-NO-FIELD
   root s" verdict" GJA-REQ s" deferred" GJA-STR= 0= IF
      s" deferral verdict is not deferred" GJA-FAIL
   THEN
   root GJA-DIAG-CLASS ;

: GJA-DIAG-INPUT ( SPAN:span<u8> -- ) {: root:SPAN:span<u8> :}
   root GJA-DIAG-HEAD
   root GJA-INPUT-FIELDS
   root s" verdict" GJA-REQ s" uncheckable" GJA-ASSERT-STR ;

: GJA-DIAG-RECORD ( SPAN:span<u8> -- ) {: root:SPAN:span<u8> :}
   root GJA-DIAG-HEAD
   root GJA-RECORD-FIELDS
   root s" verdict" GJA-REQ s" rejected" GJA-STR= 0= IF
      s" record verdict is not rejected" GJA-FAIL
   THEN ;

\ A warning is no refusal: the definition loaded and the run's status stands. It
\ names the word and why, and has no verdict, repair class or suggestion.
: GJA-DIAG-WARNING ( SPAN:span<u8> -- ) {: root:SPAN:span<u8> :}
   root GJA-SCHEMA1
   root s" word" GJA-REQ GJA-NONEMPTY-STR
   root s" file" GJA-REQ GJA-NONEMPTY-STR
   root GJA-DECL-POSITION
   root s" reason" GJA-REQ GJA-NONEMPTY-STR
   root s" verdict" GJA-NO-FIELD
   root s" repair_class" GJA-NO-FIELD
   root s" suggestion" GJA-NO-FIELD ;

: GJA-DIAG-USING ( SPAN:span<u8> -- ) {: root:SPAN:span<u8> :}
   root GJA-DIAG-HEAD
   root GJA-USING-FIELDS
   root s" verdict" GJA-REQ s" rejected" GJA-STR= 0= IF
      s" using verdict is not rejected" GJA-FAIL
   THEN ;

: GJA-DIAG-SHAPE ( SPAN:span<u8> -- ) {: root:SPAN:span<u8> :}
   root GJA-SHAPE MATCH DIAG-CODE:shape
      definition OF root GJA-DIAG-COMMON ENDOF
      declaration OF root GJA-DIAG-DECL ENDOF
      storage OF root GJA-DIAG-STORAGE ENDOF
      source-span OF root GJA-DIAG-SPAN ENDOF
      deferral OF root GJA-DIAG-DEFERRAL ENDOF
      input OF root GJA-DIAG-INPUT ENDOF
      record OF root GJA-DIAG-RECORD ENDOF
      warning OF root GJA-DIAG-WARNING ENDOF
      using-refusal OF root GJA-DIAG-USING ENDOF
   ;MATCH ;

\ Each record in the shape its code names; a refusal also under a verdict and
\ one of the repair classes its code names, with a nonempty suggestion.
: GJA-DIAG-CONTRACT-ROW ( SPAN:span<u8> -- ) {: root:SPAN:span<u8> :}
   root GJA-DIAG-SHAPE
   root GJA-CODE$ DIAG-CODE:REFUSAL? 0= IF exit THEN
   root GJA-DIAG-VERDICT
   root GJA-DIAG-CLASS ;

: GJA-DIAG-CONTRACT ( ptr u8 n -- )
   GJA-READ GJA-SPLIT-LINES
   GJA-LINE# @ 0= IF s" no JSON diagnostics" GJA-FAIL THEN
   0 GJA-I !
   begin GJA-I @ GJA-LINE# @ < while
      GJA-I @ GJA-LINE$ 2dup GJA-LINE-STARTS-OBJECT
      GJA-PARSE dup GJA-OBJ GJA-DIAG-CONTRACT-ROW
      GJA-I @ 1+ GJA-I !
   repeat ;

: GJA-DIAG-DSTACK ( SPAN:span<u8> -- )
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

: GJA-ALL-ROW0 ( SPAN:span<u8> -- )
   dup GJA-DIAG-COMMON
   dup s" word" GJA-REQ s" gdx-ae-bad1" GJA-ASSERT-STR
   dup s" code" GJA-REQ s" E-MISMATCH" GJA-ASSERT-STR
   dup s" repair_class" GJA-REQ s" remove_producer" GJA-ASSERT-STR
   GJA-DIAG-DSTACK ;

: GJA-ALL-ROW1 ( SPAN:span<u8> -- )
   dup GJA-DIAG-COMMON
   dup s" word" GJA-REQ s" gdx-ae-bad2" GJA-ASSERT-STR
   dup s" code" GJA-REQ s" E-REJECTED" GJA-ASSERT-STR
   dup s" repair_class" GJA-REQ s" fix_return_stack" GJA-ASSERT-STR
   s" return_stack" GJA-REQ s" actual" GJA-REQ GJA-NONEMPTY-STR ;

: GJA-ALL-ERRORS ( ptr u8 n -- )
   GJA-READ GJA-SPLIT-LINES
   GJA-LINE# @ 2 <> IF s" expected two all-errors diagnostics" GJA-FAIL THEN
   0 GJA-LINE$ 2dup GJA-LINE-STARTS-OBJECT GJA-PARSE dup GJA-OBJ GJA-ALL-ROW0
   1 GJA-LINE$ 2dup GJA-LINE-STARTS-OBJECT GJA-PARSE dup GJA-OBJ GJA-ALL-ROW1 ;

: GJA-START-LINE-OK ( SPAN:span<u8> -- )
   s" locations" GJA-REQ dup GJA-ARR-KIND
   0 GJA-ARR@
   s" physicalLocation" GJA-REQ
   s" region" GJA-REQ
   s" startLine" GJA-REQ GJA-INT 0 <= IF s" SARIF result missing startLine" GJA-FAIL THEN ;

: GJA-SARIF ( ptr u8 n -- )
   GJA-PARSE-FILE GJA-ROOT !
   GJA-ROOT @ s" version" GJA-REQ s" 2.1.0" GJA-ASSERT-STR
   GJA-ROOT @ s" runs" GJA-REQ dup GJA-ARR-KIND 0 GJA-ARR@
   s" results" GJA-REQ dup GJA-ARR-KIND GJA-ARR !
   GJA-ARR @ GJA-COUNT 2 <> IF s" SARIF result count mismatch" GJA-FAIL THEN
   0 GJA-I !
   begin GJA-I @ GJA-ARR @ GJA-COUNT < while
      GJA-ARR @ GJA-I @ GJA-ARR@ GJA-START-LINE-OK
      GJA-I @ 1+ GJA-I !
   repeat ;

: GJA-CHECK-PUBLIC-ITEM ( SPAN:span<u8> -- )
   {: item:SPAN:span<u8> :}
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
   begin GJA-I @ GJA-ARR @ GJA-COUNT < while
      GJA-ARR @ GJA-I @ GJA-ARR@ GJA-CHECK-PUBLIC-ITEM
      GJA-I @ 1+ GJA-I !
   repeat
   GJA-OK1 @ 0= IF s" missing SQUARE public signature" GJA-FAIL THEN
   GJA-OK2 @ 0= IF s" missing APPLY public signature" GJA-FAIL THEN ;

: GJA-AOT-COMMON ( SPAN:span<u8> -- )
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
   s" diag-contract" [: GJA-DIAG-CONTRACT ;]
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
