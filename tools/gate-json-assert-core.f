\ gate-json-assert-core.f - native JSON assertion library for gate tests.

require src/lib/error.f
require src/lib/fs.f
require src/lib/memory.f
require src/lib/span.f
require src/lib/json.f

$8000 constant GJA-IN-CAP
$100 constant GJA-LINE-MAX

create GJA-IN GJA-IN-CAP allot
TYPED-VARIABLE GJA-CURSOR JSON:cursor
create GJA-STR-BUF GJA-IN-CAP allot
GJA-LINE-MAX TYPED-BUFFER GJA-LINE-A ptr u8   \ one line start per captured line
create GJA-LINE-U GJA-LINE-MAX cells allot

variable GJA-FD
variable GJA-RD
variable GJA-LEN
TYPED-VARIABLE GJA-ROOT SPAN:span<u8>
TYPED-VARIABLE GJA-ARR SPAN:span<u8>
variable GJA-I
variable GJA-N
variable GJA-OK1
variable GJA-OK2
variable GJA-LINE#
variable GJA-LS
variable GJA-LX

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
   s" usage: tools/gate-json-assert.f public-signatures FILE" GJA-FAIL ;

: GJA-ARGC= ( n -- ) {: n :}
   SCRIPT-ARGC n <> IF GJA-USAGE THEN ;

: GJA-BYTES= ( ptr u8 n ptr u8 n -- bool )
   {: a:ptr u b:ptr v :}
   u v <> IF GJA-FALSE exit THEN
   0 begin dup u < while
      dup a + c@ over b + c@ <> IF drop GJA-FALSE exit THEN
      1+
   repeat drop GJA-TRUE ;

: GJA-CMD? ( ptr u8 n -- bool ) {: a:ptr u :}
   0 SCRIPT-ARGV$ a u GJA-BYTES= ;


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
   FS:>PATHZ 0 0 open GJA-FD !
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

: GJA-OPEN ( SPAN:span<u8> -- JSON:reader )
   SPAN:$ GJA-CURSOR -rot JSON:READ-INIT JSON:READ-NEXT drop ;

\ Turn the current token into its borrowed source value, including quotes for
\ strings. A container is walked to its close; a scalar keeps the cursor.
: GJA-VALUE ( JSON:reader -- JSON:reader SPAN:span<u8> )
   JSON:READ-TOKEN {: kind:n :}
   kind JSON:T-OBJ = kind JSON:T-ARR = or if JSON:READ-VALUE-SPAN$ SPAN:MAKE exit then
   JSON:READ-SPAN$ {: a:ptr u:n :}
   kind JSON:T-STR = if a 1- u 2 + SPAN:MAKE exit then
   a u SPAN:MAKE ;

: GJA-PARSE ( ptr u8 n -- SPAN:span<u8> )
   {: a:ptr u:n :}
   GJA-CURSOR a u JSON:READ-INIT JSON:READ-NEXT drop
   JSON:READ-SKIP-VALUE JSON:READ-NEXT JSON:T-END <> if E-JSON-READ-TRAILING throw then JSON:READ-CLOSE
   a u SPAN:MAKE ;

: GJA-PARSE-FILE ( ptr u8 n -- SPAN:span<u8> )
   GJA-READ GJA-PARSE ;

: GJA-KIND ( SPAN:span<u8> -- n )
   GJA-OPEN JSON:READ-TOKEN swap JSON:READ-CLOSE ;

: GJA-FIELD? ( SPAN:span<u8> ptr u8 n -- SPAN:span<u8> bool )
   {: value:SPAN:span<u8> key:ptr keyu:n :}
   value GJA-OPEN key keyu JSON:READ-FIND-KEY
   if GJA-VALUE swap JSON:READ-CLOSE true exit then
   JSON:READ-CLOSE value false ;

: GJA-REQ ( SPAN:span<u8> ptr u8 n -- SPAN:span<u8> )
   GJA-FIELD? 0= if drop s" missing JSON field" GJA-FAIL then ;

: GJA-ARR@ ( SPAN:span<u8> n -- SPAN:span<u8> )
   {: arr:SPAN:span<u8> idx:n :}
   idx 0 < if s" missing JSON array item" GJA-FAIL then
   arr GJA-OPEN JSON:READ-TOKEN JSON:T-ARR <> if JSON:READ-CLOSE s" expected JSON array" GJA-FAIL then
   idx 0 ?do JSON:READ-NEXT dup JSON:T-ARR-END = if drop JSON:READ-CLOSE s" missing JSON array item" GJA-FAIL then
      drop JSON:READ-SKIP-VALUE
   loop
   JSON:READ-NEXT dup JSON:T-ARR-END = if drop JSON:READ-CLOSE s" missing JSON array item" GJA-FAIL then
   drop GJA-VALUE swap JSON:READ-CLOSE ;

: GJA-COUNT ( SPAN:span<u8> -- n )
   GJA-OPEN JSON:READ-TOKEN JSON:T-ARR <> if JSON:READ-CLOSE s" expected JSON array" GJA-FAIL then
   0 GJA-N !
   begin JSON:READ-NEXT dup JSON:T-ARR-END <> while
      drop JSON:READ-SKIP-VALUE GJA-N @ 1+ GJA-N !
   repeat drop JSON:READ-CLOSE GJA-N @ ;

: GJA-OBJ ( SPAN:span<u8> -- )
   GJA-KIND JSON:T-OBJ <> IF s" expected JSON object" GJA-FAIL THEN ;

: GJA-ARR-KIND ( SPAN:span<u8> -- )
   GJA-KIND JSON:T-ARR <> IF s" expected JSON array" GJA-FAIL THEN ;

: GJA-INT ( SPAN:span<u8> -- n )
   GJA-OPEN JSON:READ-TOKEN JSON:T-INT <> if JSON:READ-CLOSE s" expected JSON integer" GJA-FAIL then
   JSON:READ-INT {: value:n :} JSON:READ-CLOSE
   value 0 < if s" expected unsigned JSON integer" GJA-FAIL then value ;


: GJA-STR= ( SPAN:span<u8> ptr u8 n -- bool )
   {: value:SPAN:span<u8> want:ptr wantu:n :}
   value GJA-OPEN JSON:READ-TOKEN JSON:T-STR <> if JSON:READ-CLOSE GJA-FALSE exit then
   want wantu JSON:READ-STR-EQ? swap JSON:READ-CLOSE ;

: GJA-ASSERT-STR ( SPAN:span<u8> ptr u8 n -- )
   GJA-STR= 0= IF s" unexpected JSON string" GJA-FAIL THEN ;

: GJA-STRING-INTO ( SPAN:span<u8> ptr u8 n -- ptr u8 n )
   {: value:SPAN:span<u8> dst:ptr cap:n :}
   value GJA-OPEN JSON:READ-TOKEN JSON:T-STR <> if JSON:READ-CLOSE s" expected JSON string" GJA-FAIL then
   dst cap JSON:READ-STR {: u:n :} JSON:READ-CLOSE dst u ;

: GJA-STRING$ ( SPAN:span<u8> -- ptr u8 n )
   GJA-STR-BUF GJA-IN-CAP GJA-STRING-INTO ;

: GJA-NONEMPTY-STR ( SPAN:span<u8> -- )
   GJA-STRING$ nip 0= IF s" expected nonempty JSON string" GJA-FAIL THEN ;

: GJA-REQ-STRF ( SPAN:span<u8> ptr u8 n -- )
   GJA-REQ GJA-STRING$ 2drop ;

: GJA-REQ-INTF ( SPAN:span<u8> ptr u8 n -- )
   GJA-REQ GJA-INT drop ;


: GJA-SCHEMA1 ( SPAN:span<u8> -- )
   s" schema_version" GJA-REQ GJA-INT 1 <> IF s" schema_version is not 1" GJA-FAIL THEN ;

: GJA-ASSERT-INT-FIELD ( SPAN:span<u8> ptr u8 n n -- )
   {: root:SPAN:span<u8> key:ptr keyu want :}
   root key keyu GJA-REQ GJA-INT want <> IF s" unexpected JSON integer field" GJA-FAIL THEN ;

: GJA-HAS? ( SPAN:span<u8> ptr u8 n -- bool )
   GJA-FIELD? {: value:SPAN:span<u8> found:bool :} found ;

: GJA-NO-FIELD ( SPAN:span<u8> ptr u8 n -- )
   GJA-HAS? IF s" unexpected JSON field" GJA-FAIL THEN ;

: GJA-FORM-KEY? ( JSON:reader -- JSON:reader bool )
   s" severity" JSON:READ-STR-EQ? IF GJA-TRUE exit THEN
   s" code" JSON:READ-STR-EQ? IF GJA-TRUE exit THEN
   s" message" JSON:READ-STR-EQ? IF GJA-TRUE exit THEN
   s" file" JSON:READ-STR-EQ? IF GJA-TRUE exit THEN
   s" line" JSON:READ-STR-EQ? IF GJA-TRUE exit THEN
   s" column" JSON:READ-STR-EQ? IF GJA-TRUE exit THEN
   s" byte_start" JSON:READ-STR-EQ? IF GJA-TRUE exit THEN
   s" byte_end" JSON:READ-STR-EQ? IF GJA-TRUE exit THEN
   s" token" JSON:READ-STR-EQ? IF GJA-TRUE exit THEN
   s" source_line" JSON:READ-STR-EQ? IF GJA-TRUE exit THEN
   s" label" JSON:READ-STR-EQ? IF GJA-TRUE exit THEN
   s" evaluated" JSON:READ-STR-EQ? IF GJA-TRUE exit THEN
   s" notes" JSON:READ-STR-EQ? IF GJA-TRUE exit THEN
   s" help" JSON:READ-STR-EQ? ;

: GJA-EVALUATED-KEY? ( JSON:reader -- JSON:reader bool )
   s" source_line" JSON:READ-STR-EQ? IF GJA-TRUE exit THEN
   s" column" JSON:READ-STR-EQ? IF GJA-TRUE exit THEN
   s" token" JSON:READ-STR-EQ? ;

\ Fails naming the key the reader is on.
: GJA-KEY-FAIL ( JSON:reader -- )
   s" unexpected JSON field " {: p:ptr pu:n :}
   p GJA-STR-BUF pu BYTE-COPY
   GJA-STR-BUF pu + GJA-IN-CAP pu - JSON:READ-STR {: u:n :} JSON:READ-CLOSE
   GJA-STR-BUF pu u + GJA-FAIL ;

\ Fails unless key? accepts each key of the object.
: GJA-KEYS ( SPAN:span<u8> [ JSON:reader -- JSON:reader bool ] -- )
   {: obj:SPAN:span<u8> key? :}
   obj GJA-OPEN
   begin JSON:READ-NEXT dup JSON:T-OBJ-END <> while
      drop key? execute 0= IF GJA-KEY-FAIL THEN
      JSON:READ-NEXT drop JSON:READ-SKIP-VALUE
   repeat drop JSON:READ-CLOSE ;

: GJA-FORM-KEYS ( SPAN:span<u8> -- )
   [: GJA-FORM-KEY? ;] GJA-KEYS ;

: GJA-EVALUATED ( SPAN:span<u8> -- ) {: ev:SPAN:span<u8> :}
   ev GJA-OBJ
   ev s" source_line" GJA-REQ-STRF
   ev s" column" GJA-REQ GJA-INT 1 < IF s" evaluated column is below 1" GJA-FAIL THEN
   ev s" token" GJA-REQ-STRF
   ev [: GJA-EVALUATED-KEY? ;] GJA-KEYS ;

: GJA-PLACED ( SPAN:span<u8> -- ) {: root:SPAN:span<u8> :}
   root s" file" GJA-REQ GJA-NONEMPTY-STR
   root s" line" GJA-REQ-INTF
   root s" column" GJA-REQ-INTF
   root s" byte_start" GJA-REQ GJA-INT
   root s" byte_end" GJA-REQ GJA-INT
   > IF s" byte_start is past byte_end" GJA-FAIL THEN
   root s" token" GJA-REQ-STRF
   root s" source_line" GJA-REQ-STRF
   root s" label" GJA-REQ-STRF
   root s" evaluated" GJA-HAS? IF root s" evaluated" GJA-REQ GJA-EVALUATED THEN ;

: GJA-UNPLACED ( SPAN:span<u8> -- ) {: root:SPAN:span<u8> :}
   root s" column" GJA-NO-FIELD
   root s" byte_start" GJA-NO-FIELD
   root s" byte_end" GJA-NO-FIELD
   root s" source_line" GJA-NO-FIELD
   root s" evaluated" GJA-NO-FIELD ;

\ A record in REJECTION's JSON form (src/core/rejection.f): severity, code and
\ message; a record placed at a line also has file, column, byte_start no
\ greater than byte_end, token, source_line and label, and may have evaluated,
\ an object of exactly source_line, column from 1 and token; notes is an object
\ and help a string; no other key.
: GJA-RECORD-FORM ( SPAN:span<u8> -- ) {: root:SPAN:span<u8> :}
   root GJA-OBJ
   root s" severity" GJA-REQ dup s" error" GJA-STR= swap s" warning" GJA-STR= or
   0= IF s" severity is neither error nor warning" GJA-FAIL THEN
   root s" code" GJA-REQ GJA-NONEMPTY-STR
   root s" message" GJA-REQ-STRF
   root s" line" GJA-HAS? IF root GJA-PLACED ELSE root GJA-UNPLACED THEN
   root s" notes" GJA-HAS? IF root s" notes" GJA-REQ GJA-OBJ THEN
   root s" help" GJA-HAS? IF root s" help" GJA-REQ-STRF THEN
   root GJA-FORM-KEYS ;

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
   GJA-ROOT @ s" definitions" GJA-REQ dup GJA-ARR-KIND GJA-ARR !
   0 GJA-I !
   begin GJA-I @ GJA-ARR @ GJA-COUNT < while
      GJA-ARR @ GJA-I @ GJA-ARR@ GJA-CHECK-PUBLIC-ITEM
      GJA-I @ 1+ GJA-I !
   repeat
   GJA-OK1 @ 0= IF s" missing SQUARE public signature" GJA-FAIL THEN
   GJA-OK2 @ 0= IF s" missing APPLY public signature" GJA-FAIL THEN ;

: GJA-DISPATCH-ONE-FILE-ROW ( ptr u8 n [ ptr u8 n -- ] -- bool )
   {: cmd:ptr cmdu q :}
   cmd cmdu GJA-CMD? IF
      2 GJA-ARGC=
      1 SCRIPT-ARGV$ q execute
      GJA-TRUE exit
   THEN
   GJA-FALSE ;

: GJA-DISPATCH ( -- )
   SCRIPT-ARGC 2 < IF GJA-USAGE THEN
   s" public-signatures" [: GJA-PUBLIC-SIGNATURES ;]
   GJA-DISPATCH-ONE-FILE-ROW IF exit THEN
   GJA-USAGE ;
