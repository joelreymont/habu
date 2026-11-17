\ gate-json-assert-core.f - native JSON assertion library for gate tests.

require src/lib/error.f
require src/lib/memory.f
require src/lib/span.f
require src/lib/json.f
require src/core/exit-status.f

$8000 constant GJA-IN-CAP
$100 constant GJA-LINE-MAX

TYPED-VARIABLE GJA-CURSOR JSON:cursor
create GJA-STR-BUF GJA-IN-CAP allot
GJA-LINE-MAX TYPED-BUFFER GJA-LINE-A ^u8   \ one line start per captured line
create GJA-LINE-U GJA-LINE-MAX cells allot

variable GJA-LINE#
variable GJA-LS
variable GJA-LX

: GJA-TRUE ( -- bool )
   0 0= ;

: GJA-FALSE ( -- bool )
   GJA-TRUE 0= ;

: GJA-LINE-A@ ( n -- ^u8 )
   GJA-LINE-A @ ;

: GJA-LINE-A! ( ^u8 n -- )
   GJA-LINE-A ! ;

: GJA-FAIL ( ^u8 n -- )
   EXIT-STATUS:FAILURE die ;

: GJA-LINE! ( ^u8 n n -- )
   {: a:^ u k :}
   a k GJA-LINE-A!
   u k cells GJA-LINE-U + ! ;

: GJA-LINE$ ( n -- ^u8 n )
   dup GJA-LINE-A@
   swap cells GJA-LINE-U + @ ;

: GJA-LINE+ ( ^u8 n -- )
   GJA-LINE# @ GJA-LINE-MAX >= IF s" too many JSON lines" GJA-FAIL THEN
   dup 0 > IF
      2dup + 1- c@ 13 = IF 1- THEN
   THEN
   GJA-LINE# @ GJA-LINE!
   GJA-LINE# @ 1+ GJA-LINE# ! ;

: GJA-SPLIT-LINES ( ^u8 n -- )
   {: a:^ u :}
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

: GJA-OPEN ( SPAN:span<u8> -- JSON:reader )
   SPAN:$ GJA-CURSOR -rot JSON:READ-INIT JSON:READ-NEXT drop ;

\ Turn the current token into its borrowed source value, including quotes for
\ strings. A container is walked to its close; a scalar keeps the cursor.
: GJA-VALUE ( JSON:reader -- JSON:reader SPAN:span<u8> )
   JSON:READ-TOKEN {: kind:n :}
   kind JSON:T-OBJ = kind JSON:T-ARR = or if JSON:READ-VALUE-SPAN$ SPAN:MAKE exit then
   JSON:READ-SPAN$ {: a:^ u:n :}
   kind JSON:T-STR = if a 1- u 2 + SPAN:MAKE exit then
   a u SPAN:MAKE ;

: GJA-PARSE ( ^u8 n -- SPAN:span<u8> )
   {: a:^ u:n :}
   GJA-CURSOR a u JSON:READ-INIT JSON:READ-NEXT drop
   JSON:READ-SKIP-VALUE JSON:READ-NEXT JSON:T-END <> if E-JSON-READ-TRAILING throw then JSON:READ-CLOSE
   a u SPAN:MAKE ;

: GJA-KIND ( SPAN:span<u8> -- n )
   GJA-OPEN JSON:READ-TOKEN swap JSON:READ-CLOSE ;

: GJA-FIELD? ( SPAN:span<u8> ^u8 n -- SPAN:span<u8> bool )
   {: value:SPAN:span<u8> key:^ keyu:n :}
   value GJA-OPEN key keyu JSON:READ-FIND-KEY
   if GJA-VALUE swap JSON:READ-CLOSE true exit then
   JSON:READ-CLOSE value false ;

: GJA-REQ ( SPAN:span<u8> ^u8 n -- SPAN:span<u8> )
   GJA-FIELD? 0= if drop s" missing JSON field" GJA-FAIL then ;

: GJA-OBJ ( SPAN:span<u8> -- )
   GJA-KIND JSON:T-OBJ <> IF s" expected JSON object" GJA-FAIL THEN ;

: GJA-INT ( SPAN:span<u8> -- n )
   GJA-OPEN JSON:READ-TOKEN JSON:T-INT <> if JSON:READ-CLOSE s" expected JSON integer" GJA-FAIL then
   JSON:READ-INT {: value:n :} JSON:READ-CLOSE
   value 0 < if s" expected unsigned JSON integer" GJA-FAIL then value ;


: GJA-STR= ( SPAN:span<u8> ^u8 n -- bool )
   {: value:SPAN:span<u8> want:^ wantu:n :}
   value GJA-OPEN JSON:READ-TOKEN JSON:T-STR <> if JSON:READ-CLOSE GJA-FALSE exit then
   want wantu JSON:READ-STR-EQ? swap JSON:READ-CLOSE ;

: GJA-ASSERT-STR ( SPAN:span<u8> ^u8 n -- )
   GJA-STR= 0= IF s" unexpected JSON string" GJA-FAIL THEN ;

: GJA-STRING-INTO ( SPAN:span<u8> ^u8 n -- ^u8 n )
   {: value:SPAN:span<u8> dst:^ cap:n :}
   value GJA-OPEN JSON:READ-TOKEN JSON:T-STR <> if JSON:READ-CLOSE s" expected JSON string" GJA-FAIL then
   dst cap JSON:READ-STR {: u:n :} JSON:READ-CLOSE dst u ;

: GJA-STRING$ ( SPAN:span<u8> -- ^u8 n )
   GJA-STR-BUF GJA-IN-CAP GJA-STRING-INTO ;

: GJA-NONEMPTY-STR ( SPAN:span<u8> -- )
   GJA-STRING$ nip 0= IF s" expected nonempty JSON string" GJA-FAIL THEN ;

: GJA-REQ-STRF ( SPAN:span<u8> ^u8 n -- )
   GJA-REQ GJA-STRING$ 2drop ;

: GJA-REQ-INTF ( SPAN:span<u8> ^u8 n -- )
   GJA-REQ GJA-INT drop ;

: GJA-ASSERT-INT-FIELD ( SPAN:span<u8> ^u8 n n -- )
   {: root:SPAN:span<u8> key:^ keyu want :}
   root key keyu GJA-REQ GJA-INT want <> IF s" unexpected JSON integer field" GJA-FAIL THEN ;

: GJA-HAS? ( SPAN:span<u8> ^u8 n -- bool )
   GJA-FIELD? {: value:SPAN:span<u8> found:bool :} found ;

: GJA-NO-FIELD ( SPAN:span<u8> ^u8 n -- )
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
   s" unexpected JSON field " {: p:^ pu:n :}
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
