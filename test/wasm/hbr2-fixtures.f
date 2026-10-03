\ hbr2-fixtures.f - the HBR2 wire goldens, read from lib/browser/hbr-v2-registry.json.
\
\ The registry is generated from HBR2's normative tables, and its `sources`
\ member names the table behind each member. This file pins what PA-r2 binds
\ to them (docs/wasm-backend.md §12.1 and §12.2): the 96-byte packet header and
\ 32-byte record header (HBR2 §24.2), the 128-byte control record, the language
\ status, result classes and submit results, the wrapper's two imports and six
\ exports (§24.5), the 136-byte canonical STOP packet (§28.1) built from the
\ registry, the ViewCallback limits (§4.2), the decoder limits (§24.6) and the
\ appendix's counts (§28.1). Offsets and sizes are decimal, as HBR2 spells them.
\
\ THE DIGEST. contentDigest is SHA-256 of the registry as sorted compact UTF-8
\ JSON without contentDigest (HBR2 §28.2), and PINNED$ holds it, so any registry
\ edit stays red until that line changes with it. The file holds exactly one
\ top-level contentDigest and stores the canonical form itself: every object's
\ keys in byte order, every key and string spelled plainly with no escape
\ sequence, no float, and so no control byte, which the reader refuses raw. One
\ pass re-emits the file compact through JSON-WRITE and counts each departure
\ instead of repairing it; an escape has no single canonical spelling, and the
\ lookups below compare a string's raw bytes.

require lib/test.f
require lib/string.f
require lib/fs.f
require lib/le.f
require lib/json-read.f
require lib/json-write.f

package HBR2-FIXTURES-TEST
private

: PINNED$ ( -- ptr u8 n )
   s" fdd071a5d0df682af409a9d4c2c7ae409b75554fa0602e3f06f790982dcc9677" ;

$40000 constant TEXT-CAP                \ the registry is about 160 KB
TEXT-CAP BUFFER: SRC
variable SRC-N
TEXT-CAP BUFFER: CANON
TYPED-VARIABLE OUT JSON-WRITE:writer
create SHA-CTX SHA256-CTX-BYTES allot
$20 BUFFER: DG
$40 BUFFER: DG-HEX
$100 constant STR-CAP
STR-CAP BUFFER: STR-BUF                 \ one decoded key or string

\ One reader per role, so a lookup never moves a walk: CANON-R re-emits, WALK-R
\ walks an array, LOOK-R finds a member, TALLY-R counts or renders a nested one.
create CANON-R JR:STORAGE-BYTES allot
create WALK-R JR:STORAGE-BYTES allot
create LOOK-R JR:STORAGE-BYTES allot
create TALLY-R JR:STORAGE-BYTES allot

: LOAD ( -- )
   s" lib/browser/hbr-v2-registry.json" SRC TEXT-CAP READ-ALL SRC-N ! ;

: REG$ ( -- ptr u8 n )
   SRC SRC-N @ ;

\ ---- canonical text ------------------------------------------------------------
8 constant MAX-DEPTH
$40 constant KEY-CAP
MAX-DEPTH TYPED-BUFFER KIND n           \ JR:T-OBJ or JR:T-ARR of each open container
MAX-DEPTH TYPED-BUFFER FIRST n          \ 1 until its first member or element
MAX-DEPTH TYPED-BUFFER KEYED bool       \ true once it has a key, contentDigest's too
MAX-DEPTH TYPED-BUFFER PREV-N n         \ the length of its last key
MAX-DEPTH KEY-CAP * BUFFER: PREV        \ its last key
variable DEPTH
variable DISORDER                       \ keys out of byte order
variable UNCANONICAL                    \ floats and escapes
variable DIGESTS                        \ top-level contentDigest members

: SEP ( -- )   \ a comma before each member or element but a container's first
   DEPTH @ FIRST @ 0<> if 0 DEPTH @ FIRST ! exit then
   OUT JSON-WRITE:COMMA drop ;

: ELEMENT ( -- )
   DEPTH @ KIND @ JR:T-ARR = if SEP then ;

: OPEN-LEVEL ( n -- ) {: kind:n :}
   DEPTH @ 1+ {: d:n :}
   d MAX-DEPTH >= if s" hbr2-fixtures: registry nested too deep" 1 die then
   d DEPTH !
   kind d KIND !
   1 d FIRST !
   false d KEYED ! ;

: CLOSE-LEVEL ( -- )
   DEPTH @ 1- DEPTH ! ;

: BEFORE? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n b:ptr v:n :}   \ a sorts first, bytewise
   u v min 0 ?do
      a i + c@ b i + c@ <> if a i + c@ b i + c@ < unloop exit then
   loop
   u v < ;

: LAST-KEY ( -- ptr u8 n )
   PREV DEPTH @ KEY-CAP * + DEPTH @ PREV-N @ ;

: ORDERED ( ptr u8 n -- ) {: k:ptr u:n :}
   u KEY-CAP > if s" hbr2-fixtures: registry key over KEY-CAP" 1 die then
   DEPTH @ KEYED @ if LAST-KEY k u BEFORE? 0= if 1 DISORDER +! then then
   k PREV DEPTH @ KEY-CAP * + u BYTE-COPY
   u DEPTH @ PREV-N !
   true DEPTH @ KEYED ! ;

: TEXT ( JR:reader -- JR:reader n )   \ an escaped key or string is uncanonical
   STR-BUF STR-CAP JR:STR {: u:n :}
   JR:SPAN$ STR-BUF u STR= 0= if 1 UNCANONICAL +! then
   u ;

: KEY ( JR:reader -- JR:reader )
   TEXT {: u:n :}
   STR-BUF u ORDERED
   DEPTH @ 1 = STR-BUF u s" contentDigest" STR= and if
      1 DIGESTS +! JR:NEXT drop JR:SKIP-VALUE exit
   then
   SEP
   OUT STR-BUF u JSON-WRITE:KEY drop ;

: SCALAR ( JR:reader n -- JR:reader ) {: kind:n :}
   ELEMENT
   kind JR:T-STR = if TEXT {: u:n :} OUT STR-BUF u JSON-WRITE:STRING drop exit then
   kind JR:T-INT = if JR:INT {: v:n :} OUT v JSON-WRITE:INT drop exit then
   kind JR:T-TRUE = if OUT true JSON-WRITE:BOOL drop exit then
   kind JR:T-FALSE = if OUT false JSON-WRITE:BOOL drop exit then
   kind JR:T-NULL = if OUT JSON-WRITE:NULL drop exit then
   1 UNCANONICAL +! ;

: STEP ( JR:reader -- JR:reader bool )   \ false at the end of the text
   JR:NEXT {: kind:n :}
   kind JR:T-END = if false exit then
   kind JR:T-KEY = if KEY true exit then
   kind JR:T-OBJ = if ELEMENT OUT JSON-WRITE:OBJECT-START drop kind OPEN-LEVEL true exit then
   kind JR:T-ARR = if ELEMENT OUT JSON-WRITE:ARRAY-START drop kind OPEN-LEVEL true exit then
   kind JR:T-OBJ-END = if OUT JSON-WRITE:OBJECT-END drop CLOSE-LEVEL true exit then
   kind JR:T-ARR-END = if OUT JSON-WRITE:ARRAY-END drop CLOSE-LEVEL true exit then
   kind SCALAR true ;

\ The canonical text of one JSON value; at its top level contentDigest is left out.
: CANON$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   OUT CANON TEXT-CAP JSON-WRITE:OPEN drop
   0 DEPTH !  -1 0 KIND !  1 0 FIRST !
   CANON-R JR:STORAGE-BYTES a u JR:INIT
   begin STEP 0= until
   JR:CLOSE
   OUT JSON-WRITE:$ ;

: DIGEST$ ( -- ptr u8 n )
   REG$ CANON$ {: a:ptr u:n :}
   SHA-CTX a u DG SHA256-IN
   DG DG-HEX SHA256>HEX
   DG-HEX $40 ;

\ ---- members and elements ------------------------------------------------------
: NO-MEMBER ( ptr u8 n -- )
   s" hbr2-fixtures: no registry member " type type cr
   s" " 1 die ;

: WRONG-KIND ( ptr u8 n -- )
   s" hbr2-fixtures: wrong JSON kind for registry member " type type cr
   s" " 1 die ;

\ LOOK-R at the value of key in the object obj spans.
: SEEK ( ptr u8 n ptr u8 n -- JR:reader ) {: obj:ptr ou:n key:ptr ku:n :}
   LOOK-R JR:STORAGE-BYTES obj ou JR:INIT
   JR:NEXT drop
   key ku JR:FIND-KEY 0= if JR:CLOSE key ku NO-MEMBER then ;

\ SEEK, refusing a value whose token is not kind.
: SEEK-KIND ( ptr u8 n ptr u8 n n -- JR:reader )
   {: key:ptr ku:n kind:n :}
   key ku SEEK
   JR:TOKEN kind <> if JR:CLOSE key ku WRONG-KIND then ;

: CONTAINER? ( n -- bool ) {: kind:n :}
   kind JR:T-OBJ = kind JR:T-ARR = or ;

\ The current value: a container whole, a string's bytes, a scalar's text.
: VALUE$ ( JR:reader -- JR:reader ptr u8 n )
   JR:TOKEN CONTAINER? if JR:VALUE-SPAN$ else JR:SPAN$ then ;

\ A member read as kind: JR:T-STR, JR:T-OBJ or JR:T-ARR.
: KIND$ ( ptr u8 n ptr u8 n n -- ptr u8 n )
   SEEK-KIND VALUE$ {: a:ptr u:n :}
   JR:CLOSE a u ;

: STRING$ ( ptr u8 n ptr u8 n -- ptr u8 n )   JR:T-STR KIND$ ;
: OBJECT$ ( ptr u8 n ptr u8 n -- ptr u8 n )   JR:T-OBJ KIND$ ;
: ARRAY$ ( ptr u8 n ptr u8 n -- ptr u8 n )   JR:T-ARR KIND$ ;

: MEMBER-N ( ptr u8 n ptr u8 n -- n )
   JR:T-INT SEEK-KIND JR:INT {: v:n :} JR:CLOSE v ;

\ A member of varying kind as its exact JSON text, a string's quotes included,
\ so that a string never equals another kind's text.
: JSON$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   SEEK JR:TOKEN {: kind:n :}
   VALUE$ {: a:ptr u:n :}
   JR:CLOSE
   kind JR:T-STR = if a 1 - u 2 + exit then
   a u ;

: ROOT-OBJECT$ ( ptr u8 n -- ptr u8 n )   REG$ 2swap OBJECT$ ;
: ROOT-ARRAY$ ( ptr u8 n -- ptr u8 n )   REG$ 2swap ARRAY$ ;

: ELEMENTS ( ptr u8 n -- JR:reader ) {: a:ptr u:n :}
   WALK-R JR:STORAGE-BYTES a u JR:INIT
   JR:NEXT drop ;

: NEXT-OBJECT ( JR:reader -- JR:reader ptr u8 n )
   JR:NEXT JR:T-OBJ <> if JR:CLOSE s" an array element" NO-MEMBER then
   JR:VALUE-SPAN$ ;

: DONE ( JR:reader -- )
   s" no element past the golden rows" T-LABEL
   JR:NEXT JR:T-ARR-END = TTRUE
   JR:CLOSE ;

: NAME? ( ptr u8 n ptr u8 n -- bool ) {: e:ptr eu:n w:ptr wu:n :}
   e eu s" name" STRING$ w wu STR= ;

\ The element of an array of named objects whose name is w.
: NAMED ( ptr u8 n ptr u8 n -- ptr u8 n ) {: arr:ptr au:n w:ptr wu:n :}
   arr au ELEMENTS
   begin JR:NEXT JR:T-OBJ = while
      JR:VALUE-SPAN$ 2dup w wu NAME? if 2>r JR:CLOSE 2r> exit then
      2drop
   repeat
   JR:CLOSE w wu NO-MEMBER ;

: COUNT-OF ( ptr u8 n -- n ) {: a:ptr u:n :}   \ an array's elements
   0 TALLY-R JR:STORAGE-BYTES a u JR:INIT
   JR:NEXT drop
   begin JR:NEXT JR:T-ARR-END <> while JR:SKIP-VALUE swap 1+ swap repeat
   JR:CLOSE ;

\ ---- one composed line: an assertion's label or a rendered signature -----------
$80 constant LINE-CAP
LINE-CAP BUFFER: LINE
variable LINE-N
LINE-CAP BUFFER: SECTION
variable SECTION-N

: LONG-LINE ( -- )
   s" hbr2-fixtures: composed line over LINE-CAP" 1 die ;

: APPEND ( ptr u8 n -- ) {: a:ptr u:n :}
   LINE-N @ u + LINE-CAP > if LONG-LINE then
   a LINE LINE-N @ + u BYTE-COPY
   LINE-N @ u + LINE-N ! ;

: SECTION! ( ptr u8 n -- ) {: a:ptr u:n :}
   u LINE-CAP > if LONG-LINE then
   a SECTION u BYTE-COPY  u SECTION-N ! ;

\ Labels the next assertion `section.what:aspect`.
: LABEL ( ptr u8 n ptr u8 n -- ) {: w:ptr wu:n asp:ptr au:n :}
   0 LINE-N !
   SECTION SECTION-N @ APPEND  s" ." APPEND  w wu APPEND  s" :" APPEND  asp au APPEND
   LINE LINE-N @ T-LABEL ;

\ ---- goldens -------------------------------------------------------------------
: DIGEST ( -- )
   s" canonical registry digest" T-LABEL  DIGEST$ PINNED$ T$=
   s" one top-level contentDigest" T-LABEL  DIGESTS @ 1 T=
   s" contentDigest member" T-LABEL  REG$ s" contentDigest" STRING$ PINNED$ T$=
   s" registry keys in byte order" T-LABEL  DISORDER @ 0 T=
   s" registry floats and escapes" T-LABEL  UNCANONICAL @ 0 T= ;

\ A layout and its byte size, read as the walk over its fields.
: LAYOUT ( ptr u8 n n -- JR:reader ) {: sec:ptr su:n size:n :}
   sec su SECTION!
   s" layout" s" bytes" LABEL  sec su ROOT-OBJECT$ s" bytes" MEMBER-N size T=
   sec su ROOT-OBJECT$ s" fields" ARRAY$ ELEMENTS ;

\ The layout's next field: its name, offset, width and type.
: ROW ( JR:reader ptr u8 n n n ptr u8 n -- JR:reader )
   {: f:ptr fu:n off:n size:n ty:ptr tu:n :}
   NEXT-OBJECT {: e:ptr eu:n :}
   f fu s" name" LABEL  e eu s" name" STRING$ f fu T$=
   f fu s" offset" LABEL  e eu s" offset" MEMBER-N off T=
   f fu s" bytes" LABEL  e eu s" bytes" MEMBER-N size T=
   f fu s" type" LABEL  e eu s" type" STRING$ ty tu T$= ;

\ The JSON text of the fixed value a layout's field carries.
: FIXED ( ptr u8 n ptr u8 n -- ptr u8 n ) {: sec:ptr su:n f:ptr fu:n :}
   sec su SECTION!
   f fu s" value" LABEL
   sec su ROOT-OBJECT$ s" fields" ARRAY$ f fu NAMED s" value" JSON$ ;

: PACKET-HEADER ( -- )
   s" packetHeader" 96 LAYOUT
   s" magic"            0  4 s" ASCII" ROW
   s" major"            4  2 s" U16"   ROW
   s" minor"            6  2 s" U16"   ROW
   s" channel"          8  2 s" U16"   ROW
   s" flags"           10  2 s" U16"   ROW
   s" headerBytes"     12  4 s" U32"   ROW
   s" totalBytes"      16  4 s" U32"   ROW
   s" recordCount"     20  4 s" U32"   ROW
   s" runtimeEpoch"    24  8 s" U64"   ROW
   s" authEpoch"       32  8 s" U64"   ROW
   s" laneGeneration"  40  8 s" U64"   ROW
   s" packetSequence"  48  8 s" U64"   ROW
   s" namespace"       56 16 s" Id128" ROW
   s" producer"        72  4 s" U32"   ROW
   s" dataStart"       76  4 s" U32"   ROW
   s" reserved"        80 16 s" Zero"  ROW
   DONE
   s" packetHeader" s" magic" FIXED s\" \"HBR2\"" T$=
   s" packetHeader" s" major" FIXED s" 2" T$=
   s" packetHeader" s" minor" FIXED s" 0" T$=
   s" packetHeader" s" flags" FIXED s" 0" T$=
   s" packetHeader" s" headerBytes" FIXED s" 96" T$= ;

: RECORD-HEADER ( -- )
   s" recordHeader" 32 LAYOUT
   s" opcode"         0 2 s" U16"    ROW
   s" flags"          2 2 s" U16"    ROW
   s" recordBytes"    4 4 s" U32"    ROW
   s" correlationId"  8 8 s" U64"    ROW
   s" scope"         16 8 s" Handle" ROW
   s" deadline"      24 8 s" F64"    ROW
   DONE
   s" recordHeader" s" flags" FIXED s" 0" T$= ;

: CONTROL-RECORD ( -- )
   s" controlRecord" 128 LAYOUT
   s" abiMajor"             0  4 s" U32"  ROW
   s" abiMinor"             4  4 s" U32"  ROW
   s" contextOffset"        8  4 s" U32"  ROW
   s" resultClass"         12  4 s" U32"  ROW
   s" reservationOffset"   16  4 s" U32"  ROW
   s" reservationCapacity" 20  4 s" U32"  ROW
   s" inputLease"          24  8 s" U64"  ROW
   s" throwCode"           32  8 s" I64"  ROW
   s" diagnosticOffset"    40  4 s" U32"  ROW
   s" diagnosticLength"    44  4 s" U32"  ROW
   s" runtimeEpoch"        48  8 s" U64"  ROW
   s" workDone"            56  8 s" U64"  ROW
   s" reserved"            64 64 s" Zero" ROW
   DONE ;

: PROTOCOL ( -- )
   s" protocol" SECTION!
   s" magic" s" value" LABEL  s" protocol" ROOT-OBJECT$ s" magic" STRING$ s" HBR2" T$=
   s" major" s" value" LABEL  s" protocol" ROOT-OBJECT$ s" major" MEMBER-N 2 T=
   s" minor" s" value" LABEL  s" protocol" ROOT-OBJECT$ s" minor" MEMBER-N 0 T=
   s" alignment" s" value" LABEL  s" protocol" ROOT-OBJECT$ s" alignment" MEMBER-N 8 T= ;

\ The next element of a list of named codes.
: CODE ( JR:reader ptr u8 n n -- JR:reader ) {: c:ptr cu:n v:n :}
   NEXT-OBJECT {: e:ptr eu:n :}
   c cu s" name" LABEL  e eu s" name" STRING$ c cu T$=
   c cu s" value" LABEL  e eu s" value" MEMBER-N v T= ;

: CODES ( ptr u8 n -- JR:reader ) {: sec:ptr su:n :}
   sec su SECTION!  sec su ROOT-ARRAY$ ELEMENTS ;

: RESULT-CODES ( -- )
   s" languageStatus" CODES
   s" Success" 0 CODE  s" Throw" 1 CODE
   DONE
   s" resultClasses" CODES
   s" OK" 0 CODE  s" Idle" 1 CODE  s" More" 2 CODE  s" Waiting" 3 CODE
   s" Stopped" 4 CODE  s" WouldBlock" 5 CODE  s" BadState" 6 CODE
   DONE
   s" submitResults" CODES
   s" Accepted" 0 CODE  s" Backpressure" 1 CODE  s" Invalid" 2 CODE
   s" Denied" 3 CODE  s" Unavailable" 4 CODE  s" OOM" 5 CODE
   s" StaleEpoch" 6 CODE  s" HostFailed" 7 CODE
   DONE ;

\ ---- the wrapper's imports and exports ------------------------------------------
\ A function rendered as HBR2 §24.5 writes it, `name(p:t, ...) -> result`, then
\ `/` and the JSON text of what its result returns: the name of a code list or
\ of the control record's offset, or null. Every name and type is a plain name,
\ so none holds a delimiter and two different functions never render alike.
: NOT-NAME ( ptr u8 n -- )
   s" hbr2-fixtures: registry member " type type s"  is not a plain name" type cr
   s" " 1 die ;

: NAME-BYTE? ( n -- bool ) {: c:n :}   \ a-z, 0-9 or _
   c $61 >= c $7A <= and  c STR-DIGIT? or  c $5F = or ;

\ A string member that is a plain name, as every Wasm name and value type is.
: NAME$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: o:ptr ou:n k:ptr ku:n :}
   o ou k ku STRING$ {: a:ptr u:n :}
   u 0= if k ku NOT-NAME then
   u 0 ?do a i + c@ NAME-BYTE? 0= if k ku NOT-NAME then loop
   a u ;

: PARAM ( JR:reader -- JR:reader )
   JR:VALUE-SPAN$ {: p:ptr pu:n :}
   p pu s" name" NAME$ APPEND  s" :" APPEND  p pu s" type" NAME$ APPEND ;

: BAD-PARAMS ( ptr u8 n -- )
   s" hbr2-fixtures: params of " type type s"  is not an array of objects" type cr
   s" " 1 die ;

\ The params of the function f, an array: its objects rendered `p:t, ...`.
: PARAMS ( ptr u8 n ptr u8 n -- ) {: f:ptr fu:n a:ptr u:n :}
   TALLY-R JR:STORAGE-BYTES a u JR:INIT
   JR:NEXT drop
   JR:NEXT dup JR:T-OBJ = if
      drop PARAM
      begin JR:NEXT dup JR:T-OBJ = while drop s" , " APPEND PARAM repeat
   then
   JR:T-ARR-END <> if JR:CLOSE f fu BAD-PARAMS then
   JR:CLOSE ;

: SIGNATURE$ ( ptr u8 n -- ptr u8 n ) {: e:ptr eu:n :}
   0 LINE-N !
   e eu s" name" NAME$ APPEND  s" (" APPEND
   e eu s" name" NAME$ e eu s" params" ARRAY$ PARAMS
   s" ) -> " APPEND  e eu s" result" NAME$ APPEND
   s" /" APPEND  e eu s" returns" JSON$ APPEND
   LINE LINE-N @ ;

: FUNCTION ( JR:reader ptr u8 n -- JR:reader ) {: want:ptr wu:n :}
   NEXT-OBJECT SIGNATURE$ {: got:ptr gu:n :}
   s" wasm function" T-LABEL  got gu want wu T$= ;

: WASM ( -- )
   s" wasm" SECTION!
   s" importModule" s" value" LABEL
   s" wasm" ROOT-OBJECT$ s" importModule" STRING$ s" habu_browser_v2" T$=
   s" wasm" ROOT-OBJECT$ s" imports" ARRAY$ ELEMENTS
   s\" submit(ctx:i32, ptr:i32, len:i32) -> i32/\"submitResults\"" FUNCTION
   s" wake(ctx:i32) -> i32/null" FUNCTION
   DONE
   s" wasm" ROOT-OBJECT$ s" exports" ARRAY$ ELEMENTS
   s\" hbr_control() -> i32/\"controlRecordOffset\"" FUNCTION
   s\" hbr_reserve_input(ctx:i32, bytes:i32) -> i32/\"languageStatus\"" FUNCTION
   s\" hbr_start(ptr:i32, bytes:i32, lease:i64) -> i32/\"languageStatus\"" FUNCTION
   s\" hbr_ingest(ctx:i32, ptr:i32, bytes:i32, lease:i64) -> i32/\"languageStatus\"" FUNCTION
   s\" hbr_step(ctx:i32, budget:i32) -> i32/\"languageStatus\"" FUNCTION
   s\" hbr_stop(ctx:i32) -> i32/\"languageStatus\"" FUNCTION
   DONE ;

\ ---- limits and counts ---------------------------------------------------------
: LIMIT ( ptr u8 n n -- ) {: k:ptr ku:n v:n :}
   k ku s" value" LABEL
   SECTION SECTION-N @ ROOT-OBJECT$ k ku MEMBER-N v T= ;

: LIMITS ( -- )
   s" callbackLimits" SECTION!
   s" loopBound" 64 LIMIT
   s" weightedOperations" 2048 LIMIT
   s" directNodes" 256 LIMIT
   s" copiedBytes" 16384 LIMIT
   s" decoderLimits" SECTION!
   s" depth" 32 LIMIT
   s" references" 65536 LIMIT
   s" semanticPacketBytes" 65536 LIMIT
   s" bulkPacketBytes" 1048576 LIMIT
   s" controlPacketBytes" 4096 LIMIT ;

variable VARIANTS

: ADD-VARIANTS ( ptr u8 n -- ) {: e:ptr eu:n :}
   e eu s" kind" STRING$ s" union" STR= if
      e eu s" variants" ARRAY$ COUNT-OF VARIANTS +!
   then ;

\ HBR2 §28.1's counts: 446 types, 143 operations, 52 properties, 42 variants.
: COUNTS ( -- )
   s" types" T-LABEL  s" types" ROOT-ARRAY$ COUNT-OF 446 T=
   s" operations" T-LABEL  s" operations" ROOT-ARRAY$ COUNT-OF 143 T=
   s" properties" T-LABEL  s" properties" ROOT-ARRAY$ COUNT-OF 52 T=
   0 VARIANTS !
   s" types" ROOT-ARRAY$ ELEMENTS
   begin JR:NEXT JR:T-OBJ = while JR:VALUE-SPAN$ ADD-VARIANTS repeat
   JR:CLOSE
   s" union variants" T-LABEL  VARIANTS @ 42 T= ;

\ ---- the canonical STOP packet -------------------------------------------------
$100 constant PKT-CAP
PKT-CAP BUFFER: PKT
PKT-CAP 2 * BUFFER: PKT-HEX

: TYPE-OF ( ptr u8 n -- ptr u8 n )
   s" types" ROOT-ARRAY$ 2swap NAMED ;

\ The value of the named member of an enum or flags type.
: ENUM-N ( ptr u8 n ptr u8 n -- n ) {: t:ptr tu:n m:ptr mu:n :}
   t tu TYPE-OF s" values" ARRAY$ m mu NAMED s" value" MEMBER-N ;

: INT! ( n ptr u8 n -- ) {: v:n p:ptr w:n :}
   w 2 = if v p LE:U16! exit then
   w 4 = if v p LE:U32! exit then
   w 8 = if v p LE:U64! exit then
   s" hbr2-fixtures: no integer of that width" 1 die ;

\ Field f of a layout, offset and width.
: FIELD@ ( ptr u8 n ptr u8 n -- n n ) {: lay:ptr lu:n f:ptr fu:n :}
   lay lu s" fields" ARRAY$ f fu NAMED {: e:ptr eu:n :}
   e eu s" offset" MEMBER-N  e eu s" bytes" MEMBER-N ;

\ Store v in field f of the layout section sec at byte base of PKT.
: SET ( n n ptr u8 n ptr u8 n -- ) {: v:n base:n sec:ptr su:n f:ptr fu:n :}
   sec su ROOT-OBJECT$ f fu FIELD@ {: off:n w:n :}
   v PKT base + off + w INT! ;

: HEADER-SET ( n ptr u8 n -- ) {: v:n f:ptr fu:n :}
   v 0 s" packetHeader" f fu SET ;

: STOP-OP$ ( -- ptr u8 n )
   s" operations" ROOT-ARRAY$ s" STOP" NAMED ;

: RECORD-BYTES ( -- n )   \ header, STOP's fixed body and zero padding to the alignment
   s" protocol" ROOT-OBJECT$ s" alignment" MEMBER-N {: al:n :}
   s" recordHeader" ROOT-OBJECT$ s" bytes" MEMBER-N
   STOP-OP$ s" body" STRING$ TYPE-OF s" bytes" MEMBER-N +
   al 1- + al negate and ;

: MAGIC ( -- )
   s" packetHeader" ROOT-OBJECT$ {: h:ptr hu:n :}
   h hu s" magic" FIELD@ {: off:n w:n :}
   h hu s" fields" ARRAY$ s" magic" NAMED s" value" STRING$ {: m:ptr mu:n :}
   s" STOP magic width" T-LABEL  mu w T=
   m PKT off + mu BYTE-COPY ;

: NAMESPACE ( -- )   \ sixteen distinct bytes, $50 up
   s" packetHeader" ROOT-OBJECT$ s" namespace" FIELD@ {: off:n w:n :}
   w 0 ?do $50 i + PKT off + i + c! loop ;

\ Header values stand apart byte by byte, so a field written at another's
\ offset or width changes the golden.
: STOP-HEADER ( n -- ) {: total:n :}
   MAGIC
   s" packetHeader" ROOT-OBJECT$ s" fields" ARRAY$ {: fs:ptr fsu:n :}
   fs fsu s" major" NAMED s" value" MEMBER-N s" major" HEADER-SET
   fs fsu s" minor" NAMED s" value" MEMBER-N s" minor" HEADER-SET
   s" Channel" STOP-OP$ s" channel" STRING$ ENUM-N s" channel" HEADER-SET
   s" packetHeader" ROOT-OBJECT$ s" bytes" MEMBER-N s" headerBytes" HEADER-SET
   total s" totalBytes" HEADER-SET
   1 s" recordCount" HEADER-SET
   $1817161514131211 s" runtimeEpoch" HEADER-SET
   $2827262524232221 s" authEpoch" HEADER-SET
   $3837363534333231 s" laneGeneration" HEADER-SET
   $4847464544434241 s" packetSequence" HEADER-SET
   NAMESPACE
   $64636261 s" producer" HEADER-SET
   total s" dataStart" HEADER-SET ;

: STOP-RECORD ( n -- ) {: base:n :}
   STOP-OP$ s" opcode" MEMBER-N base s" recordHeader" s" opcode" SET
   RECORD-BYTES base s" recordHeader" s" recordBytes" SET
   $7877767574737271 base s" recordHeader" s" correlationId" SET
   $8887868584838281 base s" recordHeader" s" scope" SET
   STOP-OP$ s" body" STRING$ TYPE-OF {: body:ptr bu:n :}
   body bu s" fields" ARRAY$ s" reason" NAMED {: r:ptr ru:n :}
   r ru s" type" STRING$ s" Shutdown" ENUM-N
   PKT base + s" recordHeader" ROOT-OBJECT$ s" bytes" MEMBER-N + r ru s" offset" MEMBER-N +
   r ru s" type" STRING$ TYPE-OF s" bytes" MEMBER-N INT! ;

\ The STOP packet in PKT, every number from the registry but the test values.
: STOP-PACKET ( -- n )
   PKT-CAP 0 ?do 0 PKT i + c! loop
   s" packetHeader" ROOT-OBJECT$ s" bytes" MEMBER-N {: hb:n :}
   hb RECORD-BYTES + {: total:n :}
   total STOP-HEADER
   hb STOP-RECORD
   total ;

: PKT-HEX$ ( n n -- ptr u8 n ) {: off:n u:n :}
   u 0 ?do PKT off + i + c@ PKT-HEX i 2 * + BYTE>HEX loop
   PKT-HEX u 2 * ;

: STOP ( -- )
   s" STOP packet extent" T-LABEL  STOP-PACKET 136 T=
   s" STOP header bytes 0-31" T-LABEL
   0 32 PKT-HEX$ s" 4842523202000000010000006000000088000000010000001112131415161718" T$=
   s" STOP header bytes 32-63" T-LABEL
   32 32 PKT-HEX$ s" 2122232425262728313233343536373841424344454647485051525354555657" T$=
   s" STOP header bytes 64-95" T-LABEL
   64 32 PKT-HEX$ s" 58595a5b5c5d5e5f616263648800000000000000000000000000000000000000" T$=
   s" STOP record header" T-LABEL
   96 32 PKT-HEX$ s" 0600000028000000717273747576777881828384858687880000000000000000" T$=
   s" STOP body and padding" T-LABEL
   128 8 PKT-HEX$ s" 0700000000000000" T$= ;

public

: RUN ( -- )
   T-RESET
   LOAD
   DIGEST
   PROTOCOL
   PACKET-HEADER
   RECORD-HEADER
   CONTROL-RECORD
   RESULT-CODES
   WASM
   LIMITS
   COUNTS
   STOP
   T-REPORT ;

;package

HBR2-FIXTURES-TEST:RUN
