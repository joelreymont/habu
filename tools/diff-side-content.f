\ diff-side-content.f - deterministic side-content artifact encoder.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/fs.f
require src/core/sha256.f

package DIFF-CONTENT
public

-7500 constant E-SYNTAX
-7501 constant E-CAPACITY
-7502 constant E-DIGEST

ENUM content-kind absent file symlink gitlink ;ENUM

112 constant HEADER-U
64 constant TRAILER-U
40 constant ROW-HEAD-U
64 constant SIDE-HEAD-U
1 constant VERSION

private

$20 constant DIGEST-U
$52 constant ROW-C
$53 constant SIDE-C
7 constant HEAD-RESERVED-U
4 constant SIDE-RESERVED-U
8000 constant BINARY-PROBE-U

PTR-VARIABLE OUT-A
variable OUT-CAP
variable OUT-U
variable ROW-N
variable EXPECT-N
variable PAYLOAD-START
variable ROW-START
variable ROW-OLD-U
variable ROW-NEW-U
variable SIDE-N
variable SIDE-START
variable BODY-EXPECT
variable BODY-SEEN
variable BODY-BINARY
variable PATH-SEG
variable STATE
1 LAYOUT-BUFFER SIDE-KIND-V content-kind

create PAYLOAD-DIGEST DIGEST-U allot
create SIDE-DIGEST DIGEST-U allot
create ZERO-DIGEST DIGEST-U allot
create BODY-SHA SHA256-CTX-U allot

0 constant STATE-READY
1 constant STATE-ROW
2 constant STATE-SIDE

: SIDE-KIND-AT ( -- ptr content-kind )
   0 SIDE-KIND-V ;

: KIND-BYTE ( content-kind -- n )
   MATCH content-kind
      absent  OF 0 ENDOF
      file    OF 1 ENDOF
      symlink OF 2 ENDOF
      gitlink OF 3 ENDOF
   ;MATCH ;

: BYTE>KIND ( n -- content-kind )
   case
      0 of construct content-kind absent endof
      1 of construct content-kind file endof
      2 of construct content-kind symlink endof
      3 of construct content-kind gitlink endof
      E-SYNTAX throw
   endcase ;

: KIND-ABSENT? ( content-kind -- bool )
   KIND-BYTE 0= ;

: KIND-FILE? ( content-kind -- bool )
   KIND-BYTE 1 = ;

: KIND-GITLINK? ( content-kind -- bool )
   KIND-BYTE 3 = ;

: OUT-A@ ( -- ptr u8 )
   OUT-A @ ;

: REQUIRE-STATE ( n -- )
   STATE @ <> if E-SYNTAX throw then ;

: CHECK-ADD ( n n -- n ) {: total:n add:n :}
   total 0 < add 0 < or if E-CAPACITY throw then
   total add + total < if E-CAPACITY throw then
   total add + ;

: NEED ( n -- ) {: add:n :}
   OUT-U @ add CHECK-ADD OUT-CAP @ > if E-CAPACITY throw then ;

: U8 ( n -- ) {: value:n :}
   value 0 < value $FF > or if E-SYNTAX throw then
   1 NEED
   value OUT-A@ OUT-U @ + c!
   OUT-U @ 1+ OUT-U ! ;

: U64! ( n ptr u8 -- ) {: value:n dst:ptr :}
   value 0 < if E-SYNTAX throw then
   0 begin dup 8 < while
      dup 8 * value swap rshift $FF and
      over dst + c!
      1+
   repeat drop ;

: U64 ( n -- ) {: value:n :}
   8 NEED
   value OUT-A@ OUT-U @ + U64!
   OUT-U @ 8 + OUT-U ! ;

: BYTES ( ptr u8 n -- ) {: a:ptr u:n :}
   u NEED
   a OUT-A@ OUT-U @ + u BYTE-COPY
   OUT-U @ u + OUT-U ! ;

: ZEROES ( n -- ) {: u:n :}
   u NEED
   0 begin dup u < while
      dup OUT-A@ OUT-U @ + + 0 swap c!
      1+
   repeat drop
   OUT-U @ u + OUT-U ! ;

: MAGIC ( -- )
   s" HABUSIDE" BYTES ;

: TRAILER-MAGIC ( -- )
   s" EDISUBAH" BYTES ;

: PATH-SEG-SAFE? ( ptr u8 n n -- bool ) {: a:ptr start:n u:n :}
   u 0= if false exit then
   u 1 = if a start + c@ $2E = if false exit then then
   u 2 = if
      a start + c@ $2E =
      a start 1+ + c@ $2E = and if false exit then
   then
   true ;

: PATH-SAFE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0 <= if false exit then
   a c@ $2F = if false exit then
   0 PATH-SEG !
   0 begin dup u < while
      a over + c@ {: c:n :}
      c 0= if drop false exit then
      c $2F = if
         dup {: idx:n :}
         a PATH-SEG @ idx PATH-SEG @ - PATH-SEG-SAFE? 0= if drop false exit then
         idx 1+ PATH-SEG !
      then
      1+
   repeat
   drop
   a PATH-SEG @ u PATH-SEG @ - PATH-SEG-SAFE? ;

: NUL? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup u < while
      a over + c@ 0= if drop true exit then
      1+
   repeat drop
   false ;

: PROBE-CHUNK ( ptr u8 n -- ) {: a:ptr u:n :}
   BODY-SEEN @ BINARY-PROBE-U >= if exit then
   BINARY-PROBE-U BODY-SEEN @ - {: left:n :}
   u left < if u else left then {: probeu:n :}
   a probeu NUL? if true BODY-BINARY ! then ;

: KIND-CHECK ( bool content-kind ptr u8 n n -- )
   {: present:bool kind:content-kind path:ptr pathu:n bodyu:n :}
   present if
      kind KIND-ABSENT? if E-SYNTAX throw then
      path pathu PATH-SAFE? 0= if E-SYNTAX throw then
   else
      kind KIND-ABSENT? 0= if E-SYNTAX throw then
      pathu 0<> bodyu 0<> or if E-SYNTAX throw then
   then
   kind KIND-GITLINK? if
      bodyu 0<> if E-SYNTAX throw then
   then ;

: BODY-BINARY? ( content-kind ptr u8 n -- bool )
   {: kind:content-kind body:ptr bodyu:n :}
   kind KIND-FILE? 0= if false exit then
   bodyu BINARY-PROBE-U < if bodyu else BINARY-PROBE-U then {: probeu:n :}
   body probeu NUL? ;

: BINARY-CHECK ( content-kind bool ptr u8 n -- )
   {: kind:content-kind binary:bool body:ptr bodyu:n :}
   kind body bodyu BODY-BINARY? if
      binary 0= if E-SYNTAX throw then
   else
      binary if E-SYNTAX throw then
   then ;

: SIDE-SIZE-INTERNAL ( n -- n )
   SIDE-HEAD-U swap CHECK-ADD ;

: SIDE-HEAD ( bool content-kind n n -- )
   {: present:bool kind:content-kind pathu:n bodyu:n :}
   SIDE-C U8
   present if 1 else 0 then U8
   kind KIND-BYTE U8
   0 U8
   SIDE-RESERVED-U ZEROES
   bodyu U64
   pathu U64
   0 U64 ;

: HEADER ( n ptr u8 n -- ) {: count:n meta:ptr metau:n :}
   MAGIC
   VERSION U8
   HEAD-RESERVED-U ZEROES
   count U64
   0 U64
   DIGEST-U ZEROES
   metau U64
   meta metau SIDE-DIGEST SHA256
   SIDE-DIGEST DIGEST-U BYTES
   8 ZEROES ;

: PATCH-PAYLOAD ( n -- )
   OUT-A@ 24 + U64! ;

: PATCH-DIGEST ( -- )
   PAYLOAD-DIGEST OUT-A@ 32 + DIGEST-U BYTE-COPY ;

: TRAILER ( n -- ) {: total:n :}
   TRAILER-MAGIC
   VERSION U8
   HEAD-RESERVED-U ZEROES
   ROW-N @ U64
   total U64
   PAYLOAD-DIGEST DIGEST-U BYTES ;

: ARTIFACT-DIGEST ( -- )
   SHA256-RESET
   OUT-A@ 32 SHA256-UPDATE
   ZERO-DIGEST DIGEST-U SHA256-UPDATE
   OUT-A@ 64 + OUT-U @ 64 - SHA256-UPDATE
   PAYLOAD-DIGEST SHA256-FINAL ;

: SIDE-PREFLIGHT ( bool content-kind ptr u8 n n -- )
   {: present:bool kind:content-kind path:ptr pathu:n bodyu:n :}
   STATE-ROW REQUIRE-STATE
   SIDE-N @ 2 >= if E-SYNTAX throw then
   present kind path pathu bodyu KIND-CHECK
   pathu SIDE-SIZE-INTERNAL NEED ;

: SIDE-OPEN ( bool content-kind ptr u8 n n -- )
   {: present:bool kind:content-kind path:ptr pathu:n bodyu:n :}
   OUT-U @ SIDE-START !
   STATE-SIDE STATE !
   kind SIDE-KIND-AT ! bodyu BODY-EXPECT ! 0 BODY-SEEN ! false BODY-BINARY !
   present kind pathu bodyu SIDE-HEAD
   DIGEST-U ZEROES
   path pathu BYTES
   BODY-SHA SHA256-CTX-RESET ;

public

: SIDE-SIZE ( n -- n )
   SIDE-SIZE-INTERNAL ;

: ROW-SIZE ( n n -- n ) {: oldu:n newu:n :}
   ROW-HEAD-U oldu CHECK-ADD newu CHECK-ADD ;

: ARTIFACT-SIZE ( n -- n ) {: payloadu:n :}
   HEADER-U payloadu CHECK-ADD TRAILER-U CHECK-ADD ;

: START ( ptr u8 n n ptr u8 n -- )
   {: out:ptr cap:n count:n meta:ptr metau:n :}
   count 0 < if E-SYNTAX throw then
   cap HEADER-U TRAILER-U + < if E-CAPACITY throw then
   out OUT-A ! cap OUT-CAP !
   0 OUT-U ! 0 ROW-N ! count EXPECT-N !
   STATE-READY STATE !
   count meta metau HEADER
   OUT-U @ PAYLOAD-START ! ;

: ROW-BEGIN ( n n n -- ) {: ordinal:n oldu:n newu:n :}
   STATE-READY REQUIRE-STATE
   ordinal ROW-N @ <> if E-SYNTAX throw then
   oldu SIDE-HEAD-U < newu SIDE-HEAD-U < or if E-SYNTAX throw then
   OUT-U @ ROW-START !
   oldu ROW-OLD-U !
   newu ROW-NEW-U !
   0 SIDE-N !
   STATE-ROW STATE !
   ROW-C U8
   HEAD-RESERVED-U ZEROES
   ordinal U64
   oldu newu ROW-SIZE U64
   oldu U64
   newu U64 ;

: ROW-END ( -- )
   STATE-ROW REQUIRE-STATE
   SIDE-N @ 2 <> if E-SYNTAX throw then
   OUT-U @ ROW-START @ - {: got:n :}
   OUT-A@ ROW-START @ + 16 + FS-U64@ got <> if E-SYNTAX throw then
   OUT-A@ ROW-START @ + 24 + FS-U64@ ROW-OLD-U @ <> if E-SYNTAX throw then
   ROW-N @ $7FFFFFFFFFFFFFFF = if E-CAPACITY throw then
   ROW-N @ 1+ ROW-N !
   STATE-READY STATE ! ;

: SIDE-BEGIN ( bool content-kind ptr u8 n n -- )
   {: present:bool kind:content-kind path:ptr pathu:n bodyu:n :}
   present kind path pathu bodyu SIDE-PREFLIGHT
   present kind path pathu bodyu SIDE-OPEN ;

: SIDE-CHUNK ( ptr u8 n -- ) {: a:ptr u:n :}
   STATE-SIDE REQUIRE-STATE
   BODY-SEEN @ u CHECK-ADD BODY-EXPECT @ > if E-CAPACITY throw then
   SIDE-KIND-AT @ KIND-FILE? if a u PROBE-CHUNK then
   BODY-SHA a u SHA256-CTX-UPDATE
   BODY-SEEN @ u + BODY-SEEN ! ;

: SIDE-END ( -- )
   STATE-SIDE REQUIRE-STATE
   BODY-SEEN @ BODY-EXPECT @ <> if E-SYNTAX throw then
   OUT-A@ SIDE-START @ + {: side:ptr :}
   BODY-BINARY @ if 1 else 0 then side 3 + c!
   SIDE-KIND-AT @ KIND-ABSENT?
   SIDE-KIND-AT @ KIND-GITLINK? or if
      ZERO-DIGEST side 32 + DIGEST-U BYTE-COPY
   else
      BODY-SHA SIDE-DIGEST SHA256-CTX-FINAL
      SIDE-DIGEST side 32 + DIGEST-U BYTE-COPY
   then
   OUT-U @ SIDE-START @ -
   SIDE-N @ 0= if ROW-OLD-U @ else ROW-NEW-U @ then
   <> if E-SYNTAX throw then
   SIDE-N @ 1+ SIDE-N !
   STATE-ROW STATE ! ;

: SIDE ( bool content-kind bool ptr u8 n ptr u8 n -- )
   {: present:bool kind:content-kind binary:bool path:ptr pathu:n body:ptr bodyu:n :}
   present kind path pathu bodyu SIDE-PREFLIGHT
   kind binary body bodyu BINARY-CHECK
   present kind path pathu bodyu SIDE-OPEN
   body bodyu SIDE-CHUNK
   SIDE-END ;

: FINISH ( -- ptr u8 n )
   STATE-READY REQUIRE-STATE
   ROW-N @ EXPECT-N @ <> if E-SYNTAX throw then
   OUT-U @ PAYLOAD-START @ - {: payloadu:n :}
   payloadu PATCH-PAYLOAD
   ARTIFACT-DIGEST
   PATCH-DIGEST
   OUT-U @ TRAILER-U CHECK-ADD {: total:n :}
   total TRAILER
   OUT-U @ total <> if E-CAPACITY throw then
   OUT-A@ OUT-U @ ;

: SAFE-PATH? ( ptr u8 n -- bool )
   PATH-SAFE? ;

: ABSENT-KIND ( -- content-kind )
   construct content-kind absent ;

: FILE-KIND ( -- content-kind )
   construct content-kind file ;

: SYMLINK-KIND ( -- content-kind )
   construct content-kind symlink ;

: GITLINK-KIND ( -- content-kind )
   construct content-kind gitlink ;

;package
