\ leb.f - WLEB, the Wasm backend's LEB128: the shortest LEB of each integer type
\ at the edges of its byte counts and its range, the padded call and address
\ fields read at their offsets and rewritten in place, round trips across every
\ bit width, and each refusal by its code.
\ Run: bin/hb --load test/wasm/leb.f
\
\ Bytes are written in hex, two lowercase digits a byte. A refused write or
\ patch is checked to leave the whole buffer under it as it was.

require lib/test.f
require lib/span.f
require src/arch/wasm/leb.f

package WLEB-TEST
private

$A5 constant CANARY
$7FFFFFFFFFFFFFFF constant MAX-N
$8000000000000000 constant MIN-N
$FFFFFFFF constant U32-MAX
$7FFFFFFF constant S32-MAX
$100000000 constant U32-PAST    \ 2^32, the first value past a u32

$10 constant CALL-OP
$42 constant I64-CONST-OP
$0B constant END-OP
1 constant CALL-AT              \ the call field, after the call opcode
7 constant ADDR-AT              \ the address field, after i64.const
18 constant CODE-LEN            \ a call, an i64.const and an end, padded

32 SPAN-BUFFER: OUT
64 SPAN-BUFFER: HEX
32 SPAN-BUFFER: STAGED
variable STAGED-LEN

: OUT$ ( n -- ptr u8 n )
   OUT SPAN:$ drop swap ;

: HEX$ ( ptr u8 n -- ptr u8 n )
   {: a u:n :}
   HEX u 2 * SPAN:TAKE SPAN:$ {: h hu:n :}
   u 0 ?do a i + c@ h i 2 * + BYTE>HEX loop
   h hu ;

: NIBBLE ( n -- n )
   {: c:n :}
   c [char] a >= if c [char] a - $0A + exit then
   c [char] 0 - ;

\ Stage the hex's bytes for a refusal, whose quotation sees no locals, with
\ canaries past them.
: STAGE ( ptr u8 n -- )
   {: a u:n :}
   u 2 / {: k:n :}
   CANARY STAGED SPAN:FILL
   k 0 ?do
      a i 2 * + c@ NIBBLE 4 lshift a i 2 * + 1+ c@ NIBBLE or STAGED i SPAN:U8!
   loop
   k STAGED-LEN ! ;

: STAGED$ ( -- ptr u8 n )
   STAGED SPAN:$ drop STAGED-LEN @ ;

: STAGED-SPAN ( -- SPAN:span<u8> )
   STAGED STAGED-LEN @ SPAN:TAKE ;

\ Whether every byte of the span is still a canary.
: CANARIES? ( SPAN:span<u8> -- bool )
   {: s :}
   true s SPAN:LEN 0 ?do s i SPAN:U8@ CANARY = and loop ;

\ ---- the shortest LEB, at the edges ---------------------------------------
\ v's shortest LEB is want, and reads back as v from exactly those bytes.
: SHORTEST ( n [ n SPAN:span<u8> -- n ] [ ptr u8 n -- n n ] ptr u8 n -- )
   {: v:n put get want wu:n :}
   v OUT put execute {: k:n :}
   want wu T-LABEL
   k OUT$ HEX$ want wu T$=
   k OUT$ get execute
   want wu T-LABEL k T=
   want wu T-LABEL v T= ;

: U32-IS ( n ptr u8 n -- )
   {: v:n a u:n :}
   v [: WLEB:U32! ;] [: WLEB:U32@ ;] a u SHORTEST ;

: U64-IS ( n ptr u8 n -- )
   {: v:n a u:n :}
   v [: WLEB:U64! ;] [: WLEB:U64@ ;] a u SHORTEST ;

: S32-IS ( n ptr u8 n -- )
   {: v:n a u:n :}
   v [: WLEB:S32! ;] [: WLEB:S32@ ;] a u SHORTEST ;

: S64-IS ( n ptr u8 n -- )
   {: v:n a u:n :}
   v [: WLEB:S64! ;] [: WLEB:S64@ ;] a u SHORTEST ;

: U32-EDGES ( -- )
   0 s" 00" U32-IS
   63 s" 3f" U32-IS
   64 s" 40" U32-IS
   127 s" 7f" U32-IS
   128 s" 8001" U32-IS
   $3FFF s" ff7f" U32-IS
   $4000 s" 808001" U32-IS
   $FFFFFFF s" ffffff7f" U32-IS
   $10000000 s" 8080808001" U32-IS
   U32-MAX s" ffffffff0f" U32-IS ;

: U64-EDGES ( -- )
   0 s" 00" U64-IS
   127 s" 7f" U64-IS
   128 s" 8001" U64-IS
   U32-PAST s" 8080808010" U64-IS
   MAX-N s" ffffffffffffffff7f" U64-IS
   MIN-N s" 80808080808080808001" U64-IS
   -1 s" ffffffffffffffffff01" U64-IS ;

: S32-EDGES ( -- )
   0 s" 00" S32-IS
   63 s" 3f" S32-IS
   64 s" c000" S32-IS
   127 s" ff00" S32-IS
   128 s" 8001" S32-IS
   -1 s" 7f" S32-IS
   -64 s" 40" S32-IS
   -65 s" bf7f" S32-IS
   8191 s" ff3f" S32-IS
   8192 s" 80c000" S32-IS
   -8192 s" 8040" S32-IS
   -8193 s" ffbf7f" S32-IS
   S32-MAX s" ffffffff07" S32-IS
   S32-MAX invert s" 8080808078" S32-IS ;

: S64-EDGES ( -- )
   0 s" 00" S64-IS
   -1 s" 7f" S64-IS
   63 s" 3f" S64-IS
   64 s" c000" S64-IS
   -64 s" 40" S64-IS
   -65 s" bf7f" S64-IS
   S32-MAX 1+ s" 8080808008" S64-IS
   S32-MAX invert 1- s" ffffffff77" S64-IS
   U32-PAST s" 8080808010" S64-IS
   U32-PAST negate s" 8080808070" S64-IS
   MIN-N s" 8080808080808080807f" S64-IS
   MAX-N s" ffffffffffffffffff00" S64-IS ;

\ ---- padded fields, in place ----------------------------------------------
\ Lay out a call to f, an i64.const of a and an end, as the encoder does.
: LAY-OUT ( u32 n -- )
   {: f:n a:n :}
   CALL-OP OUT 0 SPAN:U8!
   f OUT CALL-AT SPAN:SKIP WLEB:U32-PAD! drop
   I64-CONST-OP OUT ADDR-AT 1- SPAN:U8!
   a OUT ADDR-AT SPAN:SKIP WLEB:S64-PAD! drop
   END-OP OUT CODE-LEN 1- SPAN:U8! ;

: CODE$ ( -- ptr u8 n )
   CODE-LEN OUT$ ;

\ Each field reads back at its offset and takes a patch in place: the opcodes
\ around it and the byte past the code stay as they were.
: IN-PLACE ( -- )
   CANARY OUT SPAN:FILL
   7 -2 LAY-OUT
   s" call 7, i64.const -2, end" T-LABEL
   CODE$ HEX$ s" 10878080800042feffffffffffffffff7f0b" T$=
   s" the call field" T-LABEL
   CODE$ CALL-AT WLEB:U32-PAD@ 7 T=
   s" the address field" T-LABEL
   CODE$ ADDR-AT WLEB:S64-PAD@ -2 T=
   U32-MAX OUT CALL-AT WLEB:U32-PATCH
   MIN-N OUT ADDR-AT WLEB:S64-PATCH
   s" call U32-MAX, i64.const MIN-N, end" T-LABEL
   CODE$ HEX$ s" 10ffffffff0f428080808080808080807f0b" T$=
   0 OUT CALL-AT WLEB:U32-PATCH
   5 OUT ADDR-AT WLEB:S64-PATCH
   s" call 0, i64.const 5, end" T-LABEL
   CODE$ HEX$ s" 10808080800042858080808080808080000b" T$=
   s" the patched call field" T-LABEL
   CODE$ CALL-AT WLEB:U32-PAD@ 0 T=
   s" the patched address field" T-LABEL
   CODE$ ADDR-AT WLEB:S64-PAD@ 5 T=
   s" the byte past the code" T-LABEL
   OUT CODE-LEN SPAN:U8@ CANARY T= ;

\ v's padded field is want, whatever v's shortest LEB takes, and reads back as
\ v at offset 0.
: PADDED ( n [ n SPAN:span<u8> -- n ] [ ptr u8 n n -- n ] ptr u8 n -- )
   {: v:n put get want wu:n :}
   v OUT put execute {: k:n :}
   want wu T-LABEL
   k OUT$ HEX$ want wu T$=
   want wu T-LABEL
   k OUT$ 0 get execute v T= ;

: U32-PADS ( n ptr u8 n -- )
   {: v:n a u:n :}
   v [: WLEB:U32-PAD! ;] [: WLEB:U32-PAD@ ;] a u PADDED ;

: S64-PADS ( n ptr u8 n -- )
   {: v:n a u:n :}
   v [: WLEB:S64-PAD! ;] [: WLEB:S64-PAD@ ;] a u PADDED ;

: PADS ( -- )
   0 s" 8080808000" U32-PADS
   1 s" 8180808000" U32-PADS
   127 s" ff80808000" U32-PADS
   128 s" 8081808000" U32-PADS
   $10000000 s" 8080808001" U32-PADS
   U32-MAX s" ffffffff0f" U32-PADS
   0 s" 80808080808080808000" S64-PADS
   -1 s" ffffffffffffffffff7f" S64-PADS
   63 s" bf808080808080808000" S64-PADS
   64 s" c0808080808080808000" S64-PADS
   -64 s" c0ffffffffffffffff7f" S64-PADS
   -65 s" bfffffffffffffffff7f" S64-PADS
   MIN-N s" 8080808080808080807f" S64-PADS
   MAX-N s" ffffffffffffffffff00" S64-PADS ;

\ ---- reading what Wasm admits, and refusing the rest ----------------------
\ The staged bytes read as v and take len of them.
: READS ( ptr u8 n [ ptr u8 n -- n n ] n n -- )
   {: a u:n get v:n len:n :}
   a u STAGE
   STAGED$ get execute
   a u T-LABEL len T=
   a u T-LABEL v T= ;

\ fail, run on the staged bytes, refuses with code.
: REFUSES ( ptr u8 n [ -- ] n -- )
   {: a u:n fail code:n :}
   a u STAGE
   a u T-LABEL
   fail code TTHROWSQ ;

\ Padding inside the type's bytes, and an LEB that ends before its bytes do.
: ADMITS ( -- )
   s" 8000" [: WLEB:U32@ ;] 0 2 READS
   s" 8080808000" [: WLEB:U64@ ;] 0 5 READS
   s" ffffffff7f" [: WLEB:S32@ ;] -1 5 READS
   s" ff7f" [: WLEB:S64@ ;] -1 2 READS
   s" 7f00" [: WLEB:S32@ ;] -1 1 READS ;

\ Still marked for more at the type's last byte, whatever follows.
: OVERLONG ( -- )
   s" 8080808080" [: STAGED$ WLEB:U32@ 2drop ;] WLEB:E-OVERLONG REFUSES
   s" ffffffff8f00" [: STAGED$ WLEB:U32@ 2drop ;] WLEB:E-OVERLONG REFUSES
   s" 808080808000" [: STAGED$ WLEB:S32@ 2drop ;] WLEB:E-OVERLONG REFUSES
   s" 8080808080808080808000" [: STAGED$ WLEB:U64@ 2drop ;] WLEB:E-OVERLONG REFUSES
   s" ffffffffffffffffffff7f" [: STAGED$ WLEB:S64@ 2drop ;] WLEB:E-OVERLONG REFUSES ;

: TRUNCATED ( -- )
   s" " [: STAGED$ WLEB:U32@ 2drop ;] WLEB:E-TRUNCATED REFUSES
   s" 80" [: STAGED$ WLEB:S32@ 2drop ;] WLEB:E-TRUNCATED REFUSES
   s" ffffffff" [: STAGED$ WLEB:U32@ 2drop ;] WLEB:E-TRUNCATED REFUSES
   s" 808080808080808080" [: STAGED$ WLEB:S64@ 2drop ;] WLEB:E-TRUNCATED REFUSES
   s" ffffffffffffffffff" [: STAGED$ WLEB:U64@ 2drop ;] WLEB:E-TRUNCATED REFUSES
   s" a negative length" T-LABEL
   [: STAGED SPAN:$ drop -1 WLEB:U32@ 2drop ;] E-SPAN-LENGTH TTHROWSQ ;

\ A last byte with bits past the type: they must be zero, or copies of the sign.
: OUT-OF-RANGE ( -- )
   s" 8080808010" [: STAGED$ WLEB:U32@ 2drop ;] WLEB:E-RANGE REFUSES
   s" ffffffff1f" [: STAGED$ WLEB:U32@ 2drop ;] WLEB:E-RANGE REFUSES
   s" ffffffff0f" [: STAGED$ WLEB:S32@ 2drop ;] WLEB:E-RANGE REFUSES
   s" 8080808008" [: STAGED$ WLEB:S32@ 2drop ;] WLEB:E-RANGE REFUSES
   s" ffffffff77" [: STAGED$ WLEB:S32@ 2drop ;] WLEB:E-RANGE REFUSES
   s" 80808080808080808002" [: STAGED$ WLEB:U64@ 2drop ;] WLEB:E-RANGE REFUSES
   s" ffffffffffffffffff7f" [: STAGED$ WLEB:U64@ 2drop ;] WLEB:E-RANGE REFUSES
   s" ffffffffffffffffff01" [: STAGED$ WLEB:S64@ 2drop ;] WLEB:E-RANGE REFUSES
   s" 8080808080808080807e" [: STAGED$ WLEB:S64@ 2drop ;] WLEB:E-RANGE REFUSES ;

\ ---- refused writes --------------------------------------------------------
\ fail, writing into OUT, refuses with code, and leaves the whole of OUT as the
\ canaries it was filled with.
: WRITE-REFUSES ( ptr u8 n [ -- ] n -- )
   {: a u:n fail code:n :}
   CANARY OUT SPAN:FILL
   a u T-LABEL
   fail code TTHROWSQ
   a u T-LABEL
   OUT CANARIES? TTRUE ;

\ A value outside its type is refused before a byte is written.
: WRITE-RANGE ( -- )
   s" a u32 of 2^32"
   [: U32-PAST OUT WLEB:U32! drop ;] WLEB:E-RANGE WRITE-REFUSES
   s" a negative u32"
   [: -1 OUT WLEB:U32! drop ;] WLEB:E-RANGE WRITE-REFUSES
   s" an s32 of 2^31"
   [: S32-MAX 1+ OUT WLEB:S32! drop ;] WLEB:E-RANGE WRITE-REFUSES
   s" an s32 below -2^31"
   [: S32-MAX invert 1- OUT WLEB:S32! drop ;] WLEB:E-RANGE WRITE-REFUSES
   s" a padded u32 of 2^32"
   [: U32-PAST OUT WLEB:U32-PAD! drop ;] WLEB:E-RANGE WRITE-REFUSES
   s" a negative padded u32"
   [: -1 OUT WLEB:U32-PAD! drop ;] WLEB:E-RANGE WRITE-REFUSES ;

\ A span short of the LEB is refused before a byte is written, and an exact fit
\ is written whole.
: CAPACITY ( -- )
   s" MAX-N's nine bytes in eight"
   [: MAX-N OUT 8 SPAN:TAKE WLEB:U64! drop ;] E-SPAN-CAPACITY WRITE-REFUSES
   s" and fills nine exactly" T-LABEL
   MAX-N OUT 9 SPAN:TAKE WLEB:U64! 9 T=
   s" a padded u32 in four bytes"
   [: 0 OUT 4 SPAN:TAKE WLEB:U32-PAD! drop ;] E-SPAN-CAPACITY WRITE-REFUSES
   s" a padded s64 in nine bytes"
   [: 0 OUT 9 SPAN:TAKE WLEB:S64-PAD! drop ;] E-SPAN-CAPACITY WRITE-REFUSES ;

\ ---- refused fields and patches ---------------------------------------------
\ A padded reader takes only a field of its width, inside the bytes.
: FIELD-REFUSALS ( -- )
   s" 10070b" [: STAGED$ CALL-AT WLEB:U32-PAD@ drop ;] WLEB:E-UNPADDED REFUSES
   s" 427e0b" [: STAGED$ CALL-AT WLEB:S64-PAD@ drop ;] WLEB:E-UNPADDED REFUSES
   s" 10" [: STAGED$ 2 WLEB:U32-PAD@ drop ;] E-SPAN-RANGE REFUSES ;

\ patch, run on the staged code, refuses with code, and leaves the code as it
\ was and the canaries past it.
: PATCH-REFUSES ( ptr u8 n [ -- ] n -- )
   {: a u:n patch code:n :}
   a u patch code REFUSES
   a u T-LABEL
   STAGED$ HEX$ a u T$=
   a u T-LABEL
   STAGED STAGED-LEN @ SPAN:SKIP CANARIES? TTRUE ;

\ A patch takes only a padded field of its width and a value of its type.
: PATCH-REFUSALS ( -- )
   s" 10070b" [: 9 STAGED-SPAN CALL-AT WLEB:U32-PATCH ;] WLEB:E-UNPADDED PATCH-REFUSES
   s" 10ff7f0b" [: 9 STAGED-SPAN CALL-AT WLEB:U32-PATCH ;] WLEB:E-UNPADDED PATCH-REFUSES
   s" 10808080" [: 9 STAGED-SPAN CALL-AT WLEB:U32-PATCH ;] WLEB:E-TRUNCATED PATCH-REFUSES
   s" 10808080808000" [: 9 STAGED-SPAN CALL-AT WLEB:U32-PATCH ;] WLEB:E-OVERLONG PATCH-REFUSES
   s" 10ffffffff1f" [: 9 STAGED-SPAN CALL-AT WLEB:U32-PATCH ;] WLEB:E-RANGE PATCH-REFUSES
   s" 10808080800b" [: U32-PAST STAGED-SPAN CALL-AT WLEB:U32-PATCH ;] WLEB:E-RANGE PATCH-REFUSES
   s" 10808080800b" [: 9 STAGED-SPAN 7 WLEB:U32-PATCH ;] E-SPAN-RANGE PATCH-REFUSES
   s" 10808080800b" [: 9 STAGED-SPAN -1 WLEB:U32-PATCH ;] E-SPAN-RANGE PATCH-REFUSES
   s" 427e0b" [: -2 STAGED-SPAN CALL-AT WLEB:S64-PATCH ;] WLEB:E-UNPADDED PATCH-REFUSES
   s" 42ffffffffffffffffff01" [: -2 STAGED-SPAN CALL-AT WLEB:S64-PATCH ;]
      WLEB:E-RANGE PATCH-REFUSES ;

\ ---- round trips ------------------------------------------------------------
\ v written by put reads back through get from exactly the bytes put wrote.
: BACK ( n [ n SPAN:span<u8> -- n ] [ ptr u8 n -- n n ] -- )
   {: v:n put get :}
   v OUT put execute {: k:n :}
   k OUT$ get execute k T= v T= ;

: PAD-BACK ( n [ n SPAN:span<u8> -- n ] [ ptr u8 n n -- n ] -- )
   {: v:n put get :}
   v OUT put execute {: k:n :}
   k OUT$ 0 get execute v T= ;

\ v reads back from its shortest LEB and its padded field in each type that
\ holds it, and the shortest-form readers take the padded field too.
: TRIP ( n -- )
   {: v:n :}
   v [: WLEB:U64! ;] [: WLEB:U64@ ;] BACK
   v [: WLEB:S64! ;] [: WLEB:S64@ ;] BACK
   v [: WLEB:S64-PAD! ;] [: WLEB:S64-PAD@ ;] PAD-BACK
   v [: WLEB:S64-PAD! ;] [: WLEB:S64@ ;] BACK
   v 0 >= v U32-MAX <= and if
      v [: WLEB:U32! ;] [: WLEB:U32@ ;] BACK
      v [: WLEB:U32-PAD! ;] [: WLEB:U32-PAD@ ;] PAD-BACK
      v [: WLEB:U32-PAD! ;] [: WLEB:U32@ ;] BACK
   then
   v S32-MAX invert >= v S32-MAX <= and if v [: WLEB:S32! ;] [: WLEB:S32@ ;] BACK then ;

\ Each power of two and its neighbours, of both signs, across all 64 bits.
: AROUND ( n -- )
   {: p:n :}
   p 1- TRIP p TRIP p 1+ TRIP p negate TRIP p invert TRIP ;

: ROUND-TRIPS ( -- )
   64 0 do 1 i lshift AROUND loop ;

public

: RUN ( -- )
   T-RESET
   U32-EDGES U64-EDGES S32-EDGES S64-EDGES
   IN-PLACE PADS
   ADMITS OVERLONG TRUNCATED OUT-OF-RANGE
   WRITE-RANGE CAPACITY FIELD-REFUSALS PATCH-REFUSALS
   ROUND-TRIPS ;

;package

WLEB-TEST:RUN
T-REPORT
