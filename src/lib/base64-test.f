\ base64-test.f - RFC 4648 base64: the section 10 vectors, the whole alphabet,
\ every rejection, and the RFC 6455 handshake it exists to answer.
\ Run: bin/hb --load src/lib/base64-test.f
\ The alphabet case decodes the alphabet itself; its 48 bytes are what the
\ system's `base64 -d` answers for it, so every character's value is pinned by
\ an oracle this file does not share.

require src/lib/string.f
require src/lib/test/assert.f
require src/lib/memory.f
require src/lib/span.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/base64.f
require src/lib/reason.f
require src/lib/crypto/sha1.f
require src/lib/test/guard-page.f

package BASE64-TEST

$A5 constant CANARY
48 constant ALPHABET-BYTES
344 constant ALL-CHARS            \ 256 bytes are 86 groups of four characters

512 SPAN-BUFFER: OUT
256 SPAN-BUFFER: ALL
ALL-CHARS SPAN-BUFFER: ALL-TEXT
96 SPAN-BUFFER: HEX
16 SPAN-BUFFER: STAGED
4 SPAN-BUFFER: OUT-4
3 SPAN-BUFFER: OUT-3
2 SPAN-BUFFER: OUT-2
SHA1:CTX-BYTES SPAN-BUFFER: CTX
SHA1:DIGEST-BYTES SPAN-BUFFER: DG
SHA256-CTX-BYTES BUFFER: URL-CTX
32 SPAN-BUFFER: URL-DG
variable STAGED-LEN

: OUT$ ( n -- str )
   OUT SPAN:$ STR:DATA swap >str ;

: ENCODES ( str str -- ) {: s:str want:str :}
   s OUT BASE64:ENCODE {: k:n :}
   want T-LABEL
   k OUT$ want T$= ;

: DECODES ( str str -- ) {: s:str want:str :}
   s OUT BASE64:DECODE {: k:n :}
   s T-LABEL
   k OUT$ want T$= ;

: BOTH ( str str -- ) {: s:str b:str :}
   s b ENCODES
   b s DECODES ;

: URL-ENCODES ( str str -- ) {: s:str want:str :}
   s OUT BASE64:ENCODE-URL {: k:n :}
   want T-LABEL
   k OUT$ want T$= ;

: URL-DECODES ( str str -- ) {: s:str want:str :}
   s OUT BASE64:DECODE-URL {: k:n :}
   s T-LABEL
   k OUT$ want T$= ;

: URL-BOTH ( str str -- ) {: s:str b:str :}
   s b URL-ENCODES
   b s URL-DECODES ;

\ RFC 4648's tail shapes without padding, and RFC 7636 Appendix A's -/_ case.
: URL-VECTORS ( -- )
   0 OUT$ 0 OUT$ URL-BOTH
   s" f" s" Zg" URL-BOTH
   s" fo" s" Zm8" URL-BOTH
   s" foo" s" Zm9v" URL-BOTH
   s" foob" s" Zm9vYg" URL-BOTH
   s" fooba" s" Zm9vYmE" URL-BOTH
   s" foobar" s" Zm9vYmFy" URL-BOTH
   3 ALL 0 SPAN:U8!
   236 ALL 1 SPAN:U8!
   255 ALL 2 SPAN:U8!
   224 ALL 3 SPAN:U8!
   193 ALL 4 SPAN:U8!
   ALL SPAN:$ STR:DATA 5 >str s" A-z_4ME" URL-BOTH ;

\ RFC 7636 Appendix B: encode the SHA-256 of the ASCII verifier, then decode
\ the published challenge back to those digest bytes.
: PKCE ( -- )
   URL-CTX s" dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"
      URL-DG SPAN:$ STR:DATA SHA256-IN
   URL-DG SPAN:$ s" E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM" URL-ENCODES
   s" E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
      URL-DG SPAN:$ URL-DECODES ;

\ RFC 4648 section 10.
: VECTORS ( -- )
   0 OUT$ 0 OUT$ BOTH
   s" f" s" Zg==" BOTH
   s" fo" s" Zm8=" BOTH
   s" foo" s" Zm9v" BOTH
   s" foob" s" Zm9vYg==" BOTH
   s" fooba" s" Zm9vYmE=" BOTH
   s" foobar" s" Zm9vYmFy" BOTH ;

: HEX$ ( str -- str ) {: s:str :}
   HEX SPAN:$ STR:DATA {: h :}
   s STR:LEN 0 ?do s STR:DATA i + c@ h i 2 * + BYTE>HEX loop
   h s STR:LEN 2 * >str ;

: ALPHABET ( -- )
   s" ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
   OUT BASE64:DECODE {: k:n :}
   s" the alphabet decodes to its 48 bytes" T-LABEL
   k ALPHABET-BYTES T=
   s" each character's value, against base64 -d" T-LABEL
   k OUT$ HEX$
   s" 00108310518720928b30d38f41149351559761969b71d79f8218a39259a7a29aabb2dbafc31cb3d35db7e39ebbf3dfbf"
   T$=
   ALPHABET-BYTES OUT$ ALL-TEXT BASE64:ENCODE {: t:n :}
   s" and those bytes encode to the alphabet" T-LABEL
   ALL-TEXT SPAN:$ STR:DATA t >str
   s" ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" T$= ;

: URL-ALPHABET ( -- )
   s" ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
   OUT BASE64:DECODE-URL {: k:n :}
   s" the URL alphabet decodes to its 48 bytes" T-LABEL
   k ALPHABET-BYTES T=
   k OUT$ HEX$
   s" 00108310518720928b30d38f41149351559761969b71d79f8218a39259a7a29aabb2dbafc31cb3d35db7e39ebbf3dfbf"
   T$=
   k OUT$ ALL-TEXT BASE64:ENCODE-URL {: t:n :}
   ALL-TEXT SPAN:$ STR:DATA t >str
   s" ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_" T$= ;

\ Every byte value, through both words and back.
: ROUND-TRIP ( -- )
   256 0 do i $FF and ALL i SPAN:U8! loop
   ALL SPAN:$ ALL-TEXT BASE64:ENCODE {: t:n :}
   s" 256 bytes encode to 344 characters" T-LABEL
   t ALL-CHARS T=
   ALL-TEXT SPAN:$ STR:DATA t >str OUT BASE64:DECODE {: k:n :}
   s" and decode back to every byte value" T-LABEL
   k OUT$ ALL SPAN:$ T$= ;

\ A rejection runs in a quotation, which sees no locals, so its input is staged.
: STAGE ( str -- ) {: s:str :}
   s STAGED SPAN:COPY
   s STR:LEN STAGED-LEN ! ;

: STAGED$ ( -- str )
   STAGED SPAN:$ STR:DATA STAGED-LEN @ >str ;

: REJECTS ( str n -- ) {: s:str code:n :}
   s STAGE
   s T-LABEL
   [: STAGED$ OUT BASE64:DECODE drop ;] code TTHROWSQ ;

: URL-REJECTS ( str n -- ) {: s:str code:n :}
   s STAGE
   s T-LABEL
   [: STAGED$ OUT BASE64:DECODE-URL drop ;] code TTHROWSQ ;

: SAID ( n str -- ) {: code:n ws:str :}
   code REASON:TEXT$ ws T$=
   REASON:RELEASE ;

: URL-REJECTIONS ( -- )
   s" Z" E-BASE64-LENGTH URL-REJECTS
   s" Zm9vZ" E-BASE64-LENGTH URL-REJECTS
   E-BASE64-LENGTH s" 1:6: expected a length that leaves 0, 2 or 3 characters in the last group of four, found the end of the input; write the data as BASE64:ENCODE-URL writes it" SAID
   s" Zg=" E-BASE64-PAD URL-REJECTS
   s" Zg==" E-BASE64-PAD URL-REJECTS
   E-BASE64-PAD s" 1:3: expected a character of A-Z, a-z, 0-9, - or _, found `=`; leave `=` out: DECODE-URL takes no padding" SAID
   s" Zm+v" E-BASE64-CHAR URL-REJECTS
   s" Zm/v" E-BASE64-CHAR URL-REJECTS
   s" Zm9 " E-BASE64-CHAR URL-REJECTS
   E-BASE64-CHAR s" 1:4: expected a character of A-Z, a-z, 0-9, - or _, found byte $20; write the data as BASE64:ENCODE-URL writes it" SAID
   s" Zh" E-BASE64-PAD URL-REJECTS
   s" Zm9" E-BASE64-PAD URL-REJECTS
   E-BASE64-PAD s" 1:3: expected a character whose unused low bits are zero, found `9`; write the data as BASE64:ENCODE-URL writes it" SAID
   s" Zm9v" STAGE $80 STAGED 2 SPAN:U8!
   s" URL rejects a byte above ASCII" T-LABEL
   [: STAGED$ OUT BASE64:DECODE-URL drop ;] E-BASE64-CHAR TTHROWSQ ;

: URL-CAPACITY ( -- )
   CANARY OUT SPAN:FILL
   s" Zm9vZg!" STAGE
   s" URL rejects a bad last group after a good one" T-LABEL
   [: STAGED$ OUT BASE64:DECODE-URL drop ;] E-BASE64-CHAR TTHROWSQ
   s" URL rejection leaves the output untouched" T-LABEL
   OUT 0 SPAN:U8@ CANARY T=
   OUT 3 SPAN:U8@ CANARY T=
   s" Zm9vZh" STAGE
   [: STAGED$ OUT BASE64:DECODE-URL drop ;] E-BASE64-PAD TTHROWSQ
   OUT 0 SPAN:U8@ CANARY T=
   OUT 3 SPAN:U8@ CANARY T=
   CANARY OUT-3 SPAN:FILL
   [: s" foo" OUT-3 BASE64:ENCODE-URL drop ;] E-SPAN-CAPACITY TTHROWSQ
   OUT-3 0 SPAN:U8@ CANARY T=
   CANARY OUT-2 SPAN:FILL
   [: s" Zm9v" OUT-2 BASE64:DECODE-URL drop ;] E-SPAN-CAPACITY TTHROWSQ
   OUT-2 0 SPAN:U8@ CANARY T=
   [: s" fo" OUT-2 BASE64:ENCODE-URL drop ;] E-SPAN-CAPACITY TTHROWSQ
   OUT-2 0 SPAN:U8@ CANARY T=
   OUT-2 1 SPAN:U8@ CANARY T=
   s" f" OUT-2 BASE64:ENCODE-URL 2 T=
   s" fo" OUT-3 BASE64:ENCODE-URL 3 T=
   s" foo" OUT-4 BASE64:ENCODE-URL 4 T=
   s" Zg" OUT-2 BASE64:DECODE-URL 1 T=
   s" Zm8" OUT-2 BASE64:DECODE-URL 2 T=
   s" Zm9v" OUT-3 BASE64:DECODE-URL 3 T=
   [: 0 OUT$ STR:DATA -1 >str OUT BASE64:ENCODE-URL drop ;] E-SPAN-LENGTH TTHROWSQ
   [: 0 OUT$ STR:DATA -1 >str OUT BASE64:DECODE-URL drop ;] E-SPAN-LENGTH TTHROWSQ
   [: 0 OUT$ STR:DATA MEM-MAX-N negate 1- >str OUT BASE64:DECODE-URL drop ;]
      E-SPAN-LENGTH TTHROWSQ
   [: 7 [char] A GUARD-PAGE:TAIL 8 >str OUT-3 BASE64:DECODE-URL drop ;]
      E-SPAN-CAPACITY TTHROWSQ
   [: 7 [char] A GUARD-PAGE:TAIL MEM-MAX-N 3 invert and >str OUT-3
      BASE64:DECODE-URL drop ;] E-SPAN-CAPACITY TTHROWSQ
   [: 0 OUT$ STR:DATA MEM-MAX-N 2 - >str OUT BASE64:ENCODE-URL drop ;]
      E-SPAN-CAPACITY TTHROWSQ ;

: LENGTHS ( -- )
   s" Z" E-BASE64-LENGTH REJECTS
   s" Zg=" E-BASE64-LENGTH REJECTS
   s" Zm9vY" E-BASE64-LENGTH REJECTS
   E-BASE64-LENGTH s" 1:6: expected a length that is a multiple of four, found the end of the input; write the data as BASE64:ENCODE writes it" SAID ;

: CHARS ( -- )
   s" Zm-v" E-BASE64-CHAR REJECTS
   E-BASE64-CHAR s" 1:3: expected a character of A-Z, a-z, 0-9, + or /, found `-`; write the data as BASE64:ENCODE writes it" SAID
   s" Zm_v" E-BASE64-CHAR REJECTS
   s" Zm9 " E-BASE64-CHAR REJECTS
   s" Zg=!" E-BASE64-CHAR REJECTS
   s" Zm9v" STAGE $80 STAGED 2 SPAN:U8!
   s" a byte above ASCII" T-LABEL
   [: STAGED$ OUT BASE64:DECODE drop ;] E-BASE64-CHAR TTHROWSQ ;


: WANT ( str n -- n )
   {: s:str c:n :}
   s STR:LEN 0 ?do s STR:DATA i + c@ c = if i unloop exit then loop
   c [char] = = if E-BASE64-PAD else E-BASE64-CHAR then ;


: DECODED ( [ -- ] -- n )
   catch {: code:n :}
   code 0<> if code exit then
   OUT 0 SPAN:U8@ 2 rshift ;


: JUDGES ( str [ -- ] -- )
   {: s:str q :}
   256 0 do
      s" ?AAA" STAGE i STAGED 0 SPAN:U8!
      s" each byte as a group's first character" T-LABEL
      q DECODED s i WANT T=
   loop ;


\ Every byte is judged as Tables 1 and 2 say.
: EVERY-BYTE ( -- )
   s" ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
   [: STAGED$ OUT BASE64:DECODE drop ;] JUDGES
   s" ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
   [: STAGED$ OUT BASE64:DECODE-URL drop ;] JUDGES ;


\ Padding ends the last group and only it, and the bits it leaves below the
\ last byte are zero: 'h' leaves 0001 and '9' leaves 01.
: PADS ( -- )
   s" ====" E-BASE64-PAD REJECTS
   s" Z===" E-BASE64-PAD REJECTS
   s" Zg=v" E-BASE64-PAD REJECTS
   E-BASE64-PAD s" 1:3: expected a character of A-Z, a-z, 0-9, + or /, found `=`; use `=` only to end the last group" SAID
   s" Zg==Zg==" E-BASE64-PAD REJECTS
   s" Zh==" E-BASE64-PAD REJECTS
   E-BASE64-PAD s" 1:2: expected a character whose unused low bits are zero, found `h`; write the data as BASE64:ENCODE writes it" SAID
   s" Zm9=" E-BASE64-PAD REJECTS ;

\ A rejection writes nothing: not the valid first group of a bad input, and not
\ a result the span has no room for. The exact fit is written.
: CAPACITY ( -- )
   CANARY OUT SPAN:FILL
   s" Zm9vZg=!" STAGE
   s" a bad last group, after a good one" T-LABEL
   [: STAGED$ OUT BASE64:DECODE drop ;] E-BASE64-CHAR TTHROWSQ
   s" leaves the output as it was" T-LABEL
   OUT 0 SPAN:U8@ CANARY T=
   CANARY OUT-3 SPAN:FILL
   s" ENCODE rejects a span one byte short" T-LABEL
   [: s" foo" OUT-3 BASE64:ENCODE drop ;] E-SPAN-CAPACITY TTHROWSQ
   s" and writes none of it" T-LABEL
   OUT-3 0 SPAN:U8@ CANARY T=
   CANARY OUT-2 SPAN:FILL
   s" DECODE rejects a span one byte short" T-LABEL
   [: s" Zm9v" OUT-2 BASE64:DECODE drop ;] E-SPAN-CAPACITY TTHROWSQ
   s" and writes none of it" T-LABEL
   OUT-2 0 SPAN:U8@ CANARY T=
   s" ENCODE fills a span of exactly four" T-LABEL
   s" foo" OUT-4 BASE64:ENCODE 4 T=
   s" DECODE fills a span of exactly three" T-LABEL
   s" Zm9v" OUT-3 BASE64:DECODE 3 T=
   s" ENCODE rejects a negative length" T-LABEL
   [: 0 OUT$ STR:DATA -1 >str OUT BASE64:ENCODE drop ;] E-SPAN-LENGTH TTHROWSQ
   s" DECODE rejects a negative length" T-LABEL
   [: 0 OUT$ STR:DATA -1 >str OUT BASE64:DECODE drop ;] E-SPAN-LENGTH TTHROWSQ
   [: 0 OUT$ STR:DATA MEM-MAX-N negate 1- >str OUT BASE64:DECODE drop ;] E-SPAN-LENGTH TTHROWSQ
   \ Eight characters decode to at least four bytes, so a three-byte span is
   \ rejected before the input, which ends at an inaccessible page, is read.
   s" DECODE measures the span before it reads the input" T-LABEL
   [: 7 [char] A GUARD-PAGE:TAIL 8 >str OUT-3 BASE64:DECODE drop ;] E-SPAN-CAPACITY TTHROWSQ
   [: 7 [char] A GUARD-PAGE:TAIL MEM-MAX-N 3 invert and >str OUT-3 BASE64:DECODE drop ;]
      E-SPAN-CAPACITY TTHROWSQ
   s" ENCODE rejects a length whose encoded size wraps" T-LABEL
   [: 0 OUT$ STR:DATA MEM-MAX-N 2 - >str OUT BASE64:ENCODE drop ;] E-SPAN-CAPACITY TTHROWSQ ;

\ The text after the vector's "x" is the word's, cut back to "x" first.
: APPENDS ( ^VEC:vector<u8> str [ str ^VEC:vector<u8> -- ] str -- )
   {: v:^ s:str put want:str :}
   v 1 VEC:RESIZE
   s v put execute
   want T-LABEL
   v VEC:COUNT want STR:LEN 1+ T=
   v VEC:BYTES STR:DATA {: at :}
   at c@ [char] x T=
   at 1+ v VEC:COUNT 1- >str want T$= ;

\ ENCODE+ and ENCODE-URL+ append RFC 4648 section 10's text to a vector.
: APPENDING ( -- )
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: v:^ :}
   s" x" v VEC:APPEND
   v s" foobar" [: BASE64:ENCODE+ ;] s" Zm9vYmFy" APPENDS
   v s" fooba" [: BASE64:ENCODE+ ;] s" Zm9vYmE=" APPENDS
   v s" f" [: BASE64:ENCODE+ ;] s" Zg==" APPENDS
   v s" " [: BASE64:ENCODE+ ;] s" " APPENDS
   v s" fo" [: BASE64:ENCODE-URL+ ;] s" Zm8" APPENDS
   v s" " [: BASE64:ENCODE-URL+ ;] s" " APPENDS
   v 1 VEC:RESIZE
   \ At this length 4n wraps to 4: sized by 4n+2 first, the vector would grow by 2.
   s" ENCODE-URL+ and ENCODE+ reject a length no vector can hold" T-LABEL
   [: 0 OUT$ STR:DATA $4000000000000001 >str v BASE64:ENCODE-URL+ ;] E-MEM-MAP TTHROWSQ
   [: 0 OUT$ STR:DATA MEM-MAX-N 1- >str v BASE64:ENCODE+ ;] E-VEC-CAPACITY TTHROWSQ
   s" and leave the vector as it was" T-LABEL
   v VEC:COUNT 1 T=
   v VEC:DELETE ;

\ RFC 6455 section 1.3: the key decodes to sixteen bytes, and the accept value
\ is the base64 of SHA-1 over the key and the protocol's GUID, fed as a server
\ receives them.
: HANDSHAKE ( -- )
   s" dGhlIHNhbXBsZSBub25jZQ==" s" the sample nonce" DECODES
   CTX SHA1:START
   CTX s" dGhlIHNhbXBsZSBub25jZQ==" SHA1:FEED
   CTX s" 258EAFA5-E914-47DA-95CA-C5AB0DC85B11" SHA1:FEED
   CTX SHA1:FINISH DG SHA1:DIGEST!
   DG SPAN:$ s" s3pPLMBiTxaQ9kYGzzhZRbK+xOo=" ENCODES ;

public

: RUN ( -- )
   T-RESET
   VECTORS ALPHABET ROUND-TRIP LENGTHS CHARS PADS CAPACITY HANDSHAKE
   URL-VECTORS URL-ALPHABET PKCE URL-REJECTIONS URL-CAPACITY EVERY-BYTE
   APPENDING ;

;package

BASE64-TEST:RUN
T-REPORT
