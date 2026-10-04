\ base64-test.f - RFC 4648 base64: the section 10 vectors, the whole alphabet,
\ every refusal, and the RFC 6455 handshake it exists to answer.
\ Run: bin/hb --load lib/base64-test.f
\
\ The alphabet case decodes the alphabet itself; its 48 bytes are what the
\ system's `base64 -d` answers for it, so every character's value is pinned by
\ an oracle this file does not share.

require lib/test.f
require lib/memory.f
require lib/span.f
require lib/base64.f
require lib/crypto/sha1.f
require lib/test/guard-page.f

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

: OUT$ ( n -- ptr u8 n )
   OUT SPAN:$ drop swap ;

: ENCODES ( ptr u8 n ptr u8 n -- ) {: a u:n want wu:n :}
   a u OUT BASE64:ENCODE {: k:n :}
   want wu T-LABEL
   k OUT$ want wu T$= ;

: DECODES ( ptr u8 n ptr u8 n -- ) {: a u:n want wu:n :}
   a u OUT BASE64:DECODE {: k:n :}
   a u T-LABEL
   k OUT$ want wu T$= ;

: BOTH ( ptr u8 n ptr u8 n -- ) {: a u:n b v:n :}
   a u b v ENCODES
   b v a u DECODES ;

: URL-ENCODES ( ptr u8 n ptr u8 n -- ) {: a u:n want wu:n :}
   a u OUT BASE64:ENCODE-URL {: k:n :}
   want wu T-LABEL
   k OUT$ want wu T$= ;

: URL-DECODES ( ptr u8 n ptr u8 n -- ) {: a u:n want wu:n :}
   a u OUT BASE64:DECODE-URL {: k:n :}
   a u T-LABEL
   k OUT$ want wu T$= ;

: URL-BOTH ( ptr u8 n ptr u8 n -- ) {: a u:n b v:n :}
   a u b v URL-ENCODES
   b v a u URL-DECODES ;

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
   ALL SPAN:$ drop 5 s" A-z_4ME" URL-BOTH ;

\ RFC 7636 Appendix B: encode the SHA-256 of the ASCII verifier, then decode
\ the published challenge back to those digest bytes.
: PKCE ( -- )
   URL-CTX s" dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"
      URL-DG SPAN:$ drop SHA256-IN
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

: HEX$ ( ptr u8 n -- ptr u8 n ) {: a u:n :}
   HEX SPAN:$ drop {: h :}
   u 0 ?do a i + c@ h i 2 * + BYTE>HEX loop
   h u 2 * ;

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
   ALL-TEXT SPAN:$ drop t
   s" ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" T$= ;

\ Every byte value, through both words and back.
: ROUND-TRIP ( -- )
   256 0 do i $FF and ALL i SPAN:U8! loop
   ALL SPAN:$ ALL-TEXT BASE64:ENCODE {: t:n :}
   s" 256 bytes encode to 344 characters" T-LABEL
   t ALL-CHARS T=
   ALL-TEXT SPAN:$ drop t OUT BASE64:DECODE {: k:n :}
   s" and decode back to every byte value" T-LABEL
   k OUT$ ALL SPAN:$ T$= ;

\ A refusal runs in a quotation, which sees no locals, so its input is staged.
: STAGE ( ptr u8 n -- ) {: a u:n :}
   a u STAGED SPAN:COPY
   u STAGED-LEN ! ;

: STAGED$ ( -- ptr u8 n )
   STAGED SPAN:$ drop STAGED-LEN @ ;

: REFUSES ( ptr u8 n n -- ) {: a u:n code:n :}
   a u STAGE
   a u T-LABEL
   [: STAGED$ OUT BASE64:DECODE drop ;] code TTHROWSQ ;

: URL-REFUSES ( ptr u8 n n -- ) {: a u:n code:n :}
   a u STAGE
   a u T-LABEL
   [: STAGED$ OUT BASE64:DECODE-URL drop ;] code TTHROWSQ ;

: URL-REFUSALS ( -- )
   s" Z" E-BASE64-LENGTH URL-REFUSES
   s" Zm9vZ" E-BASE64-LENGTH URL-REFUSES
   s" Zg=" E-BASE64-PAD URL-REFUSES
   s" Zg==" E-BASE64-PAD URL-REFUSES
   s" Zm+v" E-BASE64-CHAR URL-REFUSES
   s" Zm/v" E-BASE64-CHAR URL-REFUSES
   s" Zm9 " E-BASE64-CHAR URL-REFUSES
   s" Zh" E-BASE64-PAD URL-REFUSES
   s" Zm9" E-BASE64-PAD URL-REFUSES
   s" Zm9v" STAGE $80 STAGED 2 SPAN:U8!
   s" URL rejects a byte above ASCII" T-LABEL
   [: STAGED$ OUT BASE64:DECODE-URL drop ;] E-BASE64-CHAR TTHROWSQ ;

: URL-CAPACITY ( -- )
   CANARY OUT SPAN:FILL
   s" Zm9vZg!" STAGE
   s" URL rejects a bad last group after a good one" T-LABEL
   [: STAGED$ OUT BASE64:DECODE-URL drop ;] E-BASE64-CHAR TTHROWSQ
   s" URL refusal leaves the output untouched" T-LABEL
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
   s" f" OUT-2 BASE64:ENCODE-URL 2 T=
   s" fo" OUT-3 BASE64:ENCODE-URL 3 T=
   s" foo" OUT-4 BASE64:ENCODE-URL 4 T=
   s" Zg" OUT-2 BASE64:DECODE-URL 1 T=
   s" Zm8" OUT-2 BASE64:DECODE-URL 2 T=
   s" Zm9v" OUT-3 BASE64:DECODE-URL 3 T=
   [: 0 OUT$ drop -1 OUT BASE64:ENCODE-URL drop ;] E-SPAN-LENGTH TTHROWSQ
   [: 0 OUT$ drop -1 OUT BASE64:DECODE-URL drop ;] E-SPAN-LENGTH TTHROWSQ
   [: 0 OUT$ drop MEM-MAX-N negate 1- OUT BASE64:DECODE-URL drop ;]
      E-SPAN-LENGTH TTHROWSQ
   [: 7 [char] A GUARD-PAGE:TAIL 8 OUT-3 BASE64:DECODE-URL drop ;]
      E-SPAN-CAPACITY TTHROWSQ
   [: 7 [char] A GUARD-PAGE:TAIL MEM-MAX-N 3 invert and OUT-3
      BASE64:DECODE-URL drop ;] E-SPAN-CAPACITY TTHROWSQ
   [: 0 OUT$ drop MEM-MAX-N 2 - OUT BASE64:ENCODE-URL drop ;]
      E-SPAN-CAPACITY TTHROWSQ ;

: LENGTHS ( -- )
   s" Z" E-BASE64-LENGTH REFUSES
   s" Zg=" E-BASE64-LENGTH REFUSES
   s" Zm9vY" E-BASE64-LENGTH REFUSES ;

: CHARS ( -- )
   s" Zm-v" E-BASE64-CHAR REFUSES
   s" Zm_v" E-BASE64-CHAR REFUSES
   s" Zm9 " E-BASE64-CHAR REFUSES
   s" Zg=!" E-BASE64-CHAR REFUSES
   s" Zm9v" STAGE $80 STAGED 2 SPAN:U8!
   s" a byte above ASCII" T-LABEL
   [: STAGED$ OUT BASE64:DECODE drop ;] E-BASE64-CHAR TTHROWSQ ;

\ Padding ends the last group and only it, and the bits it leaves below the
\ last byte are zero: 'h' leaves 0001 and '9' leaves 01.
: PADS ( -- )
   s" ====" E-BASE64-PAD REFUSES
   s" Z===" E-BASE64-PAD REFUSES
   s" Zg=v" E-BASE64-PAD REFUSES
   s" Zg==Zg==" E-BASE64-PAD REFUSES
   s" Zh==" E-BASE64-PAD REFUSES
   s" Zm9=" E-BASE64-PAD REFUSES ;

\ A refusal writes nothing: not the valid first group of a bad input, and not
\ a result the span has no room for. The exact fit is written.
: CAPACITY ( -- )
   CANARY OUT SPAN:FILL
   s" Zm9vZg=!" STAGE
   s" a bad last group, after a good one" T-LABEL
   [: STAGED$ OUT BASE64:DECODE drop ;] E-BASE64-CHAR TTHROWSQ
   s" leaves the output as it was" T-LABEL
   OUT 0 SPAN:U8@ CANARY T=
   CANARY OUT-3 SPAN:FILL
   s" ENCODE refuses a span one byte short" T-LABEL
   [: s" foo" OUT-3 BASE64:ENCODE drop ;] E-SPAN-CAPACITY TTHROWSQ
   s" and writes none of it" T-LABEL
   OUT-3 0 SPAN:U8@ CANARY T=
   CANARY OUT-2 SPAN:FILL
   s" DECODE refuses a span one byte short" T-LABEL
   [: s" Zm9v" OUT-2 BASE64:DECODE drop ;] E-SPAN-CAPACITY TTHROWSQ
   s" and writes none of it" T-LABEL
   OUT-2 0 SPAN:U8@ CANARY T=
   s" ENCODE fills a span of exactly four" T-LABEL
   s" foo" OUT-4 BASE64:ENCODE 4 T=
   s" DECODE fills a span of exactly three" T-LABEL
   s" Zm9v" OUT-3 BASE64:DECODE 3 T=
   s" ENCODE refuses a negative length" T-LABEL
   [: 0 OUT$ drop -1 OUT BASE64:ENCODE drop ;] E-SPAN-LENGTH TTHROWSQ
   s" DECODE refuses a negative length" T-LABEL
   [: 0 OUT$ drop -1 OUT BASE64:DECODE drop ;] E-SPAN-LENGTH TTHROWSQ
   [: 0 OUT$ drop MEM-MAX-N negate 1- OUT BASE64:DECODE drop ;] E-SPAN-LENGTH TTHROWSQ
   \ Eight characters decode to at least four bytes, so a three-byte span is
   \ refused before the input, which ends at an inaccessible page, is read.
   s" DECODE measures the span before it reads the input" T-LABEL
   [: 7 [char] A GUARD-PAGE:TAIL 8 OUT-3 BASE64:DECODE drop ;] E-SPAN-CAPACITY TTHROWSQ
   [: 7 [char] A GUARD-PAGE:TAIL MEM-MAX-N 3 invert and OUT-3 BASE64:DECODE drop ;]
      E-SPAN-CAPACITY TTHROWSQ
   s" ENCODE refuses a length whose encoded size wraps" T-LABEL
   [: 0 OUT$ drop MEM-MAX-N 2 - OUT BASE64:ENCODE drop ;] E-SPAN-CAPACITY TTHROWSQ ;

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
   URL-VECTORS PKCE URL-REFUSALS URL-CAPACITY ;

;package

BASE64-TEST:RUN
T-REPORT
