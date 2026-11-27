\ md5-test.f - MD5 against RFC 1321's test suite, a message fed in pieces, the
\ lengths where padding changes shape, and the context contract.
\ Run: bin/hb --load src/lib/crypto/md5-test.f
\
\ The block-boundary digests are runs of 'a', as `md5sum` answers them.

require src/lib/test/assert.f
require src/lib/span.f
require src/lib/crypto/md5.f

package MD5-TEST

64 constant A-COUNT
$A5 constant CANARY

MD5:CTX-BYTES SPAN-BUFFER: CTX-A
MD5:CTX-BYTES SPAN-BUFFER: CTX-B
MD5:CTX-BYTES 1- SPAN-BUFFER: CTX-SHORT
MD5:DIGEST-BYTES SPAN-BUFFER: DG
MD5:DIGEST-BYTES SPAN-BUFFER: DG-B
MD5:DIGEST-BYTES 1- SPAN-BUFFER: DG-SHORT
32 SPAN-BUFFER: HEX
A-COUNT SPAN-BUFFER: A-BYTES

: FILL-A ( -- )
   [char] a A-BYTES SPAN:FILL ;

: A$ ( n -- str )
   A-BYTES SPAN:$ STR:DATA swap >str ;

: HEX$ ( MD5:digest -- str )
   DG MD5:DIGEST!
   DG SPAN:$ STR:DATA {: d :}
   HEX SPAN:$ STR:DATA {: h :}
   MD5:DIGEST-BYTES 0 do d i + c@ h i 2 * + BYTE>HEX loop
   h 32 >str ;

: DIGEST= ( MD5:digest str -- ) {: got want:str :}
   got HEX$ want T$= ;

: VECTOR ( str str -- ) {: msg:str want:str :}
   CTX-A msg MD5:HASH want DIGEST= ;

: RFC ( -- )
   s" RFC 1321: the empty message" T-LABEL
   s" " s" d41d8cd98f00b204e9800998ecf8427e" VECTOR
   s" RFC 1321: a" T-LABEL
   s" a" s" 0cc175b9c0f1b6a831c399e269772661" VECTOR
   s" RFC 1321: abc" T-LABEL
   s" abc" s" 900150983cd24fb0d6963f7d28e17f72" VECTOR
   s" RFC 1321: message digest" T-LABEL
   s" message digest" s" f96b697d7cb7938d525a2f31aaf161d0" VECTOR
   s" RFC 1321: the alphabet" T-LABEL
   s" abcdefghijklmnopqrstuvwxyz" s" c3fcd3d76192e4007dfb496cca67e13b" VECTOR
   s" RFC 1321: letters and digits" T-LABEL
   s" ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
   s" d174ab98d277d9f5a5611c2c9f419d9f" VECTOR
   s" RFC 1321: eighty digits" T-LABEL
   s" 12345678901234567890123456789012345678901234567890123456789012345678901234567890"
   s" 57edf4a22be3c955ac49da2e2107b67a" VECTOR ;

\ The eighty digits in pieces of 1, 62 and 17 bytes: the last piece fills the
\ first block and spills into the second.
: PIECES ( -- )
   s" 12345678901234567890123456789012345678901234567890123456789012345678901234567890"
   {: msg:str :}
   CTX-B MD5:START
   CTX-B msg STR:DATA 1 >str MD5:FEED
   CTX-B msg STR:DATA 1 + 62 >str MD5:FEED
   CTX-B msg STR:DATA 63 + 17 >str MD5:FEED
   s" a message fed across a block boundary equals its HASH" T-LABEL
   CTX-B MD5:FINISH DG MD5:DIGEST!
   CTX-A msg MD5:HASH DG-B MD5:DIGEST!
   DG SPAN:$ DG-B SPAN:$ T$= ;

\ The byte-wise stream fills the tail through every position
\ before the length lands.
: BOUNDARY ( n str -- ) {: u:n want:str :}
   CTX-A u A$ MD5:HASH want DIGEST=
   CTX-B MD5:START
   u 0 do CTX-B 1 A$ MD5:FEED loop
   CTX-B MD5:FINISH want DIGEST= ;

: BOUNDARIES ( -- )
   s" 55 bytes: the length still fits the last block" T-LABEL
   55 s" ef1772b6dff9a122358552954ad0df65" BOUNDARY
   s" 56 bytes: the length needs a second padding block" T-LABEL
   56 s" 3b0c8ac703f828b04c6c197006d17218" BOUNDARY
   s" 64 bytes: exactly one block" T-LABEL
   64 s" 014842d480b571495a4a0363793f7367" BOUNDARY ;

: FINISH-KEEPS-STREAM ( -- )
   CTX-A MD5:START
   CTX-A s" ab" MD5:FEED
   s" FINISH answers the digest so far" T-LABEL
   CTX-A MD5:FINISH
   s" 187ef4436122d1cc2f40dc2b92f0eba0" DIGEST=
   s" a second FINISH answers the same digest" T-LABEL
   CTX-A MD5:FINISH
   s" 187ef4436122d1cc2f40dc2b92f0eba0" DIGEST=
   CTX-A s" c" MD5:FEED
   s" FEED after FINISH continues the stream" T-LABEL
   CTX-A MD5:FINISH
   s" 900150983cd24fb0d6963f7d28e17f72" DIGEST= ;

: INTERLEAVED ( -- )
   CTX-A MD5:START
   CTX-B MD5:START
   CTX-A s" a" MD5:FEED
   CTX-B s" abcdbcdecdefdefgefghfghighijhijk" MD5:FEED
   CTX-A s" bc" MD5:FEED
   CTX-B s" ijkljklmklmnlmnomnopnopq" MD5:FEED
   s" interleaved: the first context" T-LABEL
   CTX-A MD5:FINISH
   s" 900150983cd24fb0d6963f7d28e17f72" DIGEST=
   s" interleaved: the second context" T-LABEL
   CTX-B MD5:FINISH
   s" 8215ef0796a20bcaaae116d3876c664a" DIGEST= ;

: REJECTIONS ( -- )
   s" START rejects a context one byte short" T-LABEL
   [: CTX-SHORT MD5:START ;] E-SPAN-CAPACITY TTHROWSQ
   s" FEED rejects a context one byte short" T-LABEL
   [: CTX-SHORT s" abc" MD5:FEED ;] E-SPAN-CAPACITY TTHROWSQ
   s" FINISH rejects a context one byte short" T-LABEL
   [: CTX-SHORT MD5:FINISH HEX$ drop ;] E-SPAN-CAPACITY TTHROWSQ
   CTX-A MD5:START
   s" FEED rejects a negative length" T-LABEL
   [: CTX-A 0 A$ STR:DATA -1 >str MD5:FEED ;] E-SPAN-LENGTH TTHROWSQ
   CANARY DG-SHORT SPAN:FILL
   s" DIGEST! rejects a span one byte short" T-LABEL
   [: CTX-A s" abc" MD5:HASH DG-SHORT MD5:DIGEST! ;] E-SPAN-CAPACITY TTHROWSQ
   s" and writes none of it" T-LABEL
   DG-SHORT 0 SPAN:U8@ CANARY T= ;

public

: RUN ( -- )
   T-RESET
   FILL-A
   RFC PIECES BOUNDARIES FINISH-KEEPS-STREAM INTERLEAVED REJECTIONS ;

;package

MD5-TEST:RUN
T-REPORT
