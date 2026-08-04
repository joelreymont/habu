\ gpt2-generate-test.f - persistent GPT-2 production path.

require lib/test.f
require lib/float32-buffer.f
require lib/fs-path.f
require test/checker-assert.f
require maki/cuda-run.f
require maki/infer/gpt2-generate.f
require maki/infer/gpt2-reference-data.f

package GPT2
private

4097 constant LONG-N
1024 constant CONTEXT-N
50257 constant TEST-VOCAB
TEST-VOCAB 4 * constant TEST-LOGIT-BYTES
$A5 constant GUARD
64 constant E-USAGE

create OUT OUTPUT-CAP allot
create BEFORE OUTPUT-CAP allot
create LONG LONG-N allot
create CONTEXT CONTEXT-N allot

create U-JA 33768 , 98 , 17312 , 105 , 45739 , 252 ,
create U-AR 149 , 97 , 149 , 95 ,
create U-LATIN 2616 , 38776 ,
create MAL-C3-ASCII $C3 c, $41 c,

create T-C-ASCII 3 , 3 , 2 ,
create T-C-PREFIX 2 , 2 , 2 ,
create T-C-CONTRACTIONS
   1 , 2 , 2 , 2 , 2 , 3 , 2 , 3 , 2 , 2 , 2 , 3 , 2 , 2 ,
create T-C-TRAILING 3 , 1 , 2 ,
create T-C-FULLWIDTH 4 , 7 ,
create T-C-ASTRAL 5 , 5 ,
create T-C-COMBINING 1 , 2 , 1 ,
create T-C-PUNCT 2 ,

create T-WHITE-SPACE
   $09 c, $0A c, $0B c, $0C c, $0D c, $20 c,
   $C2 c, $85 c, $C2 c, $A0 c,
   $E1 c, $9A c, $80 c,
   $E2 c, $80 c, $80 c, $E2 c, $80 c, $81 c,
   $E2 c, $80 c, $82 c, $E2 c, $80 c, $83 c,
   $E2 c, $80 c, $84 c, $E2 c, $80 c, $85 c,
   $E2 c, $80 c, $86 c, $E2 c, $80 c, $87 c,
   $E2 c, $80 c, $88 c, $E2 c, $80 c, $89 c,
   $E2 c, $80 c, $8A c,
   $E2 c, $80 c, $A8 c, $E2 c, $80 c, $A9 c,
   $E2 c, $80 c, $AF c, $E2 c, $81 c, $9F c,
   $E3 c, $80 c, $80 c,

create T-C-TWO-ASCII 1 , 2 ,
create T-C-TWO-NBSP 2 , 2 , 1 ,
create T-C-MIXED-SPACE 1 , 3 , 1 ,

create T-M1 $C3 c, $41 c,
create T-M2 $C0 c, $41 c,
create T-M3 $E2 c, $41 c,
create T-M4 $F0 c, $41 c,
create T-M5 $80 c, $41 c,
create T-M6 $ED c, $A0 c, $80 c, $41 c,
create T-M7 $F4 c, $90 c, $80 c, $80 c, $41 c,
create T-M8 $E2 c, $82 c, $20 c,
create T-M9 $C0 c, $80 c, $41 c,

create T-C-M1 1 , 1 ,
create T-C-M2 1 , 1 ,
create T-C-M3 1 , 1 ,
create T-C-M4 1 , 1 ,
create T-C-M5 1 , 1 ,
create T-C-M6 3 , 1 ,
create T-C-M7 4 , 1 ,
create T-C-M8 2 , 1 ,
create T-C-M9 2 , 1 ,

: YES ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! -1 T= ;

: UNK ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 1 T= ;

: T-SURFACE ( -- )
   s" GEN-SIG ( GPT2:model ptr u8 CAD-NUM:byte-len CAD-NUM:item-count ptr u8 CAD-NUM:byte-len -- GPT2:model result<CAD-NUM:byte-len,n> ) GPT2:GENERATE" YES
   s" GEN-CX ( GPT2:model -- GPT2:model CAD-NUM:item-count ) GPT2:CONTEXT-LEN" YES
   s" GEN-EOS ( GPT2:model -- GPT2:model n ) GPT2:EOS-ID" YES
   s" GEN-BL-PRIVATE ( CAD-NUM:byte-len -- n ) GPT2:BL>N" UNK
   s" GEN-IC-PRIVATE ( CAD-NUM:item-count -- n ) GPT2:IC>N" UNK
   s" GPT2:BL>N" XREF-FIND XREF-FOUND? TFALSE
   s" GPT2:IC>N" XREF-FIND XREF-FOUND? TFALSE
   s" GPT2-GEN" XREF-NAMESPACE-WL XREF-FIND-WL XREF-FOUND? TFALSE
   ndict@ 0 ?do
      i XREF-REC XREF-NAME$ s" GPT2-GEN:" STARTS-WITH? TFALSE
   loop ;

: FILL ( ptr u8 n n -- )
   {: dst:ptr len:n byte:n :}
   len 0 ?do byte dst i + c! loop ;

: PREPARE ( -- )
   LONG LONG-N 120 FILL
   CONTEXT CONTEXT-N 0 FILL ;

: T-PREFIX$ ( -- ptr u8 n )
   s"  a 1 !" ;

: T-CONTRACTIONS$ ( -- ptr u8 n )
   s" a's a't a're a've a'm a'll a'd" ;

: T-TRAILING$ ( -- ptr u8 n )
   s" end.  " ;

: T-COMBINING$ ( -- ptr u8 n )
   s\" a\xCC\x81b" ;

: T-TWO-NBSP$ ( -- ptr u8 n )
   s\" \xC2\xA0\xC2\xA0x" ;

: T-MIXED-SPACE$ ( -- ptr u8 n )
   s\"  \xE3\x80\x80x" ;

: T-CHUNKS? ( ptr a n ptr u8 n -- bool )
   {: want:ptr wantn:n src:ptr srcu:n :}
   0 0
   begin over srcu < while
      dup wantn >= if 2drop T-FALSE exit then
      over {: cursor:n :}
      dup {: row:n :}
      src srcu cursor T-CHUNK {: chunk:n :}
      chunk 0 <= if 2drop T-FALSE exit then
      chunk srcu cursor - > if 2drop T-FALSE exit then
      chunk want row cells + @ <> if 2drop T-FALSE exit then
      swap chunk + swap 1+
   repeat
   swap srcu = swap wantn = and ;

: T-GRAMMAR ( -- )
   s" leftmost GPT-2 alternatives retain exact Unicode chunk boundaries" T-LABEL
   T-C-ASCII 3 s" abc123!!" T-CHUNKS? TTRUE
   T-C-PREFIX 3 T-PREFIX$ T-CHUNKS? TTRUE
   T-C-CONTRACTIONS 14 T-CONTRACTIONS$ T-CHUNKS? TTRUE
   T-C-TRAILING 3 T-TRAILING$ T-CHUNKS? TTRUE
   T-C-FULLWIDTH 2 s" ＡB１２3" T-CHUNKS? TTRUE
   T-C-ASTRAL 2 s" 𐐀A𝟘7" T-CHUNKS? TTRUE
   T-C-COMBINING 3 T-COMBINING$ T-CHUNKS? TTRUE
   T-C-PUNCT 1 s" !?" T-CHUNKS? TTRUE ;

: T-WHITE-SPACE-GRAMMAR ( -- )
   s" all 25 White_Space scalars retain GPT-2 tail backtracking" T-LABEL
   T-WHITE-SPACE 61 0 T-CHUNK 61 T=
   T-C-TWO-ASCII 2 s"   a" T-CHUNKS? TTRUE
   T-C-TWO-NBSP 3 T-TWO-NBSP$ T-CHUNKS? TTRUE
   T-C-MIXED-SPACE 3 T-MIXED-SPACE$ T-CHUNKS? TTRUE
   s\" \xC2\xA0\xE3\x80\x80" 2dup 0 T-CHUNK swap T= drop ;

: T-MALFORMED-GRAMMAR ( -- )
   s" malformed UTF-8 stays raw OTHER with one-byte decoder progress" T-LABEL
   T-C-M1 2 T-M1 2 T-CHUNKS? TTRUE
   T-C-M2 2 T-M2 2 T-CHUNKS? TTRUE
   T-C-M3 2 T-M3 2 T-CHUNKS? TTRUE
   T-C-M4 2 T-M4 2 T-CHUNKS? TTRUE
   T-C-M5 2 T-M5 2 T-CHUNKS? TTRUE
   T-C-M6 2 T-M6 4 T-CHUNKS? TTRUE
   T-C-M7 2 T-M7 5 T-CHUNKS? TTRUE
   T-C-M8 2 T-M8 3 T-CHUNKS? TTRUE
   T-C-M9 2 T-M9 3 T-CHUNKS? TTRUE ;

: OUT-RESET ( -- )
   OUT OUTPUT-CAP BL>N GUARD FILL
   BEFORE OUTPUT-CAP BL>N GUARD FILL ;

: OUT-PRESERVED ( -- )
   OUT OUTPUT-CAP BL>N BEFORE OUTPUT-CAP BL>N STR= TTRUE ;

: EXPECT-ERR
   ( GPT2:model result<CAD-NUM:byte-len,n> n -- GPT2:model )
   {: want:n :}
   MATCH result
      err OF want T= ENDOF
      ok OF drop false TTRUE ENDOF
   ;MATCH ;

: EXPECT-OK
   ( GPT2:model result<CAD-NUM:byte-len,n> -- GPT2:model CAD-NUM:byte-len )
   MATCH result
      ok OF ENDOF
      err OF throw ENDOF
   ;MATCH ;

using GPT2

: CLOSE-OK ( GPT2:model -- )
   GPT2:CLOSE MATCH result
      ok OF 0 T= ENDOF
      err OF throw ENDOF
   ;MATCH ;

: OPEN-MODEL ( -- GPT2:model )
   0 SCRIPT-ARGV$ FS-PATH:MAKE GPT2:OPEN
   MATCH result
      ok OF ENDOF
      err OF throw ENDOF
   ;MATCH ;

: T-METADATA ( GPT2:model -- GPT2:model )
   s" model metadata accessors retain the owner" T-LABEL
   CONTEXT-LEN IC>N CONTEXT-N T=
   EOS-ID 50256 T= ;

;using

: T-LIMITS ( GPT2:model -- GPT2:model )
   s" token limits refuse before model or caller-output mutation" T-LABEL
   OUT-RESET
   s" Hello" BYTE-CAP 0 TOKEN-CAP OUT OUTPUT-CAP GENERATE
   E-LIMIT EXPECT-ERR
   OUT-PRESERVED
   OUT-RESET
   s" Hello" BYTE-CAP 4097 TOKEN-CAP OUT OUTPUT-CAP GENERATE
   E-LIMIT EXPECT-ERR
   OUT-PRESERVED ;

: T-TOK-REFUSAL ( GPT2:model -- GPT2:model )
   s" tokenizer refusal returns the model and preserves caller output" T-LABEL
   OUT-RESET
   LONG LONG-N BYTE-CAP 1 TOKEN-CAP OUT OUTPUT-CAP GENERATE
   E-TOK-CAP EXPECT-ERR
   OUT-PRESERVED ;

: CONTEXT-COUNT ( GPT2:model -- GPT2:model )
   CONTEXT CONTEXT-N BYTE-CAP ENCODE
   MATCH result
      ok OF IC>N CONTEXT-N T= ENDOF
      err OF throw ENDOF
   ;MATCH ;

: T-CONTEXT ( GPT2:model -- GPT2:model )
   s" exact context boundary passes and one over refuses before mutation" T-LABEL
   CONTEXT-COUNT
   OUT-RESET
   CONTEXT CONTEXT-N BYTE-CAP 2 TOKEN-CAP OUT OUTPUT-CAP GENERATE
   E-LIMIT EXPECT-ERR
   OUT-PRESERVED
   CONTEXT CONTEXT-N BYTE-CAP 1 TOKEN-CAP OUT OUTPUT-CAP GENERATE
   EXPECT-OK BL>N drop ;

: T-ENC-EQ ( GPT2:model ptr u8 n ptr a n -- GPT2:model )
   {: src:ptr srcu:n want:ptr wantn:n :}
   src srcu BYTE-CAP ENCODE
   MATCH result
      err OF throw ENDOF
      ok OF IC>N wantn T= ENDOF
   ;MATCH
   wantn 0 ?do
      i ID-AT want i cells + @ T=
   loop ;

: T-MALFORMED-C3 ( GPT2:model -- GPT2:model )
   s" malformed C3 stays outside the following ASCII Letter run" T-LABEL
   MAL-C3-ASCII 2 BYTE-CAP ENCODE
   MATCH result
      err OF throw ENDOF
      ok OF IC>N 2 T= ENDOF
   ;MATCH
   MAL-C3-ASCII 2 0 T-CHUNK 1 T= ;

: T-DECODE-EQ ( GPT2:model n ptr u8 n -- GPT2:model )
   {: count:n src:ptr srcu:n :}
   count OUT srcu BYTE-CAP DECODE
   MATCH result
      err OF throw ENDOF
      ok OF BL>N srcu T= ENDOF
   ;MATCH
   OUT srcu src srcu T$= ;

: T-ROUNDTRIP ( GPT2:model ptr u8 n -- GPT2:model )
   {: src:ptr srcu:n :}
   src srcu BYTE-CAP ENCODE
   MATCH result
      err OF throw ENDOF
      ok OF IC>N src srcu T-DECODE-EQ ENDOF
   ;MATCH ;

: T-GRAMMAR-ROUNDTRIPS ( GPT2:model -- GPT2:model )
   s" model-owned encode and decode preserve representative grammar bytes" T-LABEL
   T-M1 2 T-ROUNDTRIP
   s" ＡB１２3" T-ROUNDTRIP
   s" 𐐀A𝟘7" T-ROUNDTRIP
   T-MIXED-SPACE$ T-ROUNDTRIP ;

: T-UNICODE ( GPT2:model -- GPT2:model )
   s" complete Unicode letter and number classes preserve pinned GPT-2 ids" T-LABEL
   s" 日本語" U-JA 6 T-ENC-EQ
   s" ٤٢" U-AR 4 T-ENC-EQ
   s\" na\xC3\xAFve" U-LATIN 2 T-ENC-EQ ;

: FAIL-DTOH ( ptr u8 cuda-devptr len -- rc )
   {: dst:ptr src:cuda-devptr len:len :}
   dst drop src drop len drop
   1 >RC ;

: EOS-DTOH ( ptr u8 cuda-devptr len -- rc )
   {: dst:ptr src:cuda-devptr len:len :}
   src drop len drop
   dst TEST-LOGIT-BYTES 0 FILL
   $3F800000 dst 50256 4 * + F32-BUF:STORE
   0 >RC ;

using MKD

: T-MODEL-REFUSAL ( GPT2:model -- GPT2:model )
   s" model refusal returns the owner and preserves caller output" T-LABEL
   OUT-RESET
   [: FAIL-DTOH ;] DTOH!
   s" Hello" BYTE-CAP 1 TOKEN-CAP OUT OUTPUT-CAP GENERATE
   USE-REAL
   1 EXPECT-ERR
   OUT-PRESERVED ;

: T-EOS ( GPT2:model -- GPT2:model )
   s" EOS stops before staging or writing a continuation" T-LABEL
   OUT-RESET
   [: EOS-DTOH ;] DTOH!
   s" Hello" BYTE-CAP 64 TOKEN-CAP OUT OUTPUT-CAP GENERATE
   USE-REAL
   EXPECT-OK BL>N 0 T=
   OUT-PRESERVED ;

;using

using GPT2-REFERENCE

: PINNED-IDS ( GPT2:model -- GPT2:model )
   REAL-ID-COUNT 0 ?do
      i ID-AT i REAL-ID T=
   loop ;

: PINNED-BYTES ( CAD-NUM:byte-len -- )
   BL>N {: outu:n :}
   REAL-BYTES$ {: want:ptr wantu:n :}
   outu wantu T=
   OUT outu want wantu T$= ;

: T-PINNED ( GPT2:model -- GPT2:model )
   s" a valid request after refusal returns exact pinned ids and bytes" T-LABEL
   OUT-RESET
   s" Hello" BYTE-CAP 4097 TOKEN-CAP OUT OUTPUT-CAP GENERATE
   E-LIMIT EXPECT-ERR
   OUT-PRESERVED
   REAL-BYTES$ nip BYTE-CAP {: exact:CAD-NUM:byte-len :}
   s" Hello" BYTE-CAP 64 TOKEN-CAP OUT exact GENERATE
   EXPECT-OK PINNED-BYTES
   PINNED-IDS ;

: T-ONE-SHORT ( GPT2:model -- GPT2:model )
   s" one-short decode capacity refuses without writing caller output" T-LABEL
   OUT-RESET
   REAL-BYTES$ nip 1- BYTE-CAP {: short:CAD-NUM:byte-len :}
   s" Hello" BYTE-CAP 64 TOKEN-CAP OUT short GENERATE
   E-TOK-CAP EXPECT-ERR
   OUT-PRESERVED ;

: T-DISTINCT ( GPT2:model GPT2:model -- GPT2:model GPT2:model )
   s" two live models keep distinct tokenizer blocks" T-LABEL
   456 4000 ID-PUT
   swap 123 4000 ID-PUT swap
   4000 ID-AT 456 T=
   swap 4000 ID-AT 123 T= swap ;

: T-ONE ( GPT2:model -- GPT2:model )
   OUT-RESET
   s" Hello" BYTE-CAP 1 TOKEN-CAP OUT OUTPUT-CAP GENERATE
   EXPECT-OK BL>N 1 T=
   OUT 1 s" ," T$=
   0 ID-AT 11 T= ;

: T-ALTERNATE ( GPT2:model GPT2:model -- GPT2:model GPT2:model )
   s" both live models generate the pinned one-token continuation" T-LABEL
   swap T-ONE swap
   T-ONE ;

;using

: T-RUN ( -- )
   SCRIPT-ARGC 1 <> if E-USAGE throw then
   T-RESET
   T-SURFACE
   PREPARE
   T-GRAMMAR
   T-WHITE-SPACE-GRAMMAR
   T-MALFORMED-GRAMMAR
   SAFET:LIVE-OWNERS {: owners:n :}
   SAFET-MAP:LIVE {: maps:n :}
   OPEN-MODEL
   OPEN-MODEL
   T-DISTINCT
   T-METADATA
   T-LIMITS
   T-TOK-REFUSAL
   T-MALFORMED-C3
   T-GRAMMAR-ROUNDTRIPS
   T-UNICODE
   T-CONTEXT
   T-MODEL-REFUSAL
   T-EOS
   T-PINNED
   T-ONE-SHORT
   T-ALTERNATE
   CLOSE-OK
   CLOSE-OK
   OPEN-MODEL
   OPEN-MODEL
   T-DISTINCT
   T-ALTERNATE
   swap CLOSE-OK
   CLOSE-OK
   SAFET:LIVE-OWNERS owners T=
   SAFET-MAP:LIVE maps T=
   T-REPORT ;

T-RUN

;package
