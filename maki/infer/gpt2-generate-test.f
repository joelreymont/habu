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
   SAFET:LIVE-OWNERS {: owners:n :}
   SAFET-MAP:LIVE {: maps:n :}
   OPEN-MODEL
   OPEN-MODEL
   T-DISTINCT
   T-METADATA
   T-LIMITS
   T-TOK-REFUSAL
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
