\ gpt2-generate-test.f - persistent GPT-2 production path.

require lib/test.f
require lib/float32-buffer.f
require lib/fs-path.f
require test/checker-assert.f
require maki/cuda-run.f
require maki/infer/gpt2-generate.f
require maki/infer/gpt2-reference-data.f

package GPT2-GEN
private

4097 constant LONG-N
1024 constant CONTEXT-N
50257 constant TEST-VOCAB
TEST-VOCAB 4 * constant TEST-LOGIT-BYTES
$A5 constant GUARD

create OUT OUTPUT-CAP allot
create BEFORE OUTPUT-CAP allot
create LONG LONG-N allot
create CONTEXT CONTEXT-N allot

: YES ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! -1 T= ;

: UNK ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 1 T= ;

: T-SURFACE ( -- )
   s" GEN-SIG ( GPT2:model ptr u8 CAD-NUM:byte-len CAD-NUM:item-count ptr u8 CAD-NUM:byte-len -- GPT2:model result<CAD-NUM:byte-len,n> ) GPT2-GEN:GENERATE" YES
   s" GEN-CX ( GPT2:model -- GPT2:model CAD-NUM:item-count ) GPT2:CONTEXT-LEN" YES
   s" GEN-EOS ( GPT2:model -- GPT2:model n ) GPT2:EOS-ID" YES
   s" GEN-BL-PRIVATE ( CAD-NUM:byte-len -- n ) GPT2-GEN:BL>N" UNK
   s" GEN-IC-PRIVATE ( CAD-NUM:item-count -- n ) GPT2-GEN:IC>N" UNK
   s" GPT2-GEN:BL>N" XREF-FIND XREF-FOUND? TFALSE
   s" GPT2-GEN:IC>N" XREF-FIND XREF-FOUND? TFALSE ;

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
   0 SCRIPT-ARGV$ FS-PATH:MAKE GPT2-GEN:OPEN
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

: T-BPE-REFUSAL ( GPT2:model -- GPT2:model )
   s" BPE refusal returns the model and preserves caller output" T-LABEL
   OUT-RESET
   LONG LONG-N BYTE-CAP 1 TOKEN-CAP OUT OUTPUT-CAP GENERATE
   E-BPE-CAP EXPECT-ERR
   OUT-PRESERVED ;

: CONTEXT-COUNT ( -- )
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

: PINNED-IDS ( -- )
   REAL-ID-COUNT 0 ?do
      i ID@ i REAL-ID T=
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
   E-BPE-CAP EXPECT-ERR
   OUT-PRESERVED ;

;using

: T-RUN ( -- )
   SCRIPT-ARGC 1 <> if E-STATE throw then
   T-RESET
   T-SURFACE
   PREPARE
   SAFET:LIVE-OWNERS {: owners:n :}
   SAFET-MAP:LIVE {: maps:n :}
   OPEN-MODEL
   T-METADATA
   T-LIMITS
   T-BPE-REFUSAL
   T-CONTEXT
   T-MODEL-REFUSAL
   T-EOS
   T-PINNED
   T-ONE-SHORT
   CLOSE-OK
   SAFET:LIVE-OWNERS owners T=
   SAFET-MAP:LIVE maps T=
   T-REPORT ;

T-RUN

;package
