\ gpt2-logits-device-test.f - mandatory production GPT-2 decode proof.

require lib/test.f
require lib/cad-num-arithmetic.f
require lib/fs-path.f
require lib/float32-buffer.f
require maki/infer/gpt2-model.f
require maki/infer/gpt2-greedy.f
require maki/infer/gpt2-reference-data.f

package GPT2
private

-7698 constant E-FIX
50257 constant LT-VO
LT-VO 4 * constant LT-BYTES

create LT-OUT LT-BYTES allot
create LT-FIRST LT-BYTES allot
create LT-SCAN 16 allot
variable LT-ARG
variable LT-MAX

: LT-LEN ( n -- CAD-NUM:byte-len )
   CAD-NUM:BYTE-LEN MATCH CAD-NUM:numeric-result
      ok OF ENDOF
      negative OF E-FIX throw ENDOF
      zero OF E-FIX throw ENDOF
      overflow OF E-FIX throw ENDOF
      underflow OF E-FIX throw ENDOF
      bad-alignment OF E-FIX throw ENDOF
      misaligned OF E-FIX throw ENDOF
   ;MATCH ;

: LT-OPEN ( -- GPT2:model )
   0 SCRIPT-ARGV$ FS-PATH:MAKE OPEN
   MATCH result
      ok OF ENDOF
      err OF throw ENDOF
   ;MATCH ;

: LT-CLOSE ( GPT2:model -- )
   CLOSE MATCH result
      ok OF drop ENDOF
      err OF throw ENDOF
   ;MATCH ;

: LT-OK ( GPT2:model n ptr u8 n -- GPT2:model )
   LT-LEN LOGITS MATCH result
      ok OF drop ENDOF
      err OF throw ENDOF
   ;MATCH ;

: LT-GREEDY ( GPT2:model n -- GPT2:model n )
   LT-OUT LT-BYTES LT-LEN GREEDY MATCH result
      ok OF ENDOF
      err OF throw ENDOF
   ;MATCH ;

: LT-ERR ( GPT2:model n ptr u8 n n -- GPT2:model )
   {: want:n :}
   LT-LEN GREEDY MATCH result
      ok OF drop E-FIX throw ENDOF
      err OF want T= ENDOF
   ;MATCH ;

: LT-F@ ( n -- r )
   4 * LT-OUT + F32-BUF:LOAD F32:WIDEN ;

: LT-FINITE? ( r -- bool )
   dup f- 0.0 f= ;

: LT-SUM ( -- r )
   0.0 LT-VO 0 ?do i LT-F@ dup LT-FINITE? TTRUE f+ loop ;

: LT-SUMSQ ( -- r )
   0.0 LT-VO 0 ?do i LT-F@ dup LT-FINITE? TTRUE dup f* f+ loop ;

: LT-ARGMAX ( -- n )
   0 LT-ARG !
   0 LT-F@ LT-MAX !
   LT-VO 1 ?do
      i LT-F@ LT-MAX @ f> if
         i LT-ARG !
         i LT-F@ LT-MAX !
      then
   loop
   LT-ARG @ ;

: LT-REFS ( -- )
   GPT2-REFERENCE:REAL-LOGIT-COUNT 0 ?do
      i GPT2-REFERENCE:REAL-LOGIT-ID LT-F@
      i GPT2-REFERENCE:REAL-LOGIT f- fabs
      0.125 f> 0= TTRUE
   loop
   LT-SUM GPT2-REFERENCE:REAL-SUM f- fabs 256.0 f> 0= TTRUE
   LT-SUMSQ GPT2-REFERENCE:REAL-SUMSQ f- fabs 32768.0 f> 0= TTRUE
   LT-ARGMAX 0 GPT2-REFERENCE:REAL-ID T= ;

: LT-SAME-FIRST ( -- )
   LT-OUT LT-BYTES LT-FIRST LT-BYTES STR= TTRUE ;

: LT-SCAN! ( n n -- )
   {: bits:n idx:n :}
   bits LT-SCAN idx 4 * + F32-BUF:STORE ;

: LT-SCAN-OK ( n n -- )
   {: count:n want:n :}
   LT-SCAN count 4 * LT-LEN G-SCAN
   MATCH result
      ok OF want T= ENDOF
      err OF throw ENDOF
   ;MATCH ;

: LT-SCAN-ERR ( n -- )
   {: count:n :}
   LT-SCAN count 4 * LT-LEN G-SCAN
   MATCH result
      ok OF drop E-FIX throw ENDOF
      err OF E-NUMERIC T= ENDOF
   ;MATCH ;

: LT-SCANNER ( -- )
   s" greedy scanner keeps the first finite maximum" T-LABEL
   $3F800000 0 LT-SCAN!
   $40A00000 1 LT-SCAN!
   $40A00000 2 LT-SCAN!
   $BF800000 3 LT-SCAN!
   4 1 LT-SCAN-OK
   1 0 LT-SCAN-OK
   $40C00000 3 LT-SCAN!
   4 3 LT-SCAN-OK
   $C0A00000 0 LT-SCAN!
   $BF800000 1 LT-SCAN!
   $C0000000 2 LT-SCAN!
   3 1 LT-SCAN-OK
   s" greedy scanner rejects every non-finite F32 class" T-LABEL
   $7FC00000 0 LT-SCAN! 1 LT-SCAN-ERR
   $3F800000 0 LT-SCAN!
   $7FC00000 1 LT-SCAN! 2 LT-SCAN-ERR
   $7F800000 1 LT-SCAN! 2 LT-SCAN-ERR
   $FF800000 1 LT-SCAN! 2 LT-SCAN-ERR ;

: LT-DECODE ( GPT2:model -- GPT2:model )
   s" first and second production decode rows match pinned GPT-2" T-LABEL
   15496 LT-GREEDY 0 GPT2-REFERENCE:REAL-ID T=
   LT-REFS
   LT-OUT LT-FIRST LT-BYTES BYTE-COPY
   0 GPT2-REFERENCE:REAL-ID LT-GREEDY 1 GPT2-REFERENCE:REAL-ID T=
   s" RESET repeats the first row bit-for-bit" T-LABEL
   RESET
   15496 LT-GREEDY 0 GPT2-REFERENCE:REAL-ID T=
   LT-SAME-FIRST
   s" invalid token and wrong length leave position unchanged" T-LABEL
   RESET
   LT-VO LT-OUT LT-BYTES E-TOKEN LT-ERR
   15496 LT-OUT LT-BYTES LT-OK
   LT-SAME-FIRST
   RESET
   15496 LT-OUT LT-BYTES 4 - E-OUTPUT LT-ERR
   15496 LT-OUT LT-BYTES LT-OK
   LT-SAME-FIRST ;

: LT-RUN ( -- )
   SCRIPT-ARGC 1 <> if E-FIX throw then
   T-RESET
   LT-SCANNER
   SAFET:LIVE-OWNERS {: owners:n :}
   SAFET-MAP:LIVE {: maps:n :}
   s" two GPT-2 models coexist and close in either order" T-LABEL
   LT-OPEN LT-OPEN swap LT-DECODE LT-CLOSE
   LT-OPEN LT-CLOSE LT-CLOSE
   SAFET:LIVE-OWNERS owners T=
   SAFET-MAP:LIVE maps T=
   T-REPORT ;

LT-RUN

;package
