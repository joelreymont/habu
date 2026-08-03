\ gpt2-logits-device-test.f - mandatory production GPT-2 decode proof.

require lib/test.f
require lib/cad-num-arithmetic.f
require lib/fs-path.f
require lib/float32-buffer.f
require maki/infer/gpt2-model.f
require maki/infer/gpt2-reference-data.f

package GPT2
private

-7698 constant E-FIX
50257 constant LT-VO
LT-VO 4 * constant LT-BYTES

create LT-OUT LT-BYTES allot
create LT-FIRST LT-BYTES allot
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

: LT-ERR ( GPT2:model n ptr u8 n n -- GPT2:model )
   {: want:n :}
   LT-LEN LOGITS MATCH result
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

: LT-DECODE ( GPT2:model -- GPT2:model )
   s" first and second production decode rows match pinned GPT-2" T-LABEL
   15496 LT-OUT LT-BYTES LT-OK
   LT-REFS
   LT-OUT LT-FIRST LT-BYTES BYTE-COPY
   11 LT-OUT LT-BYTES LT-OK
   LT-ARGMAX 1 GPT2-REFERENCE:REAL-ID T=
   s" RESET repeats the first row bit-for-bit" T-LABEL
   RESET
   15496 LT-OUT LT-BYTES LT-OK
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
