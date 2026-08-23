\ gpt2-model-test.f - packed GPT-2 layout and load refusal.

require lib/test.f
require lib/cad-num-arithmetic.f
require lib/fs.f
require lib/fs-path.f
require test/checker-assert.f
require maki/infer/gpt2-model.f

package GPT2
private

-7697 constant E-FIX

create BASE-BYTE 1 allot
create JOIN-ROOT FS-PATH-CAP allot
create JOIN-DST FS-PATH-CAP allot

: BASE ( -- ptr u8 ) BASE-BYTE ;

: CFG ( -- GPT2:config )
   MAKI-DATATYPE:DF32
   8 5 2 4 2 true 4 4 0.00001 true GPT2:BUILD ;

: IDX ( n -- CAD-NUM:index )
   CAD-NUM:INDEX
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF
      negative OF E-FIX throw ENDOF
      zero OF E-FIX throw ENDOF
      overflow OF E-FIX throw ENDOF
      underflow OF E-FIX throw ENDOF
      bad-alignment OF E-FIX throw ENDOF
      misaligned OF E-FIX throw ENDOF
   ;MATCH ;

: BOFF ( n -- CAD-NUM:byte-off )
   CAD-NUM:BYTE-OFF
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF
      negative OF E-FIX throw ENDOF
      zero OF E-FIX throw ENDOF
      overflow OF E-FIX throw ENDOF
      underflow OF E-FIX throw ENDOF
      bad-alignment OF E-FIX throw ENDOF
      misaligned OF E-FIX throw ENDOF
   ;MATCH ;

: END-OFF ( CAD-NUM:byte-off CAD-NUM:byte-len -- CAD-NUM:byte-off )
   CAD-NUM:ADVANCE-BYTE-OFF
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF
      negative OF E-FIX throw ENDOF
      zero OF E-FIX throw ENDOF
      overflow OF E-FIX throw ENDOF
      underflow OF E-FIX throw ENDOF
      bad-alignment OF E-FIX throw ENDOF
      misaligned OF E-FIX throw ENDOF
   ;MATCH ;

: SPAN= ( GPT2:config n n n -- GPT2:config )
   {: slot:n want-off:n want-len:n :}
   slot IDX GPT2:TENSOR-ID-FOR-SLOT GPT2:SPAN
   {: off:CAD-NUM:byte-off len:CAD-NUM:byte-len :}
   BASE off CAD-NUM:BYTE+
   BASE want-off BOFF CAD-NUM:BYTE+ = TTRUE
   BASE off len END-OFF CAD-NUM:BYTE+
   BASE want-off want-len + BOFF CAD-NUM:BYTE+ = TTRUE ;

: ALL-SPANS
   ( GPT2:config CAD-NUM:byte-off n n -- GPT2:config CAD-NUM:byte-off )
   {: want:CAD-NUM:byte-off slot:n total:n :}
   slot total = if want exit then
   slot IDX GPT2:TENSOR-ID-FOR-SLOT GPT2:SPAN
   {: off:CAD-NUM:byte-off len:CAD-NUM:byte-len :}
   BASE off CAD-NUM:BYTE+ BASE want CAD-NUM:BYTE+ = TTRUE
   BASE off len END-OFF CAD-NUM:BYTE+ BASE off CAD-NUM:BYTE+ > TTRUE
   want len END-OFF slot 1+ total RECURSE ;

: TEST-SPAN ( -- )
   s" SPAN packs every GPT-2 tensor contiguously through the final byte" T-LABEL
   CFG COUNT {: total:n :}
   0 BOFF 0 total ALL-SPANS
   {: final:CAD-NUM:byte-off :}
   M-TOTAL-LEN {: bytes:CAD-NUM:byte-len :}
   BASE final CAD-NUM:BYTE+
   BASE 0 BOFF bytes END-OFF CAD-NUM:BYTE+ = TTRUE
   drop
   s" SPAN retains exact global, layer, mask, and final pins" T-LABEL
   CFG
   0 0 80 SPAN=
   4 240 16 SPAN=
   6 272 256 SPAN=
   29 2688 16 SPAN=
   drop ;

: FILL ( ptr u8 n n -- ) {: dst:ptr u:n ch:n :}
   u 0 ?do ch dst i + c! loop ;

: TEST-PATH-LIMIT ( -- )
   s" joined GPT-2 paths accept the exact capacity with either root form" T-LABEL
   GPT2PIN:MODEL-NAME$ nip {: nameu:n :}
   FS-PATH-CAP nameu - 1- {: plainu:n :}
   JOIN-ROOT plainu 120 FILL
   JOIN-ROOT plainu nameu M-JOIN-LEN FS-PATH-CAP T=
   JOIN-ROOT plainu GPT2PIN:MODEL-NAME$ JOIN-DST JOIN-PATH FS-PATH-CAP T=
   FS-PATH-CAP nameu - {: slashu:n :}
   JOIN-ROOT slashu 120 FILL
   FS-SLASH JOIN-ROOT slashu 1- + c!
   JOIN-ROOT slashu nameu M-JOIN-LEN FS-PATH-CAP T=
   JOIN-ROOT slashu GPT2PIN:MODEL-NAME$ JOIN-DST JOIN-PATH FS-PATH-CAP T= ;

: YES ( ptr u8 n -- ) CHECK-QUIET-CANDIDATE! -1 T= ;
: NO ( ptr u8 n -- ) CHECK-QUIET-CANDIDATE! 0 T= ;
: UNK ( ptr u8 n -- ) CHECK-QUIET-CANDIDATE! 1 T= ;

: TEST-OPAQUE ( -- )
   s" GPT2:model has no public forge or raw-cell conversion" T-LABEL
   s" GM-CLOSE ( GPT2:model -- result<n,n> ) GPT2:CLOSE" YES
   s" GM-MAKE ( ptr u8 -- GPT2:model ) GPT2-MODEL:MAKE" UNK
   s" GM-UNMAKE ( GPT2:model -- ptr u8 ) GPT2-MODEL:UNMAKE" UNK
   s" GM-RAW-IN ( n -- GPT2:model )" NO
   s" GM-RAW-OUT ( GPT2:model -- n )" NO ;

: CLOSE-OK ( result<n,n> -- )
   MATCH result
      ok OF 0 T= ENDOF
      err OF throw ENDOF
   ;MATCH ;

: TEST-REFUSAL ( -- )
   s" OPEN refuses before GPU ownership and leaves no source owner" T-LABEL
   SAFET:LIVE-OWNERS {: before:n :}
   SAFET-MAP:LIVE {: maps:n :}
   s" /tmp/habu-no-gpt2-model" FS-PATH:MAKE GPT2:OPEN
   MATCH result
      err OF E-FS-OPEN T= ENDOF
      ok OF GPT2:CLOSE CLOSE-OK false TTRUE ENDOF
   ;MATCH
   SAFET:LIVE-OWNERS before T=
   SAFET-MAP:LIVE maps T= ;

: RUN ( -- )
   T-RESET
   TEST-SPAN
   TEST-PATH-LIMIT
   TEST-OPAQUE
   TEST-REFUSAL
   T-REPORT ;

RUN

;package
