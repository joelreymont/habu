\ gpt2-model-test.f - packed GPT-2 layout and real GPU ownership.

require lib/test.f
require lib/cad-num-arithmetic.f
require lib/fs.f
require lib/fs-mutate.f
require lib/fs-path.f
require maki/infer/gpt2-model.f

package GPT2

-7697 constant E-FIX

create BASE-BYTE 1 allot
create ROOT FS-PATH-CAP allot
create SRC FS-PATH-CAP allot
create DST FS-PATH-CAP allot
create EMPTY-MODEL 2 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 123 c, 125 c,

variable ROOT-U
variable SRC-U
variable DST-U

: BASE ( -- ptr u8 ) BASE-BYTE ;

: COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   a dst u BYTE-COPY
   u up ! ;

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: SRC$ ( -- ptr u8 n ) SRC SRC-U @ ;
: DST$ ( -- ptr u8 n ) DST DST-U @ ;

: PREPARE-EMPTY ( ptr u8 n -- ) {: model:ptr modelu:n :}
   CLEANUP-RESET
   s" habu-gpt2-empty" TMPDIR-MKDIR ROOT ROOT-U COPY!
   ROOT$ CLEANUP-TREE+
   model modelu GPT2PIN:CONFIG-NAME$ SRC JOIN-PATH SRC-U !
   ROOT$ GPT2PIN:CONFIG-NAME$ DST JOIN-PATH DST-U !
   SRC$ DST$ COPY-FILE-STREAM
   ROOT$ GPT2PIN:MODEL-NAME$ DST JOIN-PATH DST-U !
   DST$ EMPTY-MODEL 10 WRITE-ALL ;

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

: TEST-SPAN ( -- )
   s" SPAN packs every GPT-2 role into one closed-form layout" T-LABEL
   CFG
   0 0 80 SPAN=
   1 80 128 SPAN=
   2 208 16 SPAN=
   3 224 16 SPAN=
   4 240 16 SPAN=
   6 272 256 SPAN=
   7 528 192 SPAN=
   16 1456 16 SPAN=
   29 2688 16 SPAN=
   drop ;

: CLOSE-OK ( result<n,n> -- )
   MATCH result
      ok OF 0 T= ENDOF
      err OF throw ENDOF
   ;MATCH ;

: TEST-REFUSAL ( -- )
   s" OPEN refuses before GPU ownership and leaves no source owner" T-LABEL
   s" /tmp/habu-no-gpt2-model" FS-PATH:MAKE GPT2:OPEN
   MATCH result
      err OF drop 0 0= TTRUE ENDOF
      ok OF GPT2:CLOSE CLOSE-OK 0 0= 0= TTRUE ENDOF
   ;MATCH
   SAFET:LIVE-OWNERS 0 T= ;

: TEST-EMPTY ( ptr u8 n -- )
   s" OPEN rejects an empty real Safetensors catalog before GPU ownership" T-LABEL
   PREPARE-EMPTY
   SAFET:LIVE-OWNERS {: before:n :}
   ROOT$ FS-PATH:MAKE GPT2:OPEN
   MATCH result
      err OF E-CATALOG T= ENDOF
      ok OF GPT2:CLOSE CLOSE-OK false TTRUE ENDOF
   ;MATCH
   SAFET:LIVE-OWNERS before T=
   CLEANUP-RUN ;

: TEST-DEVICE ( -- )
   SCRIPT-ARGC 0= if
      s" gpt2-model: no model root argument -> device leg SKIPPED" type cr
      exit
   then
   0 SCRIPT-ARGV$ TEST-EMPTY
   s" OPEN uploads the pinned GPT-2 model and CLOSE releases every owner" T-LABEL
   SAFET:LIVE-OWNERS 0 T=
   0 SCRIPT-ARGV$ FS-PATH:MAKE GPT2:OPEN
   MATCH result
      err OF throw ENDOF
      ok OF GPT2:CLOSE CLOSE-OK ENDOF
   ;MATCH
   SAFET:LIVE-OWNERS 0 T= ;

: RUN ( -- )
   T-RESET
   TEST-SPAN
   TEST-REFUSAL
   TEST-DEVICE
   T-REPORT ;

RUN

;package
