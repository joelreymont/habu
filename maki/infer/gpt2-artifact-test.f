\ gpt2-artifact-test.f - manual pinned-checkpoint identity and census proof.
\ Run: bin/hb --load maki/infer/gpt2-artifact-test.f -- <checkpoint-root>

require lib/test.f
require lib/fs.f
require lib/content-key.f
require maki/infer/gpt2-pin.f
require maki/infer/safetensors.f

package GPT2-ARTIFACT-TEST

64 constant SHA256-LEN
-7697 constant E-FIX

create PATH FS-PATH-CAP allot
create DIGEST SHA256-LEN allot

: ROOT+ ( ptr u8 n -- ptr u8 n )
   0 SCRIPT-ARGV$ 2swap PATH JOIN-PATH
   PATH swap ;

: ASSERT-FILE ( ptr u8 n n ptr u8 n -- )
   {: name:ptr nameu:n size:n hash:ptr hashu:n :}
   name nameu ROOT+
   2dup FILE-SIZE size T=
   DIGEST SHA256-FILE-HEX 0 T=
   DIGEST SHA256-LEN hash hashu T$= ;

: OPT= ( option<n> n -- )
   {: want:n :}
   MATCH option
      none OF E-FIX throw ENDOF
      some OF want T= ENDOF
   ;MATCH ;

: ID ( SAFET:file ptr u8 n -- SAFET:file n )
   SAFET:FIND
   MATCH option
      none OF -1 ENDOF
      some OF ENDOF
   ;MATCH
   dup 0 < if E-FIX throw then ;

: LOAD-MODEL ( -- SAFET:file )
   GPT2PIN:MODEL-NAME$ ROOT+ SAFET:LOAD
   MATCH result
      ok OF ENDOF
      err OF throw ENDOF
   ;MATCH ;

: PINS ( -- )
   GPT2PIN:CONFIG-NAME$ GPT2PIN:CONFIG-LEN GPT2PIN:CONFIG-SHA256$ ASSERT-FILE
   GPT2PIN:MODEL-NAME$ GPT2PIN:MODEL-LEN GPT2PIN:MODEL-SHA256$ ASSERT-FILE
   GPT2PIN:VOCAB-NAME$ GPT2PIN:VOCAB-LEN GPT2PIN:VOCAB-SHA256$ ASSERT-FILE
   GPT2PIN:MERGES-NAME$ GPT2PIN:MERGES-LEN GPT2PIN:MERGES-SHA256$ ASSERT-FILE ;

: CENSUS ( -- )
   LOAD-MODEL
   SAFET:COUNT 160 T=
   s" wte.weight" ID {: w:n :}
   w SAFET:RANK? 2 OPT=
   w 0 SAFET:DIM? 50257 OPT=
   w 1 SAFET:DIM? 768 OPT=
   w MAKI-DATATYPE:DF32 SAFET:DATATYPE= TTRUE
   w SAFET:NBYTES? 154389504 OPT=
   s" wpe.weight" ID {: p:n :}
   p 0 SAFET:DIM? 1024 OPT=
   p 1 SAFET:DIM? 768 OPT=
   s" h.0.attn.c_attn.weight" ID {: c:n :}
   c 0 SAFET:DIM? 768 OPT=
   c 1 SAFET:DIM? 2304 OPT=
   SAFET:RELEASE ;

: RUN ( -- )
   SCRIPT-ARGC 1 <> if
      s" usage: bin/hb --load maki/infer/gpt2-artifact-test.f -- <checkpoint-root>" 64 die
   then
   T-RESET
   PINS
   CENSUS
   SAFET:LIVE-OWNERS 0 T=
   SAFET-MAP:LIVE 0 T=
   T-REPORT ;

RUN

;package
