\ gpt2-config-test.f - GPT2:config acceptance.

require lib/prelude.f
require lib/test.f
require test/checker-assert.f
require maki/infer/gpt2-config.f

package GPT2-TEST

using GPT2

: YES ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! -1 T= ;

: NO ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

: UNK ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 1 T= ;

1024 constant CX0
50257 constant VO0
12 constant NL0
768 constant NE0
12 constant NH0
50256 constant BOS0
50256 constant EOS0
$7FFFFFFFFFFFFFFF constant HUGE

: DT0 ( -- MAKI:datatype ) MAKI-DATATYPE:DF32 ;
: EPS0 ( -- r ) 0.00001 ;

: CFG ( -- config )
   DT0 CX0 VO0 NL0 NE0 NH0 true BOS0 EOS0 EPS0 true BUILD ;

: T-FIELDS ( -- )
   CFG
   DATATYPE@ DT0 MAKI-DATATYPE:EQ TTRUE
   NCTX@ CX0 T=
   NVOCAB@ VO0 T=
   NLAYER@ NL0 T=
   NEMBD@ NE0 T=
   NHEAD@ NH0 T=
   TIED? TTRUE
   BOS@ BOS0 T=
   EOS@ EOS0 T=
   LN-EPS@ EPS0 f= TTRUE
   ATTN-SCALE? TTRUE
   drop ;

: RJ-CX ( -- )
   DT0 0 VO0 NL0 NE0 NH0 true BOS0 EOS0 EPS0 true BUILD drop ;

: RJ-VO ( -- )
   DT0 CX0 0 NL0 NE0 NH0 true BOS0 EOS0 EPS0 true BUILD drop ;

: RJ-NL ( -- )
   DT0 CX0 VO0 0 NE0 NH0 true BOS0 EOS0 EPS0 true BUILD drop ;

: RJ-NE ( -- )
   DT0 CX0 VO0 NL0 0 NH0 true BOS0 EOS0 EPS0 true BUILD drop ;

: RJ-NH ( -- )
   DT0 CX0 VO0 NL0 NE0 0 true BOS0 EOS0 EPS0 true BUILD drop ;

: RJ-OVF ( -- )
   DT0 HUGE VO0 NL0 NE0 NH0 true BOS0 EOS0 EPS0 true BUILD drop ;

: RJ-HEAD ( -- )
   DT0 CX0 VO0 NL0 770 NH0 true BOS0 EOS0 EPS0 true BUILD drop ;

: RJ-BOS ( -- )
   DT0 CX0 VO0 NL0 NE0 NH0 true VO0 EOS0 EPS0 true BUILD drop ;

: RJ-EOS ( -- )
   DT0 CX0 VO0 NL0 NE0 NH0 true BOS0 -1 EPS0 true BUILD drop ;

: RJ-EPS ( -- )
   DT0 CX0 VO0 NL0 NE0 NH0 true BOS0 EOS0 0.0 true BUILD drop ;

: T-REJECTS ( -- )
   [: RJ-CX ;] E-EXTENT TTHROWSQ
   [: RJ-VO ;] E-EXTENT TTHROWSQ
   [: RJ-NL ;] E-EXTENT TTHROWSQ
   [: RJ-NE ;] E-EXTENT TTHROWSQ
   [: RJ-NH ;] E-EXTENT TTHROWSQ
   [: RJ-OVF ;] E-EXTENT TTHROWSQ
   [: RJ-HEAD ;] E-HEAD TTHROWSQ
   [: RJ-BOS ;] E-TOKEN TTHROWSQ
   [: RJ-EOS ;] E-TOKEN TTHROWSQ
   [: RJ-EPS ;] E-EPS TTHROWSQ ;

T-RESET

T-FIELDS
T-REJECTS

s" CFG-MAKE ( MAKI:datatype n n n n n bool n n r bool GPT2:cfg-proof -- GPT2:config ) GPT2-CONFIG:MAKE" YES
s" CFG-RAW-PROOF ( MAKI:datatype n n n n n bool n n r bool n -- GPT2:config ) GPT2-CONFIG:MAKE" NO
s" CFG-MINT ( -- GPT2:cfg-proof ) GPT2:MINT-CFG-PROOF" UNK
s" CFG-MINT-BARE ( -- GPT2:cfg-proof ) MINT-CFG-PROOF" UNK
s" CFG-OLD-TIED ( GPT2:config -- GPT2:config bool ) GPT2:TIED@" UNK
s" CFG-OLD-SCALE ( GPT2:config -- GPT2:config bool ) GPT2:ATTN-SCALE@" UNK

using REFLECT
   s" config" s" GPT2-CONFIG" FAMS 1 T=
   s" config" s" GPT2-CONFIG" s" datatype" SLOT 0 T=
   s" config" s" GPT2-CONFIG" s" ln-eps" SLOT 9 T=
   s" config" s" GPT2-CONFIG" s" attn-scale" SLOT 10 T=
   s" config" s" GPT2-CONFIG" FLDS 12 T=
;using

T-REPORT

;using

;package
