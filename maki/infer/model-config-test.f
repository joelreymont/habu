\ model-config-test.f - MDLCFG acceptance.
\
\ Legs, all through the public package surface:
\   1. both arms construct (llama constructor-tested from day one), and their
\      common accessors and payload projections return the built fields;
\   2. constructor rejections per field class, each a named throw raised
\      before the proof mints;
\   3. checker negatives: old generated record arity and a raw n in the proof
\      slot reject, the private mint is unresolvable outside the package, and
\      the gpt2 arm rejects a GQA-shaped field by construction;
\   4. the version cell is gone for good: the old constructor arity no longer
\      certifies, both public schema words are unresolvable, and the live type
\      registry holds no `sv` slot in mcfg.

require lib/prelude.f
require lib/test.f
require test/checker-assert.f
require maki/infer/model-config.f

package MDLCFG-TEST

: YES ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! -1 T= ;

: NO ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

: UNK ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 1 T= ;

\ ---- baseline field values (GPT-2 small; llama 7B-shaped) --------------------
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
: RMSEPS0 ( -- r ) 0.000001 ;
: THETA0 ( -- r ) 10000.0 ;

: G-ARM ( -- MDLCFG:arch )  EPS0 true MDLCFG-ARCH:GPT2 ;
: G-ARM-EPS ( r -- MDLCFG:arch )  true MDLCFG-ARCH:GPT2 ;
: L-ARM ( -- MDLCFG:arch )  4 11008 THETA0 RMSEPS0 MDLCFG-ARCH:LLAMA ;

\ ---- builders: an arm over the fixed gpt2/llama commons ----------------------
: B-G ( -- MDLCFG:mcfg )
   G-ARM DT0 CX0 VO0 NL0 NE0 NH0 true BOS0 EOS0 MDLCFG:BUILD ;

: B-L ( -- MDLCFG:mcfg )
   L-ARM DT0 4096 32000 32 4096 32 false 1 2 MDLCFG:BUILD ;

: BG-WITH ( MDLCFG:arch -- MDLCFG:mcfg )
   DT0 CX0 VO0 NL0 NE0 NH0 true BOS0 EOS0 MDLCFG:BUILD ;

: BL-WITH ( MDLCFG:arch -- MDLCFG:mcfg )
   DT0 4096 32000 32 4096 32 false 1 2 MDLCFG:BUILD ;

\ ---- payload projections -----------------------------------------------------
: GP-EPS ( r bool -- r ) {: eps:r sc:bool :}  eps ;
: GP-SC ( r bool -- bool ) {: eps:r sc:bool :}  sc ;
: LP-NKV ( n n r r -- n ) {: nkv:n ffn:n theta:r reps:r :}  nkv ;
: LP-FFN ( n n r r -- n ) {: nkv:n ffn:n theta:r reps:r :}  ffn ;
: LP-THETA ( n n r r -- r ) {: nkv:n ffn:n theta:r reps:r :}  theta ;
: LP-REPS ( n n r r -- r ) {: nkv:n ffn:n theta:r reps:r :}  reps ;

: ARM-EPS ( MDLCFG:arch -- r )
   MATCH MDLCFG:arch
      gpt2  OF GP-EPS ENDOF
      llama OF 2drop 2drop 0.0 ENDOF
   ;MATCH ;

: ARM-SC ( MDLCFG:arch -- bool )
   MATCH MDLCFG:arch
      gpt2  OF GP-SC ENDOF
      llama OF 2drop 2drop false ENDOF
   ;MATCH ;

: ARM-NKV ( MDLCFG:arch -- n )
   MATCH MDLCFG:arch
      gpt2  OF 2drop -1 ENDOF
      llama OF LP-NKV ENDOF
   ;MATCH ;

: ARM-FFN ( MDLCFG:arch -- n )
   MATCH MDLCFG:arch
      gpt2  OF 2drop -1 ENDOF
      llama OF LP-FFN ENDOF
   ;MATCH ;

: ARM-THETA ( MDLCFG:arch -- r )
   MATCH MDLCFG:arch
      gpt2  OF 2drop 0.0 ENDOF
      llama OF LP-THETA ENDOF
   ;MATCH ;

: ARM-REPS ( MDLCFG:arch -- r )
   MATCH MDLCFG:arch
      gpt2  OF 2drop 0.0 ENDOF
      llama OF LP-REPS ENDOF
   ;MATCH ;

\ ---- 1. construction + accessors, both arms ----------------------------------
: T-GPT2 ( -- )
   B-G
   MDLCFG:DTYPE@ DT0 MAKI-DATATYPE:EQ TTRUE
   MDLCFG:NCTX@ CX0 T=
   MDLCFG:NVOCAB@ VO0 T=
   MDLCFG:NLAYER@ NL0 T=
   MDLCFG:NEMBD@ NE0 T=
   MDLCFG:NHEAD@ NH0 T=
   MDLCFG:TIED@ TTRUE
   MDLCFG:BOS@ BOS0 T=
   MDLCFG:EOS@ EOS0 T=
   MDLCFG:ARCH@ ARM-EPS EPS0 f= TTRUE
   MDLCFG:ARCH@ ARM-SC TTRUE
   drop ;

: T-LLAMA ( -- )
   B-L
   MDLCFG:NCTX@ 4096 T=
   MDLCFG:NVOCAB@ 32000 T=
   MDLCFG:NLAYER@ 32 T=
   MDLCFG:NEMBD@ 4096 T=
   MDLCFG:NHEAD@ 32 T=
   MDLCFG:TIED@ TFALSE
   MDLCFG:BOS@ 1 T=
   MDLCFG:EOS@ 2 T=
   MDLCFG:ARCH@ ARM-NKV 4 T=
   MDLCFG:ARCH@ ARM-FFN 11008 T=
   MDLCFG:ARCH@ ARM-THETA THETA0 f= TTRUE
   MDLCFG:ARCH@ ARM-REPS RMSEPS0 f= TTRUE
   drop ;

: KL-NKV ( n -- MDLCFG:mcfg ) {: v:n :}
   v 11008 THETA0 RMSEPS0 MDLCFG-ARCH:LLAMA BL-WITH ;
: KL-FFN ( n -- MDLCFG:mcfg ) {: v:n :}
   4 v THETA0 RMSEPS0 MDLCFG-ARCH:LLAMA BL-WITH ;
: KL-THETA ( r -- MDLCFG:mcfg ) {: v:r :}
   4 11008 v RMSEPS0 MDLCFG-ARCH:LLAMA BL-WITH ;
: KL-REPS ( r -- MDLCFG:mcfg ) {: v:r :}
   4 11008 THETA0 v MDLCFG-ARCH:LLAMA BL-WITH ;

\ ---- 2. constructor rejections per field class --------------------------------
: RJ-CX ( -- )
   G-ARM DT0 0 VO0 NL0 NE0 NH0 true BOS0 EOS0 MDLCFG:BUILD drop ;
: RJ-VO ( -- )
   G-ARM DT0 CX0 0 NL0 NE0 NH0 true BOS0 EOS0 MDLCFG:BUILD drop ;
: RJ-NL ( -- )
   G-ARM DT0 CX0 VO0 0 NE0 NH0 true BOS0 EOS0 MDLCFG:BUILD drop ;
: RJ-NE ( -- )
   G-ARM DT0 CX0 VO0 NL0 0 NH0 true BOS0 EOS0 MDLCFG:BUILD drop ;
: RJ-NH ( -- )
   G-ARM DT0 CX0 VO0 NL0 NE0 0 true BOS0 EOS0 MDLCFG:BUILD drop ;
: RJ-OVF ( -- )
   G-ARM DT0 HUGE VO0 NL0 NE0 NH0 true BOS0 EOS0 MDLCFG:BUILD drop ;
: RJ-HEAD ( -- )
   G-ARM DT0 CX0 VO0 NL0 770 NH0 true BOS0 EOS0 MDLCFG:BUILD drop ;
: RJ-BOS ( -- )
   G-ARM DT0 CX0 VO0 NL0 NE0 NH0 true VO0 EOS0 MDLCFG:BUILD drop ;
: RJ-EOS ( -- )
   G-ARM DT0 CX0 VO0 NL0 NE0 NH0 true BOS0 -1 MDLCFG:BUILD drop ;
: RJ-EPS ( -- )
   0.0 G-ARM-EPS BG-WITH drop ;
: RJ-NKV ( -- )
   0 KL-NKV drop ;
: RJ-GQA ( -- )
   5 KL-NKV drop ;
: RJ-GQA2 ( -- )
   64 KL-NKV drop ;
: RJ-FFN ( -- )
   0 KL-FFN drop ;
: RJ-LOVF ( -- )
   HUGE KL-FFN drop ;
: RJ-THETA ( -- )
   0.0 KL-THETA drop ;
: RJ-REPS ( -- )
   -1.0 KL-REPS drop ;

: T-REJECTS ( -- )
   [: RJ-CX ;] MDLCFG:E-EXTENT TTHROWSQ
   [: RJ-VO ;] MDLCFG:E-EXTENT TTHROWSQ
   [: RJ-NL ;] MDLCFG:E-EXTENT TTHROWSQ
   [: RJ-NE ;] MDLCFG:E-EXTENT TTHROWSQ
   [: RJ-NH ;] MDLCFG:E-EXTENT TTHROWSQ
   [: RJ-OVF ;] MDLCFG:E-EXTENT TTHROWSQ
   [: RJ-HEAD ;] MDLCFG:E-HEAD TTHROWSQ
   [: RJ-BOS ;] MDLCFG:E-TOKEN TTHROWSQ
   [: RJ-EOS ;] MDLCFG:E-TOKEN TTHROWSQ
   [: RJ-EPS ;] MDLCFG:E-ARM TTHROWSQ
   [: RJ-NKV ;] MDLCFG:E-EXTENT TTHROWSQ
   [: RJ-GQA ;] MDLCFG:E-GQA TTHROWSQ
   [: RJ-GQA2 ;] MDLCFG:E-GQA TTHROWSQ
   [: RJ-FFN ;] MDLCFG:E-EXTENT TTHROWSQ
   [: RJ-LOVF ;] MDLCFG:E-EXTENT TTHROWSQ
   [: RJ-THETA ;] MDLCFG:E-ARM TTHROWSQ
   [: RJ-REPS ;] MDLCFG:E-ARM TTHROWSQ ;

T-RESET

T-GPT2
T-LLAMA
T-REJECTS

\ ---- 3. checker negatives -----------------------------------------------------
\ the generated raw MAKE certifies only with a genuine proof value...
s" MCP-MAKE ( MAKI:datatype n n n n n bool n n MDLCFG:arch MDLCFG:cfg-proof -- MDLCFG:mcfg ) MDLCFG-MCFG:MAKE" YES
\ ...a raw n in the proof slot type-rejects...
s" MCN-PROOF ( MAKI:datatype n n n n n bool n n MDLCFG:arch n -- MDLCFG:mcfg ) MDLCFG-MCFG:MAKE" NO
\ ...and the removed record cell's old generated arity no longer certifies.
s" MCN-OLD-MAKE ( MAKI:datatype n n n n n bool n n MDLCFG:arch n MDLCFG:cfg-proof -- MDLCFG:mcfg ) MDLCFG-MCFG:MAKE" NO
\ ...and the private mint is unresolvable outside package MDLCFG (verdict 1),
\ qualified or bare, so the proof cannot be produced around BUILD.
s" MCN-MINT ( -- MDLCFG:cfg-proof ) MDLCFG:MINT-CFG-PROOF" UNK
s" MCN-MINT2 ( -- MDLCFG:cfg-proof ) MINT-CFG-PROOF" UNK
\ the gpt2 arm rejects a GQA-shaped extra field by construction; the declared
\ arities certify.
s" MCP-G ( r bool -- MDLCFG:arch ) MDLCFG-ARCH:GPT2" YES
s" MCN-GQA ( n r bool -- MDLCFG:arch ) MDLCFG-ARCH:GPT2" NO
s" MCN-GQA2 ( n n r r -- MDLCFG:arch ) MDLCFG-ARCH:GPT2" NO
s" MCP-L ( n n r r -- MDLCFG:arch ) MDLCFG-ARCH:LLAMA" YES

\ ---- 4. the version cell is unrepresentable, not merely unused ----------------
\ The old eleven-input arity no longer certifies, so a caller that still hands
\ BUILD a version cannot be written at all.
s" MCN-OLDARITY ( MDLCFG:arch n MAKI:datatype n n n n n bool n n -- MDLCFG:mcfg ) MDLCFG:BUILD" NO
\ Both words the package used to export for the version are unresolvable now
\ (verdict 1); each certified before the cut, so these pin the deletion.
s" MCN-SCHEMA-ACC ( MDLCFG:mcfg -- MDLCFG:mcfg n ) MDLCFG:SCHEMA@" UNK
s" MCN-SCHEMA-ERR ( -- n ) MDLCFG:E-SCHEMA" UNK
\ Those two verdicts read checker enrollment; these read the live dictionary,
\ so an unchecked reintroduction of either exported name is caught as well.
s" MDLCFG:SCHEMA@" XREF-FIND XREF-FOUND? TFALSE
s" MDLCFG:E-SCHEMA" XREF-FIND XREF-FOUND? TFALSE
\ And the record carries no version slot: the live type registry resolves one
\ mcfg identity, answers the -1 sentinel for `sv`, and puts the dtype first -
\ so no accessor could read a version even by raw UNMAKE.
using REFLECT
   s" mcfg" s" MDLCFG-MCFG" FAMS 1 T=
   s" mcfg" s" MDLCFG-MCFG" s" sv" SLOT -1 T=
   s" mcfg" s" MDLCFG-MCFG" s" dt" SLOT 0 T=
   s" mcfg" s" MDLCFG-MCFG" FLDS 11 T=
;using

T-REPORT

;package
