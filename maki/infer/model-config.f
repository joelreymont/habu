\ model-config.f - the validated model configuration value (package MDLCFG).
\
\ CONCERN: one immutable, fully validated model configuration record (mcfg)
\ built only by the validating constructor - no partially validated value can
\ carry the private proof, and no second normalized-config authority exists.
\
\ STRUCTURE mcfg = the common behavioral fields shared by both arms
\ (MAKI:datatype - maki/tensor.f:123 is the sole dtype authority - nctx, nvocab,
\ nlayer, nembd, nhead, tied-embeddings flag, bos-id, eos-id; special tokens
\ are behavioral: decode stops on eos) + payload ENUM arch =
\ gpt2(ln-eps, attn-scale) | llama(nkvhead, ffn-dim, rope-theta, rms-eps) -
\ no meaningless fields on either arm - + the private-mint proof token.
\
\ The proof is an arity-0 NEWTYPE exactly like maki/typestate.f ART:built's
\ build-proof (and maki/db/promotion.f's five stage proofs): the engine
\ fail-closes a zero-field STRUCTURE as a product field ("invalid field layout
\ metadata", throw 7127), so the nominal cell family is the one shape that can
\ ride inside mcfg while staying constructible ONLY through the private
\ trusted mint - a raw n in the proof slot is a checker reject. Scope of the
\ guarantee (shared with ART:built and the promotion proofs): a holder of a
\ REAL mcfg can UNMAKE and re-MAKE with the stale proof; closing that
\ needs the sealed-destructure/linear-UNMAKE capability tracked by dot.
\
\ mcfg is the CURRENT representation and carries no version cell: an old shape
\ is not a value this type can hold, so no consumer has an unknown-version case
\ to answer.
\
\ Constructor validation, all before the proof mints, each class a named throw:
\ positive extents with overflow-checked composed products (vocab*embed,
\ ctx*embed, and per-arm embed*ffn); head-dim divisibility;
\ special-token range; GQA divisibility on the llama arm; positive arm
\ epsilons/theta.
\
\ maki -> habu only. Owns -5640..-5649.

require lib/prelude.f
require maki/tensor.f

package MDLCFG

public

\ ---- named rejection codes (one per validated field class) -------------------
-5641 constant E-EXTENT     \ a geometry extent nonpositive, or a composed product overflows
-5642 constant E-HEAD       \ nembd not divisible by nhead
-5643 constant E-TOKEN      \ bos-id/eos-id outside [0, nvocab)
-5644 constant E-GQA        \ llama nhead not divisible by nkvhead, or nkvhead > nhead
-5645 constant E-ARM        \ a nonpositive arch-arm epsilon/theta

\ ---- the architecture payload (consumers MATCH on the arm) -------------------
ENUM arch 0
   VARIANT gpt2  FIELD ln-eps r  FIELD attn-scale f ;VARIANT
   VARIANT llama FIELD nkvhead n  FIELD ffn-dim n  FIELD rope-theta r  FIELD rms-eps r ;VARIANT
;ENUM

\ ---- the private-mint proof (see header: arity-0 nominal, ART:built shape) ---
NEWTYPE cfg-proof 0

STRUCTURE mcfg 0
   FIELD dt MAKI:datatype
   FIELD nctx n
   FIELD nvocab n
   FIELD nlayer n
   FIELD nembd n
   FIELD nhead n
   FIELD tied f
   FIELD bos n
   FIELD eos n
   FIELD arm arch
   FIELD tok cfg-proof
;STRUCTURE

private

TRUSTED: MINT-CFG-PROOF ( -- cfg-proof )  0 ;

$7FFFFFFFFFFFFFFF constant MAX-N

\ ---- checked validation helpers ----------------------------------------------
: N-POS ( n -- )
   0 <= if E-EXTENT throw then ;

: F-POS ( r -- )
   0.0 f<= if E-ARM throw then ;

\ overflow-checked nonnegative product; a composed extent must fit a cell.
: XMUL ( n n -- n ) {: a:n b:n :}
   a 0= b 0= or if 0 exit then
   a MAX-N b / > if E-EXTENT throw then
   a b * ;

\ nctx nvocab nlayer nembd nhead: positive, and the embedding/activation
\ products (nvocab*nembd, nctx*nembd) fit a cell.
: V-EXTENTS ( n n n n n -- ) {: cx:n vo:n nl:n ne:n nh:n :}
   cx N-POS  vo N-POS  nl N-POS  ne N-POS  nh N-POS
   vo ne XMUL drop
   cx ne XMUL drop ;

: V-HEAD ( n n -- ) {: ne:n nh:n :}
   ne nh mod 0<> if E-HEAD throw then ;

: V-TOKEN ( n n -- ) {: id:n vo:n :}
   id 0 <  id vo < 0=  or if E-TOKEN throw then ;

\ arm validators consume the MATCH payload at their entry and rebuild the arm.
: V-GPT2 ( r bool -- arch ) {: eps:r sc:bool :}
   eps F-POS
   eps sc MDLCFG-ARCH:GPT2 ;

: V-LLAMA ( n n r r n n -- arch ) {: nkv:n ffn:n theta:r reps:r nh:n ne:n :}
   nkv N-POS  ffn N-POS
   theta F-POS  reps F-POS
   nh nkv mod 0<>  nkv nh >  or if E-GQA throw then
   ne ffn XMUL drop
   nkv ffn theta reps MDLCFG-ARCH:LLAMA ;

: V-ARCH ( arch n n -- arch ) {: nh:n ne:n :}
   MATCH arch
      gpt2  OF V-GPT2 ENDOF
      llama OF nh ne V-LLAMA ENDOF
   ;MATCH ;

public

\ ---- the sole constructor -------------------------------------------------------
\ Argument order is the mcfg field order with the wide arm first (deepest) so
\ the nine scalars bind as entry locals. Every rejection throws before the
\ proof mints.
: BUILD ( arch MAKI:datatype n n n n n bool n n -- mcfg )
   {: dt:MAKI:datatype cx:n vo:n nl:n ne:n nh:n te:bool bos:n eos:n :}
   cx vo nl ne nh V-EXTENTS
   ne nh V-HEAD
   bos vo V-TOKEN  eos vo V-TOKEN
   nh ne V-ARCH
   >r  dt cx vo nl ne nh te bos eos  r>
   MINT-CFG-PROOF
   MDLCFG-MCFG:MAKE ;

private

\ ---- read projections off a duplicated copy -------------------------------------
\ mcfg is non-linear: accessors dup the value, UNMAKE the copy, bind the proof
\ local, and drop or extract the wide arm by whole-bundle
\ transport. Each public accessor is ( mcfg -- mcfg x ).
: MC-COMMON ( mcfg -- MAKI:datatype n n n n n bool n n )
   MDLCFG-MCFG:UNMAKE {: tok:cfg-proof :}
   drop ;

: MC-ARM ( mcfg -- arch )
   MDLCFG-MCFG:UNMAKE {: tok:cfg-proof :}
   >r
   2drop 2drop 2drop 2drop drop
   r> ;

public

\ ---- common behavioral field accessors -------------------------------------------
: DTYPE@ ( mcfg -- mcfg MAKI:datatype )
   dup MC-COMMON {: dt:MAKI:datatype cx:n vo:n nl:n ne:n nh:n te:bool bos:n eos:n :}
   dt ;

: NCTX@ ( mcfg -- mcfg n )
   dup MC-COMMON {: dt:MAKI:datatype cx:n vo:n nl:n ne:n nh:n te:bool bos:n eos:n :}
   cx ;

: NVOCAB@ ( mcfg -- mcfg n )
   dup MC-COMMON {: dt:MAKI:datatype cx:n vo:n nl:n ne:n nh:n te:bool bos:n eos:n :}
   vo ;

: NLAYER@ ( mcfg -- mcfg n )
   dup MC-COMMON {: dt:MAKI:datatype cx:n vo:n nl:n ne:n nh:n te:bool bos:n eos:n :}
   nl ;

: NEMBD@ ( mcfg -- mcfg n )
   dup MC-COMMON {: dt:MAKI:datatype cx:n vo:n nl:n ne:n nh:n te:bool bos:n eos:n :}
   ne ;

: NHEAD@ ( mcfg -- mcfg n )
   dup MC-COMMON {: dt:MAKI:datatype cx:n vo:n nl:n ne:n nh:n te:bool bos:n eos:n :}
   nh ;

: TIED@ ( mcfg -- mcfg bool )
   dup MC-COMMON {: dt:MAKI:datatype cx:n vo:n nl:n ne:n nh:n te:bool bos:n eos:n :}
   te ;

: BOS@ ( mcfg -- mcfg n )
   dup MC-COMMON {: dt:MAKI:datatype cx:n vo:n nl:n ne:n nh:n te:bool bos:n eos:n :}
   bos ;

: EOS@ ( mcfg -- mcfg n )
   dup MC-COMMON {: dt:MAKI:datatype cx:n vo:n nl:n ne:n nh:n te:bool bos:n eos:n :}
   eos ;

\ ---- the architecture arm ---------------------------------------------------------
: ARCH@ ( mcfg -- mcfg arch )
   dup MC-ARM ;

;package
