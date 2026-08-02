\ model-config.f - the validated model configuration value (package MDLCFG).
\
\ CONCERN: one immutable, fully validated model configuration record (mcfg)
\ whose content identity is a 32-byte CONTENT-KEY digest (cfgkey) minted as the
\ sole constructor's FINAL act after all validation - no partially validated
\ value ever carries a key, and no second normalized-config authority exists.
\
\ STRUCTURE mcfg = the common behavioral fields shared by both arms
\ (MAKI:datatype - maki/tensor.f:123 is the sole dtype authority - nctx, nvocab,
\ nlayer, nembd, nhead, tied-embeddings flag, bos-id, eos-id; special tokens
\ are behavioral: decode stops on eos) + payload ENUM arch =
\ gpt2(ln-eps, attn-scale) | llama(nkvhead, ffn-dim, rope-theta, rms-eps) -
\ no meaningless fields on either arm - + the embedded cfgkey + the
\ private-mint proof token.
\
\ The proof is an arity-0 NEWTYPE exactly like maki/typestate.f ART:built's
\ build-proof (and maki/db/promotion.f's five stage proofs): the engine
\ fail-closes a zero-field STRUCTURE as a product field ("invalid field layout
\ metadata", throw 7127), so the nominal cell family is the one shape that can
\ ride inside mcfg while staying constructible ONLY through the private
\ trusted mint - a raw n in the proof slot is a checker reject. Scope of the
\ guarantee (shared with ART:built and the promotion proofs): a holder of a
\ REAL mcfg can UNMAKE and re-MAKE with the stale proof and key; closing that
\ needs the sealed-destructure/linear-UNMAKE capability tracked by dot.
\
\ mcfg is the CURRENT representation and carries no version cell: an old shape
\ is not a value this type can hold, so no consumer has an unknown-version case
\ to answer.
\
\ cfgkey preimage: every common behavioral field, then the canonical
\ serialization of the arch payload (variant tag text + payload
\ fields), all in declared order. Scalar fields fold as their 8 raw
\ little-endian cell bytes (floats: exact IEEE-754 bits, no decimal
\ rendering); the dtype folds as its MAKI:DT-KEY wire text; flags normalize
\ to 1/0 before folding. Every CONTENT-KEY:TEXT+ row is tag- and
\ length-delimited by the library, so adjacent fields cannot alias.
\
\ Constructor validation, all BEFORE the key mints, each class a named throw:
\ positive extents with overflow-checked composed products (vocab*embed,
\ ctx*embed, and per-arm embed*ffn); head-dim divisibility;
\ special-token range; GQA divisibility on the llama arm; positive arm
\ epsilons/theta; and the gpt2 tensor-census bound 4 + 13*nlayer.
\
\ maki -> habu only. Owns -5640..-5649.

require lib/prelude.f
require lib/content-key.f
require maki/tensor.f

package MDLCFG

public

\ ---- named rejection codes (one per validated field class) -------------------
-5641 constant E-EXTENT     \ a geometry extent nonpositive, or a composed product overflows
-5642 constant E-HEAD       \ nembd not divisible by nhead
-5643 constant E-TOKEN      \ bos-id/eos-id outside [0, nvocab)
-5644 constant E-GQA        \ llama nhead not divisible by nkvhead, or nkvhead > nhead
-5645 constant E-ARM        \ a nonpositive arch-arm epsilon/theta
-5646 constant E-CENSUS     \ the gpt2 tensor census 4 + 13*nlayer overflows a cell

\ ---- the architecture payload (consumers MATCH on the arm) -------------------
ENUM arch 0
   VARIANT gpt2  FIELD ln-eps r  FIELD attn-scale f ;VARIANT
   VARIANT llama FIELD nkvhead n  FIELD ffn-dim n  FIELD rope-theta r  FIELD rms-eps r ;VARIANT
;ENUM

\ ---- the 32-byte content identity as four n cells ----------------------------
STRUCTURE cfgkey 0 DERIVE eq
   FIELD k0 n  FIELD k1 n  FIELD k2 n  FIELD k3 n
;STRUCTURE

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
   FIELD key cfgkey
   FIELD tok cfg-proof
;STRUCTURE

private

TRUSTED: MINT-CFG-PROOF ( -- cfg-proof )  0 ;

$7FFFFFFFFFFFFFFF constant MAX-N
4 constant G-FIXED-T               \ gpt2 top-level tensors: wte, wpe, ln_f.g, ln_f.b
13 constant G-LAYER-T              \ gpt2 per-layer tensors: 12 block params + attn.bias mask

create NSCR 8 allot                \ one-cell fold scratch (cell bits -> raw bytes)
create KBUF 32 allot               \ CONTENT-KEY:FINAL digest landing pad

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

\ nlayer: the gpt2 tensor census 4 + 13*nlayer must fit a cell.
: V-CENSUS ( n -- )
   MAX-N G-FIXED-T - G-LAYER-T /  > if E-CENSUS throw then ;

\ arm validators consume the MATCH payload at their entry and rebuild the arm.
: V-GPT2 ( r bool n -- arch ) {: eps:r sc:bool nl:n :}
   eps F-POS
   nl V-CENSUS
   eps sc MDLCFG-ARCH:GPT2 ;

: V-LLAMA ( n n r r n n -- arch ) {: nkv:n ffn:n theta:r reps:r nh:n ne:n :}
   nkv N-POS  ffn N-POS
   theta F-POS  reps F-POS
   nh nkv mod 0<>  nkv nh >  or if E-GQA throw then
   ne ffn XMUL drop
   nkv ffn theta reps MDLCFG-ARCH:LLAMA ;

: V-ARCH ( arch n n n -- arch ) {: nl:n nh:n ne:n :}
   MATCH arch
      gpt2  OF nl V-GPT2 ENDOF
      llama OF nh ne V-LLAMA ENDOF
   ;MATCH ;

\ ---- cfgkey preimage folds -----------------------------------------------------
: FOLD-N ( n -- )
   NSCR !  NSCR 8 CONTENT-KEY:TEXT+ ;

: FOLD-R ( r -- )
   NSCR !  NSCR 8 CONTENT-KEY:TEXT+ ;

: FOLD-B ( bool -- )
   if 1 else 0 then FOLD-N ;

: FOLD-DT ( MAKI:datatype -- )
   MAKI:DT-KEY CONTENT-KEY:TEXT+ ;

: FOLD-GPT2 ( r bool -- ) {: eps:r sc:bool :}
   s" gpt2" CONTENT-KEY:TEXT+
   eps FOLD-R  sc FOLD-B ;

: FOLD-LLAMA ( n n r r -- ) {: nkv:n ffn:n theta:r reps:r :}
   s" llama" CONTENT-KEY:TEXT+
   nkv FOLD-N  ffn FOLD-N  theta FOLD-R  reps FOLD-R ;

: FOLD-ARCH ( arch -- )
   MATCH arch
      gpt2  OF FOLD-GPT2 ENDOF
      llama OF FOLD-LLAMA ENDOF
   ;MATCH ;

: KEY-FINAL ( -- cfgkey )
   KBUF CONTENT-KEY:FINAL
   KBUF 0 cells + @  KBUF 1 cells + @  KBUF 2 cells + @  KBUF 3 cells + @
   MDLCFG-CFGKEY:MAKE ;

public

\ ---- the sole constructor -------------------------------------------------------
\ Argument order is the mcfg field order with the wide arm first (deepest) so
\ the nine scalars bind as entry locals. Every rejection throws BEFORE the key
\ mint; the key mint is the final act after the folds, then the proof and MAKE.
: BUILD ( arch MAKI:datatype n n n n n bool n n -- mcfg )
   {: dt:MAKI:datatype cx:n vo:n nl:n ne:n nh:n te:bool bos:n eos:n :}
   cx vo nl ne nh V-EXTENTS
   ne nh V-HEAD
   bos vo V-TOKEN  eos vo V-TOKEN
   nl nh ne V-ARCH
   CONTENT-KEY:RESET
   dt FOLD-DT
   cx FOLD-N  vo FOLD-N  nl FOLD-N  ne FOLD-N  nh FOLD-N
   te FOLD-B  bos FOLD-N  eos FOLD-N
   dup FOLD-ARCH
   >r  dt cx vo nl ne nh te bos eos  r>
   KEY-FINAL  MINT-CFG-PROOF
   MDLCFG-MCFG:MAKE ;

private

\ ---- read projections off a duplicated copy -------------------------------------
\ mcfg is non-linear: accessors dup the value, UNMAKE the copy, bind the proof
\ local, and drop or extract the wide fields (key, arm) by whole-bundle
\ transport. Each public accessor is ( mcfg -- mcfg x ).
: MC-COMMON ( mcfg -- MAKI:datatype n n n n n bool n n )
   MDLCFG-MCFG:UNMAKE {: tok:cfg-proof :}
   drop drop ;

: MC-ARM ( mcfg -- arch )
   MDLCFG-MCFG:UNMAKE {: tok:cfg-proof :}
   drop >r
   2drop 2drop 2drop 2drop drop
   r> ;

: MC-KEY ( mcfg -- cfgkey )
   MDLCFG-MCFG:UNMAKE {: tok:cfg-proof :}
   >r >r
   2drop 2drop 2drop 2drop drop
   r> drop  r> ;

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

\ ---- the content identity ---------------------------------------------------------
: CFGKEY@ ( mcfg -- mcfg cfgkey )
   dup MC-KEY ;

: CFGKEY= ( cfgkey cfgkey -- bool )
   MDLCFG-CFGKEY:EQ ;

;package
