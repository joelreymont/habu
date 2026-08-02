\ gpt2-config.f - the validated GPT-2 configuration (package GPT2).
\
\ BUILD validates every semantic field before minting cfg-proof. The proof is
\ the single temporary construction boundary required while public STRUCTURE
\ publishes MAKE/UNMAKE; the owner-construction checker dot retires it.

require lib/prelude.f
require maki/tensor.f

package GPT2

public

-5641 constant E-EXTENT
-5642 constant E-HEAD
-5643 constant E-TOKEN
-5644 constant E-EPS

NEWTYPE cfg-proof 0

STRUCTURE config 0
   FIELD datatype MAKI:datatype
   FIELD nctx n
   FIELD nvocab n
   FIELD nlayer n
   FIELD nembd n
   FIELD nhead n
   FIELD tied f
   FIELD bos n
   FIELD eos n
   FIELD ln-eps r
   FIELD attn-scale f
   FIELD tok cfg-proof
;STRUCTURE

private

TRUSTED: MINT-CFG-PROOF ( -- cfg-proof )  0 ;

$7FFFFFFFFFFFFFFF constant MAX-N

: N-POS ( n -- )
   0 <= if E-EXTENT throw then ;

: V-EPS ( r -- )
   0.0 f<= if E-EPS throw then ;

: XMUL ( n n -- n ) {: a:n b:n :}
   a 0= b 0= or if 0 exit then
   a MAX-N b / > if E-EXTENT throw then
   a b * ;

: V-EXTENTS ( n n n n n -- ) {: cx:n vo:n nl:n ne:n nh:n :}
   cx N-POS
   vo N-POS
   nl N-POS
   ne N-POS
   nh N-POS
   vo ne XMUL drop
   cx ne XMUL drop ;

: V-HEAD ( n n -- ) {: ne:n nh:n :}
   ne nh mod 0<> if E-HEAD throw then ;

: V-TOKEN ( n n -- ) {: id:n vo:n :}
   id 0 < id vo < 0= or if E-TOKEN throw then ;

public

: BUILD ( MAKI:datatype n n n n n bool n n r bool -- config )
   {: dt:MAKI:datatype cx:n vo:n nl:n ne:n nh:n tied:bool bos:n eos:n eps:r scale:bool :}
   cx vo nl ne nh V-EXTENTS
   ne nh V-HEAD
   bos vo V-TOKEN
   eos vo V-TOKEN
   eps V-EPS
   dt cx vo nl ne nh tied bos eos eps scale
   MINT-CFG-PROOF
   GPT2-CONFIG:MAKE ;

private

: FIELDS ( config -- MAKI:datatype n n n n n bool n n r bool )
   GPT2-CONFIG:UNMAKE {: tok:cfg-proof :} ;

public

: DATATYPE@ ( config -- config MAKI:datatype )
   dup FIELDS
   {: dt:MAKI:datatype cx:n vo:n nl:n ne:n nh:n tied:bool bos:n eos:n eps:r scale:bool :}
   dt ;

: NCTX@ ( config -- config n )
   dup FIELDS
   {: dt:MAKI:datatype cx:n vo:n nl:n ne:n nh:n tied:bool bos:n eos:n eps:r scale:bool :}
   cx ;

: NVOCAB@ ( config -- config n )
   dup FIELDS
   {: dt:MAKI:datatype cx:n vo:n nl:n ne:n nh:n tied:bool bos:n eos:n eps:r scale:bool :}
   vo ;

: NLAYER@ ( config -- config n )
   dup FIELDS
   {: dt:MAKI:datatype cx:n vo:n nl:n ne:n nh:n tied:bool bos:n eos:n eps:r scale:bool :}
   nl ;

: NEMBD@ ( config -- config n )
   dup FIELDS
   {: dt:MAKI:datatype cx:n vo:n nl:n ne:n nh:n tied:bool bos:n eos:n eps:r scale:bool :}
   ne ;

: NHEAD@ ( config -- config n )
   dup FIELDS
   {: dt:MAKI:datatype cx:n vo:n nl:n ne:n nh:n tied:bool bos:n eos:n eps:r scale:bool :}
   nh ;

: TIED? ( config -- config bool )
   dup FIELDS
   {: dt:MAKI:datatype cx:n vo:n nl:n ne:n nh:n tied:bool bos:n eos:n eps:r scale:bool :}
   tied ;

: BOS@ ( config -- config n )
   dup FIELDS
   {: dt:MAKI:datatype cx:n vo:n nl:n ne:n nh:n tied:bool bos:n eos:n eps:r scale:bool :}
   bos ;

: EOS@ ( config -- config n )
   dup FIELDS
   {: dt:MAKI:datatype cx:n vo:n nl:n ne:n nh:n tied:bool bos:n eos:n eps:r scale:bool :}
   eos ;

: LN-EPS@ ( config -- config r )
   dup FIELDS
   {: dt:MAKI:datatype cx:n vo:n nl:n ne:n nh:n tied:bool bos:n eos:n eps:r scale:bool :}
   eps ;

: ATTN-SCALE? ( config -- config bool )
   dup FIELDS
   {: dt:MAKI:datatype cx:n vo:n nl:n ne:n nh:n tied:bool bos:n eos:n eps:r scale:bool :}
   scale ;

;package
