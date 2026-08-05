\ gpt2-model.f - validated GPT-2 weights in one GPU allocation.

require lib/prelude.f
require lib/adt/result.f
require lib/cad-num-arithmetic.f
require lib/fs.f
require lib/fs-path.f
require lib/ffi-abi.f
require lib/memory.f
require lib/ptx/cuda-driver.f
require lib/ptx/toolchain.f
require lib/string.f
require src/arch/ptx/emit.f
require maki/gpu-buffer.f
require maki/eval/active-target.f
require maki/infer/gpt2-pin.f
require maki/infer/gpt2-token.f
require maki/infer/hf-config.f
require maki/infer/safetensors.f
require maki/infer/gpt2-tensor.f
require maki/infer/gpt2-tensor-cg.f
require maki/infer/gpt2-attention-cg.f

package GPT2

public

DEFLINEAR GPT2:model

private

using MEM

-5651 constant E-CATALOG
-5654 constant E-DIGEST
-5655 constant E-TOKEN
-5656 constant E-CONTEXT
-5657 constant E-OUTPUT

32 constant M-DIGEST-LEN
64 constant M-HEX-LEN
64 constant M-NAME-CAP
$1000 constant M-ASM-CAP
32 constant M-KNAME-CAP

create M-ASM-OUT M-ASM-CAP allot
create M-ASM-ERR M-ASM-CAP allot
create M-CUBIN FS-PATH-CAP 1+ allot
create M-KNAME M-KNAME-CAP allot
create M-PARAM 10 cells allot

variable M-TMOD-H
variable M-AMOD-H
variable M-EMBED-H
variable M-LN-H
variable M-LINEAR-H
variable M-UNEMBED-H
variable M-GELU-H
variable M-RESIDUAL-H
variable M-ATTN-H

0 constant R-BUF
1 cells constant R-DT
2 cells constant R-CX
3 cells constant R-VO
4 cells constant R-NL
5 cells constant R-NE
6 cells constant R-NH
7 cells constant R-TIED
8 cells constant R-BOS
9 cells constant R-EOS
10 cells constant R-EPS
11 cells constant R-SCALE
12 cells constant R-PROOF
13 cells constant R-X
14 cells constant R-A
15 cells constant R-B
16 cells constant R-LOGITS
17 cells constant R-TOKEN
18 cells constant R-K
19 cells constant R-V
20 cells constant R-POS
21 cells constant R-TMOD
22 cells constant R-AMOD
23 cells constant R-EMBED
24 cells constant R-LN
25 cells constant R-LINEAR
26 cells constant R-UNEMBED
27 cells constant R-GELU
28 cells constant R-RESIDUAL
29 cells constant R-ATTN
30 cells constant R-TOKSTATE
31 cells constant R-TOKLEN
32 cells constant R-BYTES

\ One GPU:buffer is laid out as weights | x | a | b | logits | token | K | V;
\ the private offsets below are the sole authority for that allocation.
\ Cubins are transient build provenance: only two modules and seven functions
\ persist, and CLOSE unloads attention then tensor before the model buffer.
\ GPT2:model is the second production CUDA consumer after GPU and adds no runtime.
\ The checker cannot tie raw CUDA module/function cells to the model lifetime;
\ they stay private and are valid only while their owning module is loaded.
\ Retirement owner: habu-checker-ptr-lifetime-f59d1e9d.

: M-REC-LEN ( -- CAD-NUM:alloc-byte-len )
   R-BYTES BYTES-ALLOC-LEN ;

: M-DROP-CFG ( config -- )
   GPT2-CONFIG:UNMAKE
   2drop 2drop 2drop 2drop 2drop 2drop ;

\ The checker cannot tie the opaque owner to its private host record.
\ Retirement owner: habu-checker-ptr-lifetime-f59d1e9d.
TRUSTED: M-SAVE
   ( MEM:block GPU:buffer config n n n n n n n n n n n n n n n n n ptr n CAD-NUM:alloc-byte-len -- GPT2:model )
   {: buf:GPU:buffer dt:MAKI:datatype cx:n vo:n nl:n ne:n nh:n tied:bool bos:n eos:n eps:r scale:bool proof:cfg-proof x:n a:n b:n logits:n token:n k:n v:n pos:n tmod:n amod:n embed:n ln:n linear:n unembed:n gelu:n residual:n attn:n tokstate:ptr toklen:CAD-NUM:alloc-byte-len :}
   BORROW {: rec:ptr recu:CAD-NUM:byte-len :}
   recu R-BYTES <> if E-SIZE throw then
   buf rec R-BUF + !
   dt rec R-DT + !
   cx rec R-CX + !
   vo rec R-VO + !
   nl rec R-NL + !
   ne rec R-NE + !
   nh rec R-NH + !
   tied rec R-TIED + !
   bos rec R-BOS + !
   eos rec R-EOS + !
   eps rec R-EPS + !
   scale rec R-SCALE + !
   proof rec R-PROOF + !
   x rec R-X + !
   a rec R-A + !
   b rec R-B + !
   logits rec R-LOGITS + !
   token rec R-TOKEN + !
   k rec R-K + !
   v rec R-V + !
   pos rec R-POS + !
   tmod rec R-TMOD + !
   amod rec R-AMOD + !
   embed rec R-EMBED + !
   ln rec R-LN + !
   linear rec R-LINEAR + !
   unembed rec R-UNEMBED + !
   gelu rec R-GELU + !
   residual rec R-RESIDUAL + !
   attn rec R-ATTN + !
   tokstate rec R-TOKSTATE + !
   toklen rec R-TOKLEN + ! ;

TRUSTED: M-TAKE
   ( GPT2:model -- MEM:block GPU:buffer config n n n n n n n n n n n n n n n n n ptr n CAD-NUM:alloc-byte-len )
   BORROW {: rec:ptr recu:CAD-NUM:byte-len :}
   recu R-BYTES <> if E-SIZE throw then
   rec R-BUF + @
   rec R-DT + @
   rec R-CX + @
   rec R-VO + @
   rec R-NL + @
   rec R-NE + @
   rec R-NH + @
   rec R-TIED + @
   rec R-BOS + @
   rec R-EOS + @
   rec R-EPS + @
   rec R-SCALE + @
   rec R-PROOF + @
   GPT2-CONFIG:MAKE
   rec R-X + @
   rec R-A + @
   rec R-B + @
   rec R-LOGITS + @
   rec R-TOKEN + @
   rec R-K + @
   rec R-V + @
   rec R-POS + @
   rec R-TMOD + @
   rec R-AMOD + @
   rec R-EMBED + @
   rec R-LN + @
   rec R-LINEAR + @
   rec R-UNEMBED + @
   rec R-GELU + @
   rec R-RESIDUAL + @
   rec R-ATTN + @
   rec R-TOKSTATE + @
   rec R-TOKLEN + @ ;

: M-ADD ( n n -- n ) {: a:n b:n :}
   a MAX-N b - > if E-SIZE throw then
   a b + ;

: M-DTYPE-BYTES ( MAKI:datatype -- n )
   MATCH MAKI:datatype
      df32  OF 4 ENDOF
      df16  OF 2 ENDOF
      dbf16 OF 2 ENDOF
      du32  OF 4 ENDOF
      di32  OF 4 ENDOF
   ;MATCH ;

: M-GLOBAL-ELEMS ( config -- config n )
   NEMBD@ {: ne:n :}
   NVOCAB@ {: vo:n :}
   NCTX@ {: cx:n :}
   vo cx M-ADD 2 M-ADD ne CHECKED-MUL ;

: M-LAYER-ELEMS ( config -- config n )
   NEMBD@ {: ne:n :}
   NCTX@ {: cx:n :}
   cx cx CHECKED-MUL
   ne ne CHECKED-MUL 12 CHECKED-MUL M-ADD
   ne 13 CHECKED-MUL M-ADD ;

: M-ROLE-OFF ( n n n -- n ) {: role:n cx:n ne:n :}
   cx cx CHECKED-MUL {: mask:n :}
   ne ne CHECKED-MUL {: sq:n :}
   role case
      0  of 0 endof
      1  of ne endof
      2  of ne 2 CHECKED-MUL endof
      3  of mask ne 2 CHECKED-MUL M-ADD endof
      4  of mask sq 3 CHECKED-MUL M-ADD ne 2 CHECKED-MUL M-ADD endof
      5  of mask sq 3 CHECKED-MUL M-ADD ne 5 CHECKED-MUL M-ADD endof
      6  of mask sq 4 CHECKED-MUL M-ADD ne 5 CHECKED-MUL M-ADD endof
      7  of mask sq 4 CHECKED-MUL M-ADD ne 6 CHECKED-MUL M-ADD endof
      8  of mask sq 4 CHECKED-MUL M-ADD ne 7 CHECKED-MUL M-ADD endof
      9  of mask sq 4 CHECKED-MUL M-ADD ne 8 CHECKED-MUL M-ADD endof
      10 of mask sq 8 CHECKED-MUL M-ADD ne 8 CHECKED-MUL M-ADD endof
      11 of mask sq 8 CHECKED-MUL M-ADD ne 12 CHECKED-MUL M-ADD endof
      12 of mask sq 12 CHECKED-MUL M-ADD ne 12 CHECKED-MUL M-ADD endof
      E-SLOT throw
   endcase ;

: M-SLOT-ELEMS ( config n -- config n ) {: slot:n :}
   slot GLOBAL-COUNT < if
      NEMBD@ {: ne:n :}
      NVOCAB@ {: vo:n :}
      NCTX@ {: cx:n :}
      slot case
         0 of 0 endof
         1 of vo ne CHECKED-MUL endof
         2 of vo cx M-ADD ne CHECKED-MUL endof
         3 of vo cx M-ADD 1 M-ADD ne CHECKED-MUL endof
         E-SLOT throw
      endcase
      exit
   then
   M-GLOBAL-ELEMS {: globals:n :}
   M-LAYER-ELEMS {: layer-elems:n :}
   NEMBD@ {: ne:n :}
   NCTX@ {: cx:n :}
   slot GLOBAL-COUNT - LAYER-ROLE-COUNT /mod {: role:n layer:n :}
   globals layer layer-elems CHECKED-MUL M-ADD
   role cx ne M-ROLE-OFF M-ADD ;

: M-BYTE-OFF ( n -- CAD-NUM:byte-off )
   CAD-NUM:BYTE-OFF MATCH CAD-NUM:numeric-result
      ok OF ENDOF
      negative OF E-SIZE throw ENDOF
      zero OF E-SIZE throw ENDOF
      overflow OF E-SIZE throw ENDOF
      underflow OF E-SIZE throw ENDOF
      bad-alignment OF E-SIZE throw ENDOF
      misaligned OF E-SIZE throw ENDOF
   ;MATCH ;

: M-ITEM-COUNT ( n -- CAD-NUM:item-count )
   CAD-NUM:ITEM-COUNT
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF
      negative OF E-CONTEXT throw ENDOF
      zero OF E-CONTEXT throw ENDOF
      overflow OF E-CONTEXT throw ENDOF
      underflow OF E-CONTEXT throw ENDOF
      bad-alignment OF E-CONTEXT throw ENDOF
      misaligned OF E-CONTEXT throw ENDOF
   ;MATCH ;

: M-BYTE-LEN ( n -- CAD-NUM:byte-len )
   CAD-NUM:BYTE-LEN MATCH CAD-NUM:numeric-result
      ok OF ENDOF
      negative OF E-SIZE throw ENDOF
      zero OF E-SIZE throw ENDOF
      overflow OF E-SIZE throw ENDOF
      underflow OF E-SIZE throw ENDOF
      bad-alignment OF E-SIZE throw ENDOF
      misaligned OF E-SIZE throw ENDOF
   ;MATCH ;

: M-WEIGHT-BYTES ( config -- config n )
   NLAYER@ {: nl:n :}
   M-GLOBAL-ELEMS {: globals:n :}
   M-LAYER-ELEMS {: layer:n :}
   DATATYPE@ M-DTYPE-BYTES {: bytes:n :}
   globals layer nl CHECKED-MUL M-ADD bytes CHECKED-MUL ;

: M-TOTAL-LEN ( config -- config CAD-NUM:byte-len )
   M-WEIGHT-BYTES M-BYTE-LEN ;

: M-ALLOC-N ( n -- CAD-NUM:alloc-byte-len )
   M-BYTE-LEN CAD-NUM:AS-ALLOC-BYTE-LEN
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF
      negative OF E-SIZE throw ENDOF
      zero OF E-SIZE throw ENDOF
      overflow OF E-SIZE throw ENDOF
      underflow OF E-SIZE throw ENDOF
      bad-alignment OF E-SIZE throw ENDOF
      misaligned OF E-SIZE throw ENDOF
   ;MATCH ;

: M-LAYOUT-BODY
   ( config -- config n n n n n n n CAD-NUM:alloc-byte-len )
   M-WEIGHT-BYTES {: x:n :}
   NEMBD@ 4 CHECKED-MUL {: row:n :}
   NVOCAB@ 4 CHECKED-MUL {: logits-len:n :}
   NCTX@ {: cx:n :}
   NLAYER@ {: nl:n :}
   cx row CHECKED-MUL nl CHECKED-MUL {: kv-len:n :}
   x row M-ADD {: a:n :}
   a row M-ADD {: b:n :}
   b row 4 CHECKED-MUL M-ADD {: logits:n :}
   logits logits-len M-ADD {: token:n :}
   token 4 M-ADD {: k:n :}
   k kv-len M-ADD {: v:n :}
   x a b logits token k v
   v kv-len M-ADD M-ALLOC-N ;

: M-LAYOUT-FRAME
   ( config CAD-NUM:alloc-byte-len -- config CAD-NUM:alloc-byte-len )
   drop M-LAYOUT-BODY
   >r 2drop 2drop 2drop drop r> ;

: M-LAYOUT ( config -- config result<CAD-NUM:alloc-byte-len,n> )
   {: c :} \ typed-local-lint: allow-bare-local - config is a concrete multi-cell structure.
   c M-REC-LEN
   [: M-LAYOUT-FRAME ;] catch
   {: got alloc:CAD-NUM:alloc-byte-len code:n :} \ typed-local-lint: allow-bare-local - config is a concrete multi-cell structure.
   got M-DROP-CFG
   code 0<> if
      alloc drop
      c code RESULT:ERR
      exit
   then
   c alloc RESULT:OK ;

: M-SLOT-INDEX ( n -- CAD-NUM:index )
   CAD-NUM:INDEX MATCH CAD-NUM:numeric-result
      ok OF ENDOF
      negative OF E-SLOT throw ENDOF
      zero OF E-SLOT throw ENDOF
      overflow OF E-SLOT throw ENDOF
      underflow OF E-SLOT throw ENDOF
      bad-alignment OF E-SLOT throw ENDOF
      misaligned OF E-SLOT throw ENDOF
   ;MATCH ;

: M-OPT ( option<n> -- n n )
   MATCH option
      none OF 0 E-CATALOG ENDOF
      some OF 0 ENDOF
   ;MATCH ;

: M-OPT= ( option<n> n -- bool ) {: want:n :}
   MATCH option
      none OF false ENDOF
      some OF want = ENDOF
   ;MATCH ;

: M-SOME? ( option<n> -- bool )
   MATCH option
      none OF false ENDOF
      some OF drop true ENDOF
   ;MATCH ;

: M-CFG-DTYPE ( config -- MAKI:datatype )
   DATATYPE@ {: dt:MAKI:datatype :}
   drop dt ;

\ M-LAYOUT runs before SAFET:LOAD and proves the pinned geometry products;
\ the loops below only derive tensor ids from slots inside that same census.
: M-FIND-SLOT
   ( SAFET:file config n ptr u8 -- SAFET:file config n n )
   \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
   {: c slot:n scratch:ptr :}
   c slot M-SLOT-INDEX TENSOR-ID-FOR-SLOT
   scratch M-NAME-CAP COPY-NAME?
   MATCH option
      none OF
         drop c 0 E-CATALOG
      ENDOF
      some OF
         {: nameu:n :}
         drop
         scratch nameu SAFET:FIND M-OPT {: id:n code:n :}
         c id code
      ENDOF
   ;MATCH ;

: M-CATALOG-ROW
   ( SAFET:file config ptr u8 n -- SAFET:file config ptr u8 n )
   \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
   {: c scratch:ptr slot:n :}
   c slot scratch M-FIND-SLOT
   \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
   {: c2 id:n code:n :}
   code 0<> if c2 scratch code exit then
   id c2 M-CFG-DTYPE SAFET:DATATYPE=
   0= if c2 scratch E-CATALOG exit then
   c2 slot M-SLOT-INDEX TENSOR-ID-FOR-SLOT SHAPE
   {: rank:n d0:n d1:n d2:n d3:n :}
   drop
   id SAFET:RANK? rank M-OPT= 0= if c2 scratch E-CATALOG exit then
   id 0 SAFET:DIM? d0 M-OPT= 0= if c2 scratch E-CATALOG exit then
   rank 1 > if
      id 1 SAFET:DIM? d1 M-OPT= 0= if c2 scratch E-CATALOG exit then
   then
   rank 2 > if
      id 2 SAFET:DIM? d2 M-OPT= 0= if c2 scratch E-CATALOG exit then
      id 3 SAFET:DIM? d3 M-OPT= 0= if c2 scratch E-CATALOG exit then
   then
   d0 d1 CHECKED-MUL d2 CHECKED-MUL d3 CHECKED-MUL
   c2 M-CFG-DTYPE M-DTYPE-BYTES CHECKED-MUL {: want:n :}
   id SAFET:NBYTES? want M-OPT= 0= if c2 scratch E-CATALOG exit then
   id SAFET:MAP-OFFSET? M-SOME? 0= if c2 scratch E-CATALOG exit then
   c2 scratch 0 ;

: M-CATALOG-ROWS
   ( SAFET:file config ptr u8 n n -- SAFET:file config ptr u8 n )
   \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
   {: c scratch:ptr slot:n total:n :}
   slot total = if c scratch 0 exit then
   c scratch slot M-CATALOG-ROW
   \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
   {: c2 scratch2:ptr code:n :}
   code 0<> if c2 scratch2 code exit then
   c2 scratch2 slot 1+ total RECURSE ;

: M-CATALOG ( SAFET:file config ptr u8 -- SAFET:file config ptr u8 n )
   \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
   {: c scratch:ptr :}
   SAFET:COUNT {: got:n :}
   c COUNT {: want:n :}
   drop
   want got <> if c scratch E-CATALOG exit then
   c scratch
   0 want M-CATALOG-ROWS ;

: M-PIN-MAP ( ptr u8 SAFET:mapping ptr u8 n -- ptr u8 n SAFET:mapping )
   {: base:ptr len:n :}
   >r {: scratch:ptr :}
   len GPT2PIN:MODEL-LEN <> if scratch E-DIGEST r> exit then
   base len scratch SHA256
   scratch scratch M-DIGEST-LEN + SHA256>HEX
   scratch M-DIGEST-LEN + M-HEX-LEN GPT2PIN:MODEL-SHA256$ STR=
   if 0 else E-DIGEST then
   scratch swap r> ;

: M-PIN-FRAME ( ptr u8 SAFET:mapping -- ptr u8 n SAFET:mapping )
   [: M-PIN-MAP ;] SAFET:WITH-MAPPING drop ;

;using

public

: SPAN ( config tensor-id -- config CAD-NUM:byte-off CAD-NUM:byte-len )
   2dup SLOT SLOT>N {: slot:n :}
   drop
   SHAPE {: rank:n d0:n d1:n d2:n d3:n :}
   d0 d1 CHECKED-MUL d2 CHECKED-MUL d3 CHECKED-MUL {: elems:n :}
   DATATYPE@ M-DTYPE-BYTES {: bytes:n :}
   slot M-SLOT-ELEMS {: off:n :}
   off bytes CHECKED-MUL M-BYTE-OFF
   elems bytes CHECKED-MUL M-BYTE-LEN ;

private

using MEM

: M-RESULT-CODE ( result<n,n> -- n )
   MATCH result
      ok OF drop 0 ENDOF
      err OF ENDOF
   ;MATCH ;

: M-FIRST ( n n -- n ) {: first:n code:n :}
   first 0= code 0<> and if code else first then ;

: M-CUDA ( rc -- )
   RC>N dup 0<> if throw then drop ;

: M-U! ( n n -- )
   cells M-PARAM + ! ;

: M-PD ( n n n -- ) {: fn:n off:n slot:n :}
   fn >CUDA-FN off >IDX M-PARAM slot cells + 8 >LEN
   CUDA:CU-PARAM-SET-V M-CUDA ;

: M-PU ( n n n -- ) {: fn:n off:n slot:n :}
   fn >CUDA-FN off >IDX M-PARAM slot cells + 4 >LEN
   CUDA:CU-PARAM-SET-V M-CUDA ;

: M-EMBED-BODY ( n n -- n n ) {: fn:n grid:n :}
   fn grid
   fn >CUDA-FN 44 >LEN CUDA:CU-PARAM-SET-SIZE M-CUDA
   fn 0 0 M-PD  fn 8 1 M-PD  fn 16 2 M-PD  fn 24 3 M-PD
   fn 32 4 M-PU  fn 36 5 M-PU  fn 40 6 M-PU
   fn >CUDA-FN 256 1 1 CUDA:CU-FUNC-SET-BLOCK-SHAPE M-CUDA
   fn >CUDA-FN grid 1 CUDA:CU-LAUNCH-GRID M-CUDA ;

: M-EMBED-GO ( n n -- n )
   [: M-EMBED-BODY ;] catch nip nip ;

: M-LN-BODY ( n n -- n n ) {: fn:n grid:n :}
   fn grid
   fn >CUDA-FN 40 >LEN CUDA:CU-PARAM-SET-SIZE M-CUDA
   fn 0 0 M-PD  fn 8 1 M-PD  fn 16 2 M-PD  fn 24 3 M-PD
   fn 32 4 M-PU  fn 36 5 M-PU
   fn >CUDA-FN 256 1 1 CUDA:CU-FUNC-SET-BLOCK-SHAPE M-CUDA
   fn >CUDA-FN grid 1 CUDA:CU-LAUNCH-GRID M-CUDA ;

: M-LN-GO ( n n -- n )
   [: M-LN-BODY ;] catch nip nip ;

: M-LINEAR-BODY ( n n -- n n ) {: fn:n grid:n :}
   fn grid
   fn >CUDA-FN 44 >LEN CUDA:CU-PARAM-SET-SIZE M-CUDA
   fn 0 0 M-PD  fn 8 1 M-PD  fn 16 2 M-PD  fn 24 3 M-PD
   fn 32 4 M-PU  fn 36 5 M-PU  fn 40 6 M-PU
   fn >CUDA-FN 256 1 1 CUDA:CU-FUNC-SET-BLOCK-SHAPE M-CUDA
   fn >CUDA-FN grid 1 CUDA:CU-LAUNCH-GRID M-CUDA ;

: M-LINEAR-GO ( n n -- n )
   [: M-LINEAR-BODY ;] catch nip nip ;

: M-UNEMBED-BODY ( n n -- n n ) {: fn:n grid:n :}
   fn grid
   fn >CUDA-FN 32 >LEN CUDA:CU-PARAM-SET-SIZE M-CUDA
   fn 0 0 M-PD  fn 8 1 M-PD  fn 16 2 M-PD
   fn 24 3 M-PU  fn 28 4 M-PU
   fn >CUDA-FN 256 1 1 CUDA:CU-FUNC-SET-BLOCK-SHAPE M-CUDA
   fn >CUDA-FN grid 1 CUDA:CU-LAUNCH-GRID M-CUDA ;

: M-UNEMBED-GO ( n n -- n )
   [: M-UNEMBED-BODY ;] catch nip nip ;

: M-GELU-BODY ( n n -- n n ) {: fn:n grid:n :}
   fn grid
   fn >CUDA-FN 12 >LEN CUDA:CU-PARAM-SET-SIZE M-CUDA
   fn 0 0 M-PD  fn 8 1 M-PU
   fn >CUDA-FN 256 1 1 CUDA:CU-FUNC-SET-BLOCK-SHAPE M-CUDA
   fn >CUDA-FN grid 1 CUDA:CU-LAUNCH-GRID M-CUDA ;

: M-GELU-GO ( n n -- n )
   [: M-GELU-BODY ;] catch nip nip ;

: M-RESIDUAL-BODY ( n n -- n n ) {: fn:n grid:n :}
   fn grid
   fn >CUDA-FN 28 >LEN CUDA:CU-PARAM-SET-SIZE M-CUDA
   fn 0 0 M-PD  fn 8 1 M-PD  fn 16 2 M-PD  fn 24 3 M-PU
   fn >CUDA-FN 256 1 1 CUDA:CU-FUNC-SET-BLOCK-SHAPE M-CUDA
   fn >CUDA-FN grid 1 CUDA:CU-LAUNCH-GRID M-CUDA ;

: M-RESIDUAL-GO ( n n -- n )
   [: M-RESIDUAL-BODY ;] catch nip nip ;

: M-ATTN-BODY ( n n n -- n n n ) {: fn:n grid:n shared:n :}
   fn grid shared
   fn >CUDA-FN 64 >LEN CUDA:CU-PARAM-SET-SIZE M-CUDA
   fn 0 0 M-PD  fn 8 1 M-PD  fn 16 2 M-PD
   fn 24 3 M-PD  fn 32 4 M-PD  fn 40 5 M-PD
   fn 48 6 M-PU  fn 52 7 M-PU  fn 56 8 M-PU  fn 60 9 M-PU
   fn >CUDA-FN 128 1 1 CUDA:CU-FUNC-SET-BLOCK-SHAPE M-CUDA
   fn >CUDA-FN shared CUDA:CU-FUNC-SET-SHARED-SIZE M-CUDA
   fn >CUDA-FN grid 1 CUDA:CU-LAUNCH-GRID M-CUDA ;

: M-ATTN-GO ( n n n -- n )
   [: M-ATTN-BODY ;] catch nip nip nip ;

: M-GRID ( n -- n )
   256 /mod swap 0<> if 1+ then ;

: M-SUB-OK? ( CAD-NUM:byte-len CAD-NUM:byte-len -- bool )
   CAD-NUM:SUB-BYTES MATCH CAD-NUM:numeric-result
      ok OF drop true ENDOF
      negative OF false ENDOF
      zero OF true ENDOF
      overflow OF false ENDOF
      underflow OF false ENDOF
      bad-alignment OF false ENDOF
      misaligned OF false ENDOF
   ;MATCH ;

: M-BYTES= ( CAD-NUM:byte-len CAD-NUM:byte-len -- bool )
   2dup M-SUB-OK? >r swap M-SUB-OK? r> and ;

\ M-PARAM holds borrowed device addresses only through the immediate launch;
\ GPU:session and GPU:buffer remain live on the stack. The checker cannot bind
\ that lifetime yet; retirement owner: habu-checker-ptr-lifetime-f59d1e9d.
\ M-PARAM is the raw kernel-argument buffer: M-U! writes plain cells into it and
\ the attention path reads plain cells back out (M-PARAM @), so a device address
\ leaves its nominal type at the boundary rather than inside the buffer. The
\ checker refuses to store a `cuda-devptr` into a plain cell, which is what makes
\ the projection explicit here instead of silent.
: M-SPAN-SAVE ( result<cuda-devptr,n> n -- n ) {: slot:n :}
   MATCH result
      ok OF CUDA-DEVPTR>N M-PARAM slot cells + ! 0 ENDOF
      err OF ENDOF
   ;MATCH ;

: M-O-SPAN
   ( GPU:session GPU:buffer config n n n -- GPU:session GPU:buffer config n )
   {: off:n bytes:n slot:n :}
   \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
   {: c :}
   off M-BYTE-OFF bytes M-BYTE-LEN GPU:SPAN slot M-SPAN-SAVE {: code:n :}
   c code ;

: M-W-SPAN
   ( GPU:session GPU:buffer config tensor-id n -- GPU:session GPU:buffer config n )
   \ typed-local-lint: allow-bare-local - tensor-id is a variant structure.
   {: id slot:n :}
   \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
   {: c :}
   c id SPAN
   \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
   {: c2 off:CAD-NUM:byte-off bytes:CAD-NUM:byte-len :}
   off bytes GPU:SPAN slot M-SPAN-SAVE {: code:n :}
   c2 code ;

: M-EMBED
   ( GPU:session GPU:buffer config n n n n -- GPU:session GPU:buffer config n )
   {: token:n x:n pos:n fn:n :}
   NEMBD@ {: ne:n :}
   1 4 M-U!  ne 5 M-U!  pos 6 M-U!
   token 4 0 M-O-SPAN {: c0:n :}
   c0 0<> if c0 exit then
   GPT2-GLOBAL--ROLE:WTE GPT2-TENSOR--ID:GLOBAL 1 M-W-SPAN {: c1:n :}
   c1 0<> if c1 exit then
   GPT2-GLOBAL--ROLE:WPE GPT2-TENSOR--ID:GLOBAL 2 M-W-SPAN {: c2:n :}
   c2 0<> if c2 exit then
   x ne 4 CHECKED-MUL 3 M-O-SPAN {: c3:n :}
   c3 0<> if c3 exit then
   fn ne M-GRID M-EMBED-GO ;

: M-LN-LAYER
   ( GPU:session GPU:buffer config n n layer-role layer-role n n -- GPU:session GPU:buffer config n )
   \ typed-local-lint: allow-bare-local - roles are arity-zero enum values.
   {: x:n out:n grole brole layer:n fn:n :}
   NEMBD@ {: ne:n :}
   1 4 M-U!  ne 5 M-U!
   x ne 4 CHECKED-MUL 0 M-O-SPAN {: c0:n :}
   c0 0<> if c0 exit then
   layer LAYER-ID grole GPT2-TENSOR--ID:LAYER
   1 M-W-SPAN {: c1:n :}
   c1 0<> if c1 exit then
   layer LAYER-ID brole GPT2-TENSOR--ID:LAYER
   2 M-W-SPAN {: c2:n :}
   c2 0<> if c2 exit then
   out ne 4 CHECKED-MUL 3 M-O-SPAN {: c3:n :}
   c3 0<> if c3 exit then
   fn 1 M-LN-GO ;

: M-LN-FINAL
   ( GPU:session GPU:buffer config n n n -- GPU:session GPU:buffer config n )
   {: x:n out:n fn:n :}
   NEMBD@ {: ne:n :}
   1 4 M-U!  ne 5 M-U!
   x ne 4 CHECKED-MUL 0 M-O-SPAN {: c0:n :}
   c0 0<> if c0 exit then
   GPT2-GLOBAL--ROLE:LNF-G GPT2-TENSOR--ID:GLOBAL 1 M-W-SPAN {: c1:n :}
   c1 0<> if c1 exit then
   GPT2-GLOBAL--ROLE:LNF-B GPT2-TENSOR--ID:GLOBAL 2 M-W-SPAN {: c2:n :}
   c2 0<> if c2 exit then
   out ne 4 CHECKED-MUL 3 M-O-SPAN {: c3:n :}
   c3 0<> if c3 exit then
   fn 1 M-LN-GO ;

: M-LINEAR
   ( GPU:session GPU:buffer config n n n n layer-role layer-role n n -- GPU:session GPU:buffer config n )
   \ typed-local-lint: allow-bare-local - roles are arity-zero enum values.
   {: x:n out:n in:n cols:n wrole brole layer:n fn:n :}
   1 4 M-U!  in 5 M-U!  cols 6 M-U!
   x in 4 CHECKED-MUL 0 M-O-SPAN {: c0:n :}
   c0 0<> if c0 exit then
   layer LAYER-ID wrole GPT2-TENSOR--ID:LAYER 1 M-W-SPAN {: c1:n :}
   c1 0<> if c1 exit then
   layer LAYER-ID brole GPT2-TENSOR--ID:LAYER 2 M-W-SPAN {: c2:n :}
   c2 0<> if c2 exit then
   out cols 4 CHECKED-MUL 3 M-O-SPAN {: c3:n :}
   c3 0<> if c3 exit then
   fn cols M-GRID M-LINEAR-GO ;

: M-GELU
   ( GPU:session GPU:buffer config n n n -- GPU:session GPU:buffer config n )
   {: x:n elems:n fn:n :}
   elems 1 M-U!
   x elems 4 CHECKED-MUL 0 M-O-SPAN {: code:n :}
   code 0<> if code exit then
   fn elems M-GRID M-GELU-GO ;

: M-RESIDUAL
   ( GPU:session GPU:buffer config n n n n n -- GPU:session GPU:buffer config n )
   {: x:n residual:n out:n elems:n fn:n :}
   elems 3 M-U!
   x elems 4 CHECKED-MUL 0 M-O-SPAN {: c0:n :}
   c0 0<> if c0 exit then
   residual elems 4 CHECKED-MUL 1 M-O-SPAN {: c1:n :}
   c1 0<> if c1 exit then
   out elems 4 CHECKED-MUL 2 M-O-SPAN {: c2:n :}
   c2 0<> if c2 exit then
   fn elems M-GRID M-RESIDUAL-GO ;

: M-UNEMBED
   ( GPU:session GPU:buffer config n n n -- GPU:session GPU:buffer config n )
   {: x:n out:n fn:n :}
   NEMBD@ {: ne:n :}
   NVOCAB@ {: vo:n :}
   ne 3 M-U!  vo 4 M-U!
   x ne 4 CHECKED-MUL 0 M-O-SPAN {: c0:n :}
   c0 0<> if c0 exit then
   GPT2-GLOBAL--ROLE:WTE GPT2-TENSOR--ID:GLOBAL 1 M-W-SPAN {: c1:n :}
   c1 0<> if c1 exit then
   out vo 4 CHECKED-MUL 2 M-O-SPAN {: c2:n :}
   c2 0<> if c2 exit then
   fn vo M-GRID M-UNEMBED-GO ;

: M-ATTN-CHECK-BODY ( n n n n -- n n n n )
   {: pos:n heads:n hd:n cap:n :}
   pos heads hd cap
   pos heads hd cap GPT2-ATTN:LAUNCH-CHECK {: row:n cache:n shared:n :}
   row 0 M-U!  cache 1 M-U!  shared 2 M-U! ;

: M-ATTN-CHECK ( n n n n -- n )
   [: M-ATTN-CHECK-BODY ;] catch nip nip nip nip ;

: M-ATTN
   ( GPU:session GPU:buffer config n n n n n n n -- GPU:session GPU:buffer config n )
   {: b:n out:n kc:n vc:n pos:n layer:n fn:n :}
   NEMBD@ {: ne:n :}
   NHEAD@ {: heads:n :}
   NCTX@ {: cap:n :}
   ne heads / {: hd:n :}
   pos heads hd cap M-ATTN-CHECK {: check:n :}
   check 0<> if check exit then
   M-PARAM @ {: row:n :}
   M-PARAM 1 cells + @ {: cache:n :}
   M-PARAM 2 cells + @ {: shared:n :}
   layer cache CHECKED-MUL {: cache-off:n :}
   b row 0 M-O-SPAN {: c0:n :}
   c0 0<> if c0 exit then
   b row M-ADD row 1 M-O-SPAN {: c1:n :}
   c1 0<> if c1 exit then
   b row 2 CHECKED-MUL M-ADD row 2 M-O-SPAN {: c2:n :}
   c2 0<> if c2 exit then
   kc cache-off M-ADD cache 3 M-O-SPAN {: c3:n :}
   c3 0<> if c3 exit then
   vc cache-off M-ADD cache 4 M-O-SPAN {: c4:n :}
   c4 0<> if c4 exit then
   out row 5 M-O-SPAN {: c5:n :}
   c5 0<> if c5 exit then
   pos 6 M-U!  heads 7 M-U!  hd 8 M-U!  cap 9 M-U!
   fn heads shared M-ATTN-GO ;

: M-SYNC ( -- n )
   [: CUDA:CU-CTX-SYNCHRONIZE M-CUDA ;] catch ;

: M-LAYER
   ( GPU:session GPU:buffer config n n n n n n n n n n n n -- GPU:session GPU:buffer config n )
   {: x:n a:n b:n k:n v:n pos:n layer:n ln:n linear:n gelu:n residual:n attn:n :}
   NEMBD@ {: ne:n :}
   x a GPT2-LAYER--ROLE:LN1-G GPT2-LAYER--ROLE:LN1-B layer ln
   M-LN-LAYER {: c0:n :}
   c0 0<> if c0 exit then
   a b ne ne 3 CHECKED-MUL GPT2-LAYER--ROLE:QKV-W GPT2-LAYER--ROLE:QKV-B layer linear
   M-LINEAR {: c1:n :}
   c1 0<> if c1 exit then
   b a k v pos layer attn M-ATTN {: c2:n :}
   c2 0<> if c2 exit then
   a b ne ne GPT2-LAYER--ROLE:APROJ-W GPT2-LAYER--ROLE:APROJ-B layer linear
   M-LINEAR {: c3:n :}
   c3 0<> if c3 exit then
   x b x ne residual M-RESIDUAL {: c4:n :}
   c4 0<> if c4 exit then
   x a GPT2-LAYER--ROLE:LN2-G GPT2-LAYER--ROLE:LN2-B layer ln
   M-LN-LAYER {: c5:n :}
   c5 0<> if c5 exit then
   a b ne ne 4 CHECKED-MUL GPT2-LAYER--ROLE:FC-W GPT2-LAYER--ROLE:FC-B layer linear
   M-LINEAR {: c6:n :}
   c6 0<> if c6 exit then
   b ne 4 CHECKED-MUL gelu M-GELU {: c7:n :}
   c7 0<> if c7 exit then
   b a ne 4 CHECKED-MUL ne GPT2-LAYER--ROLE:MPROJ-W GPT2-LAYER--ROLE:MPROJ-B layer linear
   M-LINEAR {: c8:n :}
   c8 0<> if c8 exit then
   x a x ne residual M-RESIDUAL ;

: M-LAYERS
   ( GPU:session GPU:buffer config n n n n n n n n n n n n n -- GPU:session GPU:buffer config n )
   {: x:n a:n b:n k:n v:n pos:n ln:n linear:n gelu:n residual:n attn:n layer:n total:n :}
   layer total = if 0 exit then
   x a b k v pos layer ln linear gelu residual attn M-LAYER {: code:n :}
   code 0<> if code exit then
   x a b k v pos ln linear gelu residual attn layer 1+ total RECURSE ;

: M-VALIDATE
   ( config n n CAD-NUM:byte-len -- config n )
   {: token:n pos:n outu:CAD-NUM:byte-len :}
   NVOCAB@ {: vo:n :}
   NCTX@ {: cx:n :}
   token 0 < token vo >= or if E-TOKEN exit then
   pos 0 < pos cx >= or if E-CONTEXT exit then
   outu vo 4 CHECKED-MUL M-BYTE-LEN M-BYTES= 0= if E-OUTPUT exit then
   0 ;

: M-UPLOAD-TOKEN
   ( GPU:session GPU:buffer config n n -- GPU:session GPU:buffer config n )
   {: token:n off:n :}
   token 0 M-U!
   \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
   {: c :}
   off M-BYTE-OFF M-PARAM 4 M-BYTE-LEN GPU:UPLOAD M-RESULT-CODE {: code:n :}
   c code ;

: M-DOWNLOAD
   ( GPU:session GPU:buffer config n ptr u8 CAD-NUM:byte-len -- GPU:session GPU:buffer config n )
   {: off:n dst:ptr outu:CAD-NUM:byte-len :}
   \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
   {: c :}
   off M-BYTE-OFF dst outu GPU:DOWNLOAD M-RESULT-CODE {: code:n :}
   c code ;

: M-INFER
   ( GPU:session GPU:buffer config n n n n n n n n n n n n n n n n ptr u8 CAD-NUM:byte-len -- GPU:session GPU:buffer config n )
   {: x:n a:n b:n logits:n token:n k:n v:n pos:n embed:n ln:n linear:n unembed:n gelu:n residual:n attn:n tok:n dst:ptr outu:CAD-NUM:byte-len :}
   NLAYER@ {: nl:n :}
   tok token M-UPLOAD-TOKEN {: c0:n :}
   c0 0<> if c0 exit then
   token x pos embed M-EMBED {: c1:n :}
   c1 0<> if c1 exit then
   x a b k v pos ln linear gelu residual attn 0 nl M-LAYERS {: c2:n :}
   c2 0<> if c2 exit then
   x a ln M-LN-FINAL {: c3:n :}
   c3 0<> if c3 exit then
   a logits unembed M-UNEMBED {: c4:n :}
   c4 0<> if c4 exit then
   M-SYNC {: c5:n :}
   c5 0<> if c5 exit then
   logits dst outu M-DOWNLOAD ;

: M-MODEL-ERR ( n -- result<GPT2:model,n> )
   RESULT:ERR ;

: M-CODE-RESET ( -- )
   0 M-TMOD-H !
   0 M-AMOD-H !
   0 M-EMBED-H !
   0 M-LN-H !
   0 M-LINEAR-H !
   0 M-UNEMBED-H !
   0 M-GELU-H !
   0 M-RESIDUAL-H !
   0 M-ATTN-H ! ;

: M-PTX-PREP ( ptr u8 n -- )
   PTXTC:PREPARE
   ATGT:LABEL$ PTX-ARCH!
   ATGT:VER$ PTX-VER!
   ATGT:LABEL$ PTXTC:TC-ARCH! ;

: M-CAP-TENSOR ( -- )
   PTX-CAPTURE-ON
   [: GPT2-PTX:EMIT ;] catch {: code:n :}
   PTX-CAPTURE-OFF
   code 0<> if code throw then
   PTXTC:PTX$ PTX-CAPTURE$ WRITE-ALL ;

: M-CAP-ATTN ( -- )
   PTX-CAPTURE-ON
   [: GPT2-ATTN:EMIT ;] catch {: code:n :}
   PTX-CAPTURE-OFF
   code 0<> if code throw then
   PTXTC:PTX$ PTX-CAPTURE$ WRITE-ALL ;

: M-ASSEMBLE ( -- )
   M-ASM-OUT M-ASM-CAP >LEN M-ASM-ERR M-ASM-CAP >LEN PTXTC:ASSEMBLE
   PTXTC:ASM-REPORT dup 0<> if throw then
   drop ;

: M-MOD-LOAD ( ptr n -- ) {: out:ptr :}
   PTXTC:CUBIN$ M-CUBIN FFI:CSTR
   0 out !
   out M-CUBIN MKD:CUMODULELOAD RC>N dup 0<> if
      0 out !
      throw
   then
   drop
   out @ CUDA:HANDLE0 drop ;

: M-FN-LOAD ( n ptr n ptr u8 n -- )
   {: mod:n out:ptr name:ptr nameu:n :}
   name nameu M-KNAME FFI:CSTR
   0 out !
   out mod >CUDA-MOD M-KNAME MKD:CUMODULEGETFUNCTION RC>N dup 0<> if
      0 out !
      throw
   then
   drop
   out @ CUDA:HANDLE0 drop ;

: M-TENSOR-BUILD ( -- )
   s" habu-gpt2-tensor-model" M-PTX-PREP
   M-CAP-TENSOR
   M-ASSEMBLE
   M-TMOD-H M-MOD-LOAD
   M-TMOD-H @ M-EMBED-H s" GPT2_EMBED" M-FN-LOAD
   M-TMOD-H @ M-LN-H s" GPT2_LAYERNORM" M-FN-LOAD
   M-TMOD-H @ M-LINEAR-H s" GPT2_LINEAR" M-FN-LOAD
   M-TMOD-H @ M-UNEMBED-H s" GPT2_UNEMBED" M-FN-LOAD
   M-TMOD-H @ M-GELU-H s" GPT2_GELU" M-FN-LOAD
   M-TMOD-H @ M-RESIDUAL-H s" GPT2_RESIDUAL" M-FN-LOAD ;

: M-ATTN-BUILD ( -- )
   s" habu-gpt2-attention-model" M-PTX-PREP
   M-CAP-ATTN
   M-ASSEMBLE
   M-AMOD-H M-MOD-LOAD
   M-AMOD-H @ M-ATTN-H s" GPT2_ATTN" M-FN-LOAD ;

: M-TC-DONE ( n -- n )
   [: PTXTC:CLEAN ;] catch M-FIRST ;

: M-TENSOR-TRY ( -- n )
   [: M-TENSOR-BUILD ;] catch M-TC-DONE ;

: M-ATTN-TRY ( -- n )
   [: M-ATTN-BUILD ;] catch M-TC-DONE ;

: M-UNLOAD-BODY ( n n -- n n ) {: mod:n first:n :}
   mod
   first mod >CUDA-MOD CUDA:CU-MODULE-UNLOAD RC>N M-FIRST ;

: M-UNLOAD ( n n -- n )
   over 0= if nip exit then
   [: M-UNLOAD-BODY ;] catch M-FIRST nip ;

: M-H-UNLOAD ( ptr n n -- n ) {: hp:ptr first:n :}
   hp @ first M-UNLOAD {: code:n :}
   0 hp !
   code ;

: M-CODE-CLEAN ( n -- n )
   M-AMOD-H swap M-H-UNLOAD
   M-TMOD-H swap M-H-UNLOAD {: code:n :}
   0 M-EMBED-H !
   0 M-LN-H !
   0 M-LINEAR-H !
   0 M-UNEMBED-H !
   0 M-GELU-H !
   0 M-RESIDUAL-H !
   0 M-ATTN-H !
   code ;

: M-BUILD-CODE ( -- n )
   M-CODE-RESET
   M-TENSOR-TRY {: first:n :}
   first
   first 0= if M-ATTN-TRY M-FIRST then
   dup 0<> if M-CODE-CLEAN then ;

: M-CODE@ ( -- n n n n n n n n n )
   M-TMOD-H @
   M-AMOD-H @
   M-EMBED-H @
   M-LN-H @
   M-LINEAR-H @
   M-UNEMBED-H @
   M-GELU-H @
   M-RESIDUAL-H @
   M-ATTN-H @ ;

: M-UPLOAD-ONE
   ( GPU:session GPU:buffer SAFET:file config ptr u8 ptr u8 n -- GPU:session GPU:buffer SAFET:file config ptr u8 n )
   \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
   {: c scratch:ptr base:ptr slot:n :}
   c slot scratch M-FIND-SLOT
   \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
   {: c2 id:n find-code:n :}
   find-code 0<> if c2 scratch find-code exit then
   id SAFET:MAP-OFFSET? M-OPT {: src-off:n off-code:n :}
   off-code 0<> if c2 scratch off-code exit then
   c2 slot M-SLOT-INDEX TENSOR-ID-FOR-SLOT SPAN
   {: dst:CAD-NUM:byte-off len:CAD-NUM:byte-len :}
   drop
   >r
   dst base src-off M-BYTE-OFF CAD-NUM:BYTE+ len GPU:UPLOAD M-RESULT-CODE
   r> swap {: code:n :}
   c2 scratch code ;

: M-UPLOADS
   ( GPU:session GPU:buffer SAFET:file config ptr u8 ptr u8 n n -- GPU:session GPU:buffer SAFET:file config ptr u8 n )
   \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
   {: c scratch:ptr base:ptr slot:n total:n :}
   slot total = if c scratch 0 exit then
   c scratch base slot M-UPLOAD-ONE
   \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
   {: c2 scratch2:ptr code:n :}
   code 0<> if c2 scratch2 code exit then
   c2 scratch2 base slot 1+ total RECURSE ;

: M-UPLOAD-MAP
   ( GPU:session GPU:buffer SAFET:file config ptr u8 SAFET:mapping ptr u8 n -- GPU:session GPU:buffer SAFET:file config ptr u8 n SAFET:mapping )
   {: base:ptr len:n :}
   len drop
   >r
   \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
   {: c scratch:ptr :}
   c COUNT {: total:n :}
   drop
   c scratch base 0 total M-UPLOADS
   r> ;

: M-UPLOAD-FRAME
   ( GPU:session GPU:buffer SAFET:file config ptr u8 SAFET:mapping -- GPU:session GPU:buffer SAFET:file config ptr u8 n SAFET:mapping )
   [: M-UPLOAD-MAP ;] SAFET:WITH-MAPPING drop ;

: M-CATALOG-ORDER
   ( config ptr u8 SAFET:file SAFET:mapping -- config ptr u8 SAFET:file SAFET:mapping n )
   >r >r
   \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
   {: c scratch:ptr :}
   r> c scratch M-CATALOG
   \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
   {: c2 scratch2:ptr code:n :}
   >r c2 scratch2 r> r> code ;

: M-PIN-ORDER
   ( config ptr u8 SAFET:file SAFET:mapping n -- config ptr u8 SAFET:file SAFET:mapping n )
   {: prior:n :}
   prior 0<> if prior exit then
   >r >r
   \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
   {: c scratch:ptr :}
   r> scratch r> M-PIN-FRAME
   >r {: scratch2:ptr code:n :}
   >r c scratch2 r> r> code ;

: M-GPU-CLEAN ( GPU:session GPU:buffer n -- GPU:session n )
   {: first:n :}
   GPU:FREE M-RESULT-CODE {: free:n :}
   first free M-FIRST ;

: M-SOURCE-CLEAN ( config ptr u8 SAFET:file SAFET:mapping n -- n )
   {: first:n :}
   SAFET:UNMAP-MAPPING M-RESULT-CODE
   first swap M-FIRST {: first2:n :}
   SAFET:RELEASE
   drop M-DROP-CFG
   first2 ;

: M-ALL-CLEAN
   ( GPU:session GPU:buffer SAFET:file config ptr u8 SAFET:mapping n -- GPU:session n )
   {: first:n :}
   SAFET:UNMAP-MAPPING M-RESULT-CODE
   first swap M-FIRST {: first2:n :}
   drop M-DROP-CFG
   SAFET:RELEASE
   first2 M-GPU-CLEAN ;

: M-FINISH
   ( GPU:session GPU:buffer SAFET:file config ptr u8 SAFET:mapping ptr n CAD-NUM:alloc-byte-len -- GPU:session result<GPT2:model,n> )
   {: tokstate:ptr toklen:CAD-NUM:alloc-byte-len :}
   SAFET:UNMAP-MAPPING M-RESULT-CODE {: code:n :}
   drop
   \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
   {: c :}
   SAFET:RELEASE
   code 0<> if
      c M-DROP-CFG
      code M-GPU-CLEAN
      tokstate drop toklen drop
      {: fail:n :}
      fail M-MODEL-ERR
      exit
   then
   M-BUILD-CODE {: build:n :}
   build 0<> if
      c M-DROP-CFG
      build M-GPU-CLEAN
      tokstate drop toklen drop
      {: fail:n :}
      fail M-MODEL-ERR
      exit
   then
   M-REC-LEN MEM:OPEN
   MATCH result
      err OF
         {: code:n :}
         code M-CODE-CLEAN {: first:n :}
         c M-DROP-CFG
         first M-GPU-CLEAN
         tokstate drop toklen drop
         {: fail:n :}
         fail M-MODEL-ERR
      ENDOF
      ok OF
         swap
         \ M-FROM-CFG proved this pure replay on unchanged config before any owner mint.
         c M-LAYOUT-BODY
         {: x:n a:n b:n logits:n token:n k:n v:n alloc:CAD-NUM:alloc-byte-len :}
         alloc drop
         x a b logits token k v 0
         M-CODE@
         tokstate toklen M-SAVE
         M-CODE-RESET
         RESULT:OK
      ENDOF
   ;MATCH ;

: M-GPU-FAIL
   ( GPU:session GPU:buffer SAFET:file config ptr u8 SAFET:mapping ptr n CAD-NUM:alloc-byte-len n -- GPU:session result<GPT2:model,n> )
   {: tokstate:ptr toklen:CAD-NUM:alloc-byte-len code:n :}
   code M-ALL-CLEAN
   {: fail:n :}
   tokstate drop toklen drop
   fail M-MODEL-ERR ;

: M-GPU-FLOW
   ( GPU:session GPU:buffer SAFET:file config ptr u8 SAFET:mapping ptr n CAD-NUM:alloc-byte-len -- GPU:session result<GPT2:model,n> )
   >r >r
   M-UPLOAD-FRAME
   >r {: code:n :}
   code 0<> if
      r> r> r> code M-GPU-FAIL
   else
      r> r> r> M-FINISH
   then ;

: M-GPU
   ( GPU:session config ptr u8 SAFET:file SAFET:mapping CAD-NUM:alloc-byte-len ptr n CAD-NUM:alloc-byte-len -- GPU:session result<GPT2:model,n> )
   {: alloc:CAD-NUM:alloc-byte-len tokstate:ptr toklen:CAD-NUM:alloc-byte-len :}
   >r >r
   \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
   {: c scratch:ptr :}
   alloc GPU:ALLOC
   MATCH result
      err OF
         {: code:n :}
         c scratch r> r> code M-SOURCE-CLEAN
         tokstate drop toklen drop
         {: fail:n :}
         fail M-MODEL-ERR
      ENDOF
      ok OF
         r> r> >r c scratch r>
         tokstate toklen M-GPU-FLOW
      ENDOF
   ;MATCH ;

: M-FILE
   ( GPU:session config ptr u8 SAFET:file CAD-NUM:alloc-byte-len ptr n CAD-NUM:alloc-byte-len -- GPU:session result<GPT2:model,n> )
   {: alloc:CAD-NUM:alloc-byte-len tokstate:ptr toklen:CAD-NUM:alloc-byte-len :}
   SAFET:DETACH-MAPPING
   MATCH SAFET:map-take
      empty OF
         SAFET:RELEASE
         drop M-DROP-CFG
         tokstate drop toklen drop
         E-CATALOG M-MODEL-ERR
      ENDOF
      moved OF
         \ Parse rejects hostile structure before the digest gates GPU work.
         M-CATALOG-ORDER M-PIN-ORDER {: code:n :}
         code 0<> if
            code M-SOURCE-CLEAN
            tokstate drop toklen drop
            {: fail:n :}
            fail M-MODEL-ERR
         else
            alloc tokstate toklen M-GPU
         then
      ENDOF
   ;MATCH ;

: M-JOIN-LEN ( ptr u8 n n -- n ) {: root:ptr rootu:n nameu:n :}
   rootu 0 > root rootu 1- + c@ FS-SLASH = and if
      rootu nameu M-ADD
   else
      rootu 1 M-ADD nameu M-ADD
   then ;

: M-BODY
   ( GPU:session ptr u8 n config ptr u8 CAD-NUM:alloc-byte-len ptr n CAD-NUM:alloc-byte-len CAD-NUM:alloc-byte-len -- GPU:session result<GPT2:model,n> )
   {: root:ptr rootu:n c scratch:ptr scratchu:CAD-NUM:alloc-byte-len tokstate:ptr toklen:CAD-NUM:alloc-byte-len alloc:CAD-NUM:alloc-byte-len :} \ typed-local-lint: allow-bare-local - config is a concrete multi-cell structure.
   scratchu drop
   root rootu GPT2PIN:MODEL-NAME$ nip M-JOIN-LEN
   FS-PATH-CAP > if
      c M-DROP-CFG
      tokstate drop toklen drop
      E-FS-CAPACITY M-MODEL-ERR
      exit
   then
   c scratch
   root rootu GPT2PIN:MODEL-NAME$ scratch JOIN-PATH {: pathu:n :}
   scratch pathu SAFET:LOAD
   MATCH result
      err OF
         {: code:n :}
         drop M-DROP-CFG
         tokstate drop toklen drop
         code M-MODEL-ERR
      ENDOF
      ok OF
         alloc tokstate toklen M-FILE
      ENDOF
   ;MATCH ;

: M-ALLOC-TOK ( ptr n CAD-NUM:alloc-byte-len -- ptr n CAD-NUM:alloc-byte-len )
   2drop T-NEW ;

: M-BUILD-TOK ( ptr n CAD-NUM:alloc-byte-len ptr u8 n -- ptr n CAD-NUM:alloc-byte-len ptr u8 n )
   {: tokstate:ptr toklen:CAD-NUM:alloc-byte-len root:ptr rootu:n :}
   tokstate root rootu T-BUILD
   tokstate toklen root rootu ;

: M-OPEN-TOK
   ( GPU:session ptr u8 n config CAD-NUM:alloc-byte-len ptr u8 CAD-NUM:alloc-byte-len -- GPU:session result<GPT2:model,n> )
   \ typed-local-lint: allow-bare-local - config is a concrete multi-cell structure.
   {: root:ptr rootu:n c alloc:CAD-NUM:alloc-byte-len scratch:ptr scratchu:CAD-NUM:alloc-byte-len :}
   M-PARAM 1 BYTES-ALLOC-LEN [: M-ALLOC-TOK ;] catch {: alloc-code:n :}
   alloc-code 0<> if
      2drop c M-DROP-CFG
      alloc-code M-MODEL-ERR
      exit
   then
   {: tokstate:ptr toklen:CAD-NUM:alloc-byte-len :}
   tokstate toklen root rootu [: M-BUILD-TOK ;] catch {: build-code:n :}
   2drop 2drop
   build-code 0<> if
      tokstate toklen T-FREE
      c M-DROP-CFG
      build-code M-MODEL-ERR
      exit
   then
   root rootu c scratch scratchu tokstate toklen alloc M-BODY
   MATCH result
      err OF tokstate toklen T-FREE M-MODEL-ERR ENDOF
      ok OF RESULT:OK ENDOF
   ;MATCH ;

: M-FROM-CFG
   ( GPU:session ptr u8 n config -- GPU:session result<GPT2:model,n> )
   M-LAYOUT
   MATCH result
      err OF {: code:n :} M-DROP-CFG 2drop code M-MODEL-ERR ENDOF
      ok OF
         FS-PATH-CAP MEM:BYTES-ALLOC-LEN MEM:OPEN
         MATCH result
            err OF {: code:n :} drop M-DROP-CFG 2drop code M-MODEL-ERR ENDOF
            ok OF [: M-OPEN-TOK ;] MEM:WITH-BLOCK ENDOF
         ;MATCH
      ENDOF
   ;MATCH ;

;using

public

: OPEN ( GPU:session FS:path -- GPU:session result<GPT2:model,n> )
   FS-PATH:UNMAKE {: root:ptr rootu:n :}
   root rootu FS-PATH:MAKE HF:OPEN-GPT2
   MATCH result
      err OF M-MODEL-ERR ENDOF
      ok OF
         \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
         {: c :}
         root rootu c M-FROM-CFG
      ENDOF
   ;MATCH ;

: CONTEXT-LEN ( GPT2:model -- GPT2:model CAD-NUM:item-count )
   M-TAKE
   {: x:n a:n b:n logits:n token:n k:n v:n pos:n tmod:n amod:n embed:n ln:n linear:n unembed:n gelu:n residual:n attn:n tokstate:ptr toklen:CAD-NUM:alloc-byte-len :}
   NCTX@ {: cx:n :}
   x a b logits token k v pos
   tmod amod embed ln linear unembed gelu residual attn tokstate toklen M-SAVE
   cx M-ITEM-COUNT ;

: EOS-ID ( GPT2:model -- GPT2:model n )
   M-TAKE
   {: x:n a:n b:n logits:n token:n k:n v:n pos:n tmod:n amod:n embed:n ln:n linear:n unembed:n gelu:n residual:n attn:n tokstate:ptr toklen:CAD-NUM:alloc-byte-len :}
   EOS@ {: eos:n :}
   x a b logits token k v pos
   tmod amod embed ln linear unembed gelu residual attn tokstate toklen M-SAVE
   eos ;

: CONFIG@ ( GPT2:model -- GPT2:model config )
   M-TAKE
   {: x:n a:n b:n logits:n token:n k:n v:n pos:n tmod:n amod:n embed:n ln:n linear:n unembed:n gelu:n residual:n attn:n tokstate:ptr toklen:CAD-NUM:alloc-byte-len :}
   \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
   {: c :}
   c x a b logits token k v pos
   tmod amod embed ln linear unembed gelu residual attn tokstate toklen M-SAVE
   c ;

: LOGITS
   ( GPU:session GPT2:model n ptr u8 CAD-NUM:byte-len -- GPU:session GPT2:model result<n,n> )
   {: tok:n dst:ptr outu:CAD-NUM:byte-len :}
   M-TAKE
   {: x:n a:n b:n logits:n token:n k:n v:n pos:n tmod:n amod:n embed:n ln:n linear:n unembed:n gelu:n residual:n attn:n tokstate:ptr toklen:CAD-NUM:alloc-byte-len :}
   tok pos outu M-VALIDATE {: valid:n :}
   \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
   {: c :}
   rot swap
   c
   valid 0= if
      x a b logits token k v pos embed ln linear unembed gelu residual attn
      tok dst outu M-INFER
   else
      valid
   then
   {: code:n :}
   \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
   {: c2 :}
   rot swap
   c2
   x a b logits token k v
   code 0= if pos 1+ else pos then
   tmod amod embed ln linear unembed gelu residual attn tokstate toklen M-SAVE
   code 0= if 0 RESULT:OK else code RESULT:ERR then ;

: RESET ( GPT2:model -- GPT2:model )
   M-TAKE
   {: x:n a:n b:n logits:n token:n k:n v:n pos:n tmod:n amod:n embed:n ln:n linear:n unembed:n gelu:n residual:n attn:n tokstate:ptr toklen:CAD-NUM:alloc-byte-len :}
   pos drop
   x a b logits token k v 0
   tmod amod embed ln linear unembed gelu residual attn tokstate toklen M-SAVE ;

: CLOSE ( GPU:session GPT2:model -- GPU:session result<n,n> )
   M-TAKE {: tokstate:ptr toklen:CAD-NUM:alloc-byte-len :}
   2drop 2drop 2drop drop
   {: tmod:n amod:n :}
   2drop 2drop 2drop 2drop
   M-DROP-CFG
   rot swap
   0 M-BYTE-OFF 4 M-BYTE-LEN GPU:SPAN
   MATCH result
      ok OF drop 0 ENDOF
      err OF ENDOF
   ;MATCH
   {: bind:n :}
   amod bind M-UNLOAD
   tmod swap M-UNLOAD
   M-GPU-CLEAN {: code:n :}
   tokstate toklen T-FREE
   swap MEM:RELEASE
   code 0= if 0 RESULT:OK else code RESULT:ERR then ;

;package
