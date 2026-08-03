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
require maki/infer/hf-config.f
require maki/infer/safetensors.f
require maki/infer/gpt2-tensor.f
require maki/infer/gpt2-tensor-cg.f
require maki/infer/gpt2-attention-cg.f

package GPT2

public

DEFLINEAR GPT2:model

private

-5651 constant E-CATALOG
-5654 constant E-DIGEST

32 constant M-DIGEST-LEN
64 constant M-HEX-LEN
64 constant M-NAME-CAP
$1000 constant M-ASM-CAP
32 constant M-KNAME-CAP

create M-ASM-OUT M-ASM-CAP allot
create M-ASM-ERR M-ASM-CAP allot
create M-CUBIN FS-PATH-CAP 1+ allot
create M-KNAME M-KNAME-CAP allot

variable M-TMOD-H
variable M-AMOD-H
variable M-EMBED-H
variable M-LN-H
variable M-LINEAR-H
variable M-UNEMBED-H
variable M-GELU-H
variable M-RESIDUAL-H
variable M-ATTN-H

0 constant R-GPU
1 cells constant R-BUF
2 cells constant R-DT
3 cells constant R-CX
4 cells constant R-VO
5 cells constant R-NL
6 cells constant R-NE
7 cells constant R-NH
8 cells constant R-TIED
9 cells constant R-BOS
10 cells constant R-EOS
11 cells constant R-EPS
12 cells constant R-SCALE
13 cells constant R-PROOF
14 cells constant R-X
15 cells constant R-A
16 cells constant R-B
17 cells constant R-LOGITS
18 cells constant R-TOKEN
19 cells constant R-K
20 cells constant R-V
21 cells constant R-POS
22 cells constant R-TMOD
23 cells constant R-AMOD
24 cells constant R-EMBED
25 cells constant R-LN
26 cells constant R-LINEAR
27 cells constant R-UNEMBED
28 cells constant R-GELU
29 cells constant R-RESIDUAL
30 cells constant R-ATTN
31 cells constant R-BYTES

\ One GPU:buffer is laid out as weights | x | a | b | logits | token | K | V;
\ the private offsets below are the sole authority for that allocation.
\ Cubins are transient build provenance: only two modules and seven functions
\ persist, and CLOSE unloads attention then tensor before buffer and session.
\ GPT2:model is the second production CUDA consumer after GPU and adds no runtime.
\ The checker cannot tie raw CUDA module/function cells to the model lifetime;
\ they stay private and are valid only while their owning module is loaded.
\ Retirement owner: habu-checker-ptr-lifetime-f59d1e9d.

: M-REC-LEN ( -- CAD-NUM:alloc-byte-len )
   R-BYTES MEM:BYTES-ALLOC-LEN ;

: M-ALLOC-REC ( ptr u8 -- ptr u8 )
   drop M-REC-LEN MEM:ALLOC-BYTES drop ;

: M-ALLOC-PATH ( ptr u8 -- ptr u8 )
   drop FS-PATH-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop ;

: M-FREE-REC ( ptr u8 -- )
   M-REC-LEN MEM:RELEASE-BYTES ;

\ The checker cannot tie the opaque owner to its private host record.
\ Retirement owner: habu-checker-ptr-lifetime-f59d1e9d.
TRUSTED: M-SAVE
   ( GPU:session GPU:buffer config n n n n n n n n n n n n n n n n n ptr u8 -- GPT2:model )
   {: g:GPU:session buf:GPU:buffer dt:MAKI:datatype cx:n vo:n nl:n ne:n nh:n tied:bool bos:n eos:n eps:r scale:bool proof:cfg-proof x:n a:n b:n logits:n token:n k:n v:n pos:n tmod:n amod:n embed:n ln:n linear:n unembed:n gelu:n residual:n attn:n rec:ptr :}
   g rec R-GPU + !
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
   rec ;

TRUSTED: M-TAKE
   ( GPT2:model -- GPU:session GPU:buffer config n n n n n n n n n n n n n n n n n ptr u8 )
   {: rec:ptr :}
   rec R-GPU + @
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
   rec ;

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

: M-LAYOUT
   ( config -- config n n n n n n n CAD-NUM:alloc-byte-len )
   M-WEIGHT-BYTES {: x:n :}
   NEMBD@ 4 CHECKED-MUL {: row:n :}
   NVOCAB@ 4 CHECKED-MUL {: logits-len:n :}
   NCTX@ {: cx:n :}
   NLAYER@ {: nl:n :}
   cx row CHECKED-MUL nl CHECKED-MUL {: kv-len:n :}
   x row M-ADD {: a:n :}
   a row M-ADD {: b:n :}
   b row M-ADD {: logits:n :}
   logits logits-len M-ADD {: token:n :}
   token 4 M-ADD {: k:n :}
   k kv-len M-ADD {: v:n :}
   x a b logits token k v
   v kv-len M-ADD M-ALLOC-N ;

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

: M-RESULT-CODE ( result<n,n> -- n )
   MATCH result
      ok OF drop 0 ENDOF
      err OF ENDOF
   ;MATCH ;

: M-FIRST ( n n -- n ) {: first:n code:n :}
   first 0= code 0<> and if code else first then ;

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

: M-MOD-LOAD ( ptr a -- ) {: out:ptr :}
   PTXTC:CUBIN$ M-CUBIN FFI:CSTR
   0 out !
   out M-CUBIN MKD:CUMODULELOAD RC>N dup 0<> if
      0 out !
      throw
   then
   drop
   out @ CUDA:HANDLE0 drop ;

: M-FN-LOAD ( n ptr a ptr u8 n -- )
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

: M-H-UNLOAD ( ptr a n -- n ) {: hp:ptr first:n :}
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

: M-DROP-CFG ( config -- )
   GPT2-CONFIG:UNMAKE
   2drop 2drop 2drop 2drop 2drop 2drop ;

: M-GPU-CLEAN ( GPU:session GPU:buffer n -- n )
   {: first:n :}
   GPU:FREE M-RESULT-CODE {: free:n :}
   first free M-FIRST {: first2:n :}
   GPU:CLOSE M-RESULT-CODE
   first2 swap M-FIRST ;

: M-SESSION-CLEAN ( GPU:session n -- n )
   {: first:n :}
   GPU:CLOSE M-RESULT-CODE
   first swap M-FIRST ;

: M-SOURCE-CLEAN ( config ptr u8 SAFET:file SAFET:mapping n -- n )
   {: first:n :}
   SAFET:UNMAP-MAPPING M-RESULT-CODE
   first swap M-FIRST {: first2:n :}
   SAFET:RELEASE
   drop M-DROP-CFG
   first2 ;

: M-ALL-CLEAN
   ( GPU:session GPU:buffer SAFET:file config ptr u8 SAFET:mapping n -- n )
   {: first:n :}
   SAFET:UNMAP-MAPPING M-RESULT-CODE
   first swap M-FIRST {: first2:n :}
   drop M-DROP-CFG
   SAFET:RELEASE
   first2 M-GPU-CLEAN ;

: M-FINISH
   ( GPU:session GPU:buffer SAFET:file config ptr u8 SAFET:mapping ptr u8 -- result<GPT2:model,n> )
   {: rec:ptr :}
   SAFET:UNMAP-MAPPING M-RESULT-CODE {: code:n :}
   drop
   \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
   {: c :}
   SAFET:RELEASE
   code 0<> if
      c M-DROP-CFG
      code M-GPU-CLEAN
      rec drop M-MODEL-ERR
      exit
   then
   M-BUILD-CODE {: build:n :}
   build 0<> if
      c M-DROP-CFG
      build M-GPU-CLEAN
      rec drop M-MODEL-ERR
      exit
   then
   c M-LAYOUT
   {: x:n a:n b:n logits:n token:n k:n v:n alloc:CAD-NUM:alloc-byte-len :}
   alloc drop
   x a b logits token k v 0
   M-CODE@
   rec M-SAVE
   M-CODE-RESET
   RESULT:OK ;

: M-GPU-FAIL
   ( GPU:session GPU:buffer SAFET:file config ptr u8 SAFET:mapping ptr u8 n -- result<GPT2:model,n> )
   {: code:n :}
   drop
   code M-ALL-CLEAN M-MODEL-ERR ;

: M-GPU-FLOW
   ( GPU:session GPU:buffer SAFET:file config ptr u8 SAFET:mapping ptr u8 -- result<GPT2:model,n> )
   >r
   M-UPLOAD-FRAME
   >r {: code:n :}
   code 0<> if
      r> r> code M-GPU-FAIL
   else
      r> r> M-FINISH
   then ;

: M-GPU
   ( config ptr u8 SAFET:file SAFET:mapping CAD-NUM:alloc-byte-len ptr u8 -- result<GPT2:model,n> )
   {: alloc:CAD-NUM:alloc-byte-len rec:ptr :}
   >r >r
   \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
   {: c scratch:ptr :}
   GPU:OPEN
   MATCH result
      err OF
         {: code:n :}
         c scratch r> r> code M-SOURCE-CLEAN
         rec drop M-MODEL-ERR
      ENDOF
      ok OF
         alloc GPU:ALLOC
         MATCH result
            err OF
               {: code:n :}
               code M-SESSION-CLEAN {: first:n :}
               c scratch r> r> first M-SOURCE-CLEAN
               rec drop M-MODEL-ERR
            ENDOF
            ok OF
               r> r> >r c scratch r>
               rec M-GPU-FLOW
            ENDOF
         ;MATCH
      ENDOF
   ;MATCH ;

: M-FILE
   ( config ptr u8 SAFET:file CAD-NUM:alloc-byte-len ptr u8 -- result<GPT2:model,n> )
   {: alloc:CAD-NUM:alloc-byte-len rec:ptr :}
   SAFET:DETACH-MAPPING
   MATCH SAFET:map-take
      empty OF
         SAFET:RELEASE
         drop M-DROP-CFG
         rec drop E-CATALOG M-MODEL-ERR
      ENDOF
      moved OF
         \ Parse rejects hostile structure before the digest gates GPU work.
         M-CATALOG-ORDER M-PIN-ORDER {: code:n :}
         code 0<> if
            code M-SOURCE-CLEAN
            rec drop M-MODEL-ERR
         else
            alloc rec M-GPU
         then
      ENDOF
   ;MATCH ;

: M-BODY
   ( ptr u8 n config CAD-NUM:alloc-byte-len ptr u8 CAD-NUM:alloc-byte-len ptr u8 -- result<GPT2:model,n> )
   {: root:ptr rootu:n c alloc:CAD-NUM:alloc-byte-len scratch:ptr scratchu:CAD-NUM:alloc-byte-len rec:ptr :} \ typed-local-lint: allow-bare-local
   scratchu drop
   c scratch
   root rootu GPT2PIN:MODEL-NAME$ scratch JOIN-PATH {: pathu:n :}
   scratch pathu SAFET:LOAD
   MATCH result
      err OF
         {: code:n :}
         drop M-DROP-CFG
         rec drop code M-MODEL-ERR
      ENDOF
      ok OF
         alloc rec M-FILE
      ENDOF
   ;MATCH ;

: M-JOIN-LEN ( ptr u8 n n -- n ) {: root:ptr rootu:n nameu:n :}
   rootu 0 > root rootu 1- + c@ FS-SLASH = and if
      rootu nameu M-ADD
   else
      rootu 1 M-ADD nameu M-ADD
   then ;

: M-SCOPE-FINISH
   ( ptr u8 ptr u8 CAD-NUM:alloc-byte-len result<GPT2:model,n> -- result<GPT2:model,n> )
   MATCH result
      err OF
         {: code:n :}
         MEM:RELEASE-BYTES
         M-FREE-REC
         code M-MODEL-ERR
      ENDOF
      ok OF
         >r
         MEM:RELEASE-BYTES
         drop
         r> RESULT:OK
      ENDOF
   ;MATCH ;

: M-OPEN-CFG ( ptr u8 n config -- result<GPT2:model,n> )
   \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
   {: root:ptr rootu:n c :}
   root rootu GPT2PIN:MODEL-NAME$ nip M-JOIN-LEN
   FS-PATH-CAP > if
      c M-DROP-CFG
      E-FS-CAPACITY M-MODEL-ERR
      exit
   then
   c M-LAYOUT >r
   2drop 2drop 2drop drop
   M-DROP-CFG
   r> {: alloc:CAD-NUM:alloc-byte-len :}
   NULL$ drop [: M-ALLOC-REC ;] catch {: rec-code:n :}
   rec-code 0<> if
      drop c M-DROP-CFG
      rec-code M-MODEL-ERR
      exit
   then
   {: rec:ptr :}
   NULL$ drop [: M-ALLOC-PATH ;] catch {: path-code:n :}
   path-code 0<> if
      drop rec M-FREE-REC
      c M-DROP-CFG
      path-code M-MODEL-ERR
      exit
   then
   {: scratch:ptr :}
   rec scratch FS-PATH-CAP MEM:BYTES-ALLOC-LEN
   root rootu c alloc scratch FS-PATH-CAP MEM:BYTES-ALLOC-LEN rec M-BODY
   M-SCOPE-FINISH ;

public

: OPEN ( FS:path -- result<GPT2:model,n> )
   FS-PATH:UNMAKE {: root:ptr rootu:n :}
   root rootu FS-PATH:MAKE HF:OPEN-GPT2
   MATCH result
      err OF M-MODEL-ERR ENDOF
      ok OF
         \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
         {: c :}
         root rootu c M-OPEN-CFG
      ENDOF
   ;MATCH ;

: CLOSE ( GPT2:model -- result<n,n> )
   M-TAKE {: rec:ptr :}
   2drop 2drop 2drop drop
   {: tmod:n amod:n :}
   2drop 2drop 2drop 2drop
   M-DROP-CFG
   0 M-BYTE-OFF 4 M-BYTE-LEN GPU:SPAN
   MATCH result
      ok OF drop 0 ENDOF
      err OF ENDOF
   ;MATCH
   {: bind:n :}
   amod bind M-UNLOAD
   tmod swap M-UNLOAD
   M-GPU-CLEAN {: code:n :}
   rec M-FREE-REC
   code 0= if 0 RESULT:OK else code RESULT:ERR then ;

;package
