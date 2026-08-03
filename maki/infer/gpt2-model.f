\ gpt2-model.f - validated GPT-2 weights in one GPU allocation.

require lib/prelude.f
require lib/adt/result.f
require lib/cad-num-arithmetic.f
require lib/fs.f
require lib/fs-path.f
require lib/memory.f
require lib/string.f
require maki/gpu-buffer.f
require maki/infer/gpt2-pin.f
require maki/infer/hf-config.f
require maki/infer/safetensors.f
require maki/infer/gpt2-tensor.f

package GPT2

public

STRUCTURE model 0
   FIELD gpu GPU:session
   FIELD buf GPU:buffer
   FIELD cfg config
;STRUCTURE

private

-5651 constant E-CATALOG
-5654 constant E-DIGEST

32 constant M-DIGEST-LEN
64 constant M-HEX-LEN
64 constant M-NAME-CAP

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

: M-ALLOC-LEN ( config -- config CAD-NUM:alloc-byte-len )
   NLAYER@ {: nl:n :}
   M-GLOBAL-ELEMS {: globals:n :}
   M-LAYER-ELEMS {: layer:n :}
   DATATYPE@ M-DTYPE-BYTES {: bytes:n :}
   globals layer nl CHECKED-MUL M-ADD bytes CHECKED-MUL
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

\ M-ALLOC-LEN runs before SAFET:LOAD and proves the pinned geometry products;
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
   ( GPU:session GPU:buffer SAFET:file config ptr u8 SAFET:mapping -- result<model,n> )
   SAFET:UNMAP-MAPPING M-RESULT-CODE {: code:n :}
   drop
   \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
   {: c :}
   SAFET:RELEASE
   code 0<> if
      c M-DROP-CFG
      code M-GPU-CLEAN RESULT:ERR
      exit
   then
   c GPT2-MODEL:MAKE RESULT:OK ;

: M-GPU-FLOW
   ( GPU:session GPU:buffer SAFET:file config ptr u8 SAFET:mapping -- result<model,n> )
   M-UPLOAD-FRAME
   >r {: code:n :}
   code 0<> if
      r> code M-ALL-CLEAN RESULT:ERR
   else
      r> M-FINISH
   then ;

: M-GPU
   ( config ptr u8 SAFET:file SAFET:mapping CAD-NUM:alloc-byte-len -- result<model,n> )
   {: alloc:CAD-NUM:alloc-byte-len :}
   >r >r
   \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
   {: c scratch:ptr :}
   GPU:OPEN
   MATCH result
      err OF
         {: code:n :}
         c scratch r> r> code M-SOURCE-CLEAN RESULT:ERR
      ENDOF
      ok OF
         alloc GPU:ALLOC
         MATCH result
            err OF
               {: code:n :}
               code M-SESSION-CLEAN {: first:n :}
               c scratch r> r> first M-SOURCE-CLEAN RESULT:ERR
            ENDOF
            ok OF
               r> r> >r c scratch r>
               M-GPU-FLOW
            ENDOF
         ;MATCH
      ENDOF
   ;MATCH ;

: M-FILE
   ( config ptr u8 SAFET:file CAD-NUM:alloc-byte-len -- result<model,n> )
   {: alloc:CAD-NUM:alloc-byte-len :}
   SAFET:DETACH-MAPPING
   MATCH SAFET:map-take
      empty OF
         SAFET:RELEASE
         drop M-DROP-CFG
         E-CATALOG RESULT:ERR
      ENDOF
      moved OF
         M-CATALOG-ORDER M-PIN-ORDER {: code:n :}
         code 0<> if
            code M-SOURCE-CLEAN RESULT:ERR
         else
            alloc M-GPU
         then
      ENDOF
   ;MATCH ;

: M-BODY
   ( ptr u8 n config CAD-NUM:alloc-byte-len ptr u8 CAD-NUM:alloc-byte-len -- result<model,n> )
   \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
   {: root:ptr rootu:n c alloc:CAD-NUM:alloc-byte-len scratch:ptr scratchu:CAD-NUM:alloc-byte-len :}
   scratchu drop
   c scratch
   root rootu GPT2PIN:MODEL-NAME$ scratch JOIN-PATH {: pathu:n :}
   scratch pathu SAFET:LOAD
   MATCH result
      err OF
         {: code:n :}
         drop M-DROP-CFG
         code RESULT:ERR
      ENDOF
      ok OF
         alloc M-FILE
      ENDOF
   ;MATCH ;

: M-JOIN-LEN ( ptr u8 n n -- n ) {: root:ptr rootu:n nameu:n :}
   rootu 0 > root rootu 1- + c@ FS-SLASH = and if
      rootu nameu M-ADD
   else
      rootu 1 M-ADD nameu M-ADD
   then ;

: M-SCOPE-FINISH
   ( ptr u8 CAD-NUM:alloc-byte-len result<model,n> -- result<model,n> )
   MATCH result
      err OF
         {: code:n :}
         MEM:RELEASE-BYTES
         code RESULT:ERR
      ENDOF
      ok OF
         GPT2-MODEL:UNMAKE
         \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
         {: c :}
         >r >r
         MEM:RELEASE-BYTES
         r> r> c GPT2-MODEL:MAKE RESULT:OK
      ENDOF
   ;MATCH ;

: M-OPEN-CFG ( ptr u8 n config -- result<model,n> )
   \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
   {: root:ptr rootu:n c :}
   root rootu GPT2PIN:MODEL-NAME$ nip M-JOIN-LEN
   FS-PATH-CAP > if
      c M-DROP-CFG
      E-FS-CAPACITY RESULT:ERR
      exit
   then
   c M-ALLOC-LEN {: alloc:CAD-NUM:alloc-byte-len :}
   drop
   FS-PATH-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES
   {: scratch:ptr scratchu:CAD-NUM:alloc-byte-len :}
   scratch scratchu
   root rootu c alloc scratch scratchu M-BODY
   M-SCOPE-FINISH ;

public

: OPEN ( FS:path -- result<model,n> )
   FS-PATH:UNMAKE {: root:ptr rootu:n :}
   root rootu FS-PATH:MAKE HF:OPEN-GPT2
   MATCH result
      err OF RESULT:ERR ENDOF
      ok OF
         \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
         {: c :}
         root rootu c M-OPEN-CFG
      ENDOF
   ;MATCH ;

: CLOSE ( model -- result<n,n> )
   GPT2-MODEL:UNMAKE
   M-DROP-CFG
   0 M-GPU-CLEAN {: code:n :}
   code 0= if 0 RESULT:OK else code RESULT:ERR then ;

;package
