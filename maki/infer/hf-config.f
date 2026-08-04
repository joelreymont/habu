\ hf-config.f - authenticated pinned Hugging Face config reader.

require lib/prelude.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-path.f
require lib/json-read.f
require lib/adt/result.f
require maki/infer/gpt2-pin.f
require maki/infer/gpt2-config.f

package HF

private

using MEM
using JR
using GPT2PIN

-5660 constant E-ROOT
-5661 constant E-SIZE
-5662 constant E-FIELD
-5663 constant E-DIGEST

32 constant DIGEST-LEN
64 constant HEX-LEN
DIGEST-LEN HEX-LEN + constant DIGEST-BUF-LEN

1    constant S-MODEL
2    constant S-ARCH
4    constant S-ACT
8    constant S-CTX
16   constant S-POS
32   constant S-EMBD
64   constant S-HEAD
128  constant S-LAYER
256  constant S-VOCAB
512  constant S-BOS
1024 constant S-EOS
2048 constant S-EPS
4095 constant S-ALL

0  constant K-MODEL
1  constant K-ARCH
2  constant K-ACT
3  constant K-CTX
4  constant K-POS
5  constant K-EMBD
6  constant K-HEAD
7  constant K-LAYER
8  constant K-VOCAB
9  constant K-BOS
10 constant K-EOS
11 constant K-EPS
12 constant K-OTHER

: MARK ( n n -- n ) {: seen:n bit:n :}
   seen bit and 0<> if E-FIELD throw then
   seen bit or ;

: EMPTY-FIELDS ( -- n n n n n n n n r )
   0 0 0 0 0 0 0 0 0.0 ;

: MARK-ONLY ( n n n n n n n n r n -- n n n n n n n n r )
   {: seen:n cx:n vo:n nl:n ne:n nh:n bos:n eos:n eps:r bit:n :}
   seen bit MARK cx vo nl ne nh bos eos eps ;

: SET-CTX ( n n n n n n n n r n -- n n n n n n n n r )
   {: seen:n cx:n vo:n nl:n ne:n nh:n bos:n eos:n eps:r v:n :}
   seen S-CTX and 0<> if E-FIELD throw then
   seen S-POS and 0<> if
      v cx <> if E-FIELD throw then
      seen S-CTX or cx vo nl ne nh bos eos eps
   else
      seen S-CTX or v vo nl ne nh bos eos eps
   then ;

: SET-POS ( n n n n n n n n r n -- n n n n n n n n r )
   {: seen:n cx:n vo:n nl:n ne:n nh:n bos:n eos:n eps:r v:n :}
   seen S-POS and 0<> if E-FIELD throw then
   seen S-CTX and 0<> if
      v cx <> if E-FIELD throw then
      seen S-POS or cx vo nl ne nh bos eos eps
   else
      seen S-POS or v vo nl ne nh bos eos eps
   then ;

: SET-EMBD ( n n n n n n n n r n -- n n n n n n n n r )
   {: seen:n cx:n vo:n nl:n ne:n nh:n bos:n eos:n eps:r v:n :}
   seen S-EMBD MARK cx vo nl v nh bos eos eps ;

: SET-HEAD ( n n n n n n n n r n -- n n n n n n n n r )
   {: seen:n cx:n vo:n nl:n ne:n nh:n bos:n eos:n eps:r v:n :}
   seen S-HEAD MARK cx vo nl ne v bos eos eps ;

: SET-LAYER ( n n n n n n n n r n -- n n n n n n n n r )
   {: seen:n cx:n vo:n nl:n ne:n nh:n bos:n eos:n eps:r v:n :}
   seen S-LAYER MARK cx vo v ne nh bos eos eps ;

: SET-VOCAB ( n n n n n n n n r n -- n n n n n n n n r )
   {: seen:n cx:n vo:n nl:n ne:n nh:n bos:n eos:n eps:r v:n :}
   seen S-VOCAB MARK cx v nl ne nh bos eos eps ;

: SET-BOS ( n n n n n n n n r n -- n n n n n n n n r )
   {: seen:n cx:n vo:n nl:n ne:n nh:n bos:n eos:n eps:r v:n :}
   seen S-BOS MARK cx vo nl ne nh v eos eps ;

: SET-EOS ( n n n n n n n n r n -- n n n n n n n n r )
   {: seen:n cx:n vo:n nl:n ne:n nh:n bos:n eos:n eps:r v:n :}
   seen S-EOS MARK cx vo nl ne nh bos v eps ;

: SET-EPS ( n n n n n n n n r r -- n n n n n n n n r )
   {: seen:n cx:n vo:n nl:n ne:n nh:n bos:n eos:n eps:r v:r :}
   seen S-EPS MARK cx vo nl ne nh bos eos v ;

: KEY-ID ( ptr u8 n -- n )
   2dup s" model_type" STR= if 2drop K-MODEL exit then
   2dup s" architectures" STR= if 2drop K-ARCH exit then
   2dup s" activation_function" STR= if 2drop K-ACT exit then
   2dup s" n_ctx" STR= if 2drop K-CTX exit then
   2dup s" n_positions" STR= if 2drop K-POS exit then
   2dup s" n_embd" STR= if 2drop K-EMBD exit then
   2dup s" n_head" STR= if 2drop K-HEAD exit then
   2dup s" n_layer" STR= if 2drop K-LAYER exit then
   2dup s" vocab_size" STR= if 2drop K-VOCAB exit then
   2dup s" bos_token_id" STR= if 2drop K-BOS exit then
   2dup s" eos_token_id" STR= if 2drop K-EOS exit then
   2dup s" layer_norm_epsilon" STR= if 2drop K-EPS exit then
   2drop K-OTHER ;

: READ-MODEL ( n n n n n n n n r JR:reader ptr u8 n -- n n n n n n n n r JR:reader )
   {: dst:ptr cap:n :}
   NEXT T-STR <> if E-FIELD throw then
   dst cap STR {: got:n :}
   dst got s" gpt2" STR= 0= if E-FIELD throw then
   S-MODEL swap [: MARK-ONLY ;] DIP ;

: READ-ACT ( n n n n n n n n r JR:reader ptr u8 n -- n n n n n n n n r JR:reader )
   {: dst:ptr cap:n :}
   NEXT T-STR <> if E-FIELD throw then
   dst cap STR {: got:n :}
   dst got s" gelu_new" STR= 0= if E-FIELD throw then
   S-ACT swap [: MARK-ONLY ;] DIP ;

: READ-ARCH ( n n n n n n n n r JR:reader ptr u8 n -- n n n n n n n n r JR:reader )
   {: dst:ptr cap:n :}
   NEXT T-ARR <> if E-FIELD throw then
   NEXT T-STR <> if E-FIELD throw then
   dst cap STR {: got:n :}
   dst got s" GPT2LMHeadModel" STR= 0= if E-FIELD throw then
   NEXT T-ARR-END <> if E-FIELD throw then
   S-ARCH swap [: MARK-ONLY ;] DIP ;

: READ-CTX ( n n n n n n n n r JR:reader -- n n n n n n n n r JR:reader )
   NEXT T-INT <> if E-FIELD throw then
   INT swap [: SET-CTX ;] DIP ;

: READ-POS ( n n n n n n n n r JR:reader -- n n n n n n n n r JR:reader )
   NEXT T-INT <> if E-FIELD throw then
   INT swap [: SET-POS ;] DIP ;

: READ-EMBD ( n n n n n n n n r JR:reader -- n n n n n n n n r JR:reader )
   NEXT T-INT <> if E-FIELD throw then
   INT swap [: SET-EMBD ;] DIP ;

: READ-HEAD ( n n n n n n n n r JR:reader -- n n n n n n n n r JR:reader )
   NEXT T-INT <> if E-FIELD throw then
   INT swap [: SET-HEAD ;] DIP ;

: READ-LAYER ( n n n n n n n n r JR:reader -- n n n n n n n n r JR:reader )
   NEXT T-INT <> if E-FIELD throw then
   INT swap [: SET-LAYER ;] DIP ;

: READ-VOCAB ( n n n n n n n n r JR:reader -- n n n n n n n n r JR:reader )
   NEXT T-INT <> if E-FIELD throw then
   INT swap [: SET-VOCAB ;] DIP ;

: READ-BOS ( n n n n n n n n r JR:reader -- n n n n n n n n r JR:reader )
   NEXT T-INT <> if E-FIELD throw then
   INT swap [: SET-BOS ;] DIP ;

: READ-EOS ( n n n n n n n n r JR:reader -- n n n n n n n n r JR:reader )
   NEXT T-INT <> if E-FIELD throw then
   INT swap [: SET-EOS ;] DIP ;

: READ-EPS ( n n n n n n n n r JR:reader -- n n n n n n n n r JR:reader )
   NEXT T-FLOAT <> if E-FIELD throw then
   FLOAT swap [: SET-EPS ;] DIP ;

: SKIP-MEMBER ( n n n n n n n n r JR:reader -- n n n n n n n n r JR:reader )
   NEXT drop SKIP-VALUE ;

: JR-BODY ( ptr u8 n ptr u8 n ptr u8 CAD-NUM:alloc-byte-len -- n n n n n n n n r )
   drop {: src:ptr len:n key:ptr keycap:n storage:ptr :}
   EMPTY-FIELDS
   storage STORAGE-BYTES src len INIT
   NEXT T-OBJ <> if E-FIELD throw then
   begin
      NEXT dup T-OBJ-END <>
   while
      T-KEY <> if E-FIELD throw then
      key keycap STR key swap KEY-ID
      case
         K-MODEL of key keycap READ-MODEL endof
         K-ARCH  of key keycap READ-ARCH endof
         K-ACT   of key keycap READ-ACT endof
         K-CTX   of READ-CTX endof
         K-POS   of READ-POS endof
         K-EMBD  of READ-EMBD endof
         K-HEAD  of READ-HEAD endof
         K-LAYER of READ-LAYER endof
         K-VOCAB of READ-VOCAB endof
         K-BOS   of READ-BOS endof
         K-EOS   of READ-EOS endof
         K-EPS   of READ-EPS endof
         K-OTHER of SKIP-MEMBER endof
         E-FIELD throw
      endcase
   repeat
   drop
   NEXT T-END <> if E-FIELD throw then
   JR:CLOSE ;

: KEY-BODY ( ptr u8 n ptr u8 CAD-NUM:alloc-byte-len -- n n n n n n n n r )
   drop {: src:ptr len:n key:ptr :}
   src len key CONFIG-LEN
   STORAGE-BYTES BYTES-ALLOC-LEN [: JR-BODY ;] WITH-BYTES ;

: PARSE ( ptr u8 n -- n n n n n n n n r )
   CONFIG-LEN BYTES-ALLOC-LEN [: KEY-BODY ;] WITH-BYTES ;

: DIGEST-BODY ( ptr u8 n ptr u8 CAD-NUM:alloc-byte-len -- bool )
   drop {: src:ptr len:n digest:ptr :}
   src len digest SHA256
   digest digest DIGEST-LEN + SHA256>HEX
   digest DIGEST-LEN + HEX-LEN CONFIG-SHA256$ STR= ;

: DIGEST-MATCH? ( ptr u8 n -- bool )
   DIGEST-BUF-LEN BYTES-ALLOC-LEN [: DIGEST-BODY ;] WITH-BYTES ;

: BUILD-FIELDS ( n n n n n n n n r -- GPT2:config )
   {: seen:n cx:n vo:n nl:n ne:n nh:n bos:n eos:n eps:r :}
   seen S-ALL <> if E-FIELD throw then
   DTYPE cx vo nl ne nh TIED? bos eos eps ATTN-SCALE? GPT2:BUILD ;

: FILE-BODY ( ptr u8 n ptr u8 CAD-NUM:alloc-byte-len -- GPT2:config )
   drop {: path:ptr pathu:n file:ptr :}
   path pathu file CONFIG-LEN READ-ALL
   CONFIG-LEN <> if E-SIZE throw then
   file CONFIG-LEN DIGEST-MATCH? 0= if E-DIGEST throw then
   file CONFIG-LEN PARSE BUILD-FIELDS ;

: PATH-BODY ( ptr u8 n ptr u8 CAD-NUM:alloc-byte-len -- GPT2:config )
   drop {: root:ptr rootu:n path:ptr :}
   root rootu CONFIG-NAME$ path JOIN-PATH {: pathu:n :}
   path pathu CONFIG-LEN BYTES-ALLOC-LEN
   [: FILE-BODY ;] WITH-BYTES ;

: NUL? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0 ?do a i + c@ 0= if true unloop exit then loop
   false ;

: ROOT-OK ( ptr u8 n -- ) {: root:ptr rootu:n :}
   rootu 0 <= if E-ROOT throw then
   rootu FS-PATH-CAP > if E-FS-CAPACITY throw then
   root NULL$ drop = if E-ROOT throw then
   root rootu NUL? if E-ROOT throw then ;

: OPEN-INNER ( ptr u8 n -- GPT2:config )
   2dup ROOT-OK
   FS-PATH-CAP BYTES-ALLOC-LEN [: PATH-BODY ;] WITH-BYTES ;

: OPEN-FRAME ( result<GPT2:config,n> ptr u8 n -- result<GPT2:config,n> ptr u8 n )
   2dup OPEN-INNER RESULT:OK
   \ typed-local-lint: allow-bare-local - result is a concrete multi-cell sum.
   {: root:ptr rootu:n result :}
   drop
   result
   root rootu ;

public

: OPEN-GPT2 ( FS:path -- result<GPT2:config,n> )
   FS-PATH:UNMAKE {: root:ptr rootu:n :}
   0 RESULT:ERR root rootu
   [: OPEN-FRAME ;] catch {: code:n :}
   2drop
   MATCH result
      ok OF RESULT:OK ENDOF
      err OF drop code RESULT:ERR ENDOF
   ;MATCH ;

;using
;using
;using

;package
