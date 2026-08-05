\ engine.f - GPT-2 inference owner aggregate.

require maki/infer/gpt2-model.f
require maki/infer/kv-cache.f

package INFER

public

DEFLINEAR INFER:engine

private

0 constant I-SESSION
1 cells constant I-MODEL
2 cells constant I-CACHE
3 cells constant I-REC-BYTES

\ GPT-2 K/V kernels write F32 activations; weight DATATYPE@ never changes that storage width.
4 constant F32-BYTES

CAST: IC>N ( CAD-NUM:item-count -- n ) ;

: I-REC-LEN ( -- CAD-NUM:alloc-byte-len )
   I-REC-BYTES MEM:BYTES-ALLOC-LEN ;

: I-ALLOC-REC ( ptr u8 -- ptr u8 )
   drop I-REC-LEN MEM:ALLOC-BYTES drop ;

: I-FREE-REC ( ptr u8 -- )
   I-REC-LEN MEM:RELEASE-BYTES ;

\ The checker cannot tie three linear owners to their private record.
\ Retirement owner: habu-checker-ptr-lifetime-f59d1e9d.
TRUSTED: I-MINT
   ( GPU:session GPT2:model KV:cache ptr u8 -- INFER:engine )
   {: session:GPU:session model:GPT2:model cache:KV:cache rec:ptr :}
   session rec I-SESSION + !
   model rec I-MODEL + !
   cache rec I-CACHE + !
   rec ;

TRUSTED: I-TAKE
   ( INFER:engine -- GPU:session GPT2:model KV:cache ptr u8 )
   {: rec:ptr :}
   rec I-SESSION + @
   rec I-MODEL + @
   rec I-CACHE + @
   rec ;

: I-CODE ( result<n,n> -- n )
   MATCH result
      ok OF drop 0 ENDOF
      err OF ENDOF
   ;MATCH ;

: I-FIRST ( n n -- n ) {: first:n next:n :}
   first 0<> if first else next then ;

: I-ERR ( n -- result<INFER:engine,n> )
   RESULT:ERR ;

: I-SESSION-FAIL ( GPU:session n -- result<INFER:engine,n> )
   {: primary:n :}
   GPU:CLOSE I-CODE {: session-code:n :}
   primary session-code I-FIRST I-ERR ;

: I-MODEL-FAIL
   ( GPU:session GPT2:model n -- result<INFER:engine,n> )
   {: primary:n :}
   GPT2:CLOSE I-CODE {: model-code:n :}
   GPU:CLOSE I-CODE {: session-code:n :}
   primary model-code I-FIRST session-code I-FIRST I-ERR ;

: I-KV-TRY ( n n n n n n n n -- n n n n n n n n )
   drop KV:CONFIG KV-CONFIG:UNMAKE ;

: I-START-CACHE
   ( GPU:session GPT2:model ptr u8 n n -- result<INFER:engine,n> )
   {: rec:ptr nseq:n npages:n :}
   GPT2:CONFIG@
   GPT2:NLAYER@ {: nl:n :}
   GPT2:NHEAD@ {: nh:n :}
   GPT2:NEMBD@ {: ne:n :}
   GPT2:NCTX@ {: cx:n :}
   drop
   nl nh ne nh / F32-BYTES npages nseq cx 0 [: I-KV-TRY ;] catch
   {: knl:n knh:n khd:n kdb:n kpages:n kseq:n kctx:n kptok:n code:n :}
   code 0<> if
      code I-MODEL-FAIL
      exit
   then
   knl knh khd kdb kpages kseq kctx kptok KV-CONFIG:MAKE
   swap >r KV:OPEN r> swap
   MATCH result
      err OF I-MODEL-FAIL ENDOF
      ok OF rec I-MINT RESULT:OK ENDOF
   ;MATCH ;

: I-START-MODEL
   ( GPU:session FS:path ptr u8 n n -- result<INFER:engine,n> )
   {: rec:ptr nseq:n npages:n :}
   GPT2:OPEN
   MATCH result
      err OF I-SESSION-FAIL ENDOF
      ok OF rec nseq npages I-START-CACHE ENDOF
   ;MATCH ;

: I-START-SESSION
   ( FS:path ptr u8 n n -- result<INFER:engine,n> )
   {: p rec:ptr nseq:n npages:n :} \ typed-local-lint: allow-bare-local - p is FS:path's two-cell structure.
   GPU:OPEN
   MATCH result
      err OF
         {: code:n :}
         code I-ERR
      ENDOF
      ok OF p rec nseq npages I-START-MODEL ENDOF
   ;MATCH ;

: I-SCOPE-FINISH
   ( ptr u8 result<INFER:engine,n> -- result<INFER:engine,n> )
   MATCH result
      err OF
         {: code:n :}
         I-FREE-REC
         code I-ERR
      ENDOF
      ok OF
         >r drop r> RESULT:OK
      ENDOF
   ;MATCH ;

: I-START-SCOPE
   ( FS:path ptr u8 n n -- result<INFER:engine,n> )
   {: p rec:ptr nseq:n npages:n :} \ typed-local-lint: allow-bare-local - p is FS:path's two-cell structure.
   rec p rec nseq npages I-START-SESSION
   I-SCOPE-FINISH ;

public

: START-GPT2
   ( FS:path CAD-NUM:item-count CAD-NUM:item-count -- result<INFER:engine,n> )
   {: nseq:CAD-NUM:item-count npages:CAD-NUM:item-count :}
   nseq IC>N {: seqn:n :}
   npages IC>N {: pagesn:n :}
   seqn 0= pagesn 0= or if
      drop E-KV-CONFIG I-ERR
      exit
   then
   NULL$ drop [: I-ALLOC-REC ;] catch {: code:n :}
   code 0<> if
      drop drop code I-ERR
      exit
   then
   seqn pagesn I-START-SCOPE ;

: STOP ( INFER:engine -- result<n,n> )
   I-TAKE {: rec:ptr :}
   swap >r KV:CLOSE I-CODE {: cache-code:n :}
   r> GPT2:CLOSE I-CODE {: model-code:n :}
   GPU:CLOSE I-CODE {: session-code:n :}
   rec I-FREE-REC
   cache-code model-code I-FIRST session-code I-FIRST
   dup 0= if RESULT:OK else RESULT:ERR then ;

;package
