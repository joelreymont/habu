\ engine.f - GPT-2 inference owner aggregate.

require maki/infer/gpt2-model.f
require maki/infer/kv-cache.f

package INFER

public

DEFLINEAR INFER:engine

private

using MEM

0 constant I-SESSION
1 cells constant I-MODEL
2 cells constant I-CACHE
3 cells constant I-REC-BYTES

\ GPT-2 K/V kernels write F32 activations; weight DATATYPE@ never changes that storage width.
4 constant F32-BYTES

CAST: IC>N ( CAD-NUM:item-count -- n ) ;

: I-REC-LEN ( -- CAD-NUM:alloc-byte-len )
   I-REC-BYTES BYTES-ALLOC-LEN ;

\ The checker cannot tie three linear owners to their private record.
\ Retirement owner: habu-checker-ptr-lifetime-f59d1e9d.
TRUSTED: I-MINT
   ( MEM:block GPU:session GPT2:model KV:cache -- INFER:engine )
   {: session:GPU:session model:GPT2:model cache:KV:cache :}
   BORROW {: rec:ptr recu:CAD-NUM:byte-len :}
   recu I-REC-BYTES <> if E-MEM-SIZE throw then
   session rec I-SESSION + !
   model rec I-MODEL + !
   cache rec I-CACHE + ! ;

TRUSTED: I-TAKE
   ( INFER:engine -- MEM:block GPU:session GPT2:model KV:cache )
   BORROW {: rec:ptr recu:CAD-NUM:byte-len :}
   recu I-REC-BYTES <> if E-MEM-SIZE throw then
   rec I-SESSION + @
   rec I-MODEL + @
   rec I-CACHE + @ ;

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

: I-CLOSE ( GPU:session GPT2:model KV:cache n -- n )
   {: primary:n :}
   swap >r
   KV:CLOSE I-CODE {: cache-code:n :}
   r>
   GPT2:CLOSE I-CODE {: model-code:n :}
   GPU:CLOSE I-CODE {: session-code:n :}
   primary cache-code I-FIRST model-code I-FIRST session-code I-FIRST ;

: I-PUBLISH ( GPU:session GPT2:model KV:cache -- result<INFER:engine,n> )
   I-REC-LEN MEM:OPEN
   MATCH result
      err OF I-CLOSE I-ERR ENDOF
      ok OF swap 2swap rot I-MINT RESULT:OK ENDOF
   ;MATCH ;

: I-KV-TRY ( n n n n n n n n -- n n n n n n n n )
   drop KV:CONFIG KV-CONFIG:UNMAKE ;

: I-KV-CONFIG ( GPT2:config n n -- result<KV:config,n> )
   {: nseq:n npages:n :}
   GPT2:NLAYER@ {: nl:n :}
   GPT2:NHEAD@ {: nh:n :}
   GPT2:NEMBD@ {: ne:n :}
   GPT2:NCTX@ {: cx:n :}
   drop
   nl nh ne nh / F32-BYTES npages nseq cx 0 [: I-KV-TRY ;] catch
   {: knl:n knh:n khd:n kdb:n kpages:n kseq:n kctx:n kptok:n code:n :}
   code 0<> if
      knl drop knh drop khd drop kdb drop
      kpages drop kseq drop kctx drop kptok drop
      code RESULT:ERR
      exit
   then
   knl knh khd kdb kpages kseq kctx kptok KV-CONFIG:MAKE RESULT:OK ;

: I-START-CACHE
   ( GPU:session GPT2:model n n -- result<INFER:engine,n> )
   {: nseq:n npages:n :}
   GPT2:CONFIG@ nseq npages I-KV-CONFIG
   MATCH result
      err OF I-MODEL-FAIL ENDOF
      ok OF
         swap >r KV:OPEN r> swap
         MATCH result
            err OF I-MODEL-FAIL ENDOF
            ok OF I-PUBLISH ENDOF
         ;MATCH
      ENDOF
   ;MATCH ;

: I-START-MODEL
   ( GPU:session FS:path n n -- result<INFER:engine,n> )
   {: nseq:n npages:n :}
   GPT2:OPEN
   MATCH result
      err OF I-SESSION-FAIL ENDOF
      ok OF nseq npages I-START-CACHE ENDOF
   ;MATCH ;

: I-START-SESSION
   ( FS:path n n -- result<INFER:engine,n> )
   {: p nseq:n npages:n :} \ typed-local-lint: allow-bare-local - p is FS:path's two-cell structure.
   GPU:OPEN
   MATCH result
      err OF
         I-ERR
      ENDOF
      ok OF p nseq npages I-START-MODEL ENDOF
   ;MATCH ;

;using

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
   seqn pagesn I-START-SESSION ;

: STOP ( INFER:engine -- result<n,n> )
   I-TAKE
   0 I-CLOSE {: code:n :}
   MEM:RELEASE
   code dup 0= if RESULT:OK else RESULT:ERR then ;

;package
