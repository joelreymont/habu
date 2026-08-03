\ gpt2-generate.f - persistent GPT-2 generation.

require lib/adt/result.f
require lib/cad-num-arithmetic.f
require lib/fs.f
require lib/fs-path.f
require maki/examples/nanogpt/bpe-full.f
require maki/infer/gpt2-greedy.f
require maki/infer/gpt2-pin.f

package GPT2-GEN

public

-5665 constant E-PROMPT
-5666 constant E-STATE
-5667 constant E-LIMIT

private

CAST: BL>N ( CAD-NUM:byte-len -- n ) ;
CAST: IC>N ( CAD-NUM:item-count -- n ) ;

50257 constant VOCAB-N
4 constant F32-BYTES
VOCAB-N F32-BYTES * constant LOGIT-CAP

create IDS 4096 cells allot
create LOGITS LOGIT-CAP allot
create PATH FS-PATH-CAP allot

variable ENCODE-N
variable DECODE-N

using CAD-NUM

: BYTE-CAP ( n -- CAD-NUM:byte-len )
   BYTE-LEN
   MATCH numeric-result
      ok OF ENDOF
      negative OF E-LIMIT throw ENDOF
      zero OF E-LIMIT throw ENDOF
      overflow OF E-LIMIT throw ENDOF
      underflow OF E-LIMIT throw ENDOF
      bad-alignment OF E-LIMIT throw ENDOF
      misaligned OF E-LIMIT throw ENDOF
   ;MATCH ;

: TOKEN-CAP ( n -- CAD-NUM:item-count )
   ITEM-COUNT
   MATCH numeric-result
      ok OF ENDOF
      negative OF E-LIMIT throw ENDOF
      zero OF E-LIMIT throw ENDOF
      overflow OF E-LIMIT throw ENDOF
      underflow OF E-LIMIT throw ENDOF
      bad-alignment OF E-LIMIT throw ENDOF
      misaligned OF E-LIMIT throw ENDOF
   ;MATCH ;

;using

public

4096 BYTE-CAP constant PROMPT-CAP
8192 BYTE-CAP constant OUTPUT-CAP
4096 TOKEN-CAP constant MAX-TOKENS

private

: ID@ ( n -- n )
   IDS swap T-GET f>s ;

: ID! ( n n -- ) {: id:n idx:n :}
   id s>f IDS idx T-SET ;

using RESULT
using FS-PATH
using MAKI

: ROW-LEN ( -- CAD-NUM:byte-len )
   BPR-VOCAB F32-BYTES *
   dup LOGIT-CAP > if drop E-STATE throw then
   BYTE-CAP ;

using GPT2PIN

: LOAD-BPE-BODY ( ptr u8 n -- ptr u8 n )
   {: root:ptr rootu:n :}
   root rootu MERGES-NAME$ PATH JOIN-PATH {: pathu:n :}
   PATH pathu MERGES-SHA256$ BPF-LOAD
   root rootu ;

;using

: LOAD-BPE ( ptr u8 n -- result<FS:path,n> )
   [: LOAD-BPE-BODY ;] catch {: code:n :}
   code 0<> if 2drop code ERR exit then
   FS-PATH:MAKE RESULT:OK ;

: MODEL-ERR ( n -- result<GPT2:model,n> )
   ERR ;

: ENCODE-BODY ( ptr u8 n -- ptr u8 n )
   {: prompt:ptr promptu:n :}
   prompt promptu IDS MAX-TOKENS IC>N BPR-ENCODE ENCODE-N !
   prompt promptu ;

: ENCODE ( ptr u8 CAD-NUM:byte-len -- result<CAD-NUM:item-count,n> )
   {: prompt:ptr bytes:CAD-NUM:byte-len :}
   bytes BL>N {: promptu:n :}
   promptu 0= if E-PROMPT ERR exit then
   prompt promptu [: ENCODE-BODY ;] catch {: code:n :}
   2drop
   code 0<> if code ERR exit then
   ENCODE-N @ dup 0= if drop E-PROMPT ERR exit then
   TOKEN-CAP RESULT:OK ;

: DECODE-BODY ( ptr a n ptr u8 n -- ptr a n ptr u8 n )
   {: ids:ptr count:n out:ptr cap:n :}
   ids count out cap BPR-DECODE DECODE-N !
   ids count out cap ;

: DECODE
   ( n ptr u8 CAD-NUM:byte-len -- result<CAD-NUM:byte-len,n> )
   {: count:n out:ptr cap:CAD-NUM:byte-len :}
   cap BL>N {: outu:n :}
   IDS count out outu [: DECODE-BODY ;] catch {: code:n :}
   2drop 2drop
   code 0<> if code ERR exit then
   DECODE-N @ BYTE-CAP RESULT:OK ;

;using

: LIMIT? ( CAD-NUM:item-count -- bool )
   IC>N {: max:n :}
   max 0 <= max MAX-TOKENS IC>N > or ;

: FITS?
   ( CAD-NUM:item-count CAD-NUM:item-count CAD-NUM:item-count -- bool )
   {:
      prompt:CAD-NUM:item-count max:CAD-NUM:item-count
      context:CAD-NUM:item-count
   :}
   prompt IC>N
   max IC>N 1- +
   context IC>N <= ;

public

: OPEN ( FS:path -- result<GPT2:model,n> )
   UNMAKE LOAD-BPE
   MATCH result
      err OF MODEL-ERR ENDOF
      ok OF GPT2:OPEN ENDOF
   ;MATCH ;

private

using GPT2

: FEED-PROMPT
   ( GPT2:model ptr u8 CAD-NUM:byte-len n n -- GPT2:model result<n,n> )
   {: row:ptr bytes:CAD-NUM:byte-len idx:n count:n :}
   idx ID@ row bytes GREEDY
   MATCH result
      err OF ERR ENDOF
      ok OF
         {: id:n :}
         idx 1+ count < if
            row bytes idx 1+ count recurse
         else
            id RESULT:OK
         then
      ENDOF
   ;MATCH ;

: CONTINUE
   ( GPT2:model ptr u8 CAD-NUM:byte-len n n n n -- GPT2:model result<n,n> )
   {:
      row:ptr bytes:CAD-NUM:byte-len id:n count:n max:n eos:n
   :}
   id eos = if count RESULT:OK exit then
   id count ID!
   count 1+ {: next:n :}
   next max = if next RESULT:OK exit then
   id row bytes GREEDY
   MATCH result
      err OF ERR ENDOF
      ok OF
         {: next-id:n :}
         row bytes next-id next max eos recurse
      ENDOF
   ;MATCH ;

: GENERATE-INNER
   ( GPT2:model CAD-NUM:item-count CAD-NUM:item-count ptr u8 CAD-NUM:byte-len -- GPT2:model result<n,n> )
   {:
      prompt:CAD-NUM:item-count max:CAD-NUM:item-count
      row:ptr bytes:CAD-NUM:byte-len
   :}
   max IC>N {: maxn:n :}
   EOS-ID {: eos:n :}
   RESET
   row bytes 0 prompt IC>N FEED-PROMPT
   MATCH result
      err OF ERR ENDOF
      ok OF
         {: id:n :}
         row bytes id 0 maxn eos CONTINUE
      ENDOF
   ;MATCH ;

public

: GENERATE
   ( GPT2:model ptr u8 CAD-NUM:byte-len CAD-NUM:item-count ptr u8 CAD-NUM:byte-len -- GPT2:model result<CAD-NUM:byte-len,n> )
   {:
      prompt:ptr promptu:CAD-NUM:byte-len max:CAD-NUM:item-count
      out:ptr cap:CAD-NUM:byte-len
   :}
   max LIMIT? if E-LIMIT ERR exit then
   prompt promptu ENCODE
   MATCH result
      err OF ERR ENDOF
      ok OF
         {: count:CAD-NUM:item-count :}
         CONTEXT-LEN {: context:CAD-NUM:item-count :}
         count max context FITS? 0= if E-LIMIT ERR exit then
         count max LOGITS ROW-LEN GENERATE-INNER
         MATCH result
            err OF ERR ENDOF
            ok OF out cap DECODE ENDOF
         ;MATCH
      ENDOF
   ;MATCH ;

;using
;using
;using

;package
