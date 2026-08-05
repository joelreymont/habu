\ gpt2-generate.f - model-owned persistent GPT-2 generation.

require lib/adt/result.f
require lib/cad-num-arithmetic.f
require maki/infer/gpt2-greedy.f

package GPT2

public

-5665 constant E-PROMPT
-5667 constant E-LIMIT

private

CAST: BL>N ( CAD-NUM:byte-len -- n ) ;
CAST: IC>N ( CAD-NUM:item-count -- n ) ;

4 constant F32-BYTES

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

using RESULT

: ROW-LEN ( -- CAD-NUM:byte-len )
   T-VOCAB-N F32-BYTES * BYTE-CAP ;

: ENCODE
   ( GPT2:model ptr u8 CAD-NUM:byte-len -- GPT2:model result<CAD-NUM:item-count,n> )
   {: prompt:ptr bytes:CAD-NUM:byte-len :}
   bytes BL>N {: promptu:n :}
   promptu 0= if E-PROMPT ERR exit then
   promptu T-ID-CAP > if E-TOK-CAP ERR exit then
   prompt promptu 0 T-ENC-FITS? 0= if E-TOK-CAP ERR exit then
   M-TAKE
   {: x:n a:n b:n logits:n token:n k:n v:n pos:n tmod:n amod:n embed:n ln:n linear:n unembed:n gelu:n residual:n attn:n tokstate:ptr toklen:CAD-NUM:alloc-byte-len :}
   prompt promptu tokstate -rot T-ENCODE {: count:n :}
   x a b logits token k v pos
   tmod amod embed ln linear unembed gelu residual attn tokstate toklen M-SAVE
   count 0= if E-PROMPT ERR else count TOKEN-CAP RESULT:OK then ;

: DECODE
   ( GPT2:model n ptr u8 CAD-NUM:byte-len -- GPT2:model result<CAD-NUM:byte-len,n> )
   {: count:n out:ptr cap:CAD-NUM:byte-len :}
   cap BL>N {: outu:n :}
   M-TAKE
   {: x:n a:n b:n logits:n token:n k:n v:n pos:n tmod:n amod:n embed:n ln:n linear:n unembed:n gelu:n residual:n attn:n tokstate:ptr toklen:CAD-NUM:alloc-byte-len :}
   tokstate count T-DECODE-LEN {: bytes:n :}
   bytes outu <= if tokstate out bytes T-DECODE-OUT then
   x a b logits token k v pos
   tmod amod embed ln linear unembed gelu residual attn tokstate toklen M-SAVE
   bytes outu > if E-TOK-CAP ERR else bytes BYTE-CAP RESULT:OK then ;

: ID-AT ( GPT2:model n -- GPT2:model n ) {: idx:n :}
   M-TAKE
   {: x:n a:n b:n logits:n token:n k:n v:n pos:n tmod:n amod:n embed:n ln:n linear:n unembed:n gelu:n residual:n attn:n tokstate:ptr toklen:CAD-NUM:alloc-byte-len :}
   tokstate idx T-ID@ {: id:n :}
   x a b logits token k v pos
   tmod amod embed ln linear unembed gelu residual attn tokstate toklen M-SAVE
   id ;

: ID-PUT ( GPT2:model n n -- GPT2:model ) {: id:n idx:n :}
   M-TAKE
   {: x:n a:n b:n logits:n token:n k:n v:n pos:n tmod:n amod:n embed:n ln:n linear:n unembed:n gelu:n residual:n attn:n tokstate:ptr toklen:CAD-NUM:alloc-byte-len :}
   tokstate id idx T-ID!
   x a b logits token k v pos
   tmod amod embed ln linear unembed gelu residual attn tokstate toklen M-SAVE ;

: OWN-GREEDY
   ( GPU:session GPT2:model n -- GPU:session GPT2:model result<n,n> )
   {: tok:n :}
   M-TAKE
   {: x:n a:n b:n logits:n token:n k:n v:n pos:n tmod:n amod:n embed:n ln:n linear:n unembed:n gelu:n residual:n attn:n tokstate:ptr toklen:CAD-NUM:alloc-byte-len :}
   tokstate T-LOGITS cells + {: row:ptr :}
   ROW-LEN {: bytes:CAD-NUM:byte-len :}
   tok pos bytes M-VALIDATE {: valid:n :}
   \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
   {: c :}
   rot swap
   c
   valid 0= if
      x a b logits token k v pos embed ln linear unembed gelu residual attn
      tok row bytes M-INFER
   else valid then {: code:n :}
   \ typed-local-lint: allow-bare-local - config is a multi-cell structure.
   {: c2 :}
   rot swap
   c2
   code 0<> if
      x a b logits token k v pos
      tmod amod embed ln linear unembed gelu residual attn tokstate toklen M-SAVE
      code ERR
      exit
   then
   row bytes G-SCAN
   MATCH result
      err OF
         {: scan-code:n :}
         x a b logits token k v pos 1+
         tmod amod embed ln linear unembed gelu residual attn tokstate toklen M-SAVE
         scan-code ERR
      ENDOF
      ok OF
         {: id:n :}
         x a b logits token k v pos 1+
         tmod amod embed ln linear unembed gelu residual attn tokstate toklen M-SAVE
         id RESULT:OK
      ENDOF
   ;MATCH ;

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

: FEED-PROMPT
   ( GPU:session GPT2:model n n -- GPU:session GPT2:model result<n,n> )
   {: idx:n count:n :}
   idx ID-AT OWN-GREEDY
   MATCH result
      err OF ERR ENDOF
      ok OF
         {: id:n :}
         idx 1+ count < if
            idx 1+ count recurse
         else
            id RESULT:OK
         then
      ENDOF
   ;MATCH ;

: CONTINUE
   ( GPU:session GPT2:model n n n n -- GPU:session GPT2:model result<n,n> )
   {: id:n count:n max:n eos:n :}
   id eos = if count RESULT:OK exit then
   id count ID-PUT
   count 1+ {: next:n :}
   next max = if next RESULT:OK exit then
   id OWN-GREEDY
   MATCH result
      err OF ERR ENDOF
      ok OF
         {: next-id:n :}
         next-id next max eos recurse
      ENDOF
   ;MATCH ;

: GENERATE-INNER
   ( GPU:session GPT2:model CAD-NUM:item-count CAD-NUM:item-count -- GPU:session GPT2:model result<n,n> )
   {: prompt:CAD-NUM:item-count max:CAD-NUM:item-count :}
   max IC>N {: maxn:n :}
   EOS-ID {: eos:n :}
   RESET
   0 prompt IC>N FEED-PROMPT
   MATCH result
      err OF ERR ENDOF
      ok OF
         {: id:n :}
         id 0 maxn eos CONTINUE
      ENDOF
   ;MATCH ;

public

: GENERATE
   ( GPU:session GPT2:model ptr u8 CAD-NUM:byte-len CAD-NUM:item-count ptr u8 CAD-NUM:byte-len -- GPU:session GPT2:model result<CAD-NUM:byte-len,n> )
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
         count max GENERATE-INNER
         MATCH result
            err OF ERR ENDOF
            ok OF out cap DECODE ENDOF
         ;MATCH
      ENDOF
   ;MATCH ;

;using
;package
