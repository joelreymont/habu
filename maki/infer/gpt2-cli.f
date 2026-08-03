\ gpt2-cli.f - authenticated GPT-2 greedy generation.
\ GPT2-CLI owns errors -5665..-5666.

require lib/cad-num-arithmetic.f
require lib/fs.f
require lib/fs-path.f
require lib/memory.f
require maki/examples/nanogpt/bpe-full.f
require maki/infer/gpt2-greedy.f
require maki/infer/gpt2-pin.f

package GPT2-CLI

private

64 constant CONT-N
4096 constant ID-CAP
8192 constant OUT-CAP
50257 constant VOCAB-N
4 constant F32-BYTES
VOCAB-N F32-BYTES * constant LOGIT-CAP
$47505432 constant CANARY

64 constant E-USAGE
-5665 constant E-PROMPT
-5666 constant E-STATE

create IDS ID-CAP cells allot
create OUT OUT-CAP allot
create PATH FS-PATH-CAP allot

variable OUT-U
variable STAGE-N

: ID@ ( n -- n )
   IDS swap T-GET f>s ;

: ID! ( n n -- ) {: id:n idx:n :}
   id s>f IDS idx T-SET ;

: LOGIT-LEN ( n -- CAD-NUM:byte-len )
   CAD-NUM:BYTE-LEN MATCH CAD-NUM:numeric-result
      ok OF ENDOF
      negative OF E-STATE throw ENDOF
      zero OF E-STATE throw ENDOF
      overflow OF E-STATE throw ENDOF
      underflow OF E-STATE throw ENDOF
      bad-alignment OF E-STATE throw ENDOF
      misaligned OF E-STATE throw ENDOF
   ;MATCH ;

: FIRST ( n n -- n ) {: code:n next:n :}
   code 0<> if code else next then ;

: RESULT-CODE ( result<n,n> -- n )
   MATCH result
      ok OF ENDOF
      err OF ENDOF
   ;MATCH ;

: THROW? ( n -- )
   dup 0<> if throw then drop ;

: REQUIRE-STAGED ( -- )
   STAGE-N @ CONT-N <> if E-STATE throw then
   CONT-N ID@ CANARY <> if E-STATE throw then ;

using MAKI
using GPT2PIN

: LOAD-BPE ( ptr u8 n -- ) {: root:ptr rootu:n :}
   root rootu MERGES-NAME$ PATH JOIN-PATH {: pathu:n :}
   PATH pathu MERGES-SHA256$ BPF-LOAD ;

;using

: ENCODE ( ptr u8 n -- n ) {: prompt:ptr promptu:n :}
   promptu 0= if E-PROMPT throw then
   prompt promptu IDS ID-CAP BPR-ENCODE
   dup 0= if drop E-PROMPT throw then ;

: ROW-LEN ( -- CAD-NUM:byte-len )
   BPR-VOCAB F32-BYTES *
   dup LOGIT-CAP > if drop E-STATE throw then
   LOGIT-LEN ;

: NEXT-ID ( n n n -- n ) {: pos:n count:n staged:n :}
   pos count < if pos else staged 1- then ID@ ;

: ARM ( n -- )
   CANARY CONT-N ID!
   0 ID!
   1 STAGE-N ! ;

: STAGE ( n n -- ) {: id:n staged:n :}
   id staged ID!
   staged 1+ STAGE-N ! ;

: DECODE-STAGED ( -- )
   REQUIRE-STAGED
   IDS CONT-N OUT OUT-CAP BPR-DECODE OUT-U ! ;

using GPT2

: GENERATE
   ( GPT2:model ptr u8 CAD-NUM:byte-len n n n -- GPT2:model n )
   {: logits:ptr bytes:CAD-NUM:byte-len pos:n count:n staged:n :}
   staged CONT-N = if 0 exit then
   pos count staged NEXT-ID logits bytes GREEDY
   MATCH result
      err OF ENDOF
      ok OF
         {: id:n :}
         pos count < if
            pos 1+ {: next:n :}
            next count = if
               id ARM
               logits bytes next count 1 recurse
            else
               logits bytes next count staged recurse
            then
         else
            id staged STAGE
            logits bytes pos count staged 1+ recurse
         then
      ENDOF
   ;MATCH ;

: OPEN-GENERATE ( ptr u8 n ptr u8 CAD-NUM:byte-len n -- )
   {: root:ptr rootu:n logits:ptr bytes:CAD-NUM:byte-len count:n :}
   root rootu FS-PATH:MAKE GPT2:OPEN
   MATCH result
      err OF throw ENDOF
      ok OF
         logits bytes 0 count 0 GENERATE {: code:n :}
         GPT2:CLOSE RESULT-CODE code swap FIRST {: final:n :}
         final THROW?
      ENDOF
   ;MATCH
   DECODE-STAGED ;

;using
;using

using MEM

: LOGITS-BODY
   ( ptr u8 n n CAD-NUM:byte-len ptr u8 CAD-NUM:alloc-byte-len -- )
   {:
      root:ptr rootu:n count:n bytes:CAD-NUM:byte-len
      logits:ptr alloc:CAD-NUM:alloc-byte-len
   :}
   alloc drop
   root rootu logits bytes count OPEN-GENERATE ;

: RUN-ACT ( ptr u8 n ptr u8 n -- )
   {: root:ptr rootu:n prompt:ptr promptu:n :}
   0 OUT-U !
   0 STAGE-N !
   root rootu LOAD-BPE
   prompt promptu ENCODE {: count:n :}
   root rootu count ROW-LEN
   LOGIT-CAP BYTES-ALLOC-LEN [: LOGITS-BODY ;] WITH-BYTES ;

;using

public

: RUN ( -- )
   SCRIPT-ARGC 2 <> if E-USAGE throw then
   0 SCRIPT-ARGV$ 1 SCRIPT-ARGV$ RUN-ACT
   OUT OUT-U @ type ;

;package
