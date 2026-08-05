\ gpt2-cli.f - one-request GPT-2 generation client.

require lib/cad-num-arithmetic.f
require lib/fs-path.f
require maki/infer/gpt2-generate.f

package GPT2-CLI
private

64 constant E-USAGE
-5666 constant E-TOTALITY

CAST: BL>N ( CAD-NUM:byte-len -- n ) ;

using GPT2

create OUT OUTPUT-CAP allot

using CAD-NUM

: PROMPT-LEN ( n -- CAD-NUM:byte-len )
   BYTE-LEN
   MATCH numeric-result
      ok OF ENDOF
      negative OF E-TOTALITY throw ENDOF
      zero OF E-TOTALITY throw ENDOF
      overflow OF E-TOTALITY throw ENDOF
      underflow OF E-TOTALITY throw ENDOF
      bad-alignment OF E-TOTALITY throw ENDOF
      misaligned OF E-TOTALITY throw ENDOF
   ;MATCH ;

: TOKEN-COUNT ( n -- CAD-NUM:item-count )
   ITEM-COUNT
   MATCH numeric-result
      ok OF ENDOF
      negative OF E-TOTALITY throw ENDOF
      zero OF E-TOTALITY throw ENDOF
      overflow OF E-TOTALITY throw ENDOF
      underflow OF E-TOTALITY throw ENDOF
      bad-alignment OF E-TOTALITY throw ENDOF
      misaligned OF E-TOTALITY throw ENDOF
   ;MATCH ;

;using

64 TOKEN-COUNT constant CONT-N

: FIRST ( n n -- n ) {: first:n next:n :}
   first 0<> if first else next then ;

: CLOSE-CODE ( result<n,n> -- n )
   MATCH result
      ok OF ENDOF
      err OF ENDOF
   ;MATCH ;

: SESSION-CLEAN ( GPU:session n -- n )
   {: primary:n :}
   GPU:CLOSE CLOSE-CODE
   primary swap FIRST ;

: CLOSE-ALL ( GPU:session GPT2:model n -- n )
   {: primary:n :}
   GPT2:CLOSE CLOSE-CODE {: model-code:n :}
   GPU:CLOSE CLOSE-CODE {: session-code:n :}
   primary model-code FIRST session-code FIRST ;

: RUN-ACT ( ptr u8 n ptr u8 n -- CAD-NUM:byte-len )
   {: root:ptr rootu:n prompt:ptr promptu:n :}
   GPU:OPEN
   MATCH result
      err OF throw ENDOF
      ok OF
         root rootu FS-PATH:MAKE GPT2:OPEN
         MATCH result
            err OF
               {: open-code:n :}
               open-code SESSION-CLEAN throw
            ENDOF
            ok OF
               prompt promptu PROMPT-LEN CONT-N
               OUT OUTPUT-CAP GENERATE
               MATCH result
                  err OF
                     {: generate:n :}
                     generate CLOSE-ALL throw
                  ENDOF
                  ok OF
                     {: outu:CAD-NUM:byte-len :}
                     0 CLOSE-ALL
                     dup 0<> if throw then drop
                     outu
                  ENDOF
               ;MATCH
            ENDOF
         ;MATCH
      ENDOF
   ;MATCH ;

;using

public

: RUN ( -- )
   SCRIPT-ARGC 2 <> if E-USAGE throw then
   0 SCRIPT-ARGV$ 1 SCRIPT-ARGV$ RUN-ACT
   OUT swap BL>N type ;

;package
