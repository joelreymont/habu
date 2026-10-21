\ The Habu load loop's tier-1 body capture, x86-64's only: a local spelled
\ like the string opener is a local only inside its block, and a parser
\ operand that spells a closer or a comment opener is data. ARM64's load loop
\ is master's kernel reader, whose capture reads each of these as the global
\ word, so test/compiler/native-lexical-recovery.f loads this file on x86-64
\ alone.
require src/lib/test.f

1 set-tier

package NATIVE-LEXICAL-SCOPE

\ A local with the exact spelling of the string opener is an ordinary value
\ in its arm. The opener after `then` is the global string word again.
: NLS-STRING-WORD ( bool -- n )
   if 7 {: s":n :} s" else 0 then
   s" global" 2drop ;

\ The first semicolon is a parser operand; only the second closes the body.
: NLS-RAW-SEMI ( -- n ) [char] ; ;

\ A parser operand can spell either comment opener without starting a comment.
: NLS-RAW-PAREN ( -- n ) [char] ( ;
: NLS-RAW-SLASH ( -- n ) [char] \ ;

: SCOPE-CASE ( -- )
   s" a string spelling is local only inside its block" T-LABEL
   true NLS-STRING-WORD 7 T=
   false NLS-STRING-WORD 0 T=
   s" parser operands that spell a closer or a comment opener are data" T-LABEL
   NLS-RAW-SEMI 59 T=
   NLS-RAW-PAREN 40 T=
   NLS-RAW-SLASH 92 T= ;

SCOPE-CASE
;package
