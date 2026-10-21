\ Tier 1's body capture keeps local and parser-operand scope lexical: a failed
\ nested parser operand rolls back its opener before capture resumes, a local
\ spelled like a string opener or a control word is a local only inside its
\ block, and a parser operand that spells a closer or a comment opener is data.
require test/gate-common.f
require src/lib/test.f

1 set-tier

package NATIVE-LEXICAL-FIXTURE
public

\ A local with the exact spelling of the string opener is an ordinary value
\ in its arm. The opener after `then` is the global string word again.
: NLS-STRING-WORD ( bool -- n )
   if 7 {: s":n :} s" else 0 then
   s" global" 2drop ;

\ A local named after a control word does not close its arm. The raw operand
\ after [char] is also not a closer, although it has that spelling.
: NLS-CONTROL-WORD ( bool -- n )
   if 9 {: then:n :} [char] then drop then else 0 then ;

\ The first semicolon is a parser operand; only the second closes the body.
: NLS-RAW-SEMI ( -- n ) [char] ; ;

\ A parser operand can spell either comment opener without starting a comment.
: NLS-RAW-PAREN ( -- n ) [char] ( ;
: NLS-RAW-SLASH ( -- n ) [char] \ ;

;package

package NATIVE-LEXICAL-RECOVERY

: RECOVERY-CASE ( -- )
   GE-HB-RESET  GE-SRC-RESET
   S\" TRUSTED: LEXBAD ( -- ) s\" ['] NEVER-BOUND\" evaluate ;" GE-SRC-LINE
   s" TRUSTED: LEXTRY ( -- ) ['] LEXBAD catch . ; immediate" GE-SRC-LINE
   S\" s\" LEXTRY\" 0 parse-imm" GE-SRC-LINE
   s" 1 set-tier" GE-SRC-LINE
   S\" : LEXWORD ( -- n ) LEXTRY s\" ; text\" 2drop 7 ;" GE-SRC-LINE
   s" LEXWORD ." GE-SRC-LINE
   GE-HB$ GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   s" caught parser operand refusal" GE-EXPECT-OK
   S\" 70\n7\n" s" caught parser operand refusal" GE-EXPECT-OUT
   s" NEVER-BOUND" s" caught parser operand refusal" GE-EXPECT-ERR-HAS ;

: SCOPE-CASE ( -- )
   s" string and control spellings are local only inside their block" T-LABEL
   true NATIVE-LEXICAL-FIXTURE:NLS-STRING-WORD 7 T=
   false NATIVE-LEXICAL-FIXTURE:NLS-STRING-WORD 0 T=
   true NATIVE-LEXICAL-FIXTURE:NLS-CONTROL-WORD 9 T=
   false NATIVE-LEXICAL-FIXTURE:NLS-CONTROL-WORD 0 T=
   s" parser operands that spell a closer or a comment opener are data" T-LABEL
   NATIVE-LEXICAL-FIXTURE:NLS-RAW-SEMI 59 T=
   NATIVE-LEXICAL-FIXTURE:NLS-RAW-PAREN 40 T=
   NATIVE-LEXICAL-FIXTURE:NLS-RAW-SLASH 92 T= ;

: RUN ( -- )
   RECOVERY-CASE
   SCOPE-CASE
   T-REPORT ;

RUN
;package
