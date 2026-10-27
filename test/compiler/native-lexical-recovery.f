\ Tier 1's body capture keeps local and parser-operand scope lexical: a failed
\ nested parser operand rolls back its opener before capture resumes, a local
\ spelled like a control word, the string opener or a parser word is a local
\ only inside its block and its definition's head, and a parser operand that spells a closer or a
\ comment opener is data.
require test/gate-common.f
require src/lib/test.f

1 set-tier

package NATIVE-LEXICAL-FIXTURE
public

\ A local named after a control word does not close its arm. The raw operand
\ after [char] is also not a closer, although it has that spelling.
: NLS-CONTROL-WORD ( bool -- n )
   if 9 {: then:n :} [char] then drop then else 0 then ;

\ A local with the exact spelling of the string opener is an ordinary value
\ in its arm. The opener after `then` is the global string word again.
: NLS-STRING-WORD ( bool -- n )
   if 7 {: s":n :} s" else 0 then
   s" global" 2drop ;

\ A local spelled like a parser word takes no operand: `;` closes the body.
: NLS-LOCAL-CHAR ( -- n ) 5 {: char:n :} char ;

\ A head local ends at `does>`: the clause's string opener is the string word.
: NLS-DOES-STRING ( n -- )
   {: s":n :}
   create s" , does> ( -- ptr u8 n ) drop s" a ; b" ;
5 NLS-DOES-STRING NLS-DOES-WORD

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
   s" a control spelling is local only inside its block" T-LABEL
   true NATIVE-LEXICAL-FIXTURE:NLS-CONTROL-WORD 9 T=
   false NATIVE-LEXICAL-FIXTURE:NLS-CONTROL-WORD 0 T=
   s" a string spelling is local only inside its block" T-LABEL
   true NATIVE-LEXICAL-FIXTURE:NLS-STRING-WORD 7 T=
   false NATIVE-LEXICAL-FIXTURE:NLS-STRING-WORD 0 T=
   s" a parser-word spelling is a local, not a parser" T-LABEL
   NATIVE-LEXICAL-FIXTURE:NLS-LOCAL-CHAR 5 T=
   s" a head local is not live in its does> clause" T-LABEL
   NATIVE-LEXICAL-FIXTURE:NLS-DOES-WORD s" a ; b" T$=
   s" parser operands that spell a closer or a comment opener are data" T-LABEL
   NATIVE-LEXICAL-FIXTURE:NLS-RAW-SEMI 59 T=
   NATIVE-LEXICAL-FIXTURE:NLS-RAW-PAREN 40 T=
   NATIVE-LEXICAL-FIXTURE:NLS-RAW-SLASH 92 T= ;

public

\ Only x86-64's load loop is the Habu one that rolls back a failed parser
\ operand. ARM64's is master's kernel reader: there the failed operand joins
\ LEXWORD's body, which is refused for NEVER-BOUND.
: RUN ( -- )
   HB-TARGET-LINUX-X86-64? if RECOVERY-CASE then
   SCOPE-CASE
   T-REPORT ;

;package

NATIVE-LEXICAL-RECOVERY:RUN
