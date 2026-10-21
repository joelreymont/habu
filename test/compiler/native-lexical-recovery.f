\ Tier 1's body capture keeps local and parser-operand scope lexical: a failed
\ nested parser operand rolls back its opener before capture resumes, and a
\ local spelled like a control word is a local only inside its block. The
\ string-opener and raw-operand spellings are test/compiler/native-lexical-scope.f.
require test/gate-common.f
require src/lib/test.f

1 set-tier

package NATIVE-LEXICAL-FIXTURE
public

\ A local named after a control word does not close its arm. The raw operand
\ after [char] is also not a closer, although it has that spelling.
: NLS-CONTROL-WORD ( bool -- n )
   if 9 {: then:n :} [char] then drop then else 0 then ;

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
   false NATIVE-LEXICAL-FIXTURE:NLS-CONTROL-WORD 0 T= ;

public

\ Only x86-64's load loop is the Habu one that rolls back a failed parser
\ operand and scopes string and raw-operand spellings. ARM64's is master's
\ kernel reader: there the failed operand joins LEXWORD's body, which is
\ refused for NEVER-BOUND, and each scope fixture reads its spelling as the
\ global word.
: RUN ( -- )
   HB-TARGET-LINUX-X86-64? if
      RECOVERY-CASE
      s" test/compiler/native-lexical-scope.f" included
   then
   SCOPE-CASE
   T-REPORT ;

;package

NATIVE-LEXICAL-RECOVERY:RUN
