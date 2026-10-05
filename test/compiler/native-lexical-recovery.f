\ A failed nested parser operand rolls back its opener before capture resumes.
require test/gate-common.f
require lib/test.f

package NATIVE-LEXICAL-RECOVERY

: RUN ( -- )
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
   s" NEVER-BOUND" s" caught parser operand refusal" GE-EXPECT-ERR-HAS
   T-REPORT ;

RUN
;package
