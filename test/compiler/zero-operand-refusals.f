\ zero-operand-refusals.f - rejected programs for "the engine's `/`, `mod` and
\ `/mod` take no literal 0 divisor, and its `throw` no literal 0 code". The
\ refusal names the token, in a body and at top level, and a literal's value
\ rides through stack words in a body and across top-level steps. Controls: a
\ literal 0 dividend still certifies. Cases run through `evaluate-closed`, the
\ path `bin/hb --load` takes; a refusal is a throw.

require src/lib/test.f
require src/lib/string.f

package ZERO-OPERAND-TEST

private

70 constant CHECK-RC                 \ the engine refusing a program it cannot certify

create DIAG-BUF 8192 allot
8192 constant DIAG-CAP

: ARM ( -- )   \ capture the next diagnostic as JSON instead of printing it
   DIAG-BUF DIAG-CAP DIAG-BUFFER!  true DIAG-JSON! ;

: DISARM ( -- )
   false DIAG-JSON!  DIAG-BUFFER-OFF ;

: HAS? ( ptr u8 n -- )
   DIAG-BUFFER$ 2swap CONTAINS? TTRUE ;

: TOKEN? ( ptr u8 n -- )
   SB-RESET
   S\" \"token\":\"" SB-APPEND  SB-APPEND  34 SB-APPEND-C
   SB$ HAS? ;

: DIV-ZERO ( -- )
   S\" \"code\":\"E-DIV-ZERO\"" HAS?
   S\" \"repair_class\":\"fix_divisor\"" HAS? ;

: THROW-ZERO ( -- )
   S\" \"code\":\"E-THROW-ZERO\"" HAS?
   S\" \"repair_class\":\"fix_throw_code\"" HAS? ;

\ ---- the refusals ------------------------------------------------------------

: CASE-DIV ( -- )
   s" a literal 0 divisor of / is refused at the /" T-LABEL
   ARM
   [: s" : ZD1 ( n -- n ) 0 / ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   DIV-ZERO  s" /" TOKEN?
   DISARM ;

: CASE-MOD-CARRIED ( -- )
   s" a literal 0 carried by stack words is refused at the mod" T-LABEL
   ARM
   [: s" : ZD2 ( n -- n ) 0 dup drop mod ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   DIV-ZERO  s" mod" TOKEN?
   DISARM ;

: CASE-TOP-DIVMOD ( -- )
   s" a literal 0 divisor at top level is refused at the /mod" T-LABEL
   ARM
   [: s" 7 0 /mod" evaluate-closed ;] CHECK-RC TTHROWSQ
   DIV-ZERO  s" /mod" TOKEN?
   DISARM ;

\ `dup` and `drop` each load, step and save the interpret row, so the literal
\ reaches `mod` only if its value survives the saves.
: CASE-TOP-MOD-CARRIED ( -- )
   s" a literal 0 kept across top-level steps is refused at the mod" T-LABEL
   ARM
   [: s" 7 0 dup drop mod" evaluate-closed ;] CHECK-RC TTHROWSQ
   DIV-ZERO  s" mod" TOKEN?
   DISARM ;

: CASE-THROW ( -- )
   s" a literal 0 throw code is refused at the throw" T-LABEL
   ARM
   [: s" : ZT1 ( -- ) 0 throw ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   THROW-ZERO  s" throw" TOKEN?
   DISARM ;

: CASE-TOP-THROW ( -- )
   s" a literal 0 throw code at top level is refused at the throw" T-LABEL
   ARM
   [: s" 0 throw" evaluate-closed ;] CHECK-RC TTHROWSQ
   THROW-ZERO  s" throw" TOKEN?
   DISARM ;

\ ---- the controls ------------------------------------------------------------

: CASE-ZERO-DIVIDEND ( -- )
   s" a literal 0 dividend certifies" T-LABEL
   [: s" : ZC1 ( n -- n ) 0 swap / ;" evaluate-closed ;] 0 TTHROWSQ ;

: CASE-TOP-ZERO-DIVIDEND ( -- )
   s" a literal 0 dividend at top level certifies" T-LABEL
   [: s" 0 7 / drop" evaluate-closed ;] 0 TTHROWSQ ;

public

: RUN ( -- )
   T-RESET
   CASE-DIV
   CASE-MOD-CARRIED
   CASE-TOP-DIVMOD
   CASE-TOP-MOD-CARRIED
   CASE-THROW
   CASE-TOP-THROW
   CASE-ZERO-DIVIDEND
   CASE-TOP-ZERO-DIVIDEND
   T-REPORT ;

;package

ZERO-OPERAND-TEST:RUN
