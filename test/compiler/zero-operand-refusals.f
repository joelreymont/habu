\ zero-operand-refusals.f - rejected programs for "a literal 0 is rejected
\ where it reaches an input the callee always divides by or throws": the
\ engine's `/`, `mod`, `/mod` and `throw`, and a word that divides by or throws
\ its input on every call that returns, through another such word too. The
\ rejection names the call token, in a body and at top level, and a literal's
\ value rides through stack words in a body and across top-level steps.
\ Controls: a literal 0 dividend certifies, and a word that tests the input
\ first, divides in a branch or is called through a `defer` takes any value.
\ Cases run through `evaluate-closed`, the path `bin/hb --load` takes; a
\ rejection is a throw.

require src/lib/test/assert.f
require src/lib/string.f
require src/lib/fmath.f

package ZERO-OPERAND-TEST

private


: ARM ( -- )   \ capture the next diagnostic as JSON instead of printing it
   REJECTION:CAPTURE  true REJECTION:JSON ! ;

: DISARM ( -- )
   false REJECTION:JSON !  REJECTION:BUFFER-OFF ;

: HAS? ( str -- )
   REJECTION:BUFFER$ swap STR:CONTAINS? TTRUE ;

: TOKEN? ( str -- )
   SB-RESET
   S\" \"token\":\"" SB-APPEND  SB-APPEND  34 SB-APPEND-C
   SB$ HAS? ;

: DIV-ZERO ( -- )
   S\" \"code\":\"E-DIV-ZERO\"" HAS? ;

: THROW-ZERO ( -- )
   S\" \"code\":\"E-THROW-ZERO\"" HAS? ;

\ ---- the callees the cases call ----------------------------------------------

public   \ `evaluate-closed` text calls them qualified

: HALVE ( n n -- n ) / ;
: H2 ( n n -- n ) HALVE ;
: RDIV ( n n -- n ) swap / ;
: FAIL ( n -- ) throw ;
: SAFE/ ( n n -- n ) dup 0= IF 2drop 0 EXIT THEN / ;
: ?FAIL ( n -- ) dup 0<> IF throw THEN drop ;
defer DV ( n n -- n )

private

: DV-HALVE ( -- ) ['] HALVE is DV ;
DV-HALVE

\ ---- the refusals ------------------------------------------------------------

: CASE-DIV ( -- )
   s" a literal 0 divisor of / is refused at the /" T-LABEL
   ARM
   [: s" : ZD1 ( n -- n ) 0 / ;" evaluate-closed ;] 70 TTHROWSQ
   DIV-ZERO  s" /" TOKEN?
   DISARM ;

: CASE-MOD-CARRIED ( -- )
   s" a literal 0 carried by stack words is refused at the mod" T-LABEL
   ARM
   [: s" : ZD2 ( n -- n ) 0 dup drop mod ;" evaluate-closed ;] 70 TTHROWSQ
   DIV-ZERO  s" mod" TOKEN?
   DISARM ;

: CASE-TOP-DIVMOD ( -- )
   s" a literal 0 divisor at top level is refused at the /mod" T-LABEL
   ARM
   [: s" 7 0 /mod" evaluate-closed ;] 70 TTHROWSQ
   DIV-ZERO  s" /mod" TOKEN?
   DISARM ;

\ `dup` and `drop` each load, step and save the interpret row, so the literal
\ reaches `mod` only if its value survives the saves.
: CASE-TOP-MOD-CARRIED ( -- )
   s" a literal 0 kept across top-level steps is refused at the mod" T-LABEL
   ARM
   [: s" 7 0 dup drop mod" evaluate-closed ;] 70 TTHROWSQ
   DIV-ZERO  s" mod" TOKEN?
   DISARM ;

: CASE-THROW ( -- )
   s" a literal 0 throw code is refused at the throw" T-LABEL
   ARM
   [: s" : ZT1 ( -- ) 0 throw ;" evaluate-closed ;] 70 TTHROWSQ
   THROW-ZERO  s" throw" TOKEN?
   DISARM ;

: CASE-TOP-THROW ( -- )
   s" a literal 0 throw code at top level is refused at the throw" T-LABEL
   ARM
   [: s" 0 throw" evaluate-closed ;] 70 TTHROWSQ
   THROW-ZERO  s" throw" TOKEN?
   DISARM ;

\ A literal 0 at an input a word divides by or throws before anything takes it.
: CASE-CALL-TOP ( -- )
   s" a literal 0 at HALVE's divisor at top level is rejected at the call" T-LABEL
   ARM
   [: s" 7 0 ZERO-OPERAND-TEST:HALVE" evaluate-closed ;] 70 TTHROWSQ
   DIV-ZERO  s" ZERO-OPERAND-TEST:HALVE" TOKEN?
   s" `ZERO-OPERAND-TEST:HALVE` divides by its top input" HAS?
   DISARM ;

: CASE-CALL-BODY ( -- )
   s" a literal 0 at HALVE's divisor in a body is rejected at the call" T-LABEL
   ARM
   [: s" : ZH ( -- n ) 7 0 ZERO-OPERAND-TEST:HALVE ;" evaluate-closed ;] 70 TTHROWSQ
   DIV-ZERO  s" ZERO-OPERAND-TEST:HALVE" TOKEN?
   DISARM ;

: CASE-CALL-LIB ( -- )
   s" a literal 0 at FMATH:FLOOR/'s divisor is rejected at the call" T-LABEL
   ARM
   [: s" 7 0 FMATH:FLOOR/" evaluate-closed ;] 70 TTHROWSQ
   DIV-ZERO  s" FMATH:FLOOR/" TOKEN?
   DISARM ;

: CASE-CALL-NESTED ( -- )
   s" a literal 0 at H2's input HALVE divides by is rejected at the call" T-LABEL
   ARM
   [: s" 7 0 ZERO-OPERAND-TEST:H2" evaluate-closed ;] 70 TTHROWSQ
   DIV-ZERO  s" ZERO-OPERAND-TEST:H2" TOKEN?
   DISARM ;

: CASE-CALL-BELOW ( -- )
   s" a literal 0 at RDIV's divisor below the top is rejected at the call" T-LABEL
   ARM
   [: s" 0 5 ZERO-OPERAND-TEST:RDIV" evaluate-closed ;] 70 TTHROWSQ
   DIV-ZERO  s" ZERO-OPERAND-TEST:RDIV" TOKEN?
   s" input 1 below the top" HAS?
   DISARM ;

: CASE-CALL-THROW ( -- )
   s" a literal 0 at FAIL's thrown input is rejected at the call" T-LABEL
   ARM
   [: s" 0 ZERO-OPERAND-TEST:FAIL" evaluate-closed ;] 70 TTHROWSQ
   THROW-ZERO  s" ZERO-OPERAND-TEST:FAIL" TOKEN?
   DISARM ;

\ ---- the controls ------------------------------------------------------------

: CASE-ZERO-DIVIDEND ( -- )
   s" a literal 0 dividend certifies" T-LABEL
   [: s" : ZC1 ( n -- n ) 0 swap / ;" evaluate-closed ;] 0 TTHROWSQ ;

: CASE-TOP-ZERO-DIVIDEND ( -- )
   s" a literal 0 dividend at top level certifies" T-LABEL
   [: s" 0 7 / drop" evaluate-closed ;] 0 TTHROWSQ ;

: CASE-GUARDED-DIV ( -- )
   s" a literal 0 at SAFE/, which tests it first, certifies" T-LABEL
   [: s" 7 0 ZERO-OPERAND-TEST:SAFE/ drop" evaluate-closed ;] 0 TTHROWSQ ;

: CASE-BRANCH-THROW ( -- )
   s" a literal 0 at ?FAIL, which throws in a branch, certifies" T-LABEL
   [: s" 0 ZERO-OPERAND-TEST:?FAIL" evaluate-closed ;] 0 TTHROWSQ ;

: CASE-DEFER ( -- )
   s" a literal 0 at a defer holding HALVE certifies" T-LABEL
   [: s" : ZDV ( -- n ) 7 0 ZERO-OPERAND-TEST:DV ;" evaluate-closed ;] 0 TTHROWSQ ;

public

: RUN ( -- )
   T-RESET
   CASE-DIV
   CASE-MOD-CARRIED
   CASE-TOP-DIVMOD
   CASE-TOP-MOD-CARRIED
   CASE-THROW
   CASE-TOP-THROW
   CASE-CALL-TOP
   CASE-CALL-BODY
   CASE-CALL-LIB
   CASE-CALL-NESTED
   CASE-CALL-BELOW
   CASE-CALL-THROW
   CASE-ZERO-DIVIDEND
   CASE-TOP-ZERO-DIVIDEND
   CASE-GUARDED-DIV
   CASE-BRANCH-THROW
   CASE-DEFER
   T-REPORT ;

;package

ZERO-OPERAND-TEST:RUN
