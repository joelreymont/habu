\ profile.f - the Wasm feature profile, src/arch/wasm/profile.f, on the product
\ engine.
\
\ Proves the WPROF contract the Wasm backend reads: V1 admits exactly
\ multi-value and saturating-float-to-int and refuses the ten other listed
\ features; a profile is its own identity, so changing any one of its fields
\ makes a different value; the profile is installed once, a read before the
\ install and a second install are both refused, and the refused install leaves
\ the first one in force; and the installed profile reads back section 17.5's
\ layout, the 16/16 arity and the browser ceilings.
\
\ ONE PROCESS, ONE INSTALL. The rows run in the order an engine meets them:
\ the refusals before any install, the install, then everything read through
\ it, and the second install last.

require lib/test.f
require src/habu/stack-abi.f
require src/arch/wasm/profile.f

package WASM-PROFILE-TEST
private

\ ---- identity ----------------------------------------------------------------
\ V1 with one field moved by one: field k counts from the deepest, the
\ declaration order MAKE takes.
: BUMP ( WPROF:profile n -- WPROF:profile )
   {: p:WPROF:profile k:n :}
   p WPROF-PROFILE:UNMAKE
   {: f0:n f1:n f2:n f3:n f4:n f5:n f6:n f7:n f8:n f9:n
      f10:n f11:n f12:n f13:n f14:n f15:n f16:n f17:n :}
   f0 k 0 = if 1+ then
   f1 k 1 = if 1+ then
   f2 k 2 = if 1+ then
   f3 k 3 = if 1+ then
   f4 k 4 = if 1+ then
   f5 k 5 = if 1+ then
   f6 k 6 = if 1+ then
   f7 k 7 = if 1+ then
   f8 k 8 = if 1+ then
   f9 k 9 = if 1+ then
   f10 k 10 = if 1+ then
   f11 k 11 = if 1+ then
   f12 k 12 = if 1+ then
   f13 k 13 = if 1+ then
   f14 k 14 = if 1+ then
   f15 k 15 = if 1+ then
   f16 k 16 = if 1+ then
   f17 k 17 = if 1+ then
   WPROF-PROFILE:MAKE ;

: IDENTITY-CASE ( -- )
   s" a profile equals itself" T-LABEL
   WPROF:V1 WPROF:V1 WPROF-PROFILE:EQ TTRUE
   s" two profiles differing in any one field are distinct values" T-LABEL
   WPROF-PROFILE:CELLS 0 ?do
      WPROF:V1 i BUMP WPROF:V1 WPROF-PROFILE:EQ TFALSE
   loop ;

\ ---- before the install ------------------------------------------------------
: READ-CURRENT ( -- )   WPROF:CURRENT WPROF:V1 WPROF-PROFILE:EQ drop ;
: READ-ADMITS ( -- )    WPROF-FEATURE:MULTI-VALUE WPROF:ADMITS? drop ;
: READ-FIELD ( -- )     WPROF:CTX-BASE drop ;

: ABSENT-CASE ( -- )
   s" the profile cannot be read before the backend installs one" T-LABEL
   [: READ-CURRENT ;] E-WPROF-ABSENT TTHROWSQ
   [: READ-ADMITS ;] E-WPROF-ABSENT TTHROWSQ
   [: READ-FIELD ;] E-WPROF-ABSENT TTHROWSQ ;

\ ---- the admitted features ---------------------------------------------------
: FEATURES-CASE ( -- )
   s" V1 admits multi-value and saturating-float-to-int" T-LABEL
   WPROF-FEATURE:MULTI-VALUE WPROF:ADMITS? TTRUE
   WPROF-FEATURE:SATURATING-FLOAT-TO-INT WPROF:ADMITS? TTRUE
   s" V1 admits none of the other listed features" T-LABEL
   WPROF-FEATURE:SIGN-EXTENSION WPROF:ADMITS? TFALSE
   WPROF-FEATURE:BULK-MEMORY WPROF:ADMITS? TFALSE
   WPROF-FEATURE:MUTABLE-GLOBAL WPROF:ADMITS? TFALSE
   WPROF-FEATURE:REFERENCE-TYPES WPROF:ADMITS? TFALSE
   WPROF-FEATURE:TAIL-CALL WPROF:ADMITS? TFALSE
   WPROF-FEATURE:THREADS WPROF:ADMITS? TFALSE
   WPROF-FEATURE:MEMORY64 WPROF:ADMITS? TFALSE
   WPROF-FEATURE:SIMD WPROF:ADMITS? TFALSE
   WPROF-FEATURE:GC WPROF:ADMITS? TFALSE
   WPROF-FEATURE:EXCEPTIONS WPROF:ADMITS? TFALSE ;

\ ---- the installed numbers ---------------------------------------------------
: LAYOUT-CASE ( -- )
   s" a direct call passes at most 16 input and 16 output lanes" T-LABEL
   WPROF:PARAMS-MAX 16 T=
   WPROF:RESULTS-MAX 16 T=
   s" the regions are section 17.5's, in address order" T-LABEL
   WPROF:CTX-BASE $10000 T=
   WPROF:OUT-BASE $11000 T=
   WPROF:STACK-BASE $21000 T=
   WPROF:DATA-BASE $31000 T=
   s" the data stack holds the native boot stack's cells" T-LABEL
   WPROF:DATA-BASE WPROF:STACK-BASE - STACK-ABI:BOOT-BYTES T=
   s" each context field has its own eight-byte slot inside the context" T-LABEL
   WPROF:CTX-STACK-BASE 0 T=
   WPROF:CTX-STACK-TOP 8 T=
   WPROF:CTX-OUT-LEN 16 T=
   WPROF:CTX-THROW-CODE 24 T=
   WPROF:CTX-FAULT-KIND 32 T=
   WPROF:CTX-FAULT-ADDR 40 T=
   WPROF:CTX-EPOCH 48 T=
   WPROF:CTX-EPOCH 8 + WPROF:CTX-BASE + WPROF:OUT-BASE <= TTRUE
   s" the ceilings are the browser engines' module limits" T-LABEL
   WPROF:LOCALS-MAX 50000 T=
   WPROF:FUNCTIONS-MAX 1000000 T=
   WPROF:BODY-BYTES-MAX 7654321 T=
   WPROF:MODULE-BYTES-MAX 1073741824 T= ;

\ ---- the second install ------------------------------------------------------
: INSTALL-V1 ( -- )      WPROF:V1 WPROF:CURRENT! ;
: INSTALL-OTHER ( -- )   WPROF:V1 0 BUMP WPROF:CURRENT! ;

: ONCE-CASE ( -- )
   s" the installed profile is the one installed" T-LABEL
   WPROF:CURRENT WPROF:V1 WPROF-PROFILE:EQ TTRUE
   s" a second install is refused, even of the same profile" T-LABEL
   [: INSTALL-V1 ;] E-WPROF-INSTALLED TTHROWSQ
   [: INSTALL-OTHER ;] E-WPROF-INSTALLED TTHROWSQ
   s" the refused install leaves the first in force" T-LABEL
   WPROF:CURRENT WPROF:V1 WPROF-PROFILE:EQ TTRUE
   WPROF-FEATURE:MULTI-VALUE WPROF:ADMITS? TTRUE ;

public

: RUN ( -- )
   T-RESET
   IDENTITY-CASE
   ABSENT-CASE
   INSTALL-V1
   FEATURES-CASE
   LAYOUT-CASE
   ONCE-CASE
   T-REPORT ;

;package

WASM-PROFILE-TEST:RUN
