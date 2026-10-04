\ Source grants and native ABI facts share a row without sharing authority.
\ A WHITEBOX-SUITE: this engine's seal pass stood down, so the open-gate cases
\ run first; RUN then seals the checker and runs the cases a sealed image
\ answers. Native builds and graph fixtures cover handover.
require lib/test.f
require lib/string.f
require test/replay-scope.f
require lib/tier.f

package EFFECT-AUTHORITY-TEST

\ These are inspection and declaration boundaries over the actual source owner.
: JUDGE ( ptr u8 n -- n ) CHECK! ;
TRUSTED: ABI ( ptr u8 n -- n ) CHECK-UNJUDGED! ;
TRUSTED: ABI-MIN ( ptr u8 n -- n ) SIG-MIN-IN ;
TRUSTED: SOURCE-MIN ( ptr u8 n -- n ) EFFECT-EXTERNAL-MIN-IN ;
: MIN-LATCH ( -- n ) REC-MIN-IN@ ;
TRUSTED: ENFORCED? ( -- bool ) CHECKER-EFFECT-AUTHORITY:ENFORCED? ;
TRUSTED: ABI-SCOPE ( [ -- ] -- n ) CHECKER-EFFECT-AUTHORITY:SCAN ;
TRUSTED: DECLARE ( ptr u8 n ptr u8 n -- ) CHECKER-USIG-ADD ;
TRUSTED: ABI-ROW ( ptr u8 n ptr u8 n -- )
   CHECKER-RECORD-NAME RES-FALSE RES-FALSE USIG-ADD-AS ;
: SCOPE+ ( -- ) CHECKER-SCOPE-START ;
: SCOPE- ( -- ) CHECKER-SCOPE-DONE ;
\ The scan, PRIM, rollback and recovery cases record rows only the checker holds
\ (CHECK!, CHECK-UNJUDGED!, CHECKER-USIG-ADD): the engine compiles none of them,
\ so compiled code binds nothing to their names (src/core/checker.f "ONE LOOKUP
\ BINDS A NAME"). They run in the check tool's replay scope (test/replay-scope.f),
\ where a name binds over the checker's own records; the live cases below
\ publish real records through the evaluator.
: REPLAY+ ( -- ) REPLAY-SCOPE:OPEN ;
: REPLAY- ( -- ) REPLAY-SCOPE:CLOSE ;
: MULTI+ ( -- ) MULTI-ERR-BEGIN ;
: MULTI- ( -- n ) MULTI-ERR-END ;
TRUSTED: ROW-STATE ( ptr u8 n -- n )
   CHECKER-FIND-ACTIVE-SIG
   FEP-HIT? if FEP @ ER.ACTIVE @ else -1 then ;
TRUSTED: RECOVERY? ( -- bool ) CHECKER-EFFECT-AUTHORITY:RECOVERY-USED? ;
TRUSTED: CERT-SIZE ( -- n ) LOWER-CERT:BYTES nip ;
TRUSTED: DICT-MIN ( ptr u8 n -- n )
   0 xref-search-wl XREF-FLAGS DNAME-MIN-IN-MASK and ;
TRUSTED: EV ( ptr u8 n -- ) evaluate ;
TRUSTED: EV-N ( ptr u8 n -- n ) evaluate ;
\ The gate's state, named by checked bodies: the open gate binds SEALED?'s
\ recorded row, and SEAL carries a primitive row.
: SEALED? ( -- bool ) CHECKER-EFFECT-AUTHORITY:SEALED? ;
: SEAL ( -- ) CHECKER-EFFECT-AUTHORITY:SEAL ;

\ A refusal's code is read from its JSON diagnostic: a mismatch's text render
\ names none.
$1000 constant IO-CAP
create DIAGS IO-CAP allot

: EXPLICIT-IN-ABI ( -- )
   s" n -- n" s" EAUTH-EXPLICIT" DECLARE ;

: THROW-IN-ABI ( -- ) 7329 throw ;

: ABI-DUPLICATE ( -- )
   s" EAUTH-JUDGED ( n -- n )" ABI drop ;

: SCAN-CASES ( -- )
   s" a judged effect grants its own minimum" T-LABEL
   s" EAUTH-JUDGED ( n -- n )" JUDGE -1 T=
   MIN-LATCH 1 T=
   s" EAUTH-JUDGED" SOURCE-MIN 1 T=
   s" EAUTH-JUDGED" CHECKER-RESOLVES? TTRUE
   s" a successful native scan records shape without authority" T-LABEL
   s" EAUTH-ABI ( n -- n )" ABI -1 T=
   MIN-LATCH 0 T=
   s" EAUTH-ABI" ABI-MIN 1 T=
   s" EAUTH-ABI" SOURCE-MIN -1 T=
   s" EAUTH-ABI" CHECKER-RESOLVES? TFALSE
   s" EAUTH-ABI-CALL ( n -- n ) EAUTH-ABI" CHECK-CANDIDATE! 0 T=
   s" EAUTH-ABI-USES ( n -- n ) EAUTH-ABI" ABI -1 T=
   s" EAUTH-ABI-USES" SOURCE-MIN -1 T=
   s" explicit declarations grant authority inside the ABI scope" T-LABEL
   [: EXPLICIT-IN-ABI ;] ABI-SCOPE 0 T=
   ENFORCED? TTRUE
   s" EAUTH-EXPLICIT" SOURCE-MIN 1 T=
   s" EAUTH-EXPLICIT" CHECKER-RESOLVES? TTRUE
   s" EAUTH-EXPLICIT-CALL ( n -- n ) EAUTH-EXPLICIT" CHECK-CANDIDATE! -1 T=
   s" an ABI-scope throw restores enforcement" T-LABEL
   [: THROW-IN-ABI ;] ABI-SCOPE 7329 T=
   ENFORCED? TTRUE
   SCOPE+
   [: ABI-DUPLICATE ;] catch 78 T=
   ENFORCED? TTRUE
   SCOPE-
   s" EAUTH-JUDGED" SOURCE-MIN 1 T=
   s" a failed enforced check leaves no granted row" T-LABEL
   s" EAUTH-BAD ( n -- n ) drop" JUDGE 0 T=
   s" EAUTH-BAD" SOURCE-MIN -1 T=
   MULTI+
   s" EAUTH-MULTI-BAD ( n -- n ) drop" JUDGE 0 T=
   MULTI- 1 T=
   s" EAUTH-MULTI-BAD" ABI-MIN 1 T=
   s" EAUTH-MULTI-BAD" SOURCE-MIN -1 T= ;

: PRIM-CASES ( -- )
   s" PRIM authority uses the PRIM effect, not the unjudged user's shape" T-LABEL
   SCOPE+
   s" -- n" s" dup" ABI-ROW
   s" dup" ABI-MIN 0 T=
   s" dup" SOURCE-MIN 1 T=
   s" EAUTH-PRIM-GOOD ( n -- n n ) dup" CHECK-CANDIDATE! -1 T=
   s" EAUTH-PRIM-BAD ( -- n ) dup" CHECK-CANDIDATE! 0 T=
   s" trusted-only PRIM policy still wins over a user row" T-LABEL
   s" n --" s" set-check" DECLARE
   s" EAUTH-PRIM-TRUST ( -- ) 0 set-check" CHECK-CANDIDATE! 0 T=
   SCOPE-
   s" the checker-state writers stay trusted-only in checked code" T-LABEL
   s" EAUTH-INT-MARK ( -- ) 0 int-mark" CHECK-CANDIDATE! 0 T=
   s" EAUTH-MIN-MARK ( -- ) 0 0 min-in-mark" CHECK-CANDIDATE! 0 T= ;

: ROLLBACK-CASES ( -- )
   s" rollback restores the grant of the previous binding" T-LABEL
   s" EAUTH-ROLL ( n -- n )" JUDGE -1 T=
   SCOPE+
   s" n n -- n" s" EAUTH-ROLL" ABI-ROW
   s" EAUTH-ROLL" ABI-MIN 2 T=
   s" EAUTH-ROLL" SOURCE-MIN -1 T=
   SCOPE-
   s" EAUTH-ROLL" SOURCE-MIN 1 T=
   s" regrowth cannot retain a retired grant" T-LABEL
   SCOPE+
   s" -- n" s" EAUTH-REUSED" DECLARE
   s" EAUTH-REUSED" SOURCE-MIN 0 T=
   SCOPE-
   s" n -- n" s" EAUTH-REUSED" ABI-ROW
   s" EAUTH-REUSED" SOURCE-MIN -1 T=
   s" n -- n" s" EAUTH-REUSED" DECLARE
   s" EAUTH-REUSED" SOURCE-MIN 1 T= ;

: RECOVERY-FACT ( ptr u8 n -- )
   2dup ROW-STATE 2 T=
   2dup SOURCE-MIN -1 T=
   CHECKER-RESOLVES? TFALSE ;

: NESTED-ABI ( -- )
   s" EAUTH-NESTED-ABI ( n -- n )" ABI -1 T=
   s" n -- n" s" EAUTH-REC-EXPLICIT" DECLARE
   7329 throw ;

: RECOVERY-CASES ( -- )
   s" failed declarations support current-run analysis without authority" T-LABEL
   MULTI+
   s" EAUTH-REC-BAD ( n -- n ) drop" JUDGE 0 T=
   MIN-LATCH 0 T=
   s" EAUTH-REC-BAD" RECOVERY-FACT
   s" EAUTH-REC-DIRECT ( n -- n ) EAUTH-REC-BAD" JUDGE -1 T=
   MIN-LATCH 0 T=
   s" EAUTH-REC-DIRECT" RECOVERY-FACT
   s" EAUTH-REC-BRANCH ( n bool -- n ) if EAUTH-REC-DIRECT else 1+ then" JUDGE -1 T=
   s" EAUTH-REC-BRANCH" RECOVERY-FACT
   s" EAUTH-REC-QUOTE ( n -- n ) [: EAUTH-REC-DIRECT ;] execute" JUDGE -1 T=
   s" EAUTH-REC-QUOTE" RECOVERY-FACT
   s" EAUTH-REC-TICK ( n -- n ) ['] EAUTH-REC-QUOTE execute" JUDGE -1 T=
   s" EAUTH-REC-TICK" RECOVERY-FACT
   s" EAUTH-REC-INFER EAUTH-REC-TICK" JUDGE -1 T=
   MIN-LATCH 0 T=
   s" EAUTH-REC-INFER" RECOVERY-FACT

   s" nested candidates and thrown ABI scans restore the enclosing taint" T-LABEL
   RECOVERY? TTRUE
   s" EAUTH-REC-CAND ( n -- n )" CHECK-CANDIDATE! -1 T=
   RECOVERY? TTRUE
   [: NESTED-ABI ;] ABI-SCOPE 7329 T=
   RECOVERY? TTRUE
   ENFORCED? TTRUE
   s" EAUTH-NESTED-ABI" ROW-STATE 1 T=
   s" EAUTH-NESTED-ABI" SOURCE-MIN -1 T=
   s" EAUTH-REC-EXPLICIT" ROW-STATE 1 T=
   s" EAUTH-REC-EXPLICIT" SOURCE-MIN 1 T=

   s" recovery mode keeps unrelated ABI-only and trusted-only calls closed" T-LABEL
   s" EAUTH-REC-RAW ( n -- n )" ABI -1 T=
   s" EAUTH-REC-RAW-CALL ( n -- n ) EAUTH-REC-RAW" JUDGE 0 T=
   s" EAUTH-REC-RAW-TICK ( -- [ n -- n ] ) ['] EAUTH-REC-RAW" JUDGE 0 T=
   s" EAUTH-REC-TRUST-CALL ( -- ) 0 set-check" JUDGE 0 T=
   s" EAUTH-REC-RAW-CALL" RECOVERY-FACT
   s" EAUTH-REC-RAW-TICK" RECOVERY-FACT
   s" EAUTH-REC-TRUST-CALL" RECOVERY-FACT
   MULTI- 4 T=

   s" later collection runs cannot borrow old recovery declarations" T-LABEL
   MULTI+
   s" EAUTH-REC-STALE ( n -- n ) EAUTH-REC-BAD" JUDGE 0 T=
   s" EAUTH-REC-STALE-XT ( n -- n ) ['] EAUTH-REC-TICK execute" JUDGE 0 T=
   MULTI- 2 T=
   s" independent checks do not inherit earlier analysis taint" T-LABEL
   s" EAUTH-REC-GOOD ( n -- n ) 1+" JUDGE -1 T=
   RECOVERY? TFALSE
   s" EAUTH-REC-GOOD" SOURCE-MIN 1 T=

   s" rollback below a run floor admits only the regrown current rows" T-LABEL
   SCOPE+
   s" n -- n" s" EAUTH-REC-REMOVED" DECLARE
   MULTI+ SCOPE-
   s" EAUTH-REC-REGROW ( n -- n ) drop" JUDGE 0 T=
   s" EAUTH-REC-REGROW-CALL ( n -- n ) EAUTH-REC-REGROW" JUDGE -1 T=
   s" EAUTH-REC-REGROW-CALL" RECOVERY-FACT
   SCOPE+
   s" n -- n" s" EAUTH-REC-REGROW" DECLARE
   s" EAUTH-REC-REGROW" ROW-STATE 1 T=
   s" EAUTH-REC-REGROW" SOURCE-MIN 1 T=
   SCOPE-
   s" EAUTH-REC-REGROW" RECOVERY-FACT
   s" EAUTH-REC-RESTORED ( n -- n ) EAUTH-REC-REGROW" JUDGE -1 T=
   s" EAUTH-REC-RESTORED" RECOVERY-FACT
   MULTI- 1 T= ;

\ The real compile hook keeps diagnostic-only dictionary bindings, but neither
\ their publication latch nor their lowering certificate may certify the body.
: RECOVERY-PUBLICATION ( -- )
   s" multi-error publication retains no executable certificate" T-LABEL
   0 TIER:SELECT                    \ check-only replay publishes through the JIT hook
   MULTI+
   s" : EAUTH-REC-LIVE-BAD ( n -- n ) drop ;" EV
   s" EAUTH-REC-LIVE-BAD" RECOVERY-FACT
   s" EAUTH-REC-LIVE-BAD" DICT-MIN 0 T=
   s" : EAUTH-REC-LIVE ( ptr n -- n ) @ EAUTH-REC-LIVE-BAD ;" EV
   s" EAUTH-REC-LIVE" RECOVERY-FACT
   s" EAUTH-REC-LIVE" DICT-MIN 0 T=
   CERT-SIZE LOWER-CERT:HEADER-CELLS cells T=
   s" package EAUTH-REC-ALIAS public EXPORT EAUTH-REC-LIVE ;package" EV
   s" EAUTH-REC-ALIAS:EAUTH-REC-LIVE" RECOVERY-FACT
   s" : EAUTH-REC-EXPORT-CALL ( ptr n -- n ) EAUTH-REC-ALIAS:EAUTH-REC-LIVE ;" EV
   s" EAUTH-REC-EXPORT-CALL" RECOVERY-FACT
   s" EAUTH-REC-EXPORT-CALL" DICT-MIN 0 T=
   MULTI- 1 T=
   s" EAUTH-REC-LIVE-CALL ( ptr n -- n ) EAUTH-REC-LIVE" CHECK-CANDIDATE! 0 T=
   s" EAUTH-REC-ALIAS-CALL ( ptr n -- n ) EAUTH-REC-ALIAS:EAUTH-REC-LIVE" CHECK-CANDIDATE! 0 T=
   1 TIER:SELECT ;

\ The evaluator owns the real publication and redefinition rollback below.
\ The saved check hook is restored even when a definition throws.
variable SAVED-HOOK
TRUSTED: UNCHECKED+ ( -- ) check@ SAVED-HOOK ! 0 set-check ;
TRUSTED: UNCHECKED- ( -- ) SAVED-HOOK @ set-check ;

: PREHOOK-DECLARATIONS ( -- )
   s" : EAUTH-RAW ( n -- n ) ;" EV
   s" TRUSTED: EAUTH-TRUSTED ( n -- n ) ;" EV
   s" defer EAUTH-DEFER ( n -- n )" EV ;

: BAD-REDEFINITION ( -- )
   s" : EAUTH-LIVE ( n -- n ) drop ;" EV ;

: LIVE-CASES ( -- )
   s" real pre-hook native publication preserves explicit declarations" T-LABEL
   1 TIER:SELECT
   UNCHECKED+
   [: PREHOOK-DECLARATIONS ;] catch
   UNCHECKED- 0 T=
   s" EAUTH-RAW" ABI-MIN 1 T=
   s" EAUTH-RAW" SOURCE-MIN -1 T=
   s" EAUTH-TRUSTED" SOURCE-MIN 1 T=
   s" EAUTH-DEFER" SOURCE-MIN 1 T=
   s" package EAUTH-SHADOW private : EAUTH-RAW ( n -- n ) ; public EXPORT EAUTH-RAW ;package" EV
   s" EAUTH-SHADOW:EAUTH-RAW" SOURCE-MIN 1 T=
   s" EAUTH-RAW" SOURCE-MIN -1 T=
   s" package EAUTH-ALIAS public EXPORT EAUTH-RAW ;package" EV
   s" EAUTH-ALIAS:EAUTH-RAW" ABI-MIN 1 T=
   s" EAUTH-ALIAS:EAUTH-RAW" SOURCE-MIN -1 T=
   s" EAUTH-ALIAS-CALL ( n -- n ) EAUTH-ALIAS:EAUTH-RAW" CHECK-CANDIDATE! 0 T=
   s" : EAUTH-LIVE ( n -- n ) ;" EV
   s" 17 EAUTH-LIVE" EV-N 17 T=
   s" undefine EAUTH-LIVE" EV
   [: BAD-REDEFINITION ;] catch 70 T=
   s" EAUTH-LIVE" SOURCE-MIN -1 T=
   s" : EAUTH-LIVE ( n -- n ) ;" EV
   s" EAUTH-LIVE" SOURCE-MIN 1 T=
   s" 23 EAUTH-LIVE" EV-N 23 T= ;

\ ---- the open gate -------------------------------------------------------------
\ FRESH is a pre-hook checker word: the build recorded its row without source
\ authority, and no primitive row stands behind it. An unsealed checker binds
\ that row and checks the caller's body against it, and the row gains nothing.
\ What the gate does not open stays shut: PRIM-CASES runs unsealed too, and a
\ failed declaration's row serves only its own run.
: OPEN-X ( -- ) s" : EAUTH-OPEN-X ( -- n ) FRESH ;" EV ;

: OPEN-LIVE-CASES ( -- )
   s" an unsealed checker binds an internal word's recorded row" T-LABEL
   SEALED? TFALSE
   ['] OPEN-X catch 0 T=
   s" : EAUTH-OPEN-TICK ( -- [ -- n ] ) ['] FRESH ;" EV
   s" EAUTH-OPEN-X" SOURCE-MIN 0 T=
   s" EAUTH-OPEN-X" CHECKER-RESOLVES? TTRUE
   s" and checks the body against that row" T-LABEL
   DIAGS IO-CAP DIAG-BUFFER!  true DIAG-JSON!
   s" EAUTH-OPEN-Y ( -- ) FRESH" CHECK-CANDIDATE! 0 T=
   DIAG-BUFFER$ s\" \"code\":\"E-MISMATCH\"" CONTAINS? TTRUE
   false DIAG-JSON!  DIAG-BUFFER-OFF
   s" binding grants the row no authority" T-LABEL
   s" FRESH" CHECKER-RESOLVES? TFALSE
   s" FRESH" SOURCE-MIN -1 T=
   ENFORCED? TTRUE ;

: OPEN-REPLAY-CASES ( -- )
   PRIM-CASES
   s" an unsealed checker binds no failed declaration after its run" T-LABEL
   MULTI+
   s" EAUTH-OPEN-BAD ( n -- n ) drop" JUDGE 0 T=
   MULTI- 1 T=
   s" EAUTH-OPEN-BAD" RECOVERY-FACT
   s" EAUTH-OPEN-STALE ( n -- n ) EAUTH-OPEN-BAD" CHECK-CANDIDATE! 0 T= ;

\ The same engine sealed, as src/core/internal-mark.f seals a product: the call
\ the open gate bound is refused by name, and every case after this one is the
\ sealed image's answer.
: SEALED-CASES ( -- )
   s" sealing closes the gate" T-LABEL
   SEAL SEALED? TTRUE
   ENFORCED? TTRUE
   DIAGS IO-CAP DIAG-BUFFER!  true DIAG-JSON!
   s" EAUTH-SEALED-X ( -- n ) FRESH" CHECK-CANDIDATE! 0 T=
   DIAG-BUFFER$ s\" \"code\":\"E-CAP-TRUSTED\"" CONTAINS? TTRUE
   false DIAG-JSON!  DIAG-BUFFER-OFF ;

\ ---- copies of a failed declaration's row ------------------------------------------
\ A failed declaration's row serves only its own run, and so does every copy:
\ after that run, and inside a later one, EXPORT refuses the row as uncertified,
\ so no alias exists for the open gate to bind and a caller names nothing (1).
\ The run's own export keeps the recovery fact (RECOVERY-PUBLICATION). Tier 1
\ cannot compile a failed definition, so the case publishes at tier 0 and
\ restores the caller's tier.
: COPY-CASES ( -- )
   s" a failed declaration's row is not exported after its run" T-LABEL
   tier@ {: saved:n :}
   0 TIER:SELECT
   MULTI+
   s" : EAUTH-COPY-BAD ( n -- n ) drop ;" EV
   MULTI- 1 T=
   s" EAUTH-COPY-BAD" RECOVERY-FACT
   [: s" package EAUTH-COPY public EXPORT EAUTH-COPY-BAD ;package" EV ;] catch
   E-EXPORT-UNDEFINED T=
   s" EAUTH-COPY-CALL ( n -- n ) EAUTH-COPY:EAUTH-COPY-BAD" CHECK-CANDIDATE! 1 T=
   s" nor inside a later run" T-LABEL
   MULTI+
   [: s" package EAUTH-COPY-LATER public EXPORT EAUTH-COPY-BAD ;package" EV ;] catch
   E-EXPORT-UNDEFINED T=
   MULTI- 0 T=
   saved TIER:SELECT ;

: RUN ( -- )
   T-RESET
   OPEN-LIVE-CASES
   REPLAY+ OPEN-REPLAY-CASES REPLAY-
   COPY-CASES
   SEALED-CASES
   REPLAY+
   SCAN-CASES PRIM-CASES ROLLBACK-CASES RECOVERY-CASES
   REPLAY-
   LIVE-CASES
   RECOVERY-PUBLICATION
   T-REPORT
   s" effect authority: ok" type cr ;

: ACTION ( -- [ -- ] ) [: RUN ;] ;
ACTION
;package
execute
