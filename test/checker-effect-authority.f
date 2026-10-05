\ Source grants and native ABI facts share a row without sharing authority.
\ A WHITEBOX-SUITE: this engine's seal pass stood down, so the open-gate cases
\ run first, after the tier-0 declarations they call; RUN then seals the checker
\ and runs the cases a sealed image answers. Native builds and graph fixtures
\ cover handover.
require lib/test.f
require lib/errors.f
require lib/string.f
require test/replay-scope.f
require lib/tier.f
require src/compiler/native/backend.f
require src/compiler/native/checker-owner.f
require src/habu/verify-source.f

package EFFECT-AUTHORITY-TEST

\ These are inspection and declaration boundaries over the actual source owner.
: JUDGE ( ptr u8 n -- n ) CHECK! ;
: SOURCE-MIN ( ptr u8 n -- n ) EFFECT-EXTERNAL-MIN-IN ;
: MIN-LATCH ( -- n ) REC-MIN-IN@ ;
: ENFORCED? ( -- bool ) CHECKER-EFFECT-AUTHORITY:ENFORCED? ;
: ABI-ROW ( ptr u8 n ptr u8 n -- )
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
: ROW-STATE ( ptr u8 n -- n )
   CHECKER-FIND-ACTIVE-SIG
   FEP-HIT? if FEP @ ER.ACTIVE @ else -1 then ;
TRUSTED: CERT-SIZE ( -- n ) LOWER-CERT:BYTES nip ;
TRUSTED: DICT-MIN ( ptr u8 n -- n )
   0 xref-search-wl XREF-FLAGS DNAME-MIN-IN-MASK and ;
\ The gate's state, named by checked bodies: the open gate binds SEALED?'s
\ recorded row, and SEAL carries a primitive row.
: SEALED? ( -- bool ) CHECKER-EFFECT-AUTHORITY:SEALED? ;
: SEAL ( -- ) CHECKER-EFFECT-AUTHORITY:SEAL ;

\ A refusal's code is read from its JSON diagnostic: a mismatch's text render
\ names none.
$1000 constant IO-CAP
create DIAGS IO-CAP allot

: EXPLICIT-IN-ABI ( -- )
   s" n -- n" s" EAUTH-EXPLICIT" CHECKER-USIG-ADD ;

: THROW-IN-ABI ( -- ) 7329 throw ;

: ABI-DUPLICATE ( -- )
   s" EAUTH-JUDGED ( n -- n )" CHECK-UNJUDGED! drop ;

: SCAN-CASES ( -- )
   s" a judged effect grants its own minimum" T-LABEL
   s" EAUTH-JUDGED ( n -- n )" JUDGE -1 T=
   MIN-LATCH 1 T=
   s" EAUTH-JUDGED" SOURCE-MIN 1 T=
   s" EAUTH-JUDGED" CHECKER-RESOLVES? TTRUE
   s" a successful native scan records shape without authority" T-LABEL
   s" EAUTH-ABI ( n -- n )" CHECK-UNJUDGED! -1 T=
   MIN-LATCH 0 T=
   s" EAUTH-ABI" SIG-MIN-IN 1 T=
   s" EAUTH-ABI" SOURCE-MIN -1 T=
   s" EAUTH-ABI" CHECKER-RESOLVES? TFALSE
   s" EAUTH-ABI-CALL ( n -- n ) EAUTH-ABI" CHECK-CANDIDATE! 0 T=
   s" EAUTH-ABI-USES ( n -- n ) EAUTH-ABI" CHECK-UNJUDGED! -1 T=
   s" EAUTH-ABI-USES" SOURCE-MIN -1 T=
   s" explicit declarations grant authority inside the ABI scope" T-LABEL
   [: EXPLICIT-IN-ABI ;] CHECKER-EFFECT-AUTHORITY:SCAN 0 T=
   ENFORCED? TTRUE
   s" EAUTH-EXPLICIT" SOURCE-MIN 1 T=
   s" EAUTH-EXPLICIT" CHECKER-RESOLVES? TTRUE
   s" EAUTH-EXPLICIT-CALL ( n -- n ) EAUTH-EXPLICIT" CHECK-CANDIDATE! -1 T=
   s" an ABI-scope throw restores enforcement" T-LABEL
   [: THROW-IN-ABI ;] CHECKER-EFFECT-AUTHORITY:SCAN 7329 T=
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
   s" EAUTH-MULTI-BAD" SIG-MIN-IN 1 T=
   s" EAUTH-MULTI-BAD" SOURCE-MIN -1 T= ;

: PRIM-CASES ( -- )
   s" PRIM authority uses the PRIM effect, not the unjudged user's shape" T-LABEL
   SCOPE+
   s" -- n" s" dup" ABI-ROW
   s" dup" SIG-MIN-IN 0 T=
   s" dup" SOURCE-MIN 1 T=
   s" EAUTH-PRIM-GOOD ( n -- n n ) dup" CHECK-CANDIDATE! -1 T=
   s" EAUTH-PRIM-BAD ( -- n ) dup" CHECK-CANDIDATE! 0 T=
   s" trusted-only PRIM policy still wins over a user row" T-LABEL
   s" n --" s" set-check" CHECKER-USIG-ADD
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
   s" EAUTH-ROLL" SIG-MIN-IN 2 T=
   s" EAUTH-ROLL" SOURCE-MIN -1 T=
   SCOPE-
   s" EAUTH-ROLL" SOURCE-MIN 1 T=
   s" regrowth cannot retain a retired grant" T-LABEL
   SCOPE+
   s" -- n" s" EAUTH-REUSED" CHECKER-USIG-ADD
   s" EAUTH-REUSED" SOURCE-MIN 0 T=
   SCOPE-
   s" n -- n" s" EAUTH-REUSED" ABI-ROW
   s" EAUTH-REUSED" SOURCE-MIN -1 T=
   s" n -- n" s" EAUTH-REUSED" CHECKER-USIG-ADD
   s" EAUTH-REUSED" SOURCE-MIN 1 T= ;

: RECOVERY-FACT ( ptr u8 n -- )
   2dup ROW-STATE 2 T=
   2dup SOURCE-MIN -1 T=
   CHECKER-RESOLVES? TFALSE ;

: NESTED-ABI ( -- )
   s" EAUTH-NESTED-ABI ( n -- n )" CHECK-UNJUDGED! -1 T=
   s" n -- n" s" EAUTH-REC-EXPLICIT" CHECKER-USIG-ADD
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
   CHECKER-EFFECT-AUTHORITY:RECOVERY-USED? TTRUE
   s" EAUTH-REC-CAND ( n -- n )" CHECK-CANDIDATE! -1 T=
   CHECKER-EFFECT-AUTHORITY:RECOVERY-USED? TTRUE
   [: NESTED-ABI ;] CHECKER-EFFECT-AUTHORITY:SCAN 7329 T=
   CHECKER-EFFECT-AUTHORITY:RECOVERY-USED? TTRUE
   ENFORCED? TTRUE
   s" EAUTH-NESTED-ABI" ROW-STATE 1 T=
   s" EAUTH-NESTED-ABI" SOURCE-MIN -1 T=
   s" EAUTH-REC-EXPLICIT" ROW-STATE 1 T=
   s" EAUTH-REC-EXPLICIT" SOURCE-MIN 1 T=

   s" recovery mode keeps unrelated ABI-only and trusted-only calls closed" T-LABEL
   s" EAUTH-REC-RAW ( n -- n )" CHECK-UNJUDGED! -1 T=
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
   CHECKER-EFFECT-AUTHORITY:RECOVERY-USED? TFALSE
   s" EAUTH-REC-GOOD" SOURCE-MIN 1 T=

   s" rollback below a run floor admits only the regrown current rows" T-LABEL
   SCOPE+
   s" n -- n" s" EAUTH-REC-REMOVED" CHECKER-USIG-ADD
   MULTI+ SCOPE-
   s" EAUTH-REC-REGROW ( n -- n ) drop" JUDGE 0 T=
   s" EAUTH-REC-REGROW-CALL ( n -- n ) EAUTH-REC-REGROW" JUDGE -1 T=
   s" EAUTH-REC-REGROW-CALL" RECOVERY-FACT
   SCOPE+
   s" n -- n" s" EAUTH-REC-REGROW" CHECKER-USIG-ADD
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
   s" : EAUTH-REC-LIVE-BAD ( n -- n ) drop ;" evaluate-closed
   s" EAUTH-REC-LIVE-BAD" RECOVERY-FACT
   s" EAUTH-REC-LIVE-BAD" DICT-MIN 0 T=
   s" : EAUTH-REC-LIVE ( ptr n -- n ) @ EAUTH-REC-LIVE-BAD ;" evaluate-closed
   s" EAUTH-REC-LIVE" RECOVERY-FACT
   s" EAUTH-REC-LIVE" DICT-MIN 0 T=
   CERT-SIZE LOWER-CERT:HEADER-CELLS cells T=
   s" package EAUTH-REC-ALIAS public EXPORT EAUTH-REC-LIVE ;package" evaluate-closed
   s" EAUTH-REC-ALIAS:EAUTH-REC-LIVE" RECOVERY-FACT
   s" : EAUTH-REC-EXPORT-CALL ( ptr n -- n ) EAUTH-REC-ALIAS:EAUTH-REC-LIVE ;" evaluate-closed
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
   s" : EAUTH-RAW ( n -- n ) ;" evaluate-closed
   s" TRUSTED: EAUTH-TRUSTED ( n -- n ) ;" evaluate-closed
   s" defer EAUTH-DEFER ( n -- n )" evaluate-closed ;

: BAD-REDEFINITION ( -- )
   s" : EAUTH-LIVE ( n -- n ) drop ;" evaluate-closed ;

: LIVE-CASES ( -- )
   s" real pre-hook native publication preserves explicit declarations" T-LABEL
   1 TIER:SELECT
   UNCHECKED+
   [: PREHOOK-DECLARATIONS ;] catch
   UNCHECKED- 0 T=
   s" EAUTH-RAW" SIG-MIN-IN 1 T=
   s" EAUTH-RAW" SOURCE-MIN -1 T=
   s" EAUTH-TRUSTED" SOURCE-MIN 1 T=
   s" EAUTH-DEFER" SOURCE-MIN 1 T=
   s" package EAUTH-SHADOW private : EAUTH-RAW ( n -- n ) ; public EXPORT EAUTH-RAW ;package"
   evaluate-closed
   s" EAUTH-SHADOW:EAUTH-RAW" SOURCE-MIN 1 T=
   s" EAUTH-RAW" SOURCE-MIN -1 T=
   s" package EAUTH-ALIAS public EXPORT EAUTH-RAW ;package" evaluate-closed
   s" EAUTH-ALIAS:EAUTH-RAW" SIG-MIN-IN 1 T=
   s" EAUTH-ALIAS:EAUTH-RAW" SOURCE-MIN -1 T=
   s" EAUTH-ALIAS-CALL ( n -- n ) EAUTH-ALIAS:EAUTH-RAW" CHECK-CANDIDATE! 0 T=
   s" : EAUTH-LIVE ( n -- n ) ;" evaluate-closed
   s" 17 EAUTH-LIVE" TEST-EVAL:N 17 T=
   s" undefine EAUTH-LIVE" evaluate-closed
   [: BAD-REDEFINITION ;] catch 70 T=
   s" EAUTH-LIVE" SOURCE-MIN -1 T=
   s" : EAUTH-LIVE ( n -- n ) ;" evaluate-closed
   s" EAUTH-LIVE" SOURCE-MIN 1 T=
   s" 23 EAUTH-LIVE" TEST-EVAL:N 23 T= ;

\ ---- a hook-less definition's declaration is its row --------------------------
\ With the hook cell empty nothing judges a body, and the row a definition leaves
\ is its declaration: active, without authority, and only where no live row
\ states the symbol. A TRUSTED: declaration is the assertion and keeps its
\ authority. Tier 0 runs no scan at all; RUN calls these cases first, because
\ the open-gate and sealed cases call EAUTH-DECL.
: TIER0-DECLARATIONS ( -- )
   s" : EAUTH-DECL ( n -- n ) 1+ ;" evaluate-closed
   s" TRUSTED: EAUTH-DECL-TRUSTED ( n -- n ) 1+ ;" evaluate-closed
   s" : EAUTH-DECL-BAD ( n -- no-such-type ) ;" evaluate-closed
   s" : EAUTH-DECL-WIDTH ( n -- n ) ;" evaluate-closed
   s" undefine EAUTH-DECL-WIDTH" evaluate-closed
   s" : EAUTH-DECL-WIDTH ( n n -- n ) + ;" evaluate-closed ;

: TIER0-CASES ( -- )
   s" a hook-less tier-0 definition records its declaration without authority" T-LABEL
   tier@ {: saved:n :}
   0 TIER:SELECT
   UNCHECKED+
   [: TIER0-DECLARATIONS ;] catch
   UNCHECKED- 0 T=
   saved TIER:SELECT
   s" EAUTH-DECL" ROW-STATE 1 T=
   s" EAUTH-DECL" SIG-MIN-IN 1 T=
   s" EAUTH-DECL" SOURCE-MIN -1 T=
   s" EAUTH-DECL" CHECKER-RESOLVES? TFALSE
   s" EAUTH-DECL" EFFECT-QUERY TTRUE
   s" its TRUSTED: twin carries authority" T-LABEL
   s" EAUTH-DECL-TRUSTED" SOURCE-MIN 1 T=
   s" a declaration the checker cannot parse records nothing" T-LABEL
   s" EAUTH-DECL-BAD" EFFECT-QUERY TFALSE
   s" a redefinition after undefine records its own declaration" T-LABEL
   s" EAUTH-DECL-WIDTH" SIG-MIN-IN 2 T= ;

\ A tier-1 scan with the hook cell empty fills the tape and reports its verdict
\ without enforcing it. A body it refuses now compiles against the declaration,
\ with the reason reported; a body the elaborator then refuses takes its own
\ declared row with it, and only that: after `undefine` the name's retired row
\ and the rows recorded since stay (EAUTH-GEN). A definition that recorded no
\ row retracts nothing - not the global twin its name resolves to while it is
\ pending (EAUTH-TWIN: the package's declaration does not parse, and the twin's
\ arity compiled it), and not its own name's symbol, which the recorder interned
\ before it refused the declaration and the bare name then binds (EAUTH-T1-NO-ROW,
\ EAUTH-T1-SCHEME): nothing answers their arity, and the refusal is the check
\ hook's reject status, which a catch receives. Nor a row recorded before the
\ definition began, even of its own name (EAUTH-PRIOR): CHECK! records it with
\ no definition, the replay scope binds the name to it, and the refused body
\ compiles against its arity. That row and the row after it (EAUTH-PRIOR-LATER)
\ stay; cutting the live rows of the symbol the name binds took both.
: REFUSED-SCAN ( -- ) s" : EAUTH-T1-REFUSED ( n -- n ) 0= ;" evaluate-closed ;
: REFUSED-MULTI ( -- ) s" : EAUTH-T1-MULTI ( n -- n ) 0= ;" evaluate-closed ;
: REFUSED-BODY ( -- ) s" : EAUTH-T1-RETRACT ( n -- n ) drop ;" evaluate-closed ;
: RETRY-BODY ( -- ) s" : EAUTH-T1-RETRACT ( n n -- n ) + ;" evaluate-closed ;
: TWIN-ROWS ( -- )
   s" : EAUTH-TWIN ( n -- n ) ;" evaluate-closed
   s" : EAUTH-TWIN-LATER ( n -- n ) ;" evaluate-closed ;
: TWIN-FAIL ( -- )
   s" package EAUTH-TWIN-PKG : EAUTH-TWIN ( n -- no-such-type ) drop drop ; ;package" evaluate-closed ;
: GEN-ROWS ( -- )
   s" : EAUTH-GEN ( n -- n ) ;" evaluate-closed
   s" : EAUTH-GEN-MID ( n -- n ) ;" evaluate-closed
   s" undefine EAUTH-GEN" evaluate-closed
   s" : EAUTH-GEN-LATER ( n -- n ) ;" evaluate-closed ;
: GEN-FAIL ( -- ) s" : EAUTH-GEN ( n -- n ) drop drop ;" evaluate-closed ;
70 constant RC-REJECT                \ the check hook's reject status (src/core/check-hook.f CHECK-RC)
: NO-ROW-FAIL ( -- ) s" : EAUTH-T1-NO-ROW ( n -- no-such-type ) ;" evaluate-closed ;
: SCHEME-FAIL ( -- ) s" : EAUTH-T1-SCHEME ( forall<p,[ n -- n ]> -- ) ;" evaluate-closed ;
: AFTER-ROW ( -- ) s" : EAUTH-T1-AFTER ( n -- n ) ;" evaluate-closed ;
: PRIOR-LATER ( -- ) s" : EAUTH-PRIOR-LATER ( n -- n ) ;" evaluate-closed ;
: PRIOR-FAIL ( -- ) s" : EAUTH-PRIOR ( n -- no-such-type ) drop drop ;" evaluate-closed ;

\ A callback cannot scan while a definition compiles, so a refused definition
\ takes exactly its own row and keeps the rows before (compiler.f WORK and
\ RETRACT): the observer's CHECK! of EAUTH-CB-LATER while EAUTH-CB-OUTER
\ compiles hook-less is refused E-NCOMP-STATE and records nothing, the
\ observer's throw refuses the definition, and EAUTH-CB-BEFORE, recorded
\ before, stays. The observer acts only while CB-ARMED is set, so later
\ definitions compile.
variable CB-ARMED
: CB-OBSERVE ( n IR-CTX:ctx IR-BUILD:module -- )
   {: idx:n c:IR-CTX:ctx m:IR-BUILD:module :}
   CB-ARMED @ 0= if exit then
   [: s" EAUTH-CB-LATER ( n -- n )" JUDGE drop ;] E-NCOMP-STATE TTHROWSQ
   7329 throw ;
: CB-FAIL ( -- ) s" : EAUTH-CB-OUTER ( n -- n ) 1 + ;" evaluate-closed ;

: TIER1-DECLARED-CASES ( -- )
   s" a scan-refused hook-less tier-1 definition compiles against its declaration" T-LABEL
   1 TIER:SELECT
   DIAGS IO-CAP DIAG-BUFFER!  true DIAG-JSON!
   UNCHECKED+ ['] REFUSED-SCAN catch UNCHECKED- 0 T=
   DIAG-BUFFER$ s\" \"code\":\"E-MISMATCH\"" CONTAINS? TTRUE
   false DIAG-JSON!  DIAG-BUFFER-OFF
   s" EAUTH-T1-REFUSED" ROW-STATE 1 T=
   s" EAUTH-T1-REFUSED" SIG-MIN-IN 1 T=
   s" EAUTH-T1-REFUSED" SOURCE-MIN -1 T=
   s" a multi-error scan keeps its recovery row" T-LABEL
   UNCHECKED+ MULTI+ ['] REFUSED-MULTI catch MULTI- UNCHECKED-
   1 T=  0 T=
   s" EAUTH-T1-MULTI" ROW-STATE 2 T=
   s" a body the elaborator refuses takes its declared row with it" T-LABEL
   UNCHECKED+ ['] REFUSED-BODY catch UNCHECKED- E-NELAB-ARITY T=
   s" EAUTH-T1-RETRACT" EFFECT-QUERY TFALSE
   UNCHECKED+ ['] RETRY-BODY catch UNCHECKED- 0 T=
   s" EAUTH-T1-RETRACT" SIG-MIN-IN 2 T=
   s" a definition with no row of its own retracts no twin's row" T-LABEL
   UNCHECKED+ ['] TWIN-ROWS catch UNCHECKED- 0 T=
   UNCHECKED+ ['] TWIN-FAIL catch UNCHECKED- E-NELAB-UNDER T=
   s" EAUTH-TWIN" SIG-MIN-IN 1 T=
   s" EAUTH-TWIN-LATER" SIG-MIN-IN 1 T=
   s" a failed redefinition after undefine retracts only its own row" T-LABEL
   UNCHECKED+ ['] GEN-ROWS catch UNCHECKED- 0 T=
   UNCHECKED+ ['] GEN-FAIL catch UNCHECKED- E-NELAB-UNDER T=
   s" EAUTH-GEN" EFFECT-QUERY TFALSE
   s" EAUTH-GEN-MID" SIG-MIN-IN 1 T=
   s" EAUTH-GEN-LATER" SIG-MIN-IN 1 T=
   s" an unrecordable declaration is refused catchably and retracts nothing" T-LABEL
   UNCHECKED+ ['] NO-ROW-FAIL catch UNCHECKED- RC-REJECT T=
   UNCHECKED+ ['] SCHEME-FAIL catch UNCHECKED- RC-REJECT T=
   s" EAUTH-T1-NO-ROW" EFFECT-QUERY TFALSE
   s" EAUTH-T1-SCHEME" EFFECT-QUERY TFALSE
   s" EAUTH-GEN-LATER" SIG-MIN-IN 1 T=
   UNCHECKED+ ['] AFTER-ROW catch UNCHECKED- 0 T=
   s" EAUTH-T1-AFTER" SIG-MIN-IN 1 T=
   s" a refused definition keeps its name's row recorded before it began" T-LABEL
   REPLAY+
   s" EAUTH-PRIOR ( n -- n ) 1 +" JUDGE -1 T=
   UNCHECKED+ ['] PRIOR-LATER catch UNCHECKED- 0 T=
   UNCHECKED+ ['] PRIOR-FAIL catch UNCHECKED- E-NELAB-UNDER T=
   s" EAUTH-PRIOR" SIG-MIN-IN 1 T=
   s" EAUTH-PRIOR-LATER" SIG-MIN-IN 1 T=
   s" a callback's scan while a definition compiles is refused and records nothing" T-LABEL
   ['] CB-OBSERVE NBACK:OBSERVE!
   s" EAUTH-CB-BEFORE ( n -- n )" JUDGE -1 T=
   1 CB-ARMED !
   UNCHECKED+ ['] CB-FAIL catch UNCHECKED- 7329 T=
   0 CB-ARMED !
   s" EAUTH-CB-OUTER" EFFECT-QUERY TFALSE
   s" EAUTH-CB-LATER" EFFECT-QUERY TFALSE
   s" EAUTH-CB-BEFORE" SIG-MIN-IN 1 T=
   REPLAY- ;

\ A refused TRUSTED: definition goes back to the mark STAGE took, as a hook-less
\ one does (compiler.f RETRACT). Inside a package it cuts neither the global
\ twin its name resolves to nor the row recorded after it (EAUTH-TTWIN,
\ EAUTH-TTWIN-LATER). A declaration that
\ does not parse records no row, so the refusal is its own
\ E-BAD-STORED-SIGNATURE, which a catch receives with the hook installed or the
\ cell empty, and the name is free for the next definition (EAUTH-TBAD).
: TTWIN-ROWS ( -- )
   s" : EAUTH-TTWIN ( n -- n ) 1 + ;" evaluate-closed
   s" : EAUTH-TTWIN-LATER ( n -- n ) 2 + ;" evaluate-closed ;
: TTWIN-FAIL ( -- )
   s" package EAUTH-TTWIN-PKG TRUSTED: EAUTH-TTWIN ( n -- no-such-type ) drop drop ; ;package" evaluate-closed ;
: TBAD-FAIL ( -- ) s" TRUSTED: EAUTH-TBAD ( n -- no-such-type ) drop drop ;" evaluate-closed ;
: TBAD-RETRY ( -- ) s" : EAUTH-TBAD ( n -- n ) 1 + ;" evaluate-closed ;
: TBAD-HOOKLESS ( -- ) s" TRUSTED: EAUTH-TBAD-NH ( n -- no-such-type ) drop drop ;" evaluate-closed ;

: TIER1-TRUSTED-CASES ( -- )
   1 TIER:SELECT
   s" a refused TRUSTED: definition retracts no twin's row" T-LABEL
   ['] TTWIN-ROWS catch 0 T=
   ['] TTWIN-FAIL catch E-BAD-STORED-SIGNATURE T=
   s" EAUTH-TTWIN" SIG-MIN-IN 1 T=
   s" EAUTH-TTWIN-LATER" SIG-MIN-IN 1 T=
   s" a TRUSTED: declaration that records no row is refused catchably" T-LABEL
   ['] TBAD-FAIL catch E-BAD-STORED-SIGNATURE T=
   s" EAUTH-TBAD" EFFECT-QUERY TFALSE
   ['] TBAD-RETRY catch 0 T=
   s" EAUTH-TBAD" SIG-MIN-IN 1 T=
   s" and so is one with the hook cell empty" T-LABEL
   UNCHECKED+ ['] TBAD-HOOKLESS catch UNCHECKED- E-BAD-STORED-SIGNATURE T=
   s" EAUTH-TBAD-NH" EFFECT-QUERY TFALSE ;

\ ---- the compile window --------------------------------------------------------
\ From the end of the compiler's scan to publication's last callback the checker
\ refuses every scan and store write with E-NCOMP-STATE (compiler.f WORK), so a
\ refused definition's cut takes exactly its own rows, on the certified path and
\ the hook-less one. An observer's generates: row, recorded there and cut with
\ the definition, would leave the definer's CREATES naming whatever record later
\ definitions put where the cut one stood: VERIFY's predictions for a word it
\ defines would follow that record, and a fresh generates: would be refused as
\ a second row. generates: is a top-level word no checked body names, so the
\ observer calls it through a typed cell. Each observer acts once, when armed.
TYPED-VARIABLE GENERATES-XT [ -- ]
' generates: GENERATES-XT !
variable GENERATES-ARMED
: GENERATES-OBSERVE ( n IR-CTX:ctx IR-BUILD:module -- )
   {: idx:n c:IR-CTX:ctx m:IR-BUILD:module :}
   GENERATES-ARMED @ 0= if exit then
   0 GENERATES-ARMED !
   [: GENERATES-XT @ execute ;] E-NCOMP-STATE TTHROWSQ
   7329 throw ;

\ A certified definition and a scan-refused hook-less one; the observer's
\ generates: reads the text after the `;`.
: WIN-HOOKED ( -- )
   s" : EAUTH-WIN-HOOKED ( n -- bool ) 0= ; EAUTH-WIN-MAKE-H ( -- n )" evaluate-closed ;
: WIN-HOOKLESS ( -- )
   s" : EAUTH-WIN-HOOKLESS ( n -- n ) 0= ; EAUTH-WIN-MAKE-U ( -- n )" evaluate-closed ;

\ VERIFY's predictions for the word a definer statement defines, as ( -- n ),
\ ( -- ) and ( -- bool ): ACCEPTED, REFUSED, or 1 when it names no word.
-1 constant ACCEPTED
0 constant REFUSED
: PREDICT ( ptr u8 n -- n n n )
   CHECKER-SCOPE-START-NEUTRAL
   VERIFY:SOURCE-BUF-IN-SCOPE
   s" EAUTH-WIN-X1 ( -- n ) EAUTH-WIN-PRED" VERIFY:CANDIDATE-IN-SCOPE
   s" EAUTH-WIN-X2 ( -- ) EAUTH-WIN-PRED" VERIFY:CANDIDATE-IN-SCOPE
   s" EAUTH-WIN-X3 ( -- bool ) EAUTH-WIN-PRED" VERIFY:CANDIDATE-IN-SCOPE
   CHECKER-SCOPE-DONE ;

\ A refused callback scan keeps the pending definition's publication latch
\ and its borrowed source binding, on checked and explicitly unjudged scans.
variable LATCH-ARMED
: LATCH-BINDING ( -- )
   1 CHECKER-OWNER:SOURCE-BINDING {: row:ptr size:n :}
   size CHECKER-OWNER-ABI:BOUND-CELLS cells T=
   row CHECKER-OWNER-ABI:BOUND-ENTRY cells + CELL-VIEW @
      s" +" XREF-FIND XREF-START T= ;
: LATCH-OBSERVE ( n IR-CTX:ctx IR-BUILD:module -- )
   {: idx:n c:IR-CTX:ctx m:IR-BUILD:module :}
   LATCH-ARMED @ 0= if exit then
   0 LATCH-ARMED !
   CHECKER-OWNER:BINDING-WINDOW {: owner:ptr serial:n unjudged:bool :}
   LATCH-BINDING
   [: s" EAUTH-LATCH-CB ( -- n ) 1" JUDGE drop ;] E-NCOMP-STATE TTHROWSQ
   owner serial unjudged CHECKER-OWNER:BINDING-WINDOW-CK
   LATCH-BINDING
   [: s" EAUTH-LATCH-CB ( -- n ) 1" CHECK-UNJUDGED! drop ;] E-NCOMP-STATE TTHROWSQ
   owner serial unjudged CHECKER-OWNER:BINDING-WINDOW-CK
   LATCH-BINDING
   [: s" EAUTH-LATCH-CB ( -- n ) 1" CHECK-CANDIDATE! drop ;] E-NCOMP-STATE TTHROWSQ
   owner serial unjudged CHECKER-OWNER:BINDING-WINDOW-CK
   LATCH-BINDING
   [: s" EAUTH-LATCH-CB ( -- n ) 1" CHECK-QUIET-CANDIDATE! drop ;] E-NCOMP-STATE TTHROWSQ
   owner serial unjudged CHECKER-OWNER:BINDING-WINDOW-CK
   LATCH-BINDING ;

: WINDOW-CASES ( -- )
   1 TIER:SELECT
   ['] GENERATES-OBSERVE NBACK:OBSERVE!
   s" : EAUTH-WIN-MAKE-H ( -- ) parse-name 2drop ;" evaluate-closed
   s" : EAUTH-WIN-MAKE-U ( -- ) parse-name 2drop ;" evaluate-closed
   s" a callback's generates: while a certified definition compiles is refused" T-LABEL
   s" EAUTH-WIN-MAKE-H EAUTH-WIN-PRED" PREDICT {: h1:n h2:n h3:n :}
   1 GENERATES-ARMED !
   ['] WIN-HOOKED catch 7329 T=
   s" : EAUTH-WIN-FILL-H1 ( -- ) ;" evaluate-closed
   s" : EAUTH-WIN-FILL-H2 ( -- bool ) true ;" evaluate-closed
   s" EAUTH-WIN-MAKE-H EAUTH-WIN-PRED" PREDICT h3 T= h2 T= h1 T=
   s" generates: EAUTH-WIN-MAKE-H ( -- n )" TEST-EVAL:RC 0 T=
   s" EAUTH-WIN-MAKE-H EAUTH-WIN-PRED" PREDICT REFUSED T= REFUSED T= ACCEPTED T=
   s" a callback's generates: while a hook-less definition compiles is refused" T-LABEL
   s" EAUTH-WIN-MAKE-U EAUTH-WIN-PRED" PREDICT {: u1:n u2:n u3:n :}
   1 GENERATES-ARMED !
   UNCHECKED+ ['] WIN-HOOKLESS catch UNCHECKED- 7329 T=
   s" : EAUTH-WIN-FILL-U1 ( -- ) ;" evaluate-closed
   s" : EAUTH-WIN-FILL-U2 ( -- bool ) true ;" evaluate-closed
   s" EAUTH-WIN-MAKE-U EAUTH-WIN-PRED" PREDICT u3 T= u2 T= u1 T=
   s" generates: EAUTH-WIN-MAKE-U ( -- n )" TEST-EVAL:RC 0 T=
   s" EAUTH-WIN-MAKE-U EAUTH-WIN-PRED" PREDICT REFUSED T= REFUSED T= ACCEPTED T=
   s" a callback's scan cannot reset the latch its definition publishes" T-LABEL
   ['] LATCH-OBSERVE NBACK:OBSERVE!
   s" : EAUTH-LATCH-PLAIN ( n n -- n ) + ;" evaluate-closed
   1 LATCH-ARMED !
   s" : EAUTH-LATCH-KEPT ( n n -- n ) + ;" evaluate-closed
   s" EAUTH-LATCH-PLAIN" DICT-MIN 0 T<>
   s" EAUTH-LATCH-KEPT" DICT-MIN  s" EAUTH-LATCH-PLAIN" DICT-MIN T=
   s" a refused callback scan keeps an explicitly unjudged binding" T-LABEL
   1 LATCH-ARMED !
   s" TRUSTED: EAUTH-LATCH-UNJUDGED ( n n -- n ) + ;" evaluate-closed
   REPLAY+ s" EAUTH-LATCH-CB" EFFECT-QUERY TFALSE REPLAY- ;

\ ---- type registration in the compile window ----------------------------------
\ The window refuses type registration as it refuses rows. A callback's
\ CHECKER-DEFLINEAR, CHECKER-DEFRECORD or CHECKER-DEFFAMILY registered its type
\ while the outer definition compiled; the definition was refused and the type
\ stayed, so a signature naming it went from rejected to certified. Each case
\ arms the observer with one registry write: the write throws E-NCOMP-STATE,
\ the definition is refused, and the registry reads as it did before. Every
\ appender has a case (checker.f WRITE-WINDOW): the type table, value records,
\ the extent free-set, families, variants, layouts, schema nodes and roots, and
\ each field-owner phase. TFAM:SUMV-ADD, TFAM:LAY-ADD and the SCHEMA-REG words
\ are internal appenders this whitebox engine leaves callable and a product
\ seals.
public
NEWTYPE eauth-reg-ext 0
PRODUCT eauth-reg-pr 0 FIELD x n ;PRODUCT
private
TFAM:TFAM-N@ 1 - constant REG-FAM
TYPED-VARIABLE REG-XT [ -- ]
variable REG-ARMED
variable REG-TX
variable REG-SCH
: REG-OBSERVE ( n IR-CTX:ctx IR-BUILD:module -- )
   {: idx:n c:IR-CTX:ctx m:IR-BUILD:module :}
   REG-ARMED @ 0= if exit then
   0 REG-ARMED !
   [: REG-XT @ execute ;] E-NCOMP-STATE TTHROWSQ
   7329 throw ;
: REG-OUTER ( -- ) s" : EAUTH-REG-OUTER ( n n -- n ) + ;" evaluate-closed ;
: REG-ARM ( [ -- ] -- ) REG-XT !  1 REG-ARMED ! ;
\ The write is refused, and so is the definition it ran under.
: REG-RUN ( [ -- ] -- ) REG-ARM ['] REG-OUTER catch 7329 T= ;
: REG-RUN-HOOKLESS ( [ -- ] -- )
   REG-ARM UNCHECKED+ ['] REG-OUTER catch UNCHECKED- 7329 T= ;
\ A field frame opened before the definition compiles, as a declaration's is.
: REG-TX-OPEN ( -- ) TYPE-FIELD-OWNER:OPEN REG-TX ! ;
: REG-TX-PREPARE-RC ( -- n ) [: REG-TX @ TYPE-FIELD-OWNER:PREPARE drop ;] catch ;

: REGISTRY-CASES ( -- )
   ['] REG-OBSERVE NBACK:OBSERVE!
   s" a callback's linear type is refused and stays unknown" T-LABEL
   s" EAUTH-REG-P ( EAUTH-REG-LIN -- EAUTH-REG-LIN )" CHECK-QUIET-CANDIDATE! 0 T=
   [: s" EAUTH-REG-LIN" CHECKER-DEFLINEAR ;] REG-RUN
   s" EAUTH-REG-P ( EAUTH-REG-LIN -- EAUTH-REG-LIN )" CHECK-QUIET-CANDIDATE! 0 T=
   s" and so is one under a hook-less definition" T-LABEL
   [: s" EAUTH-REG-LIN-NH" CHECKER-DEFLINEAR ;] REG-RUN-HOOKLESS
   s" EAUTH-REG-P ( EAUTH-REG-LIN-NH -- EAUTH-REG-LIN-NH )" CHECK-QUIET-CANDIDATE! 0 T=
   s" a callback's value record is refused" T-LABEL
   [: s" EAUTH-REG-REC" s" field n" CHECKER-DEFRECORD ;] REG-RUN
   s" EAUTH-REG-P ( EAUTH-REG-REC -- EAUTH-REG-REC )" CHECK-QUIET-CANDIDATE! 0 T=
   s" a callback's family is refused" T-LABEL
   TFAM:TFAM-N@ {: fams:n :}
   [: s" eauth-reg-fam" s" 0" CHECKER-DEFFAMILY ;] REG-RUN
   TFAM:TFAM-N@ fams T=
   s" EAUTH-REG-P ( eauth-reg-fam -- eauth-reg-fam )" CHECK-QUIET-CANDIDATE! 0 T=
   s" a callback cannot mark an extent family free" T-LABEL
   s" EAUTH-REG-P ( redx<eauth-reg-ext> -- redx<eauth-reg-ext> )" CHECK-QUIET-CANDIDATE! -1 T=
   [: s" eauth-reg-ext" EXT-MARK-FREE-TAIL ;] REG-RUN
   s" EAUTH-REG-P ( redx<eauth-reg-ext> -- redx<eauth-reg-ext> )" CHECK-QUIET-CANDIDATE! -1 T=
   s" nor add a variant" T-LABEL
   TFAM:SUMV-N@ {: vars:n :}
   [: REG-FAM s" eauth-reg-v" 0 0 0 0 TFAM:SUMV-ADD drop ;] REG-RUN
   TFAM:SUMV-N@ vars T=
   s" nor a layout" T-LABEL
   TFAM:LAY-N@ {: lays:n :}
   [: REG-FAM 0 CELL CELL 0 TFAM:LAY-ADD drop ;] REG-RUN
   TFAM:LAY-N@ lays T=
   s" nor a schema node" T-LABEL
   SCHEMA-REG:SCHEMA-N@ {: nodes:n :}
   [: 1 SCHEMA-REG:SCHEMA-CON drop ;] REG-RUN
   SCHEMA-REG:SCHEMA-N@ nodes T=
   s" nor a schema root" T-LABEL
   SCHEMA-REG:SCHEMA-ROOT-N@ {: roots:n :}
   [: 1 SCHEMA-REG:SCHEMA-ROOT+ drop ;] REG-RUN
   SCHEMA-REG:SCHEMA-ROOT-N@ roots T= ;

\ A declaration's field frame is open while its generated definitions compile;
\ no phase of it moves from a callback.
: FIELD-TX-CASES ( -- )
   s" a callback cannot add a field to an open frame" T-LABEL
   REG-FAM TFAM-NAME$ s" eauth-reg-pr" T$=
   TYPE-FIELD:TX-DEPTH {: depth:n :}
   REG-FAM TYPE-FIELD:NO-VARIANT s" x" TYPE-FIELD:FIND TTRUE TYPE-FIELD:SCHEMA@ REG-SCH !
   REG-TX-OPEN
   REG-TX @ TYPE-FIELD-OWNER:PREPARE {: rows:n :}
   [: REG-TX @ REG-FAM TYPE-FIELD:NO-VARIANT s" y" REG-SCH @ 1 1 CELL CELL CELL 0
      TYPE-FIELD-OWNER:ADD drop ;] REG-RUN
   REG-TX @ TYPE-FIELD-OWNER:PREPARE rows T=
   [: REG-TX @ TYPE-FIELD-OWNER:ROLLBACK ;] catch 0 T=
   s" nor commit it" T-LABEL
   REG-TX-OPEN
   [: REG-TX @ TYPE-FIELD-OWNER:COMMIT ;] REG-RUN
   REG-TX-PREPARE-RC 0 T=
   [: REG-TX @ TYPE-FIELD-OWNER:ROLLBACK ;] catch 0 T=
   s" nor finalize a committed one" T-LABEL
   REG-TX-OPEN  REG-TX @ TYPE-FIELD-OWNER:COMMIT
   [: REG-TX @ TYPE-FIELD-OWNER:FINALIZE ;] REG-RUN
   TYPE-FIELD:TX-DEPTH depth 1 + T=
   [: REG-TX @ TYPE-FIELD-OWNER:FINALIZE ;] catch 0 T=
   s" nor roll one back" T-LABEL
   REG-TX-OPEN
   [: REG-TX @ TYPE-FIELD-OWNER:ROLLBACK ;] REG-RUN
   TYPE-FIELD:TX-DEPTH depth 1 + T=
   [: REG-TX @ TYPE-FIELD-OWNER:ROLLBACK ;] catch 0 T=
   s" nor open one" T-LABEL
   [: TYPE-FIELD-OWNER:OPEN drop ;] REG-RUN
   TYPE-FIELD:TX-DEPTH depth T= ;

\ ---- the open gate -------------------------------------------------------------
\ FRESH is a pre-hook checker word: the build recorded its row without source
\ authority, and no primitive row stands behind it. An unsealed checker binds
\ that row and checks the caller's body against it, and the row gains nothing.
\ What the gate does not open stays shut: PRIM-CASES runs unsealed too, and a
\ failed declaration's row serves only its own run.
: OPEN-X ( -- ) s" : EAUTH-OPEN-X ( -- n ) FRESH ;" evaluate-closed ;

: OPEN-LIVE-CASES ( -- )
   s" an unsealed checker binds an internal word's recorded row" T-LABEL
   SEALED? TFALSE
   ['] OPEN-X catch 0 T=
   s" : EAUTH-OPEN-TICK ( -- [ -- n ] ) ['] FRESH ;" evaluate-closed
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
   ENFORCED? TTRUE
   s" a hook-less declaration's row binds the same way" T-LABEL
   s" EAUTH-DECL-CALL ( n -- n ) EAUTH-DECL" CHECK-CANDIDATE! -1 T=
   DIAGS IO-CAP DIAG-BUFFER!  true DIAG-JSON!
   s" EAUTH-DECL-WRONG ( n -- bool ) EAUTH-DECL" CHECK-CANDIDATE! 0 T=
   DIAG-BUFFER$ s\" \"code\":\"E-MISMATCH\"" CONTAINS? TTRUE
   false DIAG-JSON!  DIAG-BUFFER-OFF
   s" EAUTH-DECL" SOURCE-MIN -1 T= ;

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
   s" and refuses a hook-less declaration's row" T-LABEL
   DIAGS IO-CAP DIAG-BUFFER!
   s" EAUTH-SEALED-DECL ( n -- n ) EAUTH-DECL" CHECK-CANDIDATE! 0 T=
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
   s" : EAUTH-COPY-BAD ( n -- n ) drop ;" evaluate-closed
   MULTI- 1 T=
   s" EAUTH-COPY-BAD" RECOVERY-FACT
   [: s" package EAUTH-COPY public EXPORT EAUTH-COPY-BAD ;package" evaluate-closed ;] catch
   E-EXPORT-UNDEFINED T=
   s" EAUTH-COPY-CALL ( n -- n ) EAUTH-COPY:EAUTH-COPY-BAD" CHECK-CANDIDATE! 1 T=
   s" nor inside a later run" T-LABEL
   MULTI+
   [: s" package EAUTH-COPY-LATER public EXPORT EAUTH-COPY-BAD ;package" evaluate-closed ;] catch
   E-EXPORT-UNDEFINED T=
   MULTI- 0 T=
   saved TIER:SELECT ;

: RUN ( -- )
   T-RESET
   TIER0-CASES
   OPEN-LIVE-CASES
   REPLAY+ OPEN-REPLAY-CASES REPLAY-
   COPY-CASES
   SEALED-CASES
   REPLAY+
   SCAN-CASES PRIM-CASES ROLLBACK-CASES RECOVERY-CASES
   REPLAY-
   LIVE-CASES
   TIER1-DECLARED-CASES
   TIER1-TRUSTED-CASES
   WINDOW-CASES
   REGISTRY-CASES
   FIELD-TX-CASES
   RECOVERY-PUBLICATION
   T-REPORT
   s" effect authority: ok" type cr ;

: ACTION ( -- [ -- ] ) [: RUN ;] ;
ACTION
;package
execute
