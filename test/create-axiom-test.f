\ create-axiom-test.f - what `create` is to the checker, and what every reader of
\ that answer does with it. One concern: the `PRIM: create` axiom row in
\ src/core/checker.f (dot habu-fix-the-create-75d82809).
\
\ WHAT THE ROW SAID AND WHY IT WAS WRONG. It said `-- ptr a` from 2026-06-11,
\ and the commit that wrote it said why in its own comment: "defining-word kinds
\ ... create/variable are addresses". That is the effect of the word `create`
\ DEFINES, filed in a table keyed by the token a BODY names, so the row asserted
\ that CALLING `create` pushes an address. It does not: the primitive parses a
\ name out of the input and defines it (src/habu/habu1.f BCREATE, `( "name" -- )`),
\ and the name it defines gets `-- ptr a` from its own definer at the moment it is
\ made (src/habu/habu2.f LASTC-TRUST:PUBLISH-PTR-A). Two authorities for one fact,
\ and the row was the one with no claim to it.
\
\ WHY IT SURVIVED SO LONG, WHICH IS THE PART A SUITE HAS TO COVER. Inside a body
\ that declares a signature the row is never read - src/core/checker.f DEFINER-TOK
\ models `create` directly and runs before TRY-PRIMS in DO-TOK - so the one reader
\ anybody exercises gave the right answer for the wrong reason, and every fixture
\ written against a signed body passed either way. The readers that saw the lie
\ ask the effect store BY NAME:
\
\   1. the checker's own inference, for a body with NO declared signature. That
\      body's rendered effect is recorded under its name, so `: MK create ;`
\      published `-- ptr a` and a CHECKED caller declaring one output then
\      certified against a word the machine leaves nothing behind. A certificate
\      the machine contradicts is the whole failure this file exists to keep out.
\   2. src/compiler/native/dict.f NDICT:SPELL-ARITY, which answered 0-in/1-out, so
\      the native chain refused every definer body - `dup create , 1 +`, the shape
\      src/core/enums.f ENUM+ ships - as E-NELAB-ARITY.
\
\ HOW THIS SUITE WOULD CATCH THE ROW COMING BACK. Restore `PE-PTR-A PE-OUT` on the
\ row and seven assertions red (re-measured, 2026-08-18): the arity pair reads
\ 0/1, the inferred effect reads 0/1, the bad caller certifies instead of being
\ refused, the honest caller is refused instead of certifying, and all three
\ chain measurements come back E-NELAB-ARITY. It was eight while a fourth chain
\ case could state a wrong arity by hand; that case is retired below, with its
\ reason, and the three that remain carry the same discrimination.
\
\ AND WHAT KEEPS IT FROM PASSING FOR A LAZY REASON. Zeroing every storage row
\ would pass a `create`-only suite, so `here`, `allot` and `,` are measured
\ beside it and one of them reds. Reading the row as the created word's authority
\ would pass too, so a word `create` MADE is measured and keeps its `-- ptr a`.
\ And `create` written inside a string and inside a comment must stay a string and
\ a comment, so a body holding either certifies as if the token were not there.
\
\ Every verdict here comes off the production load path: the source is handed to
\ the engine, the engine compiles it, and the check hook certifies or throws -
\ which is what loading a file does to every definition in it.
\
\ WHY THE ROW ITSELF CARRIES NO PROSE, which is where a reader would look first.
\ src/core/checker.f is 524245 bytes on the master this landed against and three
\ lint tools read it whole into a $80000 (524288) fixed buffer - repo, maki-dep
\ and error-code, each dying `lint: file exceeds buffer: src/core/checker.f`
\ 44 bytes later. So the file has room for the corrected row (16 bytes shorter
\ than the wrong one) and for nothing else, and this header is where the
\ reasoning went. An empty row is not anomalous there in any case: `cr` and
\ `space` two lines above it are empty rows too. The cliff itself is a finding
\ of this lane, not a property anybody chose; tools/lint/text.f already carries
\ the runtime-sized slab those three lints want in place of a fixed buffer.
\
\ THE SAME LIE, TWO ROWS LATER: `variable` AND `constant` (dot
\ habu-var-and-const-0af7da85, landed 2026-08-23). The commit that wrote
\ `create`'s row wrote these two in the same breath and for the same reason, so
\ correcting `create` alone left them stating what the words they DEFINE leave
\ behind: `variable` claimed `-- ptr a` and `constant` claimed `-- a`. Calling
\ `variable` pushes nothing (src/habu/habu2.f C-VARIABLE is `create` plus eight
\ bytes of allot) and calling `constant` POPS one cell (C-CONSTANT, `15 G-POP`),
\ which is what src/core/checker.f DEFINER-TOK has modelled all along under a
\ declared signature - STEP-N-IN for `constant`, no step at all for `variable`.
\ The rows say the same thing now: `PRIM: variable PRIM;` and
\ `PRIM: constant PE-N PE-IN PRIM;`.
\
\ WHY THE MATRIX BELOW IS NOT SHAPED LIKE `create`'s, which is the first thing a
\ reader will ask. `create` is ALSO a runtime primitive (src/habu/habu1.f
\ BCREATE, registered through FPRIM), so a colon body may name it and this file
\ can load one with `evaluate`. `variable` and `constant` are interpret-state
\ define keywords ONLY (src/habu/habu2.f EM-INTERPRET-DEFINE-KEYWORDS): no
\ dictionary record exists, so `: MKV variable ;` dies `E-UNDEFINED: variable`
\ in the ENGINE, rc 70, carrying no checker packet - measured against the
\ control of an ordinary unknown token, which comes back from tools/check.f as a
\ checker `E-UNDEFINED` JSON packet instead. The CHECKER certified that body all
\ the same, and that is where the lie lived: tools/check.f preverified
\ `: MKV variable ;  : W ( -- ptr a ) MKV ;` without a word, and refused the
\ honest `( -- )` caller with E-MISMATCH and `inferred_effect: -- ptr a`. So the
\ cases below reach the checker where a body reaches it -
\ VERIFY:SOURCE-BUF-IN-SCOPE, the same source front end preverify and the native
\ compiler use, and CHECK-QUIET-CANDIDATE! for one caller's verdict - instead of
\ through `evaluate`.
\
\ AND THE CHAIN CASE IS A CONTROL, NOT A DISCRIMINATOR, for that same reason:
\ NMIGRATE:MEASURE-HELD on `: X ( -- ) variable ;` throws the engine's
\ undefined-token 70 on the old rows and on the new ones, because there is no
\ record to lower. It is kept with its reason, the way the fourth `create` chain
\ case is retired below with its own. What discriminates these two rows for the
\ native chain is NDICT:SPELL-ARITY in VC-ARITY-CASE - the same effect-read
\ bridge the `create` arity case measures.
\
\ RESTORE EITHER OLD ROW AND SEVEN ASSERTIONS RED FOR IT (measured 2026-08-23):
\ the arity pair, the inferred effect of an unsigned body, the bad caller
\ certifying instead of being refused, and the honest caller refused instead of
\ certifying. The word a definer MADE keeps its `-- ptr a` / `-- a` either way,
\ from two authorities that never read the row - src/habu/habu2.f
\ LASTC-TRUST:PUBLISH-PTR-A / PUBLISH-A on the engine path and
\ src/habu/verify-source.f RECORD-DEFINER? on the source path - and both are
\ measured here, so a "fix" that reached for the created word's effect reds.
\
\ WHY NO GATE CAUGHT THESE WHEN `create`'s WAS FIXED (Checker-Miss RCA).
\ Static invariant: a primitive row states the effect of CALLING the word, and a
\ definer's row may never carry the effect of the word it creates. Owner:
\ primitive effect metadata, the `PRIM:` table in src/core/checker.f. This file
\ pinned one spelling, and its own head quoted the comment that wrote all three
\ rows together without checking the other two. Nothing else could see them:
\ both definer-side authorities publish the created word's effect independently
\ of the row, so every fixture that touches `variable` or `constant` reads the
\ created word and never the definer's row; and because neither token can appear
\ in a body the engine will compile, no run-based test could reach the row at
\ all. Regression: the fourteen assertions below.
\
\ ONE ROW IS LOAD-BEARING AND THE OTHER IS NOT, which is what decides correction
\ over deletion. Delete `PRIM: constant` and the boot dies at the first constant
\ in the prefix (`E-UNDEFINED habu: in e-participant-dup: undefined word
\ 'constant'`): the engine's C-CONSTANT seeds the check hook with the body
\ `NAME constant` (habu2.f `LKWCONST 8 C-DEFHOOK`) before
\ LASTC-TRUST:PUBLISH-A overwrites the row, so every `N constant FOO` in the
\ tree infers through this axiom on its way in. Delete `PRIM: variable` and the
\ tree still boots - its seed keyword is `create` - but NDICT:SPELL-ARITY then
\ answers ARITY-NONE for the spelling, which is a second lie in place of the
\ first. Both rows are corrected; neither is deleted.
\
\ Run: bin/hb --load test/create-axiom-test.f

require lib/test.f
require src/habu/verify-source.f
require test/checker-assert.f
require src/compiler/native/migrate.f

package CREATE-AXIOM-TEST
private

0 constant LOADED               \ what `catch` answers for a source that loaded
70 constant REJECT-RC           \ src/core/check-hook.f LOWER-CERT-HOOK CHECK-RC (private there)
\ The engine's own undefined-token throw, which the interpret loop reports as
\ `E-UNDEFINED: <token>`. It carries the same number as REJECT-RC and a
\ different authority: REJECT-RC is the check hook refusing a definition it read,
\ this is the engine refusing a token it has no record for, and the two are told
\ apart by whether a checker packet comes with it (tools/check.f --json-errors
\ prints one for the first and nothing for the second).
70 constant NO-RECORD-RC

\ `evaluate` is the metaprogramming boundary the checker does not model, and it is
\ how this suite loads a definition the way a source file loads one. The suite
\ runs at top level, so what these publish is global - the position an ordinary
\ program's definitions occupy, and the one the inference case needs.
TRUSTED: EV ( ptr u8 n -- )
   evaluate ;

TRUSTED: EV-N ( ptr u8 n -- n )
   evaluate ;

TRUSTED: EV-STR ( ptr u8 n -- ptr u8 n )
   evaluate ;

\ ---- what the row says, read the way the native chain reads it ----------------
\ NDICT:SPELL-ARITY is the bridge the dot named: it resolves a spelling's active
\ effect through the checker's effect-read export API and answers the pair in
\ stack cells. Asking it is asking the row.
: ARITY-CASE ( -- )
   s" the checker's effect for `create` moves no stack cell in either direction" T-LABEL
   s" create" NDICT:SPELL-ARITY {: din:n dout:n :}
   din 0 T=  dout 0 T=

   s" while the storage words that DO move one keep saying so" T-LABEL
   s" here" NDICT:SPELL-ARITY {: hin:n hout:n :}
   hin 0 T=  hout 1 T=
   s" allot" NDICT:SPELL-ARITY {: ain:n aout:n :}
   ain 1 T=  aout 0 T=
   s" ," NDICT:SPELL-ARITY {: cin:n cout:n :}
   cin 1 T=  cout 0 T= ;

\ ---- and the word `create` MAKES is unaffected, because the row was never its
\ ---- authority. The definer publishes `-- ptr a` against the new name as it
\ ---- emits the body, so emptying the definer's own row leaves this untouched.
: MADE-WORD ( -- )
   s" create CAX-DAT 8 allot" EV ;

: MADE-USE ( -- )
   s" : CAX-USE ( -- n ) CAX-DAT @ ;" EV ;

: MADE-CASE ( -- )
   s" a word `create` made still pushes one address" T-LABEL
   [: MADE-WORD ;] LOADED TTHROWSQ
   s" CAX-DAT" NDICT:SPELL-ARITY {: din:n dout:n :}
   din 0 T=  dout 1 T=

   s" and a checked body may still take it as one" T-LABEL
   [: MADE-USE ;] LOADED TTHROWSQ
   s" 41 CAX-DAT !  CAX-USE" EV-N 41 T= ;

\ ---- the load path: bodies that must load, and bodies that must not -----------
: LOAD-EMPTY ( -- )      s" : CAX-B1 ( -- ) create ;" EV ;
: LOAD-PTR-A ( -- )      s" : CAX-B2 ( -- ptr a ) create ;" EV ;
: LOAD-N ( -- )          s" : CAX-B3 ( -- n ) create ;" EV ;
: LOAD-ENUM-HALF ( -- )  s" : CAX-B4 ( n -- n ) dup create , 1 + ;" EV ;
: LOAD-IN-STRING ( -- )  s\" : CAX-B5 ( -- ptr u8 n ) s\q create\q ;" EV ;
: LOAD-IN-COMMENT ( -- ) s" : CAX-B6 ( -- ) ( create ) ;" EV ;
: LOAD-HERE ( -- )       s" : CAX-B7 ( -- ptr a ) here ;" EV ;

: BODY-CASE ( -- )
   s" a body whose whole content is `create` declares nothing and loads" T-LABEL
   [: LOAD-EMPTY ;] LOADED TTHROWSQ

   s" and the machine agrees: running it leaves the cell under it on top" T-LABEL
   s" 99 CAX-B1 CAX-B1-NAME" EV-N 99 T=
   s" CAX-B1-NAME" EV-N 0 T<>

   s" declaring the address it used to claim is refused at the token" T-LABEL
   [: LOAD-PTR-A ;] REJECT-RC TTHROWSQ

   s" and so is any other output - the row states an effect, not a wildcard" T-LABEL
   [: LOAD-N ;] REJECT-RC TTHROWSQ

   s" the definer half src/core/enums.f ENUM+ ships balances" T-LABEL
   [: LOAD-ENUM-HALF ;] LOADED TTHROWSQ

   s" and running it defines the name and stores the counter in it" T-LABEL
   s" 5 CAX-B4 CAX-B4-CELL" EV-N 6 T=
   s" CAX-B4-CELL @" EV-N 5 T=

   s" `create` inside a string is a string" T-LABEL
   [: LOAD-IN-STRING ;] LOADED TTHROWSQ
   s" CAX-B5" EV-STR s" create" T$=

   s" `create` inside a comment is a comment" T-LABEL
   [: LOAD-IN-COMMENT ;] LOADED TTHROWSQ
   s" CAX-B6" NDICT:SPELL-ARITY {: b6in:n b6out:n :}
   b6in 0 T=  b6out 0 T=

   s" and `here`, which really does push an address, still declares one" T-LABEL
   [: LOAD-HERE ;] LOADED TTHROWSQ
   s" CAX-B7" NDICT:SPELL-ARITY {: b7in:n b7out:n :}
   b7in 0 T=  b7out 1 T= ;

\ ---- the leg DEFINER-TOK does not shadow, and the miscompile it admitted ------
\ A body with no declared signature reaches the primitive row, and what the walk
\ infers for it is recorded under its name for every later caller. This is the
\ case that certified a stack cell the machine never pushes.
: INFER-MK ( -- )       s" : CAX-MK create ;" EV ;
: INFER-BAD ( -- )      s" : CAX-C1 ( -- ptr a ) CAX-MK ;" EV ;
: INFER-GOOD ( -- )     s" : CAX-C2 ( -- ) CAX-MK ;" EV ;

: INFER-CASE ( -- )
   s" an unsigned body calling `create` records what it really leaves" T-LABEL
   [: INFER-MK ;] LOADED TTHROWSQ
   s" CAX-MK" NDICT:SPELL-ARITY {: mkin:n mkout:n :}
   mkin 0 T=  mkout 0 T=

   s" so a caller claiming a cell from it is refused, not certified" T-LABEL
   [: INFER-BAD ;] REJECT-RC TTHROWSQ

   s" and the caller that claims nothing certifies and runs" T-LABEL
   [: INFER-GOOD ;] LOADED TTHROWSQ
   s" CAX-C2 CAX-C2-CELL 8 allot  7 CAX-C2-CELL !  CAX-C2-CELL @" EV-N 7 T= ;

\ ---- the reader the dot named: the native chain compiling a definer body ------
\ MEASURE-HELD runs every stage a held migration runs and keeps none of it, so a
\ measurement is the chain's own verdict on whether it could compile the body.
: MEASURE ( ptr u8 n -- )
   NMIGRATE:MEASURE-HELD ;

: CHAIN-ALONE ( -- )
   s" : CAX-D1 ( -- ) create ;" MEASURE ;

: CHAIN-ENUM-HALF ( -- )
   s" : CAX-D2 ( n -- n ) dup create , 1 + ;" MEASURE ;

\ `create allot` was lib/string.f BUFFER:'s definer half until that definer was
\ converted to a generated colon accessor (dot habu-the-reader-re-a65e56e5). The
\ shape is kept here because the row it measures is about `create` moving no stack
\ cell, and a body that allots after it is the sharpest form of that question - not
\ because the tree still ships one.
: CHAIN-BUFFER-HALF ( -- )
   s" : CAX-D3 ( n -- ) create allot ;" MEASURE ;

\ THE FOURTH CASE HERE WAS A WRONG ARITY AND IT IS RETIRED, because there is no
\ longer a caller who can state one. It handed the same body as CHAIN-ENUM-HALF
\ to the migration entry declared `1 2` and pinned E-NELAB-ARITY, which was the
\ control that said the three above were refused for the arity and not for
\ something else. The entry takes SOURCE now (dot habu-bind-checker-env-ed4f9f87)
\ and reads what the definition takes and leaves off the checker's certificate,
\ so a body cannot arrive with an arity that disagrees with the body: the
\ refusal is unreachable from the production entry rather than merely unused.
\ What still separates the two outcomes is the mutation this file's head
\ describes - restore the row and the three measurements below turn from LOADED
\ into E-NELAB-ARITY, which is the same discrimination the control was carrying.
\ The cells-against-terms question that arity number now answers alone belongs to
\ src/compiler/native/dict.f EFF-CELLS, and a case for it needs a term of a
\ family more than one cell wide - not a definer body, so not this file.

: CHAIN-CASE ( -- )
   s" the chain compiles a body that only calls `create`" T-LABEL
   NELAB:REFUSED-RESET
   [: CHAIN-ALONE ;] LOADED TTHROWSQ
   NELAB:REFUSED-ROW -1 T=

   s" the definer half the tree still ships, and the one it used to" T-LABEL
   [: CHAIN-ENUM-HALF ;] LOADED TTHROWSQ
   [: CHAIN-BUFFER-HALF ;] LOADED TTHROWSQ ;

\ ---- the two definers whose rows said the same thing, and where they are read -
\ The same questions `create` is asked above, put where a body naming these two
\ tokens actually reaches the checker. The head of this file has the reasoning;
\ what follows is only the measurement.

: VC-ARITY-CASE ( -- )
   s" the checker's effect for `variable` moves no stack cell either" T-LABEL
   s" variable" NDICT:SPELL-ARITY {: vin:n vout:n :}
   vin 0 T=  vout 0 T=

   s" and `constant` takes the one cell the machine pops, and leaves none" T-LABEL
   s" constant" NDICT:SPELL-ARITY {: kin:n kout:n :}
   kin 1 T=  kout 0 T= ;

\ ---- and the words those two definers MAKE are unaffected, from either
\ ---- authority. The engine publishes against the new name as it emits the body
\ ---- (LASTC-TRUST:PUBLISH-PTR-A / PUBLISH-A) and the source verifier registers
\ ---- the same effect from its own table (verify-source.f RECORD-DEFINER?);
\ ---- neither reads the row, so correcting the row leaves both untouched.
: VC-MADE-VAR ( -- )    s" variable CAX-VCELL" EV ;
: VC-MADE-CONST ( -- )  s" 41 constant CAX-KONST" EV ;
: VC-MADE-VUSE ( -- )   s" : CAX-VUSE ( -- n ) CAX-VCELL @ ;" EV ;
: VC-MADE-KUSE ( -- )   s" : CAX-KUSE ( -- a ) CAX-KONST ;" EV ;

: VC-MADE-CASE ( -- )
   s" a word `variable` made still pushes one address" T-LABEL
   [: VC-MADE-VAR ;] LOADED TTHROWSQ
   s" CAX-VCELL" NDICT:SPELL-ARITY {: vin:n vout:n :}
   vin 0 T=  vout 1 T=

   s" and one `constant` made still pushes its one value" T-LABEL
   [: VC-MADE-CONST ;] LOADED TTHROWSQ
   s" CAX-KONST" NDICT:SPELL-ARITY {: kin:n kout:n :}
   kin 0 T=  kout 1 T=

   s" and checked bodies may still take both, and run" T-LABEL
   [: VC-MADE-VUSE ;] LOADED TTHROWSQ
   [: VC-MADE-KUSE ;] LOADED TTHROWSQ
   s" 7 CAX-VCELL !  CAX-VUSE" EV-N 7 T=
   s" CAX-KUSE" EV-N 41 T=

   s" the source verifier's own definer table publishes the same two" T-LABEL
   s\" variable CAX-VV2\n42 constant CAX-KK2" VERIFY:SOURCE-BUF-IN-SCOPE
   s" CAX-VV2" NDICT:SPELL-ARITY {: v2in:n v2out:n :}
   v2in 0 T=  v2out 1 T=
   s" CAX-KK2" NDICT:SPELL-ARITY {: k2in:n k2out:n :}
   k2in 0 T=  k2out 1 T= ;

\ ---- the leg that certified the lie: an unsigned body, read by the verifier ---
\ The engine cannot compile either body, so `evaluate` never reaches this. The
\ source front end does, and what it records under the body's name is what every
\ later caller certifies against - which is the whole failure, since a caller
\ that certifies here is a caller tools/check.f preverifies clean.
: VC-REGISTER ( -- )
   s\" : CAX-MKV variable ;\n: CAX-MKK constant ;" VERIFY:SOURCE-BUF-IN-SCOPE ;

: VC-INFER-CASE ( -- )
   s" an unsigned body calling `variable` records what it really leaves" T-LABEL
   VC-REGISTER
   s" CAX-MKV" NDICT:SPELL-ARITY {: vin:n vout:n :}
   vin 0 T=  vout 0 T=

   s" and one calling `constant` records the cell it really takes" T-LABEL
   s" CAX-MKK" NDICT:SPELL-ARITY {: kin:n kout:n :}
   kin 1 T=  kout 0 T=

   s" so a caller claiming a cell from either is refused, not certified" T-LABEL
   s" CAX-V1 ( -- ptr a ) CAX-MKV" CHECK-QUIET-CANDIDATE! 0 T=
   s" CAX-K1 ( -- a ) CAX-MKK"     CHECK-QUIET-CANDIDATE! 0 T=

   s" and the caller that states the real effect certifies" T-LABEL
   s" CAX-V2 ( -- ) CAX-MKV"       CHECK-QUIET-CANDIDATE! -1 T=
   s" CAX-K2 ( n -- ) CAX-MKK"     CHECK-QUIET-CANDIDATE! -1 T= ;

\ ---- the control that says WHY the two cases above use the verifier ----------
\ Not a discriminator: both measurements throw on the old rows and on the new
\ ones, because neither token has a dictionary record for the chain to lower.
\ It is here because it is the fact that shapes the rest of this section - if a
\ future engine gives either spelling a compile-mode record, this case reds and
\ the reasoning in the head of this file has to be redone rather than quietly
\ outlived.
: VC-CHAIN-VAR ( -- )    s" : CAX-DV ( -- ) variable ;" MEASURE ;
: VC-CHAIN-CONST ( -- )  s" : CAX-DK ( n -- ) constant ;" MEASURE ;

: VC-CHAIN-CASE ( -- )
   s" neither body reaches the chain at all: no record to lower" T-LABEL
   [: VC-CHAIN-VAR ;] NO-RECORD-RC TTHROWSQ
   [: VC-CHAIN-CONST ;] NO-RECORD-RC TTHROWSQ ;

\ ---- and the token in a string is a string, in a comment a comment -----------
\ These are the fooling fixtures, not discriminators: every one of them is green
\ on the old rows too, and they are here so that a "fix" which scrubbed the two
\ spellings out of source text instead of correcting their effects cannot pass.
\ The last four run under a DECLARED signature, where DEFINER-TOK models the
\ definers directly and the row is never read - which is precisely why a signed
\ body never showed the defect, and why the pair `( -- ) ( constant )` certifying
\ beside `( -- ) constant` being refused is what proves the comment really is
\ inert rather than merely harmless. Their second job is to pin that the two
\ paths now AGREE: DEFINER-TOK has always answered `n --` for `constant` and
\ nothing for `variable`, and the rows say the same thing at last.
: VC-IN-STRING ( -- )   s\" : CAX-V5 ( -- ptr u8 n ) s\q variable\q ;" EV ;
: VC-IN-COMMENT ( -- )  s" : CAX-V6 ( -- ) ( constant ) ;" EV ;

: VC-STRING-CASE ( -- )
   s" `variable` inside a string is a string" T-LABEL
   [: VC-IN-STRING ;] LOADED TTHROWSQ
   s" CAX-V5" EV-STR s" variable" T$=

   s" `constant` inside a comment is a comment" T-LABEL
   [: VC-IN-COMMENT ;] LOADED TTHROWSQ
   s" CAX-V6" NDICT:SPELL-ARITY {: v6in:n v6out:n :}
   v6in 0 T=  v6out 0 T=

   s" a commented token certifies where the bare token is refused" T-LABEL
   s" CAX-V7 ( -- ) ( constant )"     CHECK-QUIET-CANDIDATE! -1 T=
   s" CAX-V8 ( -- ) constant"         CHECK-QUIET-CANDIDATE! 0 T=

   s" and the same holds for the definer that takes nothing" T-LABEL
   s" CAX-V9 ( -- ) ( variable )"     CHECK-QUIET-CANDIDATE! -1 T=
   s" CAX-VA ( -- ptr a ) variable"   CHECK-QUIET-CANDIDATE! 0 T= ;

public

: RUN ( -- )
   T-RESET
   ARITY-CASE
   MADE-CASE
   BODY-CASE
   CHAIN-CASE
   INFER-CASE
   VC-ARITY-CASE
   VC-MADE-CASE
   VC-INFER-CASE
   VC-CHAIN-CASE
   VC-STRING-CASE
   T-REPORT ;

;package

CREATE-AXIOM-TEST:RUN
