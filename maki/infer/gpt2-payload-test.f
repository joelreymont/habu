\ gpt2-payload-test.f - the linear-payload declaration capability, exercised over the
\ REAL loaded GPT-2 model rather than over a stand-in.
\
\ WHY THIS SUITE EXISTS. The capability that lets a unified ENUM variant or STRUCTURE
\ field name a family which owns a linear value is pinned in two places already:
\ test/enum-decl-suite.f and test/structure-decl-suite.f pin what the REGISTRY records,
\ and test/type-linear-suite.f pins what the CHECKER does with such a value on a row.
\ Both of those work over small stand-in owners - a one-cell DEFLINEAR token wrapped in
\ a two-cell record. The consumer the capability was built for is not that shape: it is
\ GPT2LOAD:gpt2-model, a nine-cell record whose linearity arrives through an embedded
\ three-cell WSTORE:store, carrying a four-cell configuration identity and a private construction proof
\ beside it. Nothing before this file put that type through a declared payload slot, so
\ "it works for the real model" was an inference from two smaller cases rather than a
\ measurement. This suite makes it a measurement.
\
\ WHAT "THE REAL PATH" MEANS HERE. The model this suite wraps is not constructed by the
\ suite. It is obtained by parsing the pinned 548 MB GPT-2 checkpoint through
\ SAFET:LOAD, validating it against the real 124M configuration through
\ GPT2LOAD:PREPARE, comparing the captured identity through GPT2LOAD:CHECK-MAPPED,
\ and loading mapped storage through GPT2LOAD:LOAD-MAPPED. The value that then goes into the ENUM
\ variant owns the whole checkpoint mapping, and the proof that it still does after the
\ round trip is that RELEASE-MODEL gives back exactly 548105171 bytes at the end. The
\ mapped-storage path is enough: it is zero-copy, so the round trip costs no second copy of the
\ weights, and the copied-storage path would prove nothing further about the payload slot.
\
\ WHY THIS SUITE HAS ITS OWN PACKAGE AND ITS OWN CONFIGURATION. The three GPT2LOAD
\ suites reopen package GPT2LOAD, because what they check is which rows the load
\ validated and no public word hands a row out. This suite checks the opposite thing:
\ what an outside consumer can declare and do with a loaded model. So it runs from its own
\ package, calls only the public surface, and spells out the checkpoint path and the
\ 124M configuration itself rather than reaching into the checkpoint fixture's private ones
\ (they are private: package GPT2LOAD publishes none of these private test helpers).
\ That costs one
\ repeated configuration and checks the true outside-package surface. The repeat is
\ self-checking - a configuration that drifted from the checkpoint is rejected by PREPARE
\ and this suite goes red rather than quiet.
\
\ THE WIDTH-IDENTICAL CONTROL. `model-payload` and `plain-payload`
\ are both ten cells wide - a tag cell plus a nine-cell payload - and they differ in
\ exactly one thing: `model-payload`'s payload owns a linear value and `plain-payload`'s does not. A
\ rejection on its own proves nothing, because it could be answering payload width,
\ nesting level, declared arity, return-stack balance, or a multi-cell value the operation simply
\ cannot express. Where that ambiguity matters, the suite repeats the exact
\ operation over `plain-payload`. Four control categories turn out NOT to test
\ linearity, and this file says so out loud
\ rather than banking them as evidence:
\   - a raw-cell construction, an unconsumed value and an unbalanced `>r` reject `plain-payload`
\     too, so those are nominal typing, arity and return-stack balance;
\   - a typed local capturing the payload rejects `plain-payload` too, so that is about
\     capturing a multi-cell layout value in a local, not about hiding a linear owner (a
\     one-cell typed local is accepted, which is the control for the control);
\   - `!` rejects `plain-payload` too, so only the `@` test distinguishes linearity;
\   - keeping the matched input value across a MATCH rejects `plain-payload` too, because MATCH
\     consumes what it matches for every family.
\ The paired cases left after that subtraction isolate copying, discarding,
\ return-stack re-push, typed-memory load, and selected MATCH behavior. Unpaired
\ negative cases remain contract checks, not independent proof of why the checker
\ rejected them.
\
\ WHY LINEARITY IS PINNED THROUGH THE CHECKER AND NOT THROUGH REFLECTION. REFLECT
\ cannot report linearity today: it reads the registry through the checker's published primitives, and the
\ predicate that decides this - TFAM-CONCRETE-LINEAR? - is not one of them (it has no
\ PRIM: row in src/core/checker.f, so it is unreachable from any ordinary package).
\ Independent reflection evidence is therefore unavailable. Everything REFLECT
\ can see is pinned below - width, payload slot, arity, kind, case order, and
\ constructor package - while the paired checker cases provide the current
\ linearity proof.
\
\ maki -> habu only.

require lib/prelude.f
require lib/adt/result.f
require lib/test.f
require test/checker-assert.f
require maki/infer/gpt2-load.f

package GPT2PAYLOAD

public

\ ---- the declaration the capability legalized ----------------------------------
\ `FIELD model GPT2LOAD:gpt2-model` is the exact spelling that rejected 7109 "unknown payload
\ type" before the capability landed, about a family that had just registered
\ successfully. The second variant is the ordinary rejection shape a real consumer pairs
\ it with.
ENUM model-payload 0
   VARIANT model FIELD model GPT2LOAD:gpt2-model ;VARIANT
   VARIANT rejected FIELD code n ;VARIANT
;ENUM

\ The control: the same ten cells, no linear value anywhere. MDLCFG:cfgkey is four
\ cells and the five scalars make nine, so plain-payload matches model-payload
\ cell for cell.
ENUM plain-payload 0
   VARIANT key   FIELD k MDLCFG:cfgkey
      FIELD a n FIELD b n FIELD c n FIELD d n FIELD e n ;VARIANT
   VARIANT rejected FIELD code n ;VARIANT
;ENUM

private

using SAFET
using GPT2LOAD
using SAFET-MAP
using WSTORE

\ ---- the pinned artifact and the configuration it was exported from -------------
\ The exact file size is the assertion that matters at the end of the cycle: a mapped
\ model's release reports the byte count WSTORE gave back, so pinning it proves the value
\ that came out of the payload slot was still serving the WHOLE checkpoint and not some
\ truncated span of it. The layer count cannot be asserted here because
\ GPT2LOAD:MODEL-LAYER-COUNT is package-private, which is exactly the
\ outside-package surface this suite tests.
548105171 constant FILE-BYTES                   \ the pinned checkpoint's exact file size

: CHECKPOINT-PATH ( -- ptr u8 n )  s" gpt2-model/model.safetensors" ;

: CHECKPOINT-PRESENT? ( -- bool )  CHECKPOINT-PATH PRESENT? ;

: MODEL-CONFIG ( -- MDLCFG:mcfg )                        \ the real 124M geometry
   0.00001 true MDLCFG-ARCH:GPT2
   1 MAKI-DTYPE:DF32 1024 50257 12 768 12 true 50256 50256 MDLCFG:BUILD ;

\ ---- candidate verdicts --------------------------------------------------------
\ -1 is "the checker certified this definition", 0 is "the checker rejected it". The
\ two are kept apart from verdict 1, which is "the dictionary could not resolve a
\ token", so a typo can never be mistaken for a type error.
: ACCEPTED ( ptr u8 n -- )  CHECK-QUIET-CANDIDATE! -1 T= ;
: REJECTED ( ptr u8 n -- )  CHECK-QUIET-CANDIDATE! 0 T= ;

\ ---- leak accounting, as a delta against this suite's own entry -----------------
\ The four counters are process-wide and a combined run reaches this file after suites
\ with known counter deltas, so every check is relative to this suite's entry
\ baseline and never absolute. One word takes all four numbers, so each check reads as
\ the four owners it expects rather than as four separate assertions.
variable BASE-MAPPING                           \ SAFET kernel mappings
variable BASE-OWNER                             \ SAFET owners
variable BASE-STORE                             \ WSTORE blocks
variable BASE-PREPARED-LOADS                   \ live GPT2LOAD prepared loads

: BASELINE! ( -- )
   SAFET-MAP:LIVE BASE-MAPPING !
   LIVE-OWNERS BASE-OWNER !
   WSTORE:LIVE BASE-STORE !
   LIVE-PREPARED-LOADS BASE-PREPARED-LOADS ! ;

: COUNTERS ( n n n n -- ) {: mapping:n owner:n store:n prepared:n :}
   SAFET-MAP:LIVE BASE-MAPPING @ - mapping T=
   LIVE-OWNERS BASE-OWNER @ - owner T=
   WSTORE:LIVE BASE-STORE @ - store T=
   LIVE-PREPARED-LOADS BASE-PREPARED-LOADS @ - prepared T= ;

;using
;using

\ The ok payload of a release outcome. Reading it is the difference between "the exit
\ reported success" and "the exit gave back the bytes it was holding".
: RESULT-VALUE ( result<n,n> -- n )
   MATCH result
      ok  OF ENDOF
      err OF
         s" a release reported err, code" T-LABEL
         . cr
         false TTRUE
         -1
      ENDOF
   ;MATCH ;

\ ---- a real model through a declared payload slot -----------------------------
\ The MATCH branch receives the model as an ordinary linear value on the row, so the branch has
\ to discharge it exactly once; discharging it through RELEASE-MODEL is what proves the
\ value that came back out is the same owner that went in, because only the real owner
\ can hand back the whole checkpoint mapping.
: RELEASE-MODEL-PAYLOAD ( model-payload -- )
   MATCH model-payload
      model OF
         s" the counters do not move when MATCH hands the model back out" T-LABEL
         1 1 1 0 COUNTERS
         s" and the model still owns the whole checkpoint mapping, to the byte" T-LABEL
         RELEASE-MODEL RESULT-VALUE FILE-BYTES T=
      ENDOF
      rejected OF
         s" the wrapped model came back as the rejected variant" T-LABEL
         drop false TTRUE
      ENDOF
   ;MATCH ;

: CHECK-PAYLOAD-OWNER ( GPT2LOAD:mapped-ready -- )
   LOAD-MAPPED
   s" the real checkpoint loads to a mapped model" T-LABEL
   1 1 1 0 COUNTERS
   GPT2PAYLOAD-MODEL--PAYLOAD:MODEL
   s" and wrapping it in a declared payload slot moves no owner" T-LABEL
   1 1 1 0 COUNTERS
   RELEASE-MODEL-PAYLOAD
   s" and the whole cycle gives every owner back" T-LABEL
   0 0 0 0 COUNTERS ;

: CHECK-MAPPED-PAYLOAD ( GPT2LOAD:prepared-load -- )
   MODEL-CONFIG CHECK-MAPPED
   MATCH GPT2LOAD:mapped-check-result
      ready OF CHECK-PAYLOAD-OWNER ENDOF
      rejected OF
         {: code:n :}
         s" the real checkpoint was rejected by its matching configuration, code" T-LABEL
         code . cr
         false TTRUE
         DISCARD-PREPARED
      ENDOF
   ;MATCH ;

: T-REAL-PAYLOAD ( -- )
   s" a real loaded GPT-2 model is stored in a declared linear payload field" T-LABEL
   BASELINE!
   0 0 0 0 COUNTERS
   CHECKPOINT-PATH LOAD MODEL-CONFIG PREPARE
   MATCH GPT2LOAD:prepare-result
      prepared OF CHECK-MAPPED-PAYLOAD ENDOF
      rejected OF
         {: code:n :}
         s" the real checkpoint did not prepare, code" T-LABEL
         code . cr
         false TTRUE
         RELEASE
      ENDOF
   ;MATCH
   0 0 0 0 COUNTERS ;

;using

\ ---- family identity for the registry readers ----------------------------------
\ A family is identified by its tail PLUS the constructor package its variants carry, so
\ these readers cannot silently pin a neighbouring family that happens to share a tail.
: MODEL-PAYLOAD-IDENTITY ( -- ptr u8 n ptr u8 n )
   s" model-payload" s" GPT2PAYLOAD-MODEL--PAYLOAD" ;

: PLAIN-PAYLOAD-IDENTITY ( -- ptr u8 n ptr u8 n )
   s" plain-payload" s" GPT2PAYLOAD-PLAIN--PAYLOAD" ;

;using

\ ---------------------------------------------------------------------------------
\ What the registry recorded. These are top-level lines rather than the body of a word
\ because TK-SUM is an engine registry constant the checker does not publish into user
\ definitions; the existing registry pins (maki/db/obligation-test.f,
\ lib/cad-num-types-test.f) read it the same way.
\ ---------------------------------------------------------------------------------
T-RESET

using REFLECT

s" model-payload is one public arity-0 payload-bearing ENUM" T-LABEL
MODEL-PAYLOAD-IDENTITY FAMS 1 T=
MODEL-PAYLOAD-IDENTITY ARITY 0 T=
MODEL-PAYLOAD-IDENTITY VIS 1 T=
MODEL-PAYLOAD-IDENTITY KIND TK-SUM T=             \ internal kind for payload-bearing ENUM
MODEL-PAYLOAD-IDENTITY KIND TK-ENUM = 0 T=          \ not a payloadless enum
MODEL-PAYLOAD-IDENTITY VARS 2 T=
s" its cases are in declaration order, under this suite's constructor package" T-LABEL
MODEL-PAYLOAD-IDENTITY 0 ARM$ s" model" T$=
MODEL-PAYLOAD-IDENTITY 1 ARM$ s" rejected" T$=
MODEL-PAYLOAD-IDENTITY 0 ARM-CTOR$ s" GPT2PAYLOAD-MODEL--PAYLOAD" T$=
s" the model occupies one named payload field, nine cells wide, at slot 0" T-LABEL
MODEL-PAYLOAD-IDENTITY 0 ARM-FLDS 1 T=
MODEL-PAYLOAD-IDENTITY 0 s" model" ARM-SLOT 0 T=
MODEL-PAYLOAD-IDENTITY 0 s" model" ARM-CELLS 9 T=
s" so the family is a tag cell plus that payload: ten cells" T-LABEL
MODEL-PAYLOAD-IDENTITY WIDTH 10 T=
s" and the non-linear control is ten cells wide too, from six declared fields" T-LABEL
PLAIN-PAYLOAD-IDENTITY FAMS 1 T=
PLAIN-PAYLOAD-IDENTITY WIDTH 10 T=
PLAIN-PAYLOAD-IDENTITY 0 ARM-FLDS 6 T=

;using

\ ---------------------------------------------------------------------------------
\ The model-payload value is ONE linear unit. Paired plain-payload controls isolate
\ the linearity reason where needed; unpaired negatives are contract checks.
\ ---------------------------------------------------------------------------------
s" identity and permutation conserve model-payload" T-LABEL
s" MODEL-PAYLOAD-PASS ( model-payload -- model-payload )" ACCEPTED
s" MODEL-PAYLOAD-SWAP ( model-payload n -- n model-payload ) swap" ACCEPTED
s" MODEL-PAYLOAD-ROTATE ( model-payload n n -- n n model-payload ) rot" ACCEPTED
s" MODEL-PAYLOAD-2SWAP ( model-payload n n n -- n n model-payload n ) 2swap" ACCEPTED

s" copying model-payload is rejected, and plain-payload copies freely" T-LABEL
s" MODEL-PAYLOAD-DUP ( model-payload -- model-payload model-payload ) dup" REJECTED
s" PLAIN-PAYLOAD-DUP ( plain-payload -- plain-payload plain-payload ) dup" ACCEPTED
s" MODEL-PAYLOAD-OVER ( model-payload n -- model-payload n model-payload ) over" REJECTED
s" MODEL-PAYLOAD-2DUP ( model-payload n -- model-payload n model-payload n ) 2dup" REJECTED
s" PLAIN-PAYLOAD-2DUP ( plain-payload n -- plain-payload n plain-payload n ) 2dup" ACCEPTED
s" MODEL-PAYLOAD-TUCK ( n model-payload -- model-payload n model-payload ) tuck" REJECTED

s" discarding model-payload is rejected, and plain-payload discards freely" T-LABEL
s" MODEL-PAYLOAD-DROP ( model-payload -- ) drop" REJECTED
s" PLAIN-PAYLOAD-DROP ( plain-payload -- ) drop" ACCEPTED
s" MODEL-PAYLOAD-NIP ( model-payload model-payload -- model-payload ) nip" REJECTED
s" PLAIN-PAYLOAD-NIP ( plain-payload plain-payload -- plain-payload ) nip" ACCEPTED
s" MODEL-PAYLOAD-2DROP ( model-payload n -- ) 2drop" REJECTED
s" PLAIN-PAYLOAD-2DROP ( plain-payload n -- ) 2drop" ACCEPTED

s" a return-stack round trip conserves model-payload; re-pushing it copies it" T-LABEL
s" MODEL-PAYLOAD-RSTACK-MOVE ( model-payload -- model-payload ) >r r>" ACCEPTED
s" MODEL-PAYLOAD-RSTACK-COPY ( model-payload -- model-payload model-payload ) >r r@ r>" REJECTED
s" PLAIN-PAYLOAD-RSTACK-COPY ( plain-payload -- plain-payload plain-payload ) >r r@ r>" ACCEPTED

s" typed memory stays closed to model-payload: the load discriminates, the store does not" T-LABEL
s" MODEL-PAYLOAD-MEMORY-LOAD ( ptr model-payload -- model-payload ) @" REJECTED
s" PLAIN-PAYLOAD-MEMORY-LOAD ( ptr plain-payload -- plain-payload ) @" ACCEPTED
\ NOT LINEARITY: `!` rejects plain-payload as well, so the store half of this pair is the
\ checker declining to express a multi-cell store at all. It is pinned so a later change
\ that made `!` work for layout values cannot quietly open it for the linear one.
s" MODEL-PAYLOAD-MEMORY-STORE ( model-payload ptr n -- ) !" REJECTED
s" PLAIN-PAYLOAD-MEMORY-STORE ( plain-payload ptr n -- ) !" REJECTED

\ NOT LINEARITY: a typed local capturing model-payload rejects plain-payload too, so this is
\ about capturing a multi-cell layout value in a local. The one-cell control shows typed
\ locals themselves are fine, which is what makes the pair readable.
s" a typed local cannot capture either ten-cell payload (locals, not linearity)" T-LABEL
s" MODEL-PAYLOAD-LOCAL ( model-payload -- model-payload ) {: v:model-payload :} v" REJECTED
s" PLAIN-PAYLOAD-LOCAL ( plain-payload -- plain-payload ) {: v:plain-payload :} v" REJECTED
s" SCALAR-LOCAL ( n -- n ) {: v:n :} v" ACCEPTED

\ NOT LINEARITY: raw-cell construction and leaving a value unconsumed reject
\ plain-payload too - the first is nominal typing, the second is the declared arity.
s" a payload cannot be built from a raw cell or left unconsumed (typing and arity)" T-LABEL
s" MODEL-PAYLOAD-FROM-RAW-CELL ( n -- model-payload ) " REJECTED
s" PLAIN-PAYLOAD-FROM-RAW-CELL ( n -- plain-payload ) " REJECTED
s" MODEL-PAYLOAD-UNCONSUMED ( model-payload -- )" REJECTED
s" PLAIN-PAYLOAD-UNCONSUMED ( plain-payload -- )" REJECTED
\ NOT LINEARITY either: an unbalanced `>r` rejects plain-payload, so this rejection
\ is return-stack balance. MODEL-PAYLOAD-RSTACK-COPY above provides the
\ linear-specific evidence.
s" MODEL-PAYLOAD-UNBALANCED-RSTACK ( model-payload -- ) >r" REJECTED
s" PLAIN-PAYLOAD-UNBALANCED-RSTACK ( plain-payload -- ) >r" REJECTED

\ ---------------------------------------------------------------------------------
\ Construction consumes the model exactly once and creates one model-payload.
\ ---------------------------------------------------------------------------------
s" wrapping a real model creates one model-payload through the word or inline form" T-LABEL
s" MODEL-PAYLOAD-FROM-MODEL ( GPT2LOAD:gpt2-model -- model-payload ) GPT2PAYLOAD-MODEL--PAYLOAD:MODEL" ACCEPTED
s" MODEL-PAYLOAD-FROM-MODEL-INLINE ( GPT2LOAD:gpt2-model -- model-payload ) construct model-payload model" ACCEPTED
s" MODEL-PAYLOAD-FROM-REJECTION ( n -- model-payload ) GPT2PAYLOAD-MODEL--PAYLOAD:REJECTED" ACCEPTED

\ Wrong roles: the payload slot is nominal, so neither variant will take the other's
\ argument, the two ten-cell families do not substitute for each other, and a MATCH
\ naming the wrong family rejects rather than reading the tag it happens to share.
s" the two variants' payloads cannot cross, and neither can the two families" T-LABEL
s" MODEL-PAYLOAD-WRONG-MODEL-CELL ( n -- model-payload ) GPT2PAYLOAD-MODEL--PAYLOAD:MODEL" REJECTED
s" MODEL-PAYLOAD-WRONG-REJECTION-MODEL ( GPT2LOAD:gpt2-model -- model-payload ) GPT2PAYLOAD-MODEL--PAYLOAD:REJECTED" REJECTED
s" PLAIN-PAYLOAD-WRONG-MODEL ( GPT2LOAD:gpt2-model -- plain-payload ) GPT2PAYLOAD-PLAIN--PAYLOAD:KEY" REJECTED
s" MODEL-PAYLOAD-AS-PLAIN-PAYLOAD ( model-payload -- plain-payload )" REJECTED
s" MODEL-PAYLOAD-MATCH-WRONG-FAMILY ( model-payload -- n ) MATCH plain-payload key OF drop drop drop drop drop drop 0 ENDOF rejected OF ENDOF ;MATCH" REJECTED

s" losing, copying or re-using the model at construction is rejected" T-LABEL
s" MODEL-PAYLOAD-CONSTRUCT-DROPS-MODEL ( GPT2LOAD:gpt2-model n -- model-payload ) nip GPT2PAYLOAD-MODEL--PAYLOAD:REJECTED" REJECTED
s" MODEL-PAYLOAD-CONSTRUCT-COPIES-MODEL ( GPT2LOAD:gpt2-model -- model-payload model-payload ) GPT2PAYLOAD-MODEL--PAYLOAD:MODEL dup" REJECTED
s" MODEL-PAYLOAD-CONSTRUCT-MISSING-BRANCH ( GPT2LOAD:gpt2-model f -- model-payload ) if GPT2PAYLOAD-MODEL--PAYLOAD:MODEL then" REJECTED
s" MODEL-PAYLOAD-CONSTRUCT-TWICE ( GPT2LOAD:gpt2-model -- model-payload ) GPT2PAYLOAD-MODEL--PAYLOAD:MODEL GPT2PAYLOAD-MODEL--PAYLOAD:MODEL" REJECTED

\ ---------------------------------------------------------------------------------
\ MATCH hands the payload to a branch as a linear value that branch must discharge exactly
\ once. Disposing it through the real production exit is valid; so is wrapping
\ the payload. Every other ending is rejected.
\ ---------------------------------------------------------------------------------
s" a MATCH branch that releases the model through its real exit certifies" T-LABEL
s" MODEL-PAYLOAD-MATCH-RELEASE ( model-payload -- n ) MATCH model-payload model OF GPT2LOAD:RELEASE-MODEL drop 0 ENDOF rejected OF ENDOF ;MATCH" ACCEPTED
s" MODEL-PAYLOAD-MATCH-RELEASE-RESULT ( model-payload -- n ) MATCH model-payload model OF GPT2LOAD:RELEASE-MODEL MATCH result ok OF ENDOF err OF ENDOF ;MATCH ENDOF rejected OF ENDOF ;MATCH" ACCEPTED
s" and so does a branch that wraps the payload again" T-LABEL
s" MODEL-PAYLOAD-MATCH-REWRAP ( model-payload -- model-payload ) MATCH model-payload model OF GPT2PAYLOAD-MODEL--PAYLOAD:MODEL ENDOF rejected OF GPT2PAYLOAD-MODEL--PAYLOAD:REJECTED ENDOF ;MATCH" ACCEPTED

s" dropping the payload inside the branch is rejected, and plain-payload drops freely" T-LABEL
s" MODEL-PAYLOAD-MATCH-DROP ( model-payload -- n ) MATCH model-payload model OF drop 0 ENDOF rejected OF ENDOF ;MATCH" REJECTED
s" PLAIN-PAYLOAD-MATCH-DROP ( plain-payload -- n ) MATCH plain-payload key OF drop drop drop drop drop drop 0 ENDOF rejected OF ENDOF ;MATCH" ACCEPTED

s" copying the payload to release it twice is rejected" T-LABEL
s" MODEL-PAYLOAD-MATCH-RELEASE-TWICE ( model-payload -- n ) MATCH model-payload model OF dup GPT2LOAD:RELEASE-MODEL drop GPT2LOAD:RELEASE-MODEL drop 0 ENDOF rejected OF ENDOF ;MATCH" REJECTED
s" and so is matching the same model-payload twice, which requires a copy" T-LABEL
s" MODEL-PAYLOAD-MATCH-TWICE ( model-payload -- n n ) dup MATCH model-payload model OF GPT2LOAD:RELEASE-MODEL drop 0 ENDOF rejected OF ENDOF ;MATCH swap MATCH model-payload model OF GPT2LOAD:RELEASE-MODEL drop 0 ENDOF rejected OF ENDOF ;MATCH" REJECTED

s" leaving the payload on the row, stranding it, or exiting past it is rejected" T-LABEL
s" MODEL-PAYLOAD-MATCH-KEEP-PAYLOAD ( model-payload -- n ) MATCH model-payload model OF 0 ENDOF rejected OF ENDOF ;MATCH" REJECTED
s" MODEL-PAYLOAD-MATCH-RSTACK-ESCAPE ( model-payload -- n ) MATCH model-payload model OF >r 0 ENDOF rejected OF ENDOF ;MATCH" REJECTED
s" MODEL-PAYLOAD-MATCH-EARLY-EXIT ( model-payload -- n ) MATCH model-payload model OF exit ENDOF rejected OF ENDOF ;MATCH" REJECTED

s" unpacking the model inside the branch keeps the obligation on its store" T-LABEL
s" MODEL-PAYLOAD-MATCH-UNMAKE-MODEL ( model-payload -- n ) MATCH model-payload model OF GPT2LOAD-GPT2--MODEL:UNMAKE drop drop drop drop 0 ENDOF rejected OF ENDOF ;MATCH" REJECTED

\ NOT LINEARITY: MATCH consumes what it matches for every family, so keeping the
\ matched input value is rejected for plain-payload as well.
s" a model-payload value cannot survive its own MATCH (MATCH consumes, not linearity)" T-LABEL
s" MODEL-PAYLOAD-MATCH-KEEP-INPUT ( model-payload -- model-payload n ) MATCH model-payload model OF GPT2LOAD:RELEASE-MODEL drop 0 ENDOF rejected OF ENDOF ;MATCH" REJECTED
s" PLAIN-PAYLOAD-MATCH-KEEP-INPUT ( plain-payload -- plain-payload n ) MATCH plain-payload key OF drop drop drop drop drop drop 0 ENDOF rejected OF ENDOF ;MATCH" REJECTED

\ ---------------------------------------------------------------------------------
\ KNOWN GAP: `throw` abandons a linear value owned by a MATCH branch.
\
\ The three pins below are written as ACCEPT deliberately - they record what the checker
\ certifies today, in the MODELPROV T-KNOWN-GAP / GPT2LOAD-OUTSIDE-TEST convention. A branch that
\ throws while still owning the model certifies, and so does a branch that catches a
\ throw and carries on. On the throw edge the payload is simply abandoned: nothing
\ releases it, and a surrounding catch resumes with the checkpoint mapping stranded and
\ no handle left to reach it.
\
\ This is not something the linear-payload capability introduced. It is the engine-wide
\ behaviour recorded as finding 6 of .blackboard/txn-v2-plan-20260726.md ("throw abandons
\ linears in MATCH branches and quotations", measured byte-identical on the parent engine).
\ The real fix is checker-enforced linear-scope quotations: a
\ quotation shape that guarantees threaded owners are returned or released on every edge
\ including throw. When that capability lands these three tests fail, and that failure is
\ the signal to turn them into REJECTED and delete this banner.
\
\ Nothing here guards or works around the gap, and the runtime consequence is
\ deliberately not executed: driving the throw edge over the real model would abandon the
\ 548 MB checkpoint mapping for the rest of the process with no handle to restore it.
\
\ The throw code in these candidates is never thrown - a candidate is checked, never
\ run - so it is written as a bare cell rather than borrowed from a module's range.
\ ---------------------------------------------------------------------------------
s" KNOWN GAP: a branch that throws while owning the model certifies, abandoning it" T-LABEL
s" MODEL-PAYLOAD-THROW-GAP ( model-payload -- n ) MATCH model-payload model OF -5699 throw ENDOF rejected OF ENDOF ;MATCH" ACCEPTED
s" KNOWN GAP: a catch inside the branch resumes past the throw while the branch still owns the model" T-LABEL
s" MODEL-PAYLOAD-CATCH-GAP ( model-payload -- n ) MATCH model-payload model OF [: -5699 throw ;] catch drop GPT2LOAD:RELEASE-MODEL drop 0 ENDOF rejected OF ENDOF ;MATCH" ACCEPTED
s" CONTROL: plain-payload accepts the throw edge, so the gap is the abandoned owner" T-LABEL
s" PLAIN-PAYLOAD-THROW ( plain-payload -- n ) MATCH plain-payload key OF drop drop drop drop drop drop -5699 throw ENDOF rejected OF ENDOF ;MATCH" ACCEPTED
s" CONTROL: a branch that releases before it throws is legitimate, not a gap" T-LABEL
s" MODEL-PAYLOAD-RELEASE-THEN-THROW ( model-payload -- n ) MATCH model-payload model OF GPT2LOAD:RELEASE-MODEL drop -5699 throw ENDOF rejected OF ENDOF ;MATCH" ACCEPTED

\ ---- the real-model test ------------------------------------------------------
\ Presence-gated the way the other real-checkpoint tests are (gpt2-mapped-test.f,
\ gpt2-copy-test.f), so a host without the 548 MB artifact reports the skip loudly
\ instead of failing. Everything above runs unconditionally.
: RUN ( -- )
   CHECKPOINT-PRESENT? if
      T-REAL-PAYLOAD
   else
      s" gpt2-payload: gpt2-model/model.safetensors absent -> real-model test SKIPPED" type cr
   then ;

RUN
T-REPORT

;package
