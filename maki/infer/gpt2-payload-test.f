\ gpt2-payload-test.f - the linear-payload declaration capability, exercised over the
\ REAL bound GPT-2 model rather than over a stand-in.
\
\ WHY THIS SUITE EXISTS. The capability that lets a unified ENUM variant or STRUCTURE
\ field name a family which owns a linear value is pinned in two places already:
\ test/enum-decl-suite.f and test/structure-decl-suite.f pin what the REGISTRY records,
\ and test/type-linear-suite.f pins what the CHECKER does with such a value on a row.
\ Both of those work over small stand-in owners - a one-cell DEFLINEAR token wrapped in
\ a two-cell record. The consumer the capability was built for is not that shape: it is
\ GPT2TX:gpt2-model, a seven-cell record whose linearity arrives through a nested
\ WSTORE:resident, carrying a four-cell configuration identity and a private-mint proof
\ beside it. Nothing before this file put that type through a declared payload slot, so
\ "it works for the real model" was an inference from two smaller cases rather than a
\ measurement. This suite makes it a measurement.
\
\ WHAT "THE REAL PATH" MEANS HERE. The model this suite wraps is not constructed by the
\ suite. It is obtained the only way production obtains one: census the pinned 548 MB
\ GPT-2 checkpoint through SAFET:LOAD, validate it against the real 124M configuration
\ through GPT2TX:PREPARE, compare the captured identity through GPT2TX:CHECK, and commit
\ the mapped arm through GPT2TX:COMMIT-MAPPED. The value that then goes into the ENUM
\ variant owns the whole checkpoint mapping, and the proof that it still does after the
\ round trip is that MODEL-DISPOSE gives back exactly 548105171 bytes at the end. The
\ mapped arm is enough: it is zero-copy, so the round trip costs no second copy of the
\ weights, and the allocated arm would prove nothing further about the payload slot.
\
\ WHY THIS SUITE HAS ITS OWN PACKAGE AND ITS OWN CONFIGURATION. The three GPT2TX bind
\ suites reopen package GPT2TX, because what they check is which rows the transaction
\ validated and no public word hands a row out. This suite checks the opposite thing:
\ what a FOREIGN consumer can declare and do with a bound model. So it runs from its own
\ package, calls only the public surface, and spells out the checkpoint path and the
\ 124M configuration itself rather than reaching into the bind fixture's private ones
\ (they are private: package GPT2TX publishes none of its TX- fixtures). That costs one
\ repeated configuration and buys the honest vantage point, and the repeat is
\ self-checking - a configuration that drifted from the checkpoint is refused by PREPARE
\ and this suite goes red rather than quiet.
\
\ THE WIDTH-IDENTICAL CONTROL, AND WHY EVERY REFUSAL IS PAIRED. `held` and `held-twin`
\ are both eight cells wide - a tag cell plus a seven-cell payload - and they differ in
\ exactly one thing: `held`'s payload owns a linear value and `held-twin`'s does not. A
\ refusal on its own proves nothing, because it could be answering payload width, nesting
\ depth, declared arity, return-stack balance, or a multi-cell value the operation simply
\ cannot express. So every refusal below is run again over the twin, and the pair is what
\ is reported. Three of them turn out NOT to be linearity, and this file says so out loud
\ rather than banking them as evidence:
\   - a raw-cell forge, an unconsumed value and an unbalanced `>r` refuse for the twin
\     too, so those are nominal typing, arity and return-stack balance;
\   - a typed local capturing the bundle refuses for the twin too, so that is about
\     capturing a multi-cell layout value in a local, not about laundering an owner (a
\     one-cell typed local is accepted, which is the control for the control);
\   - `!` refuses for the twin too, so only the `@` half of the typed-memory pair
\     discriminates;
\   - keeping the scrutinee across a MATCH refuses for the twin too, because MATCH
\     consumes what it matches for every family.
\ What is left after that subtraction is the linearity obligation, and it is large:
\ copy, discard, re-push from the return stack, load through typed memory, payload loss
\ at construction, and every way a MATCH arm can fail to discharge the payload exactly
\ once.
\
\ WHY LINEARITY IS PINNED THROUGH THE CHECKER AND NOT THROUGH REFLECTION. The leaf asked
\ for the declared family to "read linear" through package REFLECT. REFLECT cannot answer
\ that today: it reads the registry through the checker's published primitives, and the
\ predicate that decides this - TFAM-CONCRETE-LINEAR? - is not one of them (it has no
\ PRIM: row in src/core/checker.f, so it is unreachable from any ordinary package).
\ Publishing it is an engine change and is not this leaf's to make. Everything REFLECT
\ CAN see is pinned below - the eight-cell width, the seven-cell payload at slot 0, the
\ arity, the kind, the case order and the constructor package - and linearity itself is
\ pinned where it is actually decided, on the checker, by the paired battery.
\
\ maki -> habu only.

require lib/prelude.f
require lib/adt/result.f
require lib/test.f
require test/checker-assert.f
require maki/infer/gpt2-bind.f

package GPT2PAY

public

\ ---- the declaration the capability legalized ----------------------------------
\ `FIELD m GPT2TX:gpt2-model` is the exact spelling that rejected 7109 "unknown payload
\ type" before the capability landed, about a family that had just registered
\ successfully. The second variant is the ordinary refusal shape a real consumer pairs
\ it with.
ENUM held 0
   VARIANT model FIELD m GPT2TX:gpt2-model ;VARIANT
   VARIANT empty FIELD code n ;VARIANT
;ENUM

\ The control: the same eight cells, no linear value anywhere. MDLCFG:cfgkey is four
\ cells and the three scalars make seven, so the twin's payload matches the model's cell
\ for cell.
ENUM held-twin 0
   VARIANT key   FIELD k MDLCFG:cfgkey FIELD a n FIELD b n FIELD c n ;VARIANT
   VARIANT empty FIELD code n ;VARIANT
;ENUM

private

\ ---- the pinned artifact and the configuration it was exported from -------------
\ The exact file size is the assertion that matters at the end of the cycle: a mapped
\ model's release reports the byte count WSTORE gave back, so pinning it proves the value
\ that came out of the payload slot was still serving the WHOLE checkpoint and not some
\ truncated span of it. The depth cannot be asserted from here - GPT2TX:MODEL-NL is
\ package-private, which is exactly the foreign vantage point this suite is written from.
548105171 constant FILE-BYTES                   \ the pinned checkpoint's exact file size

: PATH ( -- ptr u8 n )  s" gpt2-model/model.safetensors" ;

: CFG ( -- MDLCFG:mcfg )                        \ the real 124M geometry
   0.00001 true MDLCFG-ARCH:GPT2
   1 MAKI-DTYPE:DF32 1024 50257 12 768 12 true 50256 50256 MDLCFG:BUILD ;

\ ---- candidate verdicts --------------------------------------------------------
\ -1 is "the checker certified this definition", 0 is "the checker refused it". The
\ two are kept apart from verdict 1, which is "the dictionary could not resolve a
\ token", so a typo can never be mistaken for a type error.
: ACCEPTED ( ptr u8 n -- )  CHECK-QUIET-CANDIDATE! -1 T= ;
: REJECTED ( ptr u8 n -- )  CHECK-QUIET-CANDIDATE! 0 T= ;

\ ---- leak accounting, as a delta against this suite's own entry -----------------
\ The four counters are process-wide and a combined run reaches this file after suites
\ that leave documented strands of their own, so every station is a delta from the entry
\ baseline and never an absolute. One word takes all four numbers, so a station reads as
\ the four owners it expects rather than as four separate assertions.
variable BASE-MAP                               \ SAFET kernel mappings
variable BASE-OWN                               \ SAFET owners
variable BASE-WS                                \ WSTORE blocks
variable BASE-PREP                              \ live GPT2TX preps

: BASELINE! ( -- )
   SAFET-MAP:LIVE BASE-MAP !
   SAFET:LIVE-OWNERS BASE-OWN !
   WSTORE:LIVE BASE-WS !
   GPT2TX:LIVE BASE-PREP ! ;

: COUNTERS ( n n n n -- ) {: map:n own:n ws:n prep:n :}
   SAFET-MAP:LIVE BASE-MAP @ - map T=
   SAFET:LIVE-OWNERS BASE-OWN @ - own T=
   WSTORE:LIVE BASE-WS @ - ws T=
   GPT2TX:LIVE BASE-PREP @ - prep T= ;

\ The ok payload of a release outcome. Reading it is the difference between "the exit
\ reported success" and "the exit gave back the bytes it was holding".
: RES-VAL ( result<n,n> -- n )
   MATCH result
      ok  OF ENDOF
      err OF
         s" a release reported err, code" T-LABEL
         . cr
         false TTRUE
         -1
      ENDOF
   ;MATCH ;

\ ---- the positive leg: a real model through a declared payload slot -------------
\ The MATCH arm receives the model as an ordinary linear value on the row, so the arm has
\ to discharge it exactly once; discharging it through MODEL-DISPOSE is what proves the
\ value that came back out is the same owner that went in, because only the real owner
\ can hand back the whole checkpoint mapping.
: SPEND ( held -- )
   MATCH held
      model OF
         s" the counters do not move when MATCH hands the model back out" T-LABEL
         1 1 1 0 COUNTERS
         s" and the model still owns the whole checkpoint mapping, to the byte" T-LABEL
         GPT2TX:MODEL-DISPOSE RES-VAL FILE-BYTES T=
      ENDOF
      empty OF
         s" the wrapped model came back as the empty variant" T-LABEL
         drop false TTRUE
      ENDOF
   ;MATCH ;

: WRAP-AND-SPEND ( GPT2TX:checked-prep -- )
   GPT2TX:COMMIT-MAPPED
   s" the real checkpoint commits to a mapped model" T-LABEL
   1 1 1 0 COUNTERS
   GPT2PAY-HELD:MODEL
   s" and wrapping it in a declared payload slot moves no owner" T-LABEL
   1 1 1 0 COUNTERS
   SPEND
   s" and the whole cycle gives every owner back" T-LABEL
   0 0 0 0 COUNTERS ;

: CHECKED-ARM ( GPT2TX:prep -- )
   CFG GPT2TX:CHECK
   MATCH GPT2TX:check-result
      matched OF WRAP-AND-SPEND ENDOF
      refused OF
         {: code:n :}
         s" the real checkpoint was refused as foreign, code" T-LABEL
         code . cr
         false TTRUE
         GPT2TX:ABORT
      ENDOF
   ;MATCH ;

: T-REAL-PAYLOAD ( -- )
   s" a real bound GPT-2 model rides a declared linear payload slot" T-LABEL
   BASELINE!
   0 0 0 0 COUNTERS
   PATH SAFET:LOAD CFG GPT2TX:PREPARE
   MATCH GPT2TX:prep-result
      prepared OF CHECKED-ARM ENDOF
      rejected OF
         {: code:n :}
         s" the real checkpoint did not prepare, code" T-LABEL
         code . cr
         false TTRUE
         SAFET:RELEASE
      ENDOF
   ;MATCH
   0 0 0 0 COUNTERS ;

\ ---- family identity for the registry readers ----------------------------------
\ A family is identified by its tail PLUS the constructor package its variants carry, so
\ these readers cannot silently pin a neighbouring family that happens to share a tail.
: H$ ( -- ptr u8 n ptr u8 n )  s" held" s" GPT2PAY-HELD" ;
: W$ ( -- ptr u8 n ptr u8 n )  s" held-twin" s" GPT2PAY-HELD--TWIN" ;

\ ---------------------------------------------------------------------------------
\ What the registry recorded. These are top-level lines rather than the body of a word
\ because TK-SUM is an engine registry constant the checker does not publish into user
\ definitions; the existing registry pins (maki/db/obligation-test.f,
\ lib/cad-num-types-test.f) read it the same way.
\ ---------------------------------------------------------------------------------
T-RESET

s" the payload family is unambiguous, arity-0, public, and a general sum" T-LABEL
H$ REFLECT:FAMS 1 T=
H$ REFLECT:ARITY 0 T=
H$ REFLECT:VIS 1 T=
H$ REFLECT:KIND TK-SUM T=
H$ REFLECT:KIND TK-ENUM = 0 T=                  \ never recorded as a payloadless enum
H$ REFLECT:VARS 2 T=
s" its cases are in declaration order, under this suite's constructor package" T-LABEL
H$ 0 REFLECT:ARM$ s" model" T$=
H$ 1 REFLECT:ARM$ s" empty" T$=
H$ 0 REFLECT:ARM-CTOR$ s" GPT2PAY-HELD" T$=
s" the model rides ONE named payload field, seven cells wide, at slot 0" T-LABEL
H$ 0 REFLECT:ARM-FLDS 1 T=
H$ 0 s" m" REFLECT:ARM-SLOT 0 T=
H$ 0 s" m" REFLECT:ARM-CELLS 7 T=
s" so the family is a tag cell plus that payload: eight cells" T-LABEL
H$ REFLECT:WIDTH 8 T=
s" and the non-linear control is eight cells wide too, from four declared fields" T-LABEL
W$ REFLECT:FAMS 1 T=
W$ REFLECT:WIDTH 8 T=
W$ 0 REFLECT:ARM-FLDS 4 T=

\ ---------------------------------------------------------------------------------
\ The bundle is ONE linear unit. Each refusal is followed by the same candidate over the
\ width-identical twin; where the twin is accepted the refusal is linearity, and where
\ the twin is refused too this file says which other rule answered.
\ ---------------------------------------------------------------------------------
s" identity and permutation conserve the bundle" T-LABEL
s" GP-ID ( held -- held )" ACCEPTED
s" GP-SWAP ( held n -- n held ) swap" ACCEPTED
s" GP-ROT ( held n n -- n n held ) rot" ACCEPTED
s" GP-2SWAP ( held n n n -- n n held n ) 2swap" ACCEPTED

s" copying the bundle is refused, and the non-linear twin copies freely" T-LABEL
s" GP-DUP ( held -- held held ) dup" REJECTED
s" GP-DUP-CTL ( held-twin -- held-twin held-twin ) dup" ACCEPTED
s" GP-OVER ( held n -- held n held ) over" REJECTED
s" GP-2DUP ( held n -- held n held n ) 2dup" REJECTED
s" GP-2DUP-CTL ( held-twin n -- held-twin n held-twin n ) 2dup" ACCEPTED
s" GP-TUCK ( n held -- held n held ) tuck" REJECTED

s" discarding the bundle is refused, and the twin discards freely" T-LABEL
s" GP-DROP ( held -- ) drop" REJECTED
s" GP-DROP-CTL ( held-twin -- ) drop" ACCEPTED
s" GP-NIP ( held held -- held ) nip" REJECTED
s" GP-NIP-CTL ( held-twin held-twin -- held-twin ) nip" ACCEPTED
s" GP-2DROP ( held n -- ) 2drop" REJECTED
s" GP-2DROP-CTL ( held-twin n -- ) 2drop" ACCEPTED

s" a return-stack ROUND TRIP conserves the bundle; re-pushing it copies it" T-LABEL
s" GP-RTRIP ( held -- held ) >r r>" ACCEPTED
s" GP-RAT ( held -- held held ) >r r@ r>" REJECTED
s" GP-RAT-CTL ( held-twin -- held-twin held-twin ) >r r@ r>" ACCEPTED

s" typed memory stays closed to the bundle: the load discriminates, the store does not" T-LABEL
s" GP-LOAD ( ptr held -- held ) @" REJECTED
s" GP-LOAD-CTL ( ptr held-twin -- held-twin ) @" ACCEPTED
\ NOT LINEARITY: `!` refuses for the twin as well, so the store half of this pair is the
\ checker declining to express a multi-cell store at all. It is pinned so a later change
\ that made `!` work for layout values cannot quietly open it for the linear one.
s" GP-STORE ( held ptr n -- ) !" REJECTED
s" GP-STORE-CTL ( held-twin ptr n -- ) !" REJECTED

\ NOT LINEARITY: a typed local capturing the bundle refuses for the twin too, so this is
\ about capturing a multi-cell layout value in a local. The one-cell control shows typed
\ locals themselves are fine, which is what makes the pair readable.
s" a typed local cannot capture either eight-cell bundle (locals, not linearity)" T-LABEL
s" GP-LOCAL ( held -- held ) {: v:held :} v" REJECTED
s" GP-LOCAL-CTL ( held-twin -- held-twin ) {: v:held-twin :} v" REJECTED
s" GP-LOCAL-CELL ( n -- n ) {: v:n :} v" ACCEPTED

\ NOT LINEARITY: forging from a raw cell and leaving a value unconsumed refuse for the
\ twin too - the first is nominal typing, the second is the declared arity.
s" a bundle cannot be forged from a raw cell, nor left unconsumed (typing and arity)" T-LABEL
s" GP-FORGE ( n -- held ) " REJECTED
s" GP-FORGE-CTL ( n -- held-twin ) " REJECTED
s" GP-UNCONS ( held -- )" REJECTED
s" GP-UNCONS-CTL ( held-twin -- )" REJECTED
\ NOT LINEARITY either: an unbalanced `>r` refuses for the twin, so the strand refusal
\ is return-stack balance. The linear-specific half of this story is GP-RAT above.
s" GP-STRAND ( held -- ) >r" REJECTED
s" GP-STRAND-CTL ( held-twin -- ) >r" REJECTED

\ ---------------------------------------------------------------------------------
\ Construction consumes the model exactly once and mints one bundle.
\ ---------------------------------------------------------------------------------
s" wrapping a real model mints one bundle, through the word and the inline form" T-LABEL
s" GP-MINT ( GPT2TX:gpt2-model -- held ) GPT2PAY-HELD:MODEL" ACCEPTED
s" GP-MINT-INLINE ( GPT2TX:gpt2-model -- held ) construct held model" ACCEPTED
s" GP-MINT-CODE ( n -- held ) GPT2PAY-HELD:EMPTY" ACCEPTED

\ Wrong roles: the payload slot is nominal, so neither variant will take the other's
\ argument, the two eight-cell families do not substitute for each other, and a MATCH
\ naming the wrong family refuses rather than reading the tag it happens to share.
s" the two variants' payloads cannot cross, and neither can the two families" T-LABEL
s" GP-ROLE-MODEL-CELL ( n -- held ) GPT2PAY-HELD:MODEL" REJECTED
s" GP-ROLE-EMPTY-MODEL ( GPT2TX:gpt2-model -- held ) GPT2PAY-HELD:EMPTY" REJECTED
s" GP-ROLE-TWIN-MODEL ( GPT2TX:gpt2-model -- held-twin ) GPT2PAY-HELD--TWIN:KEY" REJECTED
s" GP-ROLE-BUNDLE ( held -- held-twin )" REJECTED
s" GP-ROLE-MATCH-FAM ( held -- n ) MATCH held-twin key OF drop drop drop drop 0 ENDOF empty OF ENDOF ;MATCH" REJECTED

s" losing, copying or re-using the model at construction is refused" T-LABEL
s" GP-CTOR-DROPPED ( GPT2TX:gpt2-model n -- held ) nip GPT2PAY-HELD:EMPTY" REJECTED
s" GP-CTOR-COPY ( GPT2TX:gpt2-model -- held held ) GPT2PAY-HELD:MODEL dup" REJECTED
s" GP-CTOR-BRANCH ( GPT2TX:gpt2-model f -- held ) if GPT2PAY-HELD:MODEL then" REJECTED
s" GP-CTOR-TWICE ( GPT2TX:gpt2-model -- held ) GPT2PAY-HELD:MODEL GPT2PAY-HELD:MODEL" REJECTED

\ ---------------------------------------------------------------------------------
\ MATCH hands the payload to the arm as a linear value the arm must discharge exactly
\ once. Disposing it through the real production exit is a discharge; so is re-minting
\ the bundle. Every other ending is refused.
\ ---------------------------------------------------------------------------------
s" an arm that disposes the model through its real exit certifies" T-LABEL
s" GP-M-DISPOSE ( held -- n ) MATCH held model OF GPT2TX:MODEL-DISPOSE drop 0 ENDOF empty OF ENDOF ;MATCH" ACCEPTED
s" GP-M-DISPOSE-READ ( held -- n ) MATCH held model OF GPT2TX:MODEL-DISPOSE MATCH result ok OF ENDOF err OF ENDOF ;MATCH ENDOF empty OF ENDOF ;MATCH" ACCEPTED
s" and so does an arm that re-mints the bundle" T-LABEL
s" GP-M-REMINT ( held -- held ) MATCH held model OF GPT2PAY-HELD:MODEL ENDOF empty OF GPT2PAY-HELD:EMPTY ENDOF ;MATCH" ACCEPTED

s" dropping the payload inside the arm is refused, and the twin drops its payload freely" T-LABEL
s" GP-M-DROP ( held -- n ) MATCH held model OF drop 0 ENDOF empty OF ENDOF ;MATCH" REJECTED
s" GP-M-DROP-CTL ( held-twin -- n ) MATCH held-twin key OF drop drop drop drop 0 ENDOF empty OF ENDOF ;MATCH" ACCEPTED

s" copying the payload to dispose it twice is refused" T-LABEL
s" GP-M-DOUBLE ( held -- n ) MATCH held model OF dup GPT2TX:MODEL-DISPOSE drop GPT2TX:MODEL-DISPOSE drop 0 ENDOF empty OF ENDOF ;MATCH" REJECTED
s" and so is matching the same bundle twice, which needs a copy of the bundle" T-LABEL
s" GP-M-TWICE ( held -- n n ) dup MATCH held model OF GPT2TX:MODEL-DISPOSE drop 0 ENDOF empty OF ENDOF ;MATCH swap MATCH held model OF GPT2TX:MODEL-DISPOSE drop 0 ENDOF empty OF ENDOF ;MATCH" REJECTED

s" leaving the payload on the row, stranding it, or exiting past it is refused" T-LABEL
s" GP-M-KEEP ( held -- n ) MATCH held model OF 0 ENDOF empty OF ENDOF ;MATCH" REJECTED
s" GP-M-ESCAPE ( held -- n ) MATCH held model OF >r 0 ENDOF empty OF ENDOF ;MATCH" REJECTED
s" GP-M-EXIT ( held -- n ) MATCH held model OF exit ENDOF empty OF ENDOF ;MATCH" REJECTED

s" unpacking the model inside the arm keeps the obligation on its residency" T-LABEL
s" GP-M-UNMAKE ( held -- n ) MATCH held model OF GPT2TX-GPT2--MODEL:UNMAKE drop drop drop drop 0 ENDOF empty OF ENDOF ;MATCH" REJECTED

\ NOT LINEARITY: MATCH consumes what it matches for every family, so keeping the
\ scrutinee refuses for the twin as well.
s" the matched bundle itself cannot survive its own MATCH (MATCH consumes, not linearity)" T-LABEL
s" GP-M-KEPT ( held -- held n ) MATCH held model OF GPT2TX:MODEL-DISPOSE drop 0 ENDOF empty OF ENDOF ;MATCH" REJECTED
s" GP-M-KEPT-CTL ( held-twin -- held-twin n ) MATCH held-twin key OF drop drop drop drop 0 ENDOF empty OF ENDOF ;MATCH" REJECTED

\ ---------------------------------------------------------------------------------
\ KNOWN GAP: `throw` abandons a linear value held by a MATCH arm.
\
\ The three pins below are written as ACCEPT deliberately - they record what the checker
\ certifies TODAY, in the MODELPROV T-KNOWN-GAP / GPT2TX-DR convention. An arm that
\ throws while still holding the model certifies, and so does an arm that catches a
\ throw and carries on. On the throw edge the payload is simply abandoned: nothing
\ disposes it, and a surrounding catch resumes with the checkpoint mapping stranded and
\ no handle left to reach it.
\
\ This is not something the linear-payload capability introduced. It is the engine-wide
\ behaviour recorded as finding 6 of .blackboard/txn-v2-plan-20260726.md ("throw abandons
\ linears in MATCH arms and quotations", measured byte-identical on the parent engine),
\ and its real fix is that plan's pillar B, checker-enforced linear-scope quotations - a
\ quotation shape that guarantees threaded owners are returned or disposed on every edge
\ including throw. When that capability lands these three legs FAIL, and that failure is
\ the signal to turn them into REJECTED and delete this banner.
\
\ Nothing here guards or works around the gap, and the runtime consequence is
\ deliberately not executed: driving the throw edge over the real model would strand the
\ 548 MB checkpoint mapping for the rest of the process with no handle to recover it.
\
\ The throw code in these candidates is never thrown - a candidate is checked, never
\ run - so it is written as a bare cell rather than borrowed from a module's range.
\ ---------------------------------------------------------------------------------
s" KNOWN GAP: an arm that throws while holding the model certifies, abandoning it" T-LABEL
s" GP-GAP-THROW ( held -- n ) MATCH held model OF -5699 throw ENDOF empty OF ENDOF ;MATCH" ACCEPTED
s" KNOWN GAP: and a catch inside the arm resumes past the throw, model still held" T-LABEL
s" GP-GAP-CATCH ( held -- n ) MATCH held model OF [: -5699 throw ;] catch drop GPT2TX:MODEL-DISPOSE drop 0 ENDOF empty OF ENDOF ;MATCH" ACCEPTED
s" CONTROL: the throw edge is accepted for the twin too, so the gap is the ABANDONED" T-LABEL
s" owner and not the throw" T-LABEL
s" GP-GAP-THROW-CTL ( held-twin -- n ) MATCH held-twin key OF drop drop drop drop -5699 throw ENDOF empty OF ENDOF ;MATCH" ACCEPTED
s" CONTROL: an arm that disposes BEFORE it throws is legitimate, not a gap" T-LABEL
s" GP-GAP-THROW-AFTER ( held -- n ) MATCH held model OF GPT2TX:MODEL-DISPOSE drop -5699 throw ENDOF empty OF ENDOF ;MATCH" ACCEPTED

\ ---- the real-model leg --------------------------------------------------------
\ Presence-gated the way the other real-checkpoint legs are (gpt2-check-test.f,
\ gpt2-alloc-test.f), so a host without the 548 MB artifact reports the skip loudly
\ instead of failing. Everything above runs unconditionally.
: RUN ( -- )
   PATH SAFET:PRESENT? if
      T-REAL-PAYLOAD
   else
      s" gpt2-payload: gpt2-model/model.safetensors absent -> real-model leg SKIPPED" type cr
   then ;

RUN
T-REPORT

;package
