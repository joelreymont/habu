---
title: Model T-ATOM rigid host identities
status: open
priority: 2
issue-type: task
created-at: "\"2026-07-30T00:02:43.415137+02:00\""
---

Full context: this is one of the omission leaves split out of habu-model-the-declared-4a2eb3c9, which asked for one leaf per declared omission rather than one big change.

What is being discharged. The header of formal/Common/Effects.v lists, under "Deliberate omissions from the modelled fragment", the line "T-ATOM rigid host identities (region / extent / generation)". Remove exactly that line when this leaf lands, and leave the other omission lines alone.

What the checker actually decides today with no model behind it. src/core/checker.f mints an atom term with MK-ATOM / MK-ATOM-K (checker.f:376-383) and stamps it with a rigid host-allocation identity drawn from one of three private per-check counters: RGN-FRESH, EXT-FRESH and GEN-FRESH (checker.f:358-367), routed by the name prefix of a template fresh atom in RIGID-AK-MINT (checker.f:370-374). RIGID-RESET (checker.f:348) restarts all four counters at 1 for every check, and RIGID-MAX (checker.f:333) is deliberately below the sign wrap so a domain throws E-RIGID-EXHAUST rather than wrapping and handing out an identity that is still live. Two atoms unify only through ATOM-OK? (checker.f:1272), which qualifies the numeric id by its domain kind, so equal numbers from different domains never unify. None of that is in either model file.

Where the rule belongs. formal/Common/Effects.v, in the type-term section next to the con/ty vocabulary: an atom is a new ty shape carrying a domain and an id, ATOM-OK? becomes a decidable equality on the pair, and unification's atom arm calls it. The two facts worth stating as results are that identities from two different domains never unify however their numbers compare, and that fresh identities are never reused within one check (monotonicity of the counters), which is what makes an allocation's region, extent and generation meaningful at all.

The vector shape that would bind it. Two shared program vectors in test/compiler/checker-model-schema.f whose verdicts differ in class: a definition whose signature names the same rigid atom twice and certifies, against one that names two atoms from different domains and is refused. Derive any number that appears (a domain code, a counter start) structurally from the checker's own constants rather than writing it down twice, exactly as the CMV15-CMV18 rows derive the match depth guard from the frame ceiling.

The mutation that must go red. Make ATOM-OK? (checker.f:1272) ignore the domain and compare the raw ids: the cross-domain row must flip from refused to certified and the gate must fail on exactly that row. Restore src/core/checker.f byte-identically afterwards and record the matrix in the dot.

Blocked by nothing. It touches the type vocabulary only, so it does not wait on the construct, transport or match leaves.

Claim: agent=tatom workspace=.jj-ws/habu-model-t-atom-8110cc18 (RELEASED 2026-08-21: workspace gone, no live lane - gc)

MEASURED. The rule is modelled, the omission line is gone from
formal/Common/Effects.v, and the parity gate
(`bin/hb --load test/compiler/checker-model-proof.f`) is green. src/core/checker.f
is byte-identical to the parent change.

What the model now says. `Effects.ty` has a `TAtom` shape carrying a domain and
an id, exactly the name-plus-kind pair `ATOM-OK?` (checker.f:1312) decides on:
the domain is the word every atom name leads with (`region-`, `extent-`, `gen-`,
and one shared catch-all for everything the router does not recognise), and the
id has the checker's three sign cases as three constructors - a template slot, an
ordinary atom token whose identity is its spelling, and an identity a call site
minted. `atom_okb` is the decidable equality on that pair and unification's atom
arm calls it and nothing else. Beside it the four per-domain counters are
modelled with their reset, their hand-out-and-advance, and their refusal at the
bound; instantiation mints one identity per template slot per call site, which is
what makes one call's two outputs one allocation and two calls two.

The bound is a PARAMETER, not a number. The checker's `RIGID-MAX` is
`$4000000000000000` and no unary Rocq `nat` can hold it, so every result is
stated for every bound and the executable configuration runs at a small one -
which only refuses sooner, the same fail-closed direction the file already
declares for exhausted fuel. The checker's own literal is held structurally
instead, by reading the guard out of each mint word's body.

Four results, all closed under the global context and bound by the committed
manifest:

  - cross_domain_identities_never_unify - two identities from different domains
    never unify, whatever their numbers are, which is the point because every
    domain's counter starts at 1 and so the ordinary case is that the numbers
    are EQUAL;
  - a_template_slot_is_not_an_identity - a `fresh-*` name in a signature unifies
    with nothing at all until a call site mints it, not even with a slot spelled
    exactly the same way;
  - no_wrap_can_re_grant_a_live_identity - no identity is handed out twice inside
    one instantiation and none of them reaches the bound;
  - two_instantiations_never_share_an_identity - and two call sites never share
    one either, because the counters reset once per check and never again.

Six new shared vectors (CMV29-CMV34) in test/compiler/checker-model-schema.f,
answered by the shipped checker through `CHECK-QUIET-CANDIDATE!` and by the model
through `check_ctl`, with one verdict written once for both. They come in three
pairs: one call against two calls, two domains at the same number against the
same shape in one domain, and a template slot against an ordinary atom token.
Four trusted host constructors in test/compiler/checker-model-cases.f produce the
identities, because minting one is exactly what checked Habu may not do - the
same boundary `lib/ptx/tile.f` declares `MK-SPAN` at.

A seventh frozen table, the identity domains. One row per domain naming the
leading word, the mint word, the counter variable and the model constructor;
the guard, the advance and the per-check restart are BUILT from the counter's
name and the router's two lengths from the leading word's own length, so each
number is written once. The cases file reads all of them out of the checker's
source and walks `RIGID-AK-MINT` token by token including its string literals,
and the obligations file runs each domain's model counter from its restart
through a mint to its bound.

FALSIFICATION MATRIX. Each mutation was applied to src/core/checker.f, the
fixpoint was rebuilt with
`bin/hb --load tools/build-fixpoint-refresh.f -- install --force` (exit 0, 4237
certified, every time), the gate was rerun, and the file was restored
byte-for-byte afterwards.

  1. `ATOM-OK?` compares ids only - its final name comparison replaced by
     `RES-TRUE`. Gate red on exactly two rows and no others:
     two_domains_at_the_same_number_still_reject and
     a_different_spelling_is_a_different_atom, both flipping from refusal to
     certification. This is the measurement that the two domains really are at
     the same number: nothing else could make that row certify.
  2. A domain counter reuses an id - `RGN-FRESH` hands out `RGN-N @` without
     advancing. Gate red on exactly two rows: the structural row "the DRegion
     domain hands out its value and advances", and the vector
     two_calls_are_two_allocations_and_never_one, which flips to certification.
     The shared-identity row does not move, which is what makes the pair sharp.
  3. The bound is unguarded - the `RIGID-MAX` test deleted from `RGN-FRESH`, so
     the counter can wrap. Gate red on exactly one row, "the DRegion domain
     refuses at the bound before it mints".
  4. The per-check restart is wrong - `RIGID-RESET` starts the region domain at
     2. Gate red on exactly one row, "the per-check restart puts the DRegion
     domain back to 1".
  5. The MODEL side is bound too, not only the checker side. Changing
     `aMaskB` in formal/Common/Control.v to the same name as `aMaskA` - a
     mutation that leaves both model files compiling - turns the generated
     obligation for a_different_spelling_is_a_different_atom red with
     `Unable to unify "VReject" with "VCert"`. Restored.

HONEST GAPS.

  - Exhaustion is not reachable through a checked candidate program. A vector
    would have to mint `$4000000000000000` identities, so mutation 3 above is
    the strongest form the refusal can take here: the guard is held structurally
    in each of the four mint words, and the model's own result covers every
    bound. test/rigid-region-suite.f reaches the throw itself by lowering
    `RIGID-MAX` to 2 from a test, which the parity gate's vector shape has no
    place for.
  - The model turns `E-RIGID-EXHAUST` into a refusal. The checker throws and the
    check stops with no verdict at all; this fragment has no such outcome, so
    `apply_eff` fails the step instead. That is the fail-closed direction and it
    is written down where it happens.
  - `ty_eqb` on two atoms answers `atom_okb` rather than structural equality,
    because the checker's fast path is arena-POINTER identity and that is finer
    than the spelling. Comparing one template atom term with itself would be
    accepted by the checker and refused by the model; no program in the tree
    does it, and the direction is again reject-more.
  - The domain of an ordinary atom token is always the shared one, because the
    checker never routes a kind-0 atom anywhere - its whole name is its
    identity. So `DExtent` paired with an `AName` is a combination the model
    never builds. It is legal and decides nothing.
  - Only the region domain was mutated in 2, 3 and 4. The other three rows are
    built from the same frozen column by the same code, so a mutation in one is
    the measurement for all four.

BEST LONG-TERM OR A PATCH? Long-term. The identity is modelled as the pair the
checker actually decides on - the name's leading domain word and the kind - and
`atom_okb` is that decision, not a value heuristic that happens to separate the
cases in the fixtures. The counters are modelled as counters with their real
reset and their real refusal, so the freshness result is an induction over
minting rather than an observation about three programs. The one number that
could not be carried honestly, `RIGID-MAX`, was made a parameter rather than a
smaller number pretending to be it, and the checker's literal is held by reading
the guard out of the code. The vectors are pairs whose verdicts differ in what
they turn on, and each mutation moved exactly the rows it should and no others.
