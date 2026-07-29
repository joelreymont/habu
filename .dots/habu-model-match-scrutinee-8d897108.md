---
title: Model MATCH scrutinee pop
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-30T00:05:03.636384+02:00\""
---

Full context: one of the omission leaves split out of habu-model-the-declared-4a2eb3c9.

What is being discharged. The header of formal/Common/Control.v lists "MATCH's SCRUTINEE POP, which stays abstracted here", and is precise about the gap: the checker walks a width-expanded hidden-field bundle cell by cell (MATCH-SCRUT?, checker.f:8236-8246) while the model's match_scrut requires one arity-0 logical TFam cell. It also records that Effects.v now has everything that pop needs - t_width, fam_hid and layout_push_fields - so this is undone work rather than a missing representation, and that every rule ABOVE the pop (payload refinement, the branch join, MD-JOIN, exhaustiveness) is modelled exactly. Remove exactly that clause when this leaf lands.

What the checker actually decides today with no model behind it. MATCH-SCRUT-CELL? (src/core/checker.f:8284) checks one bundle level top-down and MATCH-SCRUT? (checker.f:8294) walks the whole width-expanded bundle off the row, answering the row below it; MATCH-SCRUT-DIAG (checker.f:8304) classifies a failed scrutinee. The model's match_scrut (formal/Common/Control.v, in the MATCH section) short-circuits all of that to a single-cell test, so today a match on a real multi-cell layout value is outside the modelled fragment even though every rule around it is inside.

Where the rule belongs. formal/Common/Control.v, replacing match_scrut in place with a walk written over Effects.v's t_width, fam_hid and layout_push_fields. Nothing above it should need to change; if it does, that is a finding worth writing down. The results worth stating: the pop consumes exactly the family's instantiated width and leaves the row below untouched; a bundle of the wrong family is refused whatever its width; and a partially popped bundle is never left on the row.

The vector shape that would bind it. A verdict-class-changing pair of shared program vectors over a sum family with a real multi-cell payload: a match that pops the whole bundle and certifies, against one whose scrutinee is a different family of the SAME width, which must be refused. Same-width is the sharp case, because a width-only test would pass it. This leaf also unlocks the stronger statement the depth lane wanted, that a match at the deepest frame that fits actually CERTIFIES rather than merely being unresolvable; add that row if it comes out of the work.

The mutation that must go red. Make MATCH-SCRUT-CELL? (checker.f:8284) ignore the family id and check only the cell count; the same-width different-family row must flip from refused to certified. Restore src/core/checker.f byte-identically and record the matrix.

Blocked by nothing, but the certifying deep-match row also wants habu-model-construct-row-bcdd5ef6 landed, because construct is how a body mints a family value of a known variant.

Claim: agent=scrutinee workspace=.jj-ws/habu-model-match-scrutinee-8d897108

MEASURED. The scrutinee pop is modelled in formal/Common/Control.v and the
clause is gone from the header's omission list. What landed:

- `match_scrut_cell` / `match_scrut_cells` / `match_scrut_row` replace the
  single-cell `match_scrut`, written over `Effects.hidden_paramb`,
  `param_famb`, `hidden_slot` and `t_width` — no private copies. The walk counts
  the cells STILL TO COME rather than the ones taken, because the checker's
  expected slot `w - 1 - j` is exactly one less than that count, so the model
  carries the checker's arithmetic with no second index to drift.
- `match_scrut_diag` models `MATCH-SCRUT-DIAG` (checker.f:8304-8311), which had
  no model at all: `MD-QUOT`, `MD-SCRUT`, the new `MD-FAM-MISMATCH` and
  `MD-OPEN-ARGS`. Only `MD-OPEN-ARGS` is unreachable from this file, because it
  needs an arity-1 family and `Control.fam` is arity 0; that is written down
  where the definition is rather than left as a surprise.
- Four proved laws, no `Admitted` and no new assumption:
  `the_scrutinee_pop_undoes_the_bundle_push` (the pop is the exact inverse of
  `Effects.layout_push_fields`, so it consumes the family's instantiated width
  and answers the row underneath), `a_bundle_of_another_family_is_never_a_scrutinee`
  (refused at ANY width, so a same-width family is refused too),
  `a_refused_scrutinee_leaves_the_rows_untouched` (no row moves, no `MF` record,
  no frame — the "never half-popped" statement), and
  `a_popped_scrutinee_becomes_the_branch_base`. These are the first universally
  quantified results in Control.v; everything else there is one program computed.
- Two shared vectors, CMV27 and CMV28, over two new fixture families: `cmwide`,
  whose variants carry two cells so its bundle is three, and `cmtwin`, the same
  shape under another identity. The two programs are the same text with only the
  family token and variant names changed, and the verdicts differ in class.
- Six existing MATCH/construct results and eleven vector/frame rows moved from
  the EMPTY registry to a real one (`sig_fam`, over `fam_env`, whose entries are
  derived from each `fam` record's own payload list). This was forced, and it is
  the finding of the leaf: with no registry entry a declared family is one
  unexpanded logical cell, so those examples had been agreeing with the checker
  about a DIFFERENT program. Nothing above the pop changed.
- `a_match_at_the_deepest_frame_that_fits_certifies`: the stronger statement the
  depth lane wanted. It needed a row-neutral body, which `construct` now
  supplies — the arms rebuild the value they matched. Measured -1 at 30 opens,
  0 at 31, -1 at 29 and at 0. It is not a frozen vector because the schema
  builds vector text with the 1 KB shared string builder and the body is larger;
  dot habu-freeze-the-deep-e120e19c.

FALSIFICATION MATRIX. Each mutation applied to src/core/checker.f alone, the
fixpoint rebuilt with `tools/build-fixpoint-refresh.f -- install --force`, the
gate rerun, and the file restored byte-identically (sha256
1b1859ab024b991bfcb255f932120f6fd7be0eac88b181f8742b7adcd03a7bdb before and
after each).

  baseline                                  gate exit 0, 0 failures
  drop `t PARAM>FAM fam <>` from
    MATCH-SCRUT-CELL? (the dot's mutation)  gate exit 1, exactly 1 failure:
                                            CMV28 flips VReject -> VCert
  walk a three-cell bundle as two
    (`T-WIDTH dup 3 = IF drop 2 THEN`)      gate exit 1, exactly 1 failure:
                                            CMV27 flips VCert -> VReject
  walk EVERY bundle as two                  never reaches the gate: the fixpoint
                                            self-check refuses the build,
                                            `FIND-EXECUTABLE-IN-PATH`
                                            (lib/process-env.f) stops certifying
                                            because it matches an `option` whose
                                            payload is itself a multi-cell layout
  restored                                  gate exit 0, 0 failures

The third row is why the second names a width nothing in `lib/` uses. It is also
evidence in its own right: the shipped library cannot be built if this walk stops
early.

GATES on the final tree: `bin/hb --load test/compiler/checker-model-proof.f`
exit 0 (both models compile, 28 vectors, every published result pinned by
test/compiler/checker-model-axioms.txt and reported closed under the global
context, assumption set empty); `rocq compile` clean on Control.v; no `Admitted`
or `admit` anywhere in the diff; tools/package-diff-lint.f and
tools/typed-local-diff-lint.f exit 0 on `jj diff --git` (both checked live
against hostile fixtures first: a global definition and an untyped locals group
are flagged); tools/suite-coverage-lint.f 0 findings; tools/dot-dep-lint.f
0 findings; src/core/checker.f byte-identical to the parent.

HONEST GAPS.
- `MD-OPEN-ARGS` is modelled and unexercised (needs an arity-1 family in
  `Control.fam`).
- The pop's walk does not compare the bundle cells' ARGUMENT runs and neither
  does the model, faithfully; what stops a hidden field pairing with anything
  else is `PARAM-HID-OK?` in Effects.v, which is modelled there.
- `MF.TERM` (the saved tag term) is still abstracted: it exists so
  `MATCH-PAY-XT` can instantiate a payload against the scrutinee's arguments,
  and this file's `fam` carries the payload already instantiated.
- The reason codes `MD-FAM-MISMATCH` and `MD-TRUNC` were measured through the
  rendered prose of a text TRUNCATED after the family token, because in a full
  match form `MATCH-REJECT` clears `MM` and the following variant tokens become
  unknown words whose latch supplies the message.
- Line references in both model files have drifted by about seventy lines
  through this region; this leaf refreshed only the ones it owns.
  Dot habu-refresh-checker-citations-ba310827.
- The certifying deep-match row is proved in the model and measured against the
  checker but is not a frozen vector. Dot habu-freeze-the-deep-e120e19c.

BEST LONG-TERM OR A PATCH? Long-term. The rule is modelled where checker.f puts
it, as a walk over Effects.v's own `t_width` / `mk_hidden` / `layout_push_fields`
rather than a re-description of them, and the one number it needs — the expected
slot — is derived from the walk's own countdown instead of pinned. The registry
the examples now run in is derived from the payload list the model already
carried, so a family's width and its payloads cannot drift apart. Nothing rests
on a value heuristic: the laws are quantified over every registry, family,
argument run and row, the vectors are two-sided with one verdict written once,
and each of the two is independently falsifiable by a checker mutation.
