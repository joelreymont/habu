---
title: clobber-lint cannot see a packaged label
status: active
priority: 2
issue-type: task
created-at: "2026-08-11T22:01:29.226022+02:00"
---

tools/lint/clobber-lint.f:98 START-L? asks whether the FIRST character of the label token is `l`, so a package-qualified label (PROT:LOPEN, SNAP-RELOC:LCALLS, HIDX:LREBUILD, AOT-WINDOW:LDATA, DEFER-DIAG:LDEFNOTOKEN, HOLD-EMIT:LHOLDQ, KWDATA:LKWTRUSTRAW) fails it however its tail is spelled. The predicate is shared by COLLECT-OPENINGS (routine bodies) and CALLEE? (call sites), so such a label is not merely uncounted in the census - its routine is never analysed and its callers never see its clobbers. Every packaged label in the tree has been invisible since packaging started. It surfaced when the protection narrowing (dot habu-narrow-the-code-291b2cef) moved 47 call sites into `package PROT` and the call census fell 504 -> 458; that lanes MIN-CALLS row was lowered to 458 with the derivation recorded in the source, so this dot owns restoring the coverage.

FIX, MEASURED TO WORK on 2026-08-11 in the stageb lane before being reverted: read the segment after the LAST colon instead of the first character. With that change alone the census reads routines 335 -> 347, calls 504 -> 518, and two CLOBBER findings appear (c-esdq x11 via lbcs, c-local-ref x0 via lvspill). Both are FALSE POSITIVES of the union model, and both go away by adding two PRESERVE-MASK rows the model already supports: LCEMIT preserves x0/x1/x2/x8/x12/x13/x16/x30 (its window-miss path frames x30 and x1 around a PROT:LGROW that frames the rest), and PROT:LGROW preserves x0/x2/x8/x16/x30. With those rows the lint reads clean at routines 347, calls 518.

WHY IT WAS NOT DONE THERE: package-diff-lint refuses it. START-L?, PSEUDO?, PSEUDO-EFFECTS, PRESERVE-MASK in tools/lint/clobber-lint.f, every CLT-* word in tools/lint/clobber-lint-test.f, and the new fixture words are all bare globals, so E-PACKAGE-OWNERSHIP requires the whole legacy file to be packaged first. That is the real prerequisite and the reason this is its own leaf: package clobber-lint.f (LESSONS 2026-08-11 says EXPORT NAME inside a public section is the tool for a large legacy file - definitions stay private in place and the export list reads in one block), then apply the predicate fix and the two preserve rows.

REGRESSION TO KEEP, also written and measured in that lane: append to tools/lint/clobber-sys-x8-fixture.f a callee whose label lives in a package (`package CLOBBER-FIX public variable LQUAL ;package`) plus a caller that keeps x13 live across `CLOBBER-FIX:LQUAL LABEL@ BL,`. The fixture count goes 3 -> 4 findings and the census 13/6 -> 15/7. Falsified by mutation: restoring the first-character predicate drops the finding back to 3 and the test dies. Add direct assertions too - START-L? true for LPROT, PROT:LOPEN, SNAP-RELOC:LCALLS and A:B:LTAIL, false for LHEAD:OPEN, PROT:OPEN, PROT: and CP.

Files: tools/lint/clobber-lint.f, tools/lint/clobber-lint-test.f, tools/lint/clobber-sys-x8-fixture.f. Depends: none.

NOTE 2026-08-16 (from the WLFIND landing, 9d7d8e72): habu1.f's
"the stale rows cost nothing today" is now one read from false -
the seed's patch pass deliberately keys its gate check on the
requested scope instead of re-reading the resolved record to
dodge the stale LFIND x5 model. Repairing this lint unblocks
that simplification.

Claim: alder. Current measured baseline is 369 routines / 527 calls; restoring
qualified labels yields 399 / 623. LCEMIT and PROT:LGROW save/restore masks were
checked against EMIT-CEMIT and EMIT-PROT-GROW, and PROT:RESERVE is modeled.
Declarations are collected before analysis, so a bare label inside its package
and a qualified external call resolve to the same owned identity. This connects
SNAP-RELOC and WLFIND callers that a spelling-only predicate still misses.
WLFIND's explicit x11/x12 results and the relocation helpers' x8/x16 preservation
on returning paths are now modeled; their syscall writes occur in fatal arms.

Restored coverage also exposed PASS2 carrying LPAT's poisoned x1 past its RET
into the unrelated LKWCMP entry. Independent entries now start fresh after a
terminator. A direct jump crossing any entry boundary (including a local join)
or an indirect branch keeps whole-definition state instead. No CFG or saved-state
table was added. Fixtures pin independent entries, fall-through, qualified calls
to bare package openings, and jumps over returns and unused entries.

Validation: production clobber-lint is clean at 399 routines / 623 calls; those
are now the census floors. The focused fixture row passes tiers 0 and 1 (32
routines, 12 calls, 11 expected findings in the syscall/entry fixture). Mutations
restoring the old spelling predicate, disconnecting owner resolution, or dropping
the crossing check fail the regressions. Astra's ownership and boundary findings
were fixed and follow-up review is clear. No engine source edits or full gate.
