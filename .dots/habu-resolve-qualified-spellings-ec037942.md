---
title: Resolve qualified spellings in the name check
status: active
priority: 2
issue-type: task
created-at: "2026-08-03T22:45:51.601209+02:00"
---

Regression on proofs introduced by f7eb936d (the inline row name check): tools/codegen-compare-test.f assertion 238 red — SPEND-FOUR migrates FAN-CEILING-N whose body names its callees with package-qualified spellings (CODEGEN-CORPUS4:C-ADD1-N), the NINL row records the bare published name (C-ADD1-N), NELAB:CALLEE-COPY?'s NAMED? compares raw strings, mismatches, and throws E-NELAB-INLINE (-8559) on a legitimate program. Green at 23bb4070, red at proofs; verified by bisect and by the -8559 fingerprint. The check must compare resolved identity, not raw spelling: a package-qualified reference and the bare published name denote the same word. Fix in the elaborator's side (resolve the site's spelling through the same naming grammar the engine uses — the package-qualified form's final component — or better, compare against the resolved symbol's own name), NOT by widening NAMED? to substring matching. Regression tests: a cross-package qualified caller of a recorded callee (the compare-test shape) in test/compiler/native-inline.f, plus the existing mismatch case must still refuse. Gate: tools/codegen-compare-test.f green again; add it to the per-landing gate list.

Claim: agent=qualified-names workspace=.jj-ws/habu-resolve-qualified-spellings-ec037942

Fixed by comparing the name a spelling DENOTES rather than the spelling. A
publication's name is a bare tail — a word published inside a package is stored
as its tail in that package's wordlist — so that is what a row records, while a
call site may write either form for the same word. NELAB:BARE-NAME$ reduces the
site's spelling through XREF-QUAL-INDEX, the engine's own naming grammar: the
index of the single non-edge colon, -1 for an ordinary name (no colon, or one at
an edge), -2 for a token a second colon makes name nothing. The tail arithmetic
is the same as XREF-FIND-QUALIFIED's, so a spelling this reduces and a spelling
the engine resolves cannot come apart. A -2 token is left whole and refuses by
itself, because a published name never contains a colon — the same answer
XREF-FIND gives it, with no second refusal to write down.

PACKAGE IDENTITY IS NOT COMPARED, and this is the reasoning rather than an
oversight. The publication's package is cheap to record (NMIGRATE's NAME-WID,
one cell). The reference's package is not cheap to put in the same currency: the
engine's package-name-to-public-wordlist step is buried inside
XREF-FIND-QUALIFIED in src/habu/xref.f, an engine-prefix file, so factoring it
out is seed-affecting; and consuming it in elaborate.f would give the IR
elaborator a live-dictionary dependency it does not have, against a doctrine of
working off the module's own word model. More decisively, the package half of a
qualified reference is already consumed by the CALLER's own resolution — the
address it stages comes from resolving that spelling — so a package
disagreement is a disagreement inside the caller's declaration, and its complete
guard is to hold the staged address against what the staged spelling resolves
to, one XREF-FIND where the addresses are staged. That settles name, package and
address together, needs no new grammar and no engine change, and is a different
concern from the splice key: dot habu-hold-the-staged-6837d532.

RESIDUAL GAP, stated rather than tested away: a caller that states one package's
address while writing another package's identical tail is still spliced. Habu
allows the same tail in different packages by design, so this is reachable; it
is closed by the dot above and by nothing here.
