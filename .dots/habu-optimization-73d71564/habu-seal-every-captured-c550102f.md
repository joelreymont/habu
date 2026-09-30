---
title: "Seal every captured package against reopening"
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T16:49:49.727440+03:00"
---

Problem: only the seven RESTAB names (`habu2.f:2153`, mirrored by `checker.f` CHECKER-SEALED-PKG?) and 173 self-protected wordlists are sealed; user source can reopen any other engine package with `package NAME`, and that reach is the only reason a private word of an engine package needs its checker data.
Acceptance:
- **Seal at capture:** every package the capture window ships gets both wordlists `prot-wid-add`ed before `NATIVE-RUNTIME:CAPTURE-PREPARE` (`native-runtime.f`, run by `PREPARE-TARGET` in `tools/native-build-core.f`) reaches `CHECKER-CAPTURE-PREPARE`, except on the whitebox image (`ACAP-WHITEBOX?`). The 974304d0 sweep reads the protected bit there, so sealing inside `ACAP-PWIN-CAPTURE` is too late. Assert `prot-wid-room` first (the bitmap bounds 8,192 ids; the engine uses 347).
- **Refuse:** the engine guard (`habu2.f C-PACKAGE-PROT-GUARD`) refuses the reopen at load and names the package. The checker adds no package refusal: engine-source certification and `tools/check.f` replay packages under mirror authority (`CHECKER-VERIFY-PKG-DEPTH` 1), and `prefix-src` re-declares all 72 core-prefix packages, so a `CHECKER-PACKAGE` refusal would reject the engine's own source. Check-time refusal by name comes from 974304d0: a private word of a sealed package is E-UNDEFINED.
- **Application packages:** a `--repl` snapshot keeps them reopenable.
- **Docs:** `docs/forth.md` Packages and `docs/forth-card.md` state the rule.
Files: `src/habu/aot-capture.f`, `docs/forth.md`, `docs/forth-card.md`, new `test/package-seal.f` (forked subjects, as in `test/internal-word-gate.f`).
Verify:
- On the product engine: `package XREF` is refused at load (rc 84, the name printed), and `tools/check.f --json-errors` reports a private XREF word as E-UNDEFINED, located.
- SUITE build-fixpoint-source green on the sealed product.
- `package MYAPP` opened twice succeeds.
- After `tools/hb-build.f -- --repl`, the application package reopens and XREF does not.
- On the whitebox engine, `package XREF` succeeds.
- `test/run.f`, generations byte-identical, and Etch's tests on the candidate.
Depends: habu-give-every-baked-9ca94f18; lands after the reopen-name fix (its checker hunk at 9218-9240 is adjacent).
Parent: habu-ship-only-the-d7d38629. Design: the Fable surface design of 2026-09-30 (~/.cache/tmp/heron-arm64/design-surface.md); census: ~/.cache/tmp/heron-arm64/size-census/.
