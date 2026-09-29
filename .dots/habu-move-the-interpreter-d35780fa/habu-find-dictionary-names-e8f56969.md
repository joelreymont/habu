---
title: Find dictionary names in Habu
status: active
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.629799+03:00"
---

Problem: dictionary lookup for the interpreter is assembly (`LFIND`/`LFINDUSED`) while `NDICT` (`src/compiler/native/dict.f:47-103`) already reads records with package visibility in Habu.
Acceptance: `src/habu/outer.f` `OUTER:FIND` with the `LFIND`/`LFINDUSED` rules (open package public/private, used packages with `E-USING-AMBIGUOUS`, qualified names, global-first order, hash index), built on `NDICT`'s readers where they agree; a differential test against `search-wl` over the whole booted dictionary.
Files: `src/habu/outer.f`, `test/outer-find.f`.
Verify: spark `bin/hb --load test/outer-find.f`.
Depends: none in the lane (I1 was folded into I9a, `habu-move-evaluate-and-9119f746`).
Route: Alder (shared: src/habu/outer.f, test/outer-find.f).
Ownership: krait (Intel lane).
Claim: agent=krait workspace=.jj-ws/habu-find-dictionary-names-e8f56969.
Preflight corrections (these override the lines above where they differ):
- Base: `master` `bca98173` (dots-only on `72a4b76a`; K4/X7 moved `habu1.f`, P2 `prims.f` and `test/gate-stdlib-cases.f`). Workspace `.jj-ws/habu-find-dictionary-names-e8f56969`.
- Order (replaces "global-first"): open private, then open public, then global (`src/habu/habu1.f:4279`, `4448-4455`), then used publics only after that chain misses (`habu2.f:8110-8131`; `forth.fs:2126-2128`; `dict.f:4-6` agree). `P:tail` searches P's public wid through the namespace row (wid -1, `habu1.f:4288-4347`); `:x` and `x:` are bare, `a:b:c` misses (`4275-4287`; `xref.f:272-284`); a colon-bearing token never reaches the used chain.
- Probe: the per-wordlist lookup is the primitive `xref-search-wl` (WLFIND, the routine LFIND's probe shares, `habu1.f:3163-3288`) behind one `TRUSTED:` wrapper (trusted-only row, `prims.f:563-564`). Not `search-wl`: it hides `OWNER-API-PRI-WID` and `DNAME-INT` rows (`habu1.f:3327-3337`), while LFIND returns them with flag bit 4 (`4207-4210`), which I4's fail-closed refusal reads. No Habu re-hash.
- Owner: `OUTER` owns the record-level chain; `dict.f` is untouched. NDICT's chain is a different contract: `WL-CANDIDATE` (`dict.f:47-59`) applies `VISIBLE-RECORD?` at every step and continues past an internal or START-0 row, where LFIND stops at the first match and reports the flag. Rebuilding `dict.f` on `OUTER` now would put `outer.f` in the product closure (`compiler.f:31` requires `dict.f`) before I9a. Unification is `habu-derive-ndict-s-563ae6d8`, after I9a.
- Acceptance: `OUTER:FIND ( ptr u8 n -- ptr n )` in `package OUTER`: the record or `XREF-NULL`; flags via `XREF-FLAGS`; two distinct records across used publics throw `E-USING-AMBIGUOUS` (checker 7144, as `dict.f:70`; the engine dies rc 94, `habu2.f:8239-8243`, `test/using-test.f:47`; I4 maps the throw to that die); one package used twice is one binding. Quirk kept for parity and pinned: `P:tail` while P is open and `tail` global-only answers the global (`habu1.f:4448-4455` retries when the qualified wid equals `PKG-PUB-CELL`; NDICT misses, `dict.f:95-101`).
- Load: test-only, `require src/habu/outer.f` from the test (precedent P2: `src/habu/sites.f`, required only by `test/sites.f:32`). No manifest row: that is I9a.
- Test (`test/outer-find.f`): walk records `0 … ndict@` (`test/aot-seed-surface.f:52-58`); for each `XREF-NAME$` the expectation is the chain over `search-wl` on `NDICT:OPEN-PRI`, `NDICT:OPEN-PUB` (`dict.f:23-27`), 0, then `USE-WIDS[0..depth)`. Asserted classes: `DNAME-INT` rows (`search-wl` 0, FIND returns the record with the flag); `DICT-WL:RETIRED` rows never found; namespace rows only as qualifiers; case-folded spellings find the same record. Fixtures: two packages exporting one tail under both `using`s (ambiguous), one package used twice, a private tail shadowing a global, a global-only tail under a `using`, the qualified quirk. Run at top level and inside a package.
- Files: `src/habu/outer.f`, `test/outer-find.f`, `test/gate-stdlib-cases.f` (`SUITE outer-find`).
- Verify (spark or the ThinkPad under qemu): `bin/hb --load test/outer-find.f`; gate.
- Overlap: I3 creates the same `outer.f` (`package OUTER`) concurrently; jj records an add/add conflict and the integrator keeps one header, one `package OUTER` block and the union of requires. I2's helpers carry a `FIND-` prefix so no name collides (rc 78).
- Route: Alder (`test/gate-stdlib-cases.f`; `outer.f` is new and loaded only by tests).
