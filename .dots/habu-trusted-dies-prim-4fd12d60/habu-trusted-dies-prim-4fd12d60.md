---
title: "TRUSTED: dies; PRIM axioms remain for the foreign handful"
status: open
priority: 2
issue-type: task
created-at: "2026-08-19T09:53:28.150430+02:00"
---

Goal: PRIM is confined to genuine engine, syscall and FFI boundaries, ordinary Forth is checked, and the TRUSTED: definer is gone.

The native-build children in this directory are closed. What remains is the TRUSTED: retirement, carried by these open dots: habu-visibility-discharge-548-fab55650, habu-sweep-trusted-out-41e973ce, habu-sweep-trusted-out-f872acb0, habu-give-tests-a-8d8cdc19, habu-turn-deliberate-cast-ad2e237d, habu-retire-the-audited-85c43acf, habu-honour-owner-private-0a19f45d, habu-delete-the-trusted-42b30edd.

Close this parent when habu-delete-the-trusted-42b30edd closes.

## TRUSTED: census and order (2026-10-01, master 19a775f4, Fable plan)

Census `rg -c '^\s*TRUSTED:'`: 1,310 = src 354, lib 101, tools 108, test 747. Bodies naming `evaluate` belong to habu-add-the-evaluate-12ffa000 and its leaves (src 2, lib 2, tools 11, test 93); the rest, 1,202, are the leaves below. Per-site rows and real-path verdicts: ~/.cache/tmp/heron-arm64/trusted/rows.json, rp-lib.json, rp-tools.json, rp-testreg.json; probe script realpath.py.

Proof instrument: not tools/check.f (40 of 57 lib/tools files fail it unconverted). Convert one site and load the file through its real path: product rows `bin/hb --load <file>`; WHITEBOX rows the cached unsealed engine (`ls -t ~/.cache/habu-build/hb-whitebox-* | head -1` as HABU_UNDER_TEST); window fixtures `<engine> --load test/native-window-owner-child.f -- <fixture>`. The hook's refusal line (`hook: non-certified definition: NAME at 'TOK'` and the E- line before it) classifies the site.

Classes: (a) certifies as `:`; (b) a cast; (c) authority gap: the token's effect row exists but is not external, refused E-CAP-TRUSTED (checker.f DO-TOK-BODY ~12121-12135, ENFORCED? ~834); (d) a trusted-only primitive (prims.f ETRUSTED-ONLY! rows, checker.f PRIM-TRUSTED-ONLY! rows, UNSAFE-TOK? spellings), refused E-CAP-TRUSTED; (e) raw data-base cells or an opaque execute.

Order: ready now B1, B2 (habu-sweep-trusted-out-f872acb0), B11 (habu-honour-owner-private-0a19f45d), and the design passes for B8 (habu-visibility-discharge-548-fab55650, with the src post-hook move of B7) and B10a (habu-turn-deliberate-cast-ad2e237d). After 12ffa000: B4 and the evaluate leaves. After B11: B3, B5. After B8 (lands after habu-share-reopen-name-92885254 and the seal c550102f): B9a-d in parallel. After B10a: B6, B10b. B12 (habu-delete-the-trusted-42b30edd) last.
