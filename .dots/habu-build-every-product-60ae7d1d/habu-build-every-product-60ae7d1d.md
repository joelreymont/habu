---
title: Build every product as a cross-build with one compiler
status: open
priority: 1
issue-type: task
created-at: "2026-11-04T11:48:45.259779+02:00"
blocks:
  - habu-make-incremental-builds-41fd3214
  - habu-take-checker-working-6703bc4f
  - habu-move-the-interpreter-d35780fa
  - habu-name-the-build-abed309a
  - habu-run-one-tier-705c2c00
  - habu-read-each-def-f7ce769a
---

Why: Joel, 2026-11-04 ("how Habu builds Habu", "one compiler"): an arm64 build is a cross-build whose product is data the build writes, and every machine runs one typed interpreter; the front end is shared and only code generation differs per architecture. What remains of the build and compiler work sits under this dot.

Order (each step lands as small green commits; a later step starts when the one before it lands; Joel, 2026-11-09: arm64 to arm64, then Wasm, then x86-64):
1. The cross-build and the one linker (habu-make-a-file-dfc7e030), under habu-make-incremental-builds-41fd3214. Prerequisite: checker working state outside DATA (habu-take-checker-working-6703bc4f).
2. arm64 end to end: one typed interpreter (habu-move-the-interpreter-d35780fa); the build names (habu-name-the-build-abed309a).
3. Wasm on T0 and the one linker (habu-emit-wasm-from-0d5dab47).
4. x86-64 through the same linker (habu-build-x86-64-3ed6f45d).
5. Deletions: tier 1 (habu-delete-tier-1-f5c44fb4) and the JIT (habu-delete-the-jit-d9fdaf67), held until T0 is as fast.
Beside steps 1-4: one T0 front end (habu-copy-every-tier-86155a25, habu-make-a-cb-6de47af1, habu-read-each-def-f7ce769a, habu-compile-field-loan-5b06237b) under habu-run-one-tier-705c2c00.

Other replacements, not ordered against this one: TRUSTED: and privacy (habu-delete-the-trusted-42b30edd, habu-move-engine-internal-fc2ba551, habu-delete-the-internal-3a818b8a); the hard cutover (habu-drop-schema-and-12ddb887, habu-retire-the-engine-b5850c5c); hooks to plain calls (habu-remove-the-checker-4425bcef, habu-make-the-src-f0bdcbe1, habu-move-type-family-c1875868, habu-simplify-the-lower-2f175f5c); fork only to exec (habu-fork-only-to-59d3557e).

Acceptance cases (defects the replaced machinery shows):
- A stripped or cross-built program names a baked error code and its own (ERROR:NAME$, ERROR:TEXT$). A stripped hb-build of a program that declares a code with ERROR:CODE and logs it and -9690 with LOG:CODE+ exits 0 and names its own code but prints -9690 (LINKER:E-FAILED, a row in hb's data) as the number alone; the same source under hb --load names both.
- One 5-minute deadline bounds each engine build whole, and the build's own timeout names the step that ran out. Today tools/hb-build.f has no deadline of its own: tools/seed.f:17 and tools/chain-run.f:27 kill it from outside at 300000 ms, and the E-PROC-TIMEOUT that follows names no step.
- Once no build stores into the running hb's global, namespace and types wordlist cells (src/habu/linker.f MOVE-IN :4714, RESTORE :5264), DATA-BANDS gains a row for each of BOOT:GLOBAL-WL-CELL, BOOT:NAMESPACE-WL-CELL and BOOT:TYPES-WL-CELL, a store from checked source into each exits 83, and test/seal.f holds a negative case per cell and the reproducer of /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/wlcell.
- A read or write through a null the checker cannot see (': R ( ptr n -- n ) @ ;' then 'NULL-PTR R') stops with a catchable code, not a segmentation fault (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/nullread-hunt.md item 4).

Rule (docs/design.md "Machinery being replaced"): no change extends or patches replaced machinery; a defect it shows becomes an acceptance case of the replacement here.
Campaign only; do not dispatch.
Consumers: none meets this today.
Claim: unassigned.
