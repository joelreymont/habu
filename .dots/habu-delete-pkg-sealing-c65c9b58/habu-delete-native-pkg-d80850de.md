---
title: Delete native package-name wall
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T22:10:14.562565+02:00"
blocks:
  - habu-own-engine-emitter-42db38aa
  - habu-recognize-gforth-vocabulary-33e79326
---

Problem: the native and Gforth recovery compilers reserve package names in a second hard-coded wall even though protected wordlist identity is already the sole structural authority. This duplicate name authority blocks ordinary package HB-ERROR and qualified HB-ERROR:BAD-TAG while adding no protection that the registry does not own.

Result: in src/habu/habu2.f delete RESTAB and LRESTAB data, C-SEAL-PACKAGE-FAIL, C-SEAL-MATCH, C-QUALIFY-SEAL-GUARD, C-PACKAGE-SEAL-GUARD, and every call from qualified definition, POSTPONE, tick, backtick, package, and export paths. In bootstrap/cg/forth.fs delete LRESCHECKCERT, LRESLOWERCERT, LRESLOWERHOOK, LRESENGINEERROR, their strings and labels, every qualified-name comparison and refusal block, plus C-PACKAGE-SEAL-GUARD and its call. Retain C-PACKAGE-PROT-GUARD and route its refusal directly to error 84. Retain every protected-wordlist registry state and call.

Hard-rename HB-ERROR:SEAL-PACKAGE to HB-ERROR:PROTECTED-WID in src/core/engine-error.f, src/core/engine-error-effects.f, src/habu/habu1.f, src/habu/habu2.f, bootstrap/cg/forth.fs, test/engine-suite.f, test/aot-wid-suite.f, test/seal.f, and tools/bootstrap-codegen-test.f. There is no alias, old arity, second constant, or translation layer.

Gforth ownership and hard rename: bootstrap/cg/forth.fs declares vocabulary HB-ERROR and owns the error constants there under their short tails. It declares vocabulary HB-EMIT and owns the direct bootstrap emission closure: BPROTWIDADD, EMIT-ENGINE-PRIMS, EMIT-PRIMS, the four LRES variables, EMIT-KWDATA, C-QUALIFY-DEF, EMIT-CREATE, C-CONSTANT, C-COLON-PENDING-DREC, C-DEFER, C-TRUSTED, C-PACKAGE-PROT-GUARD, C-PACKAGE-SEAL-GUARD until deletion, C-PACKAGE, EMIT-INTERPRET-COLON, EMIT-INTERPRET-WORDS, EMIT-INTERPRET, EMIT-MAIN, EMIT-LABEL-CONTROL, EMIT-LABELS, EMIT-PRIMITIVE-SECTIONS, EMIT-DICTIONARY-SECTIONS, EMIT-CODE-SECTIONS, and EMIT-FORTH. Every Gforth ENGINE-ERROR and ENGINE-EMIT qualified caller changes with the vocabulary; no old vocabulary or alias remains. The external recovery API remains exactly FORTH-EXE, FORTH-BUILD-EXE, and FORTH-REPL-EXE.

Fixture ownership: test/seal.f opens package SEAL-SUITE. SLV-PUBLISH-FORGE$, SLV-ASSERT-PROT-PUBLISH, SLV-PROT-PUBLISH, SLV-OWNER-PRI-FORGE$, SLV-OWNER-PUB-FORGE$, and SLV-OWNER-FORGE are private; hard-rename SLV-MAIN to public RUN and invoke SEAL-SUITE:RUN. tools/bootstrap-codegen-test.f opens package BCG-SUITE; BCG-TEST-HB-ERROR is private; hard-rename BCG-MAIN to public RUN and invoke BCG-SUITE:RUN. Add no forwarding word.

Owner and exact files: src/habu/habu1.f, src/habu/habu2.f, bootstrap/cg/forth.fs, src/core/engine-error.f, src/core/engine-error-effects.f, test/engine-suite.f, test/aot-wid-suite.f, test/seal.f, and tools/bootstrap-codegen-test.f. Dependencies: completed native HB-EMIT ownership and structural Gforth vocabulary recognition in the existing package gate. Pre-change production probes: package HB-ERROR and qualified HB-ERROR:BAD-TAG both exit 84; a generated-constructor publication also exits 84. Acceptance before M17: the first two probes succeed; the constructor still refuses with PROTECTED-WID 84; native and recovery behavior match; ENGINE-ERROR, ENGINE-EMIT, and SEAL-PACKAGE have zero live spellings; exact absence, rename, package, typed-local, and trust gates pass. Runtime suites execute only in M17.

Forbidden: protected registry deletion, ordinary owner registration changes, checker/export mirror edits, compatibility, aliases, versions, new lint or exemption, allowlists, documentation, unrelated seal work, or any public bootstrap helper beyond the three existing recovery entry points. Claim: unassigned.
