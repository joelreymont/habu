---
title: Strip the names of sealed-internal words too
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-17T01:17:31.896498+03:00\""
---

Problem: e74437cf strips a name only for a package-PRIVATE word the payload does not name; Joel's scope (2026-09-16) is every word the compiler, JIT and REPL do not expose, which includes globals and package-public words the internal-mark pass seals as internal (src/core/internal-mark.f) and that no source can reach after the seal. Acceptance: ACAP-NAMED? consults the internal mark at capture so a sealed-internal word's name is stripped by the same rule (payload-named, keep-set, package rows excepted), the sidecar records it, the name pool shrinks by the measured amount on a native-runtime engine (host class stated), test/gate-dictionary.f and the whitebox suites that name internals are handled per habu-whitebox-suites-7fe05e62 rather than by widening the keep-set; byte fixpoint; test/run.f. Files: src/habu/aot-capture.f, src/core/internal-mark.f, test/. Verify: engine-size; gate-dictionary; fixpoint. Depends: habu-whitebox-suites-7fe05e62 for the suites it breaks. Ownership: AOT capture. Claim: agent=hazel-strip-internal workspace=.jj-ws/hazel-strip-internal.
