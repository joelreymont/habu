---
title: Correct comments that say a quotation cannot read a local
status: active
priority: 2
issue-type: task
created-at: "2026-11-11T17:02:03.659543+02:00"
---

Problem: T0 compiles a quotation that reads its word's locals on arm64 and Wasm, yet comments still say it cannot, some beside globals kept only as that workaround. `rg -i 'quotation cannot (read|reach|capture)'` finds them in src/core/sumtype.f, src/habu/layout.f, src/habu/verify-source.f, src/lib/genio-core.f, src/lib/net/http.f, src/lib/net/http-response.f, src/lib/net/http-test.f, test/engine-suite.f, test/checker-dup-record.f, test/certify-does-definer.f, test/checker-verify-pkg-scope.f and test/compiler/integer-literals.f. A comment whose quotation goes to a task stays true (E-LOCAL-ESCAPE). Acceptance: each such comment is true (corrected or removed, by forth-card '9 Comments'); a global that exists only as the workaround becomes a local read by the quotation, where x86-64 does not compile the file (x86-64 rejects the read until dot habu-compile-a-quotation-ed6d5042). Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-qloc (files the landing stack does not touch; sumtype.f, layout.f and verify-source.f after the built-items landing).
