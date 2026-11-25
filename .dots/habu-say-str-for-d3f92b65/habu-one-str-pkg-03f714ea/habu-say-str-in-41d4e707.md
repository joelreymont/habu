---
title: "Say str in test helpers' public rows"
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T09:49:05.752836+01:00"
---

Problem: public words in test files say ^u8 n for bytes they only read, so the files that define them keep the split-str marker: test/gate-common-lib.f GE-ARG+ (keeps test/aot-shifted-bytes.f marked), test/checker-dead-path-suite.f DEADPATH-OWN:DIE, test/checker-verify-pkg-scope.f CVPS:REPLAY, test/browser/memory.f MEMORY and FREED; test/catch-stale-suite.f CS-RT, CS-CRT and CS-TRT say str for an address and a throw code. Acceptance: those rows say str (CS-* say ^u8 n); aot-shifted-bytes.f, checker-dead-path-suite.f, checker-verify-pkg-scope.f and catch-stale-suite.f carry no split-str; their suites pass. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-testrows.
