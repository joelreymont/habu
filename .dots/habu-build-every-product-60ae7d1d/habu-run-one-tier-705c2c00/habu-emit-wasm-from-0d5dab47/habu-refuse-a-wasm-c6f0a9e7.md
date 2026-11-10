---
title: Refuse a Wasm engine or REPL build at the command line and test the Wasm refusals
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T19:18:46.582060+02:00"
closed-at: "2026-11-10T20:15:33.040473+02:00"
close-reason: "landed in master e4ec011f; its gate: engines converge, 12 focused tests rc 0, suite 273 of 273, chain fixpoint at generation 2"
---

Problem (feature review B, F1 and F2): tools/hb-build-core.f ENGINE and REPL run `true target KERNEL-BUILD` for a Wasm target (only APP branches on WASM?), so `hb-build --engine --target wasm -o x` dies after 3 s inside src/arch/arm64/kernel-object.f with 'boot: unknown target'; test/wasm/hb-build.f REFUSAL-CASE tests one refusal (depth) of the writer's. Acceptance: tools/hb-build-lib.f HBB-PARSE refuses --engine or --repl with a Wasm target with its own sentence and 64 before any build; test/wasm/hb-build.f covers that refusal, the whole list for two unprovided words, a defer and a does> clause, each 74 with no module. Files: tools/hb-build-lib.f, test/wasm/hb-build.f. Verify: bin/hb --load test/wasm/hb-build.f ends test: ok. Depends: none. Ownership: those files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-r1.
