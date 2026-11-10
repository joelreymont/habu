---
title: The build core builds hb for a Wasm target unrefused
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T21:34:33.166658+02:00"
---

Problem (review of soxqmyus, 2026-11-10): tools/hb-build-lib.f HBB-PARSE (:198-200) refuses an --engine or --repl line for a Wasm target, but HB-BUILD-CORE:ENGINE and REPL (tools/hb-build-core.f :315-328) take any target, so tools/build-profile.f reaches ENGINE with a Wasm target; what the build then does was not traced. Acceptance: ENGINE and REPL end with 'hb-build: a Wasm target builds a program, not an engine or a REPL' and status 64 for a Wasm target through one public word, HB-BUILD-CORE:REFUSE-WASM, which HBB-PARSE also calls so a refused line makes nothing (at the link alone it left an empty out root); the parser's own copy of the rule is gone; test/wasm/hb-build.f's two refusal cases pass unchanged. Files: tools/hb-build-core.f, tools/hb-build-lib.f. Verify: the brief's four command lines (.jj-ws.noindex/jerry-wasm/.out/briefs/wr.md), test/wasm/hb-build.f. Depends: none. Ownership: jerry. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-wr.
