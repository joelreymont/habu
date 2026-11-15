---
title: Check a declared parse-imm payload as the load does
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T22:39:24.435903+01:00"
---

Problem: with 's" PI00" 1 parse-imm', ': USE ( -- n ) PI00 zz 73 ;' loads and runs under --load, but tools/check.f rejects it E-UNDEFINED on the payload token zz (installed hb too; /tmp/carl-pimm/g1.f): the verifier and the load disagree on a declared parsing immediate's payload. Acceptance: tools/check.f accepts what --load accepts for a declared payload, and a test pins it; test/diag-position-test.f TEST-OWN-PARSE-IMM (no longer caught since the parse-imm table grows) is restated so a package's own `parse-imm` replayed as the engine's would change a payload count the check applies. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-payload.
