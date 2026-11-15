---
title: "Store a build's strings at here"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-05T14:55:27.196281+02:00"
closed-at: "2026-11-05T21:39:39.559397+02:00"
close-reason: "Landed on master 64671059 'Store a build's strings at here' with S5b1 (6f3717e8); proof at the tip: native-build rc 0, compiler-native-string, compiler-native-string-forms, compiler-tier0-string-literals, repl-literal-segment, linker and arm64-place pass (/tmp/jerry-s5b1/rows-land.log)."
---

Problem: the cross-build's data space (step 5 S5) needs its strings stored in the space: today INTERN keeps every body in segments chained through DATA (src/habu/intern.f NEW-SEGMENT :151, SLOT :108). Joel's rulings (2026-11-03 and 2026-11-05): a length cell then the bytes at here, a hash table of addresses in memory outside DATA, no list, the table only adds; HERE moving back in an open space fails the build at its next string with E-INTERN-REWIND. Approach (lead): STRING takes a space path when BOOT:DP-FLOOR-CELL differs from data-base + DATA-START; one table per floor, mapped at the space's first string, released when the floor changes; FNV-1a split out of HASH; refusal when DPHW-CELL is nonzero (the opener zeroes it). Acceptance: scratch probe (hand-opened space): equal strings share an address in the space, 600 strings grow the table, a negative allot then a string throws E-INTERN-REWIND, the host path unchanged after close. Lands with S5, whose build tests reach the space path through the real load path. Files: src/habu/intern.f, src/lib/errors.f. Verify: arm64 build; compiler-native-string compiler-native-string-forms compiler-tier0-string-literals repl-literal-segment (baseline green on master f9b79d8f); probe /tmp/jerry-s3/probe.f. Brief: /tmp/jerry-big/brief-s3.md. Depends: none. Ownership: those files. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-s3
