---
title: Lay only the kernel a stripped program reaches
status: active
priority: 2
issue-type: task
created-at: "2026-11-06T02:54:31.486948+02:00"
---

Problem: LINK-STRIPPED (src/habu/link-data.f) lays the whole arm64 engine, ENGINE-EMIT:KERNEL (src/habu/habu2.f:14046-14052, 207,552 bytes: EMIT-MAIN's machine-code interpreter 90,604, the primitives 61,044, the dictionary and runtime sections, EMIT-DICT), into every stripped program; LR-HI reaches 2,068 bytes of it and stripped-pool-reach's app 2,700 (census .jj-ws.noindex/carl-dots/.out/carl-file/reach-report.md). Design r2 section 5 (/tmp/jerry-big/design-xbuild-r2.md:454-490, approved 'Approve, start building') makes the product a primitive kernel plus the typed runtime that never links habu2.f. Acceptance: a stripped program carries only the kernel code its closure reaches; arm64-link-run's programs run as before and LR-HI's code is under 16 KB; arm64-link-data, arm64-place and linker pass. Design: a Fable design (Plan) from brief .jj-ws.noindex/carl-dots/.out/carl-file/brief-kcut-design.md; significant (product layout), so provisional while Joel is away (dot habu-s6-l5-provisional-5f8fd488). Files: per the design. Verify: bin/hb --load test/run.f -- arm64-link-run arm64-link-data arm64-place linker. Depends: none. Ownership: carl. Claim: agent=carl (Fable design on Plan, brief .jj-ws.noindex/carl-dots/.out/carl-file/brief-kcut-design.md).
