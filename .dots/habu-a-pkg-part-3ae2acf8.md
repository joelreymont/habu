---
title: "A package part named NFETCH-CHECK switches off tier 0's tag check"
status: active
priority: 1
issue-type: task
created-at: "2026-11-10T13:31:22.381889+02:00"
---

Problem: a checked fetch's compiled code calls NFETCH-CHECK:BAD-TAG and NFETCH-CHECK:TAGS by spelling (src/habu/t0.f:1181 and :1203, src/compiler/ir/elaborate.f:3615, through NDICT:CALL-TARGET, dict.f:65 and :335-345 SCOPE-REC); inside package APP a part APP:NFETCH-CHECK with its own BAD-TAG is found first, so the program's fetch of an out-of-domain enum passes: /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/boot/src-probes/part-hijack.f builds and prints 'passed' with exit 0, where part-control.f dies 'hb: bad layout tag' with 85; the same under --load. Found by the review of habu-a-built-program-e5617780. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: the three sites bind the runtime words in the top-level namespace, not the scope of the code being compiled, as tools/hb-build-core.f:267-272 finds KERNEL-OBJECT:WRITE (BOOT:NAMESPACE-WL, then XREF-PKG-PUBLIC); part-hijack.f then dies with the bad-tag error in a build and under --load, and a test pins it. Files: src/habu/t0.f, src/compiler/ir/elaborate.f, a test. Verify: the two probes on bin/hb; the test fails on master's engine. Depends: habu-a-built-program-e5617780 landed (every build carries the package). Ownership: tier 0 and tier 1 fetch validation. Claim: agent=bob workspace=.jj-ws.noindex/bob-fetchname
