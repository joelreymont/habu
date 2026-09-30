---
title: Persist clobber summaries in dictionary records
status: open
priority: 3
issue-type: task
created-at: "2026-09-30T10:58:43.409183+02:00"
blocks:
  - habu-publish-and-use-db11d4c1
  - habu-pool-data-addresses-d65bdc94
---

Campaign: ARM64 code-size fixes, design revision 3 (§3.6). Line references are at master 8c9b75af; re-verify before editing.

Problem: after habu-publish-and-use-db11d4c1 a caller compiled in a later session (an application calling baked lib/ words) sees the whole pool for every baked callee; the 20-byte capture record (src/habu/aot-capture.f:1387-1389) has one spare byte and no summary field.

Acceptance: the record carries the GPR and FPR summaries (the format grows; AOT-FILE:VERSION bumps once together with habu-pool-data-addresses-d65bdc94's bump), the seed reader (habu2.f) installs them, XREF exposes them and RESOLVE-CALLABLE reads a baked callee's summary. Fixture: an application source compiled on the product against a baked leaf keeps a value in a register across it; an old payload is refused by version. Artifact: suite output; an Etch --size-report before and after.

Optional: dispatch only if habu-publish-and-use-db11d4c1's report shows more than 10 KB of call-crossing spills against baked callees in Etch.

Files: src/habu/aot-capture.f, aot-file.f, habu2.f (seed reader), xref.f, src/compiler/native/hir-word.f, tools/native-build-core.f only if the record writer lives there (coordinate with swift). Engine text: yes; two-stage landing per docs/bootstrap.md if the host resolves a new name; seed mirror: no.

Verify: tools/native-build.f product; tools/two-generation-build.f; bin/hb --load test/run.f; tools/hb-build.f -- --repl --size-report <etch entry> before and after.

Depends: habu-publish-and-use-db11d4c1, habu-pool-data-addresses-d65bdc94.

Ownership: the files above.

Claim: unassigned.
