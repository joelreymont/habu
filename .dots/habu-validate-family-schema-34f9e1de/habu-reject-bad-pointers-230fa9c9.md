---
title: Reject bad pointers at STRUCTURE close
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T22:41:07.311515+02:00"
blocks:
  - habu-add-shared-family-76a761c3
---

STRUCTURE integration of the shared query: remove the early REQUIRE-POINTEE authority; after final range and width binding and before generation, call FAMILY-SCHEMA:BAD-PTR, arm DECL-REJECT at the returned field name, reject with anchored diagnostic, and prove byte-identical rollback plus a clean following declaration. Production-declarer negative for the slaunder reproduction with exact diagnostic; existing rejection fixtures stay green. LAND TOGETHER with the ENUM integration - master never carries two authorities. Acceptance: structure-decl-suite green; rollback-clean fixture; both diff lints.
