---
title: Resolve nested trust owner dots
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-16T13:33:09.628192+02:00\""
---

Full context: strict trusted inventory reports missing owner habu-add-checked-mem-ebd95492 although dot show resolves it at .dots/habu-add-bounded-host-b40b048f/habu-add-checked-mem-ebd95492.md. Cause: tools/trusted-inventory.f:1151-1154 accepts only .dots/<id>.md or .dots/<id>/<id>.md, but dot CLI stores child dots under their parent-id directory. Fix Habu-native owning-dot resolution for arbitrary valid nested child placement without accepting archive/closed dots or basename ambiguity; add scratch fixtures for child-under-parent, archive exclusion, duplicates/ambiguity, traversal safety. Acceptance: STRICT-OWNERS recognizes live nested dot, rejects archived/missing/ambiguous owners, trusted-inventory test/report strict, typed diff, host/filemap green.
