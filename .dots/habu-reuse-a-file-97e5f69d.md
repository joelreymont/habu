---
title: Reuse a file again after its source is restored
status: open
priority: 2
issue-type: task
created-at: "2026-10-24T03:51:02.265335+03:00"
---

Cache file objects are keyed by load place, so edit lib.f then restore it overwrites lib.f's object and the restored build recompiles it. The old storage e2e asserted reuse after restore per definition; the file-unit restatement (carl-adm, zlnyxvvn) asserts only that an unused entry survives. Keeping objects per source version would restore it. Astra adm review F2. Workspace: none yet.
