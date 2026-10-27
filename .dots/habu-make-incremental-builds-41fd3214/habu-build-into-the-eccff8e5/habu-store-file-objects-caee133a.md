---
title: "Store file objects under the tree's .out"
status: open
priority: 2
issue-type: task
created-at: "2026-10-27T13:24:03.136834+02:00"
---

Problem: see parent. Change: Lane 1 of the design: new tools/build-out.f (BUILD-OUT:ROOT$ OBJ$ IMG$), tools/native-definition-cache.f ROOT$/FILE-PATH/USED/DISCARD/OPEN onto it, HBB-PRUNE-OBJECTS deleted, native-file-storage-e2e rewritten, native-file-root-e2e deleted, HABU_BUILD_CACHE users in native-file-* tests switch to a scratch cwd. FORMAT-VERSION stays here; habu-drop-schema-and-12ddb887 removes it only where the key provably covers the writer. Design: /Users/joel/.cache/tmp/carl-file/design-build-out.md §5. Verify: the lane's proof list in the design. Depends: previous lane. Ownership: carl. Claim: unassigned.
