---
title: "Constraint: fixpoint + gate green for engine fixes"
status: open
priority: 1
issue-type: task
created-at: "2026-06-27T13:15:58.146800+02:00"
---

Cross-cutting constraint for B/C: every checker/compiler change must (1) self-rebuild byte-for-byte to fixpoint (BF-BUILD-ALL), (2) pass the full gate, (3) be prototyped in a jj workspace via a temp hb-new before proposing (don't disturb the rebuild-agent's shared bin/hb). Sequence A->B->C by risk. A and prelude land like lib/hashmap.f (manifest/FILEMAP/gate/lints).
