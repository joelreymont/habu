---
title: EOF inside a definition is silently accepted or crashes
status: open
priority: 2
issue-type: task
created-at: "2026-07-08T09:52:08.028726+02:00"
---

Found reducing habu-gate-runner-entry-81c84af0. A source file ending mid-definition (': HALFDEF ( -- )' then EOF) exits rc 0 with no diagnostic on a small load (bin/hb --load half.f), i.e. an incomplete definition is silently discarded or left latent; the same mid-definition truncation inside a large closure (gate-runner-support prefix + truncated tools/check-core.f) crashes rc 134 (SIGABRT-class) with the habu-crash register dump. Neither behavior is right: EOF inside a colon definition should be a deterministic named parse error (fail-closed) regardless of context. Engine parser territory (src/habu include/interpret loop). Repro: (1) printf ': HALFDEF ( -- )\n' > half.f; bin/hb --load half.f; echo $? -> 0. (2) prefix file of the first 41 gate-runner-support requires + head -476 tools/check-core.f as an include -> rc 134 crash dump. Minimal fixture + negative regression belong with the engine fix.
