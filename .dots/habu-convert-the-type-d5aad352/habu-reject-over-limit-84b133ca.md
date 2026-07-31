---
title: Reject over-limit generated names
status: closed
priority: 1
issue-type: task
created-at: "2026-07-30T16:16:01.769515+02:00"
closed-at: "2026-07-31T12:09:57.902076+02:00"
close-reason: "Absorbed atomically into E3 habu-nest-generated-family-70b2f31a: natural full-path builder, hard hash deletion, and exact capacity refusal must land together."
---

Why (Joel, 2026-07-30): a generated name past the 32-char limit silently falls back to a hash-built name - a silent fallback; authors must never wrap long words to appease a mangler. Result: delete the hash fallback; a declaration whose generated name exceeds the engine limit is REJECTED at check time with a named error. With nested namespaces the generated component is family-local so the limit rarely binds. Owner: engine half (codex). Prerequisite census: prove zero live hash-named words exist in the tree before deletion. Acceptance: an over-limit declaration fails loudly with the named code through the production load path; no hash path remains in the generator.
