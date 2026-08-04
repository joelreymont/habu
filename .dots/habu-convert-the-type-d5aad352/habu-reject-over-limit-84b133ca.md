---
title: Reject over-limit generated names
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T16:16:01.769515+02:00"
---

Why (Joel, 2026-07-30): a generated name past the 32-char limit silently falls back to a hash-built name - a silent fallback; authors must never wrap long words to appease a mangler. Result: delete the hash fallback; a declaration whose generated name exceeds the engine limit is REJECTED at check time with a named error. With nested namespaces the generated component is family-local so the limit rarely binds. Owner: engine half (codex). Prerequisite census: prove zero live hash-named words exist in the tree before deletion. Acceptance: an over-limit declaration fails loudly with the named code through the production load path; no hash path remains in the generator.
