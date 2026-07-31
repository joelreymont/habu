---
title: Reject over-limit generated names
status: closed
priority: 1
issue-type: task
created-at: "2026-07-30T16:16:01.769515+02:00"
closed-at: "2026-07-31T12:09:57.902076+02:00"
close-reason: "Absorbed atomically into E3 habu-nest-generated-family-70b2f31a: natural full-path builder, hard hash deletion, and named transactional capacity refusal must land together."
---

Why (Joel, 2026-07-30): a generated name past the 32-char limit silently falls back to a hash-built name - a silent fallback; authors must never wrap long words to appease a mangler. Result: delete the hash fallback. The sole namespace builder accepts exactly 1024 output bytes and throws `E-TFAM-NS-CAP` (7135) at byte 1025, before namespace lookup. Declaration transactions map only `E-TFAM-NS-CAP` to the family token and exact reason `tfam: constructor namespace too long`; other throws are unchanged, and the top-level renderer exits 76. Owner: engine half (codex). Acceptance: direct 1024/1025 builder cases plus all four real declarers prove the code, reason, no namespace query, process survival, and observable rollback; no second length formula, hash path, fallback, fixture, or implementation leaf remains.
