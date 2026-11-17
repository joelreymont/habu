---
title: Copy JSON values without recursion
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T15:34:08.672592+01:00"
---

Problem: src/lib/json/copy.f:34,45 COPY-VALUE recurses once per nesting level of the text it copies, while the reader accepts any depth (src/lib/json/read.f:50-54), so untrusted deep nesting exhausts the return stack. Acceptance: COPY-VALUE copies a value nested deeper than the return stack holds, with the open containers kept in a growing buffer. Depends: none. Claim: unassigned.
