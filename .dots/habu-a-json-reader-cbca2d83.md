---
title: "A JSON reader word for the current value's source text, whatever its kind"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T21:55:01.919147+02:00"
---

Problem: JSON:READ-VALUE-SPAN$ (src/lib/json/read.f) answers the source text of an object or array and throws on a string or number. Three Maki callers need a member's text as written, to copy it into another document, and each widens READ-SPAN$ by hand to take in a string's quotes (maki src/project-import.f ARG-RAW$, src/ops.f QUOTES, deploy/rehearsal/rehearse.f COPY-MEMBER). Requested by tim, /tmp/tim-habu-json-span.md; nothing is blocked on it. Acceptance: one reader word answers the current value's source text for every kind (object, array, string with its quotes, number, true, false, null); Maki deletes its three copies. Open: widen READ-VALUE-SPAN$ or add a word (a public API: settle before a worker starts). Files: src/lib/json/read.f and its test. Verify: the reader's test covers each kind. Depends: none. Ownership: src/lib/json/read.f. Claim: unassigned.
