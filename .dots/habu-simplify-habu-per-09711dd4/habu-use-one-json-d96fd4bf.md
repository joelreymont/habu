---
title: Use one JSON reader and writer in tools
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:31.135133+03:00"
---

Problem: tools/json.f:100-730 is a second JSON parser beside src/lib/json-read.f. There are three writers: src/lib/json-write.f, tools/json.f:733-826 (JSONW-*) and tools/lint/json-writer.f (119 lines). tools/json.f:851-949 (JSONL-*) reimplements jsonl.f iteration. Acceptance: tools use src/lib/json-read.f, json-write.f and jsonl.f; the duplicates are deleted; tool output byte-identical. Design with the Plan agent before dispatch (API differences between the readers). Files: tools/json.f, tools/lint/json-writer.f, their users. Verify: tool test rows that read or write JSON. Depends: carl-final-core chain landed on master (it edits these files); start from master after it lands. Ownership: listed files. Claim: unassigned.
