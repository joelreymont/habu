---
title: "Name a replay refusal's place in the refused file"
status: open
priority: 2
issue-type: task
created-at: "2026-11-01T15:11:22.045611+02:00"
---

Problem: when the context replay refuses at a record (tools/source-discovery.f SD-R-CONTEXT-FAMILY and SD-R-CONTEXT-KEY set SD-TOK-OFF and SD-TOK-LEN to the record's offset in the file being replayed), DISCOVER:BYTES$ still holds the bytes of the last file walked, often a child's, so a caller that prints LAST-TOKEN against BYTES$ names the wrong text (found by lane disc's fold 1 review; on master too). Acceptance: after every discovery refusal BYTES$ and LAST-TOKEN describe the same file, the refused one; a tools/event-closure-test.f case where a replay refusal follows a child's walk reports the record's own text, failing before. Files: tools/source-discovery.f; callers tools/check-verify-core.f ~338, tools/event-closure-lib.f. Verify: tools/event-closure-test.f, tools/source-discovery-test.f. Depends: lane disc landed (habu-key-discovery-records-b367e77e). Ownership: tools/source-discovery.f. Claim: unassigned.
