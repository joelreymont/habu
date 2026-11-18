---
title: Write every place-and-expectation rejection through one writer
status: active
priority: 2
issue-type: task
created-at: "2026-11-18T09:52:01.034750+01:00"
---

Problem: src/lib/base64.f, xml/reject.f, json/read.f, json-schema.f, regex.f, odata/metadata.f, net/mcp.f and net/model.f each carry a private writer of the text `L:C: expected X, found Y; fix` (base64.f PUT$, json-schema.f:188 PUT and regex.f:169 PUT are one helper three times), and each sizes its text its own way, some by a hand-counted frame (json-schema.f and regex.f TEXT-SLACK). Acceptance: one writer, in REASON, writes the text from its parts and every one of those packages calls it; their pinned texts are unchanged. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-reason
