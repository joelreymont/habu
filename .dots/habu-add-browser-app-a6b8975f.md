---
title: "Add BROWSER-APP's page, turn records and file record"
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T19:29:11.644480+02:00"
---

Maki design r2 (/Users/joel/Work/maki/.jj-ws.noindex/tim/.out/chat-tools/design-chatgpt-r2.md §4.3, §4.5, §5 item 1, lane H2; r1 §4.3-4.4): BROWSER-APP:PAGE$ (page template, turn.js, viewport.js, bridge script, module base64-inlined), events tick and result, records call, context and after; plus a file record: a file the person picks or drops on the page reaches the module as {name, size} and the module reads it by byte range, so a large file never sits whole in wasm memory. Files: src/lib/browser/{app.f,app.html,app.js,turn.js,host.js,host-cli.mjs}, host-test.f. Caller: Maki's view resource and sidebar upload. Needs the lead's lane design, then names to Joel through the interview. Ownership: carl. Claim: unassigned.
