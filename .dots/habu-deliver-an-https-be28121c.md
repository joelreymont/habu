---
title: Deliver an HTTPS response body incrementally
status: open
priority: 2
issue-type: task
created-at: "2026-10-23T10:48:30.978029+03:00"
---

Problem: CURL PERFORM and AWAIT expose only a completed, buffered body (src/lib/net/curl.f header). Maki's OpenAI chat turn cannot emit live progress events while the response streams. Acceptance: a caller receives body chunks as they arrive (callback or pull), with a test against a local server that sends a body in delayed parts and observes the first part before the last is sent. Files: Habu CURL library (owner decides). Verify: that test. Depends: none. Ownership: Habu CURL owner. Caller: Maki src/openai.f and src/chat.f. Claim: unassigned.
