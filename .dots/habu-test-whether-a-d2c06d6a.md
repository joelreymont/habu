---
title: Test whether a string holds another, ignoring ASCII case
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T14:37:49.207343+01:00"
---

Problem: Maki's find tool matches part names case-insensitively and today would carry its own 5-line helper (maki src/inspect.f:701 HOLDS-CI?, tim's request 2026-11-26); STR has STR:=CI but no substring test. Acceptance: STR answers ( str str -- bool ) whether the first holds the second with letters compared as STR:=CI does, with a test, and Maki drops its helper. Depends: none. Claim: unassigned.
