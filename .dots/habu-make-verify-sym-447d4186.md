---
title: "Make VERIFY:SYM-SOURCE private"
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T03:36:29.595172+01:00"
---

Problem: src/habu/verify-source.f:1335 SYM-SOURCE is public, made so for VERIFY:ON-BINDING installers; ON-BINDING is deleted and nothing outside VERIFY calls SYM-SOURCE. The ON-USE comment (~1865) lacks what the deleted ON-BINDING comment said about a primitive's visit. Acceptance: SYM-SOURCE is private to VERIFY; the ON-USE comment states a primitive's visit and target bytes. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-pub.
