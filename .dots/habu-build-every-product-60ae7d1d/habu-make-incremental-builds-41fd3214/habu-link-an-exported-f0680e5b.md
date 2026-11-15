---
title: Link an exported does> definer warm
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T05:11:36.407043+02:00"
closed-at: "2026-11-13T05:24:20.433491+02:00"
---

Problem: a file whose package EXPORTs a private create ... does> definer builds cold with tools/hb-build.f --repl, but the warm build of the same file throws 7113 E-EXPORT-UNDEFINED; installed master fails the same way. A warm build must never fail where a cold one does not. Acceptance: the warm build succeeds and its image equals the cold one before the signature; a test/file-objects.f case guards it. Depends: none. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-dx.
