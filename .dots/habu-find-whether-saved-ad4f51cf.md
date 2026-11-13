---
title: Find whether saved images lose their checker record links
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T10:15:56.217074+02:00"
---

Problem: RECORD-LINK-RESTAMP, RECORD-LINK-RESTORE and RECORD-LINK-SEED (src/core/checker.f:16690-16821) are stored only into DECLARATIONS slots nothing reads, and RECORD-SNAPSHOT-COPY (:16709) has no caller, so a process started from a saved image (hb, an application image) may see the image's record links as not current. Acceptance: the cause is known as path:line; a lost caller is restored, or the words are shown unneeded and deleted. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-rlink.
