---
title: An hb that starts during an install loads a mix of old and new sources
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T22:13:19.810051+01:00"
---

Problem: tools/install.f (:8-12) writes each file beside the old one and renames it into place one at a time, src/ in walk order and hb last, so an hb that loads sources while the walk runs opens some new files and some old ones and compiles a tree that matches neither release. Seen on spark 2026-11-13: during the 22:03:40 install of 632e1946, 7 Tender corpus forms whose hb started in the 4-second window failed with E-UNDEFINED BUF:RESIZE at src/lib/net/model.f:818; forms before and after loaded. Acceptance: an hb that starts before or during an install, and loads while it runs, loads either the old release whole or the new one whole, installs still never wait for clients, and a warm client build after an install still equals a cold one. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-instdot.
