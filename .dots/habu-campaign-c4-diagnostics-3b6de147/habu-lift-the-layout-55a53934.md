---
title: "Lift the layout buffer's name and signature caps"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T06:35:37.161492+01:00"
closed-at: "2026-11-16T04:07:35.561474+01:00"
---

Problem: layout-buffer.f:27 LBUF-C, (a 4092-byte DEFER-LAYOUT-BUFFER name), checker.f:15003 LBUF-SIG-C, (a 4088-byte quotation type) and checker.f LBUF-NM-APP (a 300-byte name under check.f) end in a bare 7121 throw (rc 67); check.f also rejects bare a 250-byte DYNAMIC-BUFFER or HEAP name and a long ARENA type that a plain load accepts. Reproducers in /tmp/jerry-lbufrec/r. Acceptance: no fixed cap; each input loads or is rejected for its own reason. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-w49lbuf.
