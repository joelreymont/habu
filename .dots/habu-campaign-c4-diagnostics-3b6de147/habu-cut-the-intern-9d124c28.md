---
title: Cut the intern table without mapping
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T14:36:35.662427+01:00"
closed-at: "2026-11-16T04:07:36.521662+01:00"
---

Problem: src/habu/intern-table.f TABLE-CUT (:187-191) forgets bodies by REBUILD (:104-119), which maps a new table of the same size (TABLE-MAP :84). Every frame a throw unwinds rolls back through INTERN:KEEP-BELOW (src/habu/interpreter.f:3064), so once the system refuses memory, the unwind of E-MAP's throw dies 76 'intern: table mmap failed' before UNCAUGHT sends the record. Acceptance: a cut needs no memory; /tmp/jerry-w52rej/prog.f under ulimit -v 272629760 prints the placed E-MAP record and exits 67. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-w52rej.
