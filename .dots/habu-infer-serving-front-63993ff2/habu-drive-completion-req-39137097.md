---
title: Drive completion request I/O
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T01:12:14.348645+02:00"
blocks:
  - habu-submit-completion-req-de3befe4
  - habu-write-completion-resp-b1068d68
---

Why: the completed read, preflight, submit, write, and close transitions need one public state dispatcher. Result: SERVE-CONN:READABLE consumes a prepared connection and composes READ, PREFLIGHT, and SUBMIT for states that accept input, calling the owning CLOSE transition on EOF or any terminal request refusal; SERVE-CONN:WRITABLE forwards a prepared connection to WRITE for states that expose output. Each entry returns the exact underlying result and implements no clock read, deadline formula or commit, parsing, capacity calculation, scheduler operation, rendering, close, or socket logic itself. Owner: public connection-I/O dispatch, prepared-state forwarding, and terminal-result routing only. Production red: the state-specific transitions have no product entry for the poller. Acceptance: every connection state and READ result selects exactly one permitted next transition; terminal input results close once; other states refuse unchanged; focused production traces prove the composed result equals the direct transition chain and forwards prepared state unchanged. Forbidden: new state, copied logic, result application, scheduler tick, second request, retry, blocking I/O, task, callback, version, compatibility, metric, or lint. Smallest owning check: focused connection-state dispatch through the real transition tests. Claim: unassigned.
