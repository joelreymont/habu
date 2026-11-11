---
title: End a browser instance when a turn throws
status: active
priority: 2
issue-type: task
created-at: "2026-11-07T00:10:18.975564+02:00"
---

Problem (found by tim from Maki's viewer, 2026-11-07, /tmp/tim-habu-h2.md; read from master, not run): src/lib/browser/turn.js run(event) :338-356 sets this.ended and rethrows on a trap (:343-346), but a run answering a nonzero status throws at :348 before this.records() :350, so every AFTER, RPC, DRAW and TREE record that run wrote is dropped, this.ended stays unset and the instance takes the next event; event.f:55-59 HANDLE rethrows a word's throw, so any throw in a turn reaches :348. An rpc the turn issued is never sent and its answer cell waits for ever. Acceptance: a run answering a nonzero status ends the instance as a trap does: this.ended set with the throw code, the throw passed on, no later event run. Files: src/lib/browser/turn.js, its host-cli test. Verify: under host-cli, a module whose turn throws after issuing an rpc reports the throw, and the next event is refused with the ended text (no rpc record emitted, no second run). Depends: none. Ownership: carl (lead) or tim; not on the incremental-builds goal. Brief .out/briefs/turn-throw-ends-instance.md. Claim: agent=carl workspace=.jj-ws.noindex/carl-turn
