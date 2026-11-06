---
title: Design one Habu way to name an error code
status: active
priority: 2
issue-type: task
created-at: "2026-11-06T12:46:54.357514+02:00"
---

Problem: a log line, a saved failure fact and Habu's own 'hb: uncaught throw code 7136' (src/core/checker.f) carry only an error number. MODEL keeps a private table of its own codes' names (src/lib/net/model.f CODE$ :2198-2210); nothing else names a code, so Tender would need its own table of every Tender and Habu code it meets (kevin, Tender lead, 2026-11-06, /tmp/kevin-carl-log.md item 2). Asked: one Habu way to turn a number into its constant's name and comment, covering Habu's codes and an application's own. A stripped program has no dictionary (DECISIONS 2026-11-05 'where a built program's code runs'), so a lookup that scans names does not reach it. Joel 2026-11-06, asked when this should be designed: chose 'Design now, in parallel'. Acceptance: a design (Fable on Plan) with its public items, the path that reaches each, and the cost across every error constant; then one question per new public item to Joel; lanes after his approval. Depends: none. Ownership: carl (lead). Claim: agent=carl, Fable design running (Plan, read-only, root on master).
