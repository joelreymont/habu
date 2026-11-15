---
title: Name a missed publication in lsp tests
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T17:42:57.364707+01:00"
---

Problem: a language-server case whose server never publishes a packet the case expects waits in HEAR (tools/lsp-test-lib.f:1516) until the conversation's CHECKED-MS deadline (:745, set :1566), and HELD-BY (:1572-1585) reports the timeout with the case's label and its streams but not the packet the turn waited for: definition-engine-open, waiting at ENGINE-OPEN-TURNS's `PRIM-PATH 1 EXPECT PUBLISHES` (:3156) for a prim-nav.f republication a linked hb never sends (dot habu-report-a-primitive-dc40eac9), ends with E-PROC-TIMEOUT after 64 s, and it took a root-cause hunt to find which wait that was. Acceptance: a case whose expected publication never comes ends as a timeout whose report names the case and the publication it waited for; the run still ends as a timeout, since a deadline is no verdict on the server (src/lib/test/ending.f:2, src/lib/test/wait.f:1-3). Depends: none. Claim: unassigned.
