---
title: Name a missed publication in lsp tests
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T17:42:57.364707+01:00"
---

Problem: a language-server case whose server never publishes a packet the case expects waits in HEAR (tools/lsp-test-lib.f:1514) until the conversation's CHECKED-MS deadline (:743, set :1564), and HELD-BY (:1573-1580) reports E-PROC-TIMEOUT with no assertion, so the failure names neither the turn nor the packet it waited for: definition-engine-open, waiting at ENGINE-OPEN-TURNS's PRIM-PATH PUBLISHES (:3151) for a prim-nav.f republication a linked hb never sends (dot habu-report-a-primitive-dc40eac9), ends with E-PROC-TIMEOUT after 64 s, and it took a root-cause hunt to find which wait that was; the timeout also ends the whole lsp-test run, so a run shows only its first such case. Acceptance: a case whose expected publication never comes fails as the assertion that expected it, naming the case and the packet, within its deadline, and the cases after it still run. Depends: none. Claim: unassigned.
