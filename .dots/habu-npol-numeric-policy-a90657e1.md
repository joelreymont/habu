---
title: "NPOL: numeric-policy-id constructor + wire codec"
status: open
priority: 2
issue-type: task
created-at: "2026-07-17T15:33:59.940020+02:00"
---

Per-family leg of plan 23.9 foreign-id contract (676d5a7b): reopen package NPOL (existing DOM>N/N>DOM is the minimal precedent), resolve the ENGINEERING decision recorded there (policy descriptor scope: single dom vs bundle - decide inside this dot from how maki numeric policy is actually consumed today), then publish the audited constructor + RAW refinements + NPOL:ID>WIRE / WIRE>ID pair with round-trip + reject tests. Files: the NPOL owner file, focused test. Ownership: V2 artifact id codecs.
