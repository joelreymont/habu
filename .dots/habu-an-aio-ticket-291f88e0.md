---
title: "An AIO ticket is its record's address"
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T16:00:56.324860+01:00"
---

Problem: AIO:ticket and AIO:xfer are an index nothing checks (src/lib/aio.f:36-37, :1157 AWAIT-ONE), so one made while a build runs and kept in storage is awaited in the program against another record, and nothing reports it. Design: .out/designs/design-scratch-values.md item 4 (NEWTYPE ticket ^n and xfer ^n, private casts over ^n, the record carries its own index, the first record chunk mapped like the grown ones). Acceptance: test/fixtures/file-objects/kept-aio.f, keeping a real ticket, fails its build with LINKER:E-ADDRESS naming the cell, cold and warm; the aio, http, curl and ws suites pass. Depends: none. Claim: unassigned.
