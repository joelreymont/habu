---
title: Add completion operations to AIO
status: open
priority: 2
issue-type: task
created-at: "2026-09-21T18:07:21.885075+03:00"
blocks:
  - habu-add-the-io-0c5a9630
---

Problem: the readiness loop answers readiness only; regular files have no readiness, and every transfer still copies through a blocking read(2)/write(2) after the wait. Design: READ and WRITE ( fd ptr u8 len n -- AIO:ticket ) (offset -1 = current position), ACCEPT ( fd -- AIO:ticket ) and CONNECT ( fd ptr u8 n -- AIO:ticket ) over IORING_OP_READ/WRITE/ACCEPT/CONNECT (5.6), with the buffer owned by the ticket until its outcome is taken: the caller hands over a MEM allocation (the unique bounded borrow of habu-add-unique-bounded-527e05ca when landed, else a MEM-owned span the ticket holds and AWAIT returns), so no checked code can read, write or free it while the kernel writes it; the ready arm carries the byte count. Acceptance: lib/aio-test.f reads a file through READ, runs a loopback stream through ACCEPT/CONNECT/READ/WRITE, and pins that a span cannot be reused before its outcome (by type, a rejected program); docs/aio.md. Files: lib/aio.f, lib/aio-test.f, docs/aio.md. Verify: lib/aio-test.f, test/run.f. Depends: habu-add-the-io-0c5a9630. Ownership: as the loop dot. Claim: unassigned.
