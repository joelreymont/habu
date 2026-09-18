---
title: Declare the lib records with storage questions
status: open
priority: 3
issue-type: task
created-at: "2026-09-18T10:24:05.105523+03:00"
---

Problem: three lib records need a storage decision before they can be declared: the xmodem session (lib/serial-xmodem.f:29-45, pointer fields at cells 12 and 15, a nested BUF at cells 8-10), the ZIP node and member (lib/zip-state.f, lib/zip-raw.f, mmap-allocated per node: an individually allocated node has no sound ptr a to ptr F crossing, so it becomes an element of a DYNAMIC-BUFFER arena or gets one audited typed allocation crossing for its family), and pq (lib/db/pq.f:92-112, struct-of-arrays plus the parameter arena's own mixed extent at :381). Acceptance per record: the storage decision recorded in the file header and here, the record declared, accessors generated, no cast left, its suite unchanged in what it asserts. Files: as listed. Verify: their suites; test/run.f. Depends: habu-declare-the-lib-b0170dff. Ownership: lib. Parent: habu-campaign-c2-mem-c3d7662b. Claim: unassigned.
