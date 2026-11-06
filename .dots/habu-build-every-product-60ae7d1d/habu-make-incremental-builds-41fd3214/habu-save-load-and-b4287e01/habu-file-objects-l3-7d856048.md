---
title: "File objects L3: stores into earlier cells"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T12:21:51.516404+02:00"
blocks:
  - habu-file-objects-l2d-e39a1778
---

Problem: a plain ! from a file's top level into an earlier file's cell leaves no trace (habu2.f:7938-7961 rows only xt! and ptr-cell! stores), so an object without it links a different program. Acceptance: the runner keeps a copy of the space below each piece at its start and compares at its end (address-cell header and rows excepted), plus the earlier records' fields and emissions' patch slots; each difference becomes a STORES row of design section 3. Measure the compare's share of a cold build on spark; above a tenth, stop and report: the page write-protect alternative goes to design before L4. Design: .out/carl-file/design-fobj.md section 2 item 4. Files: the L2 object writer, test/file-objects.f. Verify: the fixture's top-level store appears as a STORES row; two fresh-process cold builds still write byte-identical objects; wall and CPU of a cold build before and after on spark. Depends: L2b, L2d. Ownership: carl (lead), worker lane. Claim: unassigned.

From the L2a re-review: when LOAD-CURRENT's INCLUDE-EVALERR? check fails the process dies with "include: evaluation failed" after the runner has written that file's object. Establish whether a linker build reaches that path; if it does, no object may survive it once objects are reused.
