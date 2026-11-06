---
title: "File objects L3: stores into earlier cells"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-06T12:21:51.516404+02:00"
closed-at: "2026-11-06T23:38:41.431881+02:00"
close-reason: "landed with this commit: each file object carries STORES (earlier-cell stores by file and offset or header, kind, value); a file changing an earlier record or code, or storing into a cell no file holds, ends the build LINKER:E-STORE; a non-aligned new address cell below FILE-DP E-COMP-STATE; a throw from KEY! through the object's write ends the build E-FAILED; the program's entry comes from main.f's object (APP-ENTRY:XT-CELL), build.f's STORE-ENTRY gone; STRS and COPY-CELLS released at close. Reviewed (code-reviewer twice: F1-F4, F6, N1, N2 folded). Proof at the rebased tip: bin/hb --load test/run.f -- linker arm64-link-data arm64-link-run file-objects rc 0, real 5.60 s, user 12.11 s. The compare's cost on a real build waits for one through the linker: dot habu-cut-the-store-e641b7a7 (depends L2e)."
---

Problem: a plain ! from a file's top level into an earlier file's cell leaves no trace (habu2.f:7938-7961 rows only xt! and ptr-cell! stores), so an object without it links a different program. Acceptance: the runner keeps a copy of the space below each piece at its start and compares at its end (address-cell header and rows excepted), plus the earlier records' fields and emissions' patch slots; each difference becomes a STORES row of design section 3. Measure the compare's share of a cold build on spark; above a tenth, stop and report: the page write-protect alternative goes to design before L4. Design: .out/carl-file/design-fobj.md section 2 item 4. Files: the L2 object writer, test/file-objects.f. Verify: the fixture's top-level store appears as a STORES row; two fresh-process cold builds still write byte-identical objects; wall and CPU of a cold build before and after on spark. Depends: L2b, L2d. Ownership: carl (lead), worker lane. Claim: agent=carl workspace=.jj-ws.noindex/carl-l3 (census done; tests commit nrupwtxv with the MARKED and STOP-GAP cases, briefs .out/brief-l3-tests.md and brief-l3-fold.md; implementation lane next).
