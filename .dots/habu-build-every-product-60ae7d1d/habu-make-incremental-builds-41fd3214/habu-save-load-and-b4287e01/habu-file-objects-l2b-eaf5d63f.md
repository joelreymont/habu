---
title: "File objects L2b: cut pieces and write RECORDS"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T13:06:11.964733+02:00"
blocks:
  - habu-file-objects-l2-f473e4b0
---

Problem: an object must say what each part of a file's compile added, in load order (design .out/carl-file/design-fobj.md section 2 items 1, 3, 4; section 3 PIECES). Acceptance: the runner marks each piece's start and end (ndict@, the shadow's counts, DP, the address-cell count, the wordlist counter) at the file's start, at each nested load the file makes and at its end; the object's PIECES section holds, per piece, RECORDS (kind definition, package, private wordlist, alias or does; name; flags; wordlist by role (the build's global, WL:PRIMITIVES, the package marker) or by (package record, public or private); entry as an emission of the piece or the alias's record by (file, number)) and REQUEST, the nested load's number in the file (none after the last); the header's PIECES offset and length are set; no address appears. Files: src/habu/linker.f (the object section), test/file-objects.f, the fixture. Verify: test/file-objects.f (two cold builds in two fresh hb processes write byte-identical objects; the program still prints Hi and ok), test/linker.f, test/compiler/arm64-link-run.f. Depends: L2a. Ownership: carl (lead), worker lane. Claim: unassigned.
