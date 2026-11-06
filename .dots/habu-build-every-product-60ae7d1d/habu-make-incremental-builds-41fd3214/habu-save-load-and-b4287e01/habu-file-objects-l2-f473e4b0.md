---
title: "File objects L2: write each file's object"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T12:21:51.500694+02:00"
blocks:
  - habu-file-objects-l1-fbe728a5
---

Problem: a build keeps nothing of a compiled file for the next build. Acceptance: LINKER's runner marks each piece of a file (its start, each load it performs, its end: ndict@, the shadow's counts, DP, the address-cell count, the wordlist counter), turns every address in the piece's records, emissions and DATA into a reference by name (design section 2 item 3) and writes <out>/obj/<key>.hfo in the format of design section 3 (Joel 2026-11-06, DECISIONS 'the saved file object'), atomically (temporary file, then rename); tools/build-out.f moves to src/lib/build-out.f unchanged (a move) and its requirers follow. Design: .out/carl-file/design-fobj.md sections 2-3. Files: src/habu/linker.f (or a new src/habu/link-object.f it requires), src/lib/build-out.f, tools/build-out.f's requirers, test/file-objects.f. Verify: two cold builds of test/file-objects/ in two fresh processes (the boundary under test) with HABU_OUT pointed at two temp dirs write byte-identical objects; test/file-objects.f; test/linker.f. Depends: L1. Ownership: carl (lead), worker lane. Claim: unassigned.
