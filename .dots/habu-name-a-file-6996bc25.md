---
title: "Name a file's empty last body by its own file"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T10:16:12.799771+02:00"
---

Problem (read, not run; from the F1 design, .jj-ws.noindex/jerry-wasm/.out/f1-design.md section 10): a zero-size body at the very end of a file's data has the address of the next file's first byte, and src/habu/linker.f DATA-REF+ (:1456-1475) tries the running piece first, so a reference to it can be recorded against the neighbour. If a new file is later loaded between the two, a warm build would resolve the reference to the old neighbour's start and a cold build to the new file's start. Acceptance: first a reproducer (file a.f ends with 'create Z', b.f follows and names Z; build; load a new file between them; warm against cold); then the reference names Z's own file. Files: src/habu/linker.f; test/file-objects.f. Verify: the reproducer's warm build equals its cold build before the signature. Depends: none. Ownership: unassigned. Claim: unassigned.
