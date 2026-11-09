---
title: "Stage, sign and pair the program in LINKER:WRITE"
status: open
priority: 3
issue-type: task
created-at: "2026-11-09T18:37:14.543623+02:00"
---

Problem: .out/a2a/step5-design.md 'Output' says LINKER:WRITE stages, signs and writes the map, and HBB-INSTALL-*, HBB-PAIR-*, HBB-MAP-* and CODESIGN:ENSURE go. The arm64 landing instead writes into a private TMPDIR directory, signs for the team, and installs through about 190 lines of temp-dir, install and map-pair code in tools/hb-build-lib.f (HBB-PREPARE-TMP, HBB-GOT-NAME$, HBB-INSTALL-*, HBB-PAIR-*, HBB-MAP-*). It works (a refused build keeps -o and removes its directory). HBB-GOT-NAME$ keeps three names though each build has its own directory; dot habu-retire-the-whitebox-2467e4f2's acceptance wanted its repl arms gone, and that item moved here. Found by the step-5 build-path review (2026-11-09). Acceptance: LINKER:WRITE signs its staged file for the team and replaces the program and its .map together; those hb-build words go; -o is still replaced only by a whole, signed program with its map. Files: src/habu/linker.f, tools/hb-build-lib.f. Verify: hb-build --engine gen 2 equals gen 3; a refused build leaves -o and its .map untouched. Depends: none (lands after arm64). Ownership: those files. Claim: unassigned.
