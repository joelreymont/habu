---
title: Fix the stale text the comment sweep left
status: open
priority: 3
issue-type: task
created-at: "2026-11-09T17:59:10.999511+02:00"
---

Problem: comments and docs still name the deleted builders, the retired Gforth route or capture-era machinery that the arm64 build no longer has; a sweep for the deleted builders' names (pattern .out/a2a/jerry-dead-re.txt) missed these, and ee225fc4 fixed the step-5 review's sites. Sites still present on master: src/habu/habu2.f:1171 'the compiler capture window' and :2746 'Gforth stage0 mirror'; src/habu/layout.f:584 'hb-stdin engine'; src/compiler/native/checker-owner.f:85 (build-fixpoint's stage-built hb-host); 'metabuild host' in src/habu/aot-file.f:2 and :933, src/habu/aot-decl.f:3 and src/habu/aot-capture.f:1540; SND-ZERO-LIVE in src/habu/created-body-abi.f:18, 24, 28 (no arm64 successor zeroes the comma-run cells; the linker saves and restores hb's own COMMA cells, src/habu/linker.f SAVE/RESTORE; inferred, untested); src/habu/habu1.f:5599 'the ahead-of-time closure walker' and :3152, :3235 'a stripped image'; src/lib/error.f:844 E-NSTR-CAP's 'a stripped link' and its src/habu/intern.f WINDOW-CLOSE, which nothing calls (habu-delete-the-dead-c212d5a2); src/habu/AGENTS.md:13 describes aot-*.f and snap*.f as capture and saved images without saying only the x86-64 and Wasm paths use them; tools/native-emit-x64.f:4 says the deleted tools/native-build-core.f loads it; src/habu/address-cells.f:82 and :227 ('Native-build'; whether LIVE-N-CELL :85 and KEEP-BELOW :229 still have a caller decides whether the code goes too); tools/native-layout.f:66's 'native-build:' message; src/lib/error.f:9 gives src/core/util.f's ERROR-TEXT as the reason error.f does not require util.f, which may no longer hold; src/habu/driver-io.f:24 calls FDIO:WALL without requiring src/habu/fdio.f, relying on the load order its header (:3) states; src/lib/fs-mutate.f:12 requires src/lib/fd.f, which requires src/lib/task.f (:17), so the reason src/habu/layout.f gives for the FS-ABI and FS-MUT-ABI bands (:1510-1583) may be gone.
Acceptance: each site states what the code does now (docs/forth-card.md section 9) or goes; driver-io.f requires what it uses.
Files: those named.
Verify: read the diff; rg the quoted phrases over src tools docs; hb rebuilds itself byte for byte.
Depends: none (lands after arm64). Ownership: those files. Claim: unassigned.
