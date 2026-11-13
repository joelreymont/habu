---
title: Remove words only the deleted build-cache tests called
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T17:57:09.197077+02:00"
---

Problem: wave 1 prune lane 3 (Cut the build-cache tests to what stays) deleted the only callers of FFI:MARK (src/lib/ffi-abi.f:995, was frame-e2e.f GROW), FFI:SELECTION (ffi-abi.f:1856), NCOMP:SESSION-ENSURE (src/compiler/native/compiler.f:755, was replay-e2e.f), NCOMP:PUBLISHED! (compiler.f:760, was auto-e2e.f; the only setter of the NATIVE-OBS-CELLS:PUBLISHED hook cell, so its reads at compiler.f:126, :769 and its layout entries data-claims.f:169, habu2.f:8876, :8906, tools/native-layout.f:19 are dead) and GENIO:FAULT@ (src/lib/genio-core.f:372, paired with FAULT!). Acceptance: each word with no caller in Habu or Etch is removed with its comment; the PUBLISHED hook cell goes with the per-definition cache (habu-make-a-file-dfc7e030) if its layout entries cannot go alone; GENIO:FAULT@ stays only if FAULT! is its pair by design. Files: the paths above. Verify: rg finds no caller; bin/hb --load test/run.f -- ffi-abi native-file-inputs. Depends: none. Ownership: those files. Claim: unassigned.
