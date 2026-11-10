---
title: Store FFI table bases as DATA pointer rows
status: closed
priority: 2
issue-type: task
created-at: "2026-11-06T00:13:13.471304+02:00"
closed-at: "2026-11-10T21:50:20.691254+02:00"
close-reason: "obsolete: src/habu/aot-link-arm.f (EMIT-FFI-BASES, OWNED-BASES) is gone; the file-objects kept case runs zip through FFI in a linked program"
---

Problem: the old stripped arm64 entry writes each FFI table base at start-up from a list the link walks (src/habu/aot-link-arm.f EMIT-FFI-BASES :193-210, FFI:OWNED-BASES), because the captured image held hb's own pointers. A product linked from a build's space copies DATA raw and slides only listed code cells (src/habu/link-data.f), so a space that loads FFI would carry its table bases as build-time DATA addresses with no row. Acceptance: FFI's table bases are stored with ptr-cell! where FFI stores them, so each is a DATA pointer row that COPY-ROW moves to DATA-VA; no link-time list of FFI bases; a stripped arm64 app built in a space that calls a foreign function runs. Files: the FFI library's base stores (src/lib/ffi*.f), src/habu/link-data.f only if a row kind is missing. Verify: a stripped-app test through LINKER:WITH and LINKER:WRITE that binds a foreign function. Depends: habu-build-stripped-apps-805dee2b (a space that loads FFI). Ownership: unassigned. Claim: unassigned.
