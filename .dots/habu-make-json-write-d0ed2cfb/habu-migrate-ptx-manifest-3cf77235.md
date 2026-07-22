---
title: Migrate PTX manifest JSON writer
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T16:07:53.370522+02:00"
blocks:
  - habu-build-explicit-json-399f5929
---

Why: KMAN:MANIFEST$ depends on the deleted JSON-WRITE singleton and returns a borrowed global buffer. Exact interface: KMAN becomes a pure renderer that consumes and returns a caller-supplied JSON-WRITE:writer; the caller supplies a bounded prefix-copy buffer used to compute manifest_content_hash before the final field. The renderer MATCHes JSON-WRITE:COPY: on required it returns a KMAN render-result preserving the writer and exact len without hashing or publishing; on copied it hashes exactly the canonical prefix bytes, appends manifest_content_hash, and returns ready(writer). No package-global writer/output buffer and no raw MANIFEST$ remain. The kernel-export caller owns writer state, scratch, prefix buffer, final output, and closes exactly once. Acceptance: canonical bytes and both hashes stay byte-identical; two manifests render interleaved; too-small prefix/final buffers leave their destinations unchanged and return exact requirements; kernel-manifest and kernel-export suites pass. Smallest check: bin/hb --load lib/ptx/kernel-manifest-test.f plus the kernel-export test. Depends: Build explicit JSON writer core. Ownership: lib/ptx/kernel-manifest.f, lib/ptx/kernel-manifest-test.f, tools/ptx/kernel-export-lib.f and its focused test, FILEMAP.md. Claim: unassigned.
