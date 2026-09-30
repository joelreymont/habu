---
title: "Write the maker's image bytes on an object hit"
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-30T18:26:53.919161+02:00\""
---

Problem: hb-build writes a different executable for the same program depending on cache state. A fresh stripped build (the maker child writes the image) is 33276 bytes; the same program rebuilt on an object-cache hit (the CLI process relinks the cached object text through tools/object-image.f OBJIMG:WRITE) is 33275 bytes, the size report's 'other' row 1373 against 1372. Measured by the review of change qzxmwqpo on both d40cc36d and qzxmwqpo, so not caused by it. Content keys exist so identical trees share artifacts (docs/stdlib.md); an image whose bytes depend on which cache path produced it breaks that and hides a difference between the two writers. Acceptance: the differing bytes are named with the writer that produces each; the two paths write byte-identical images for the same program, target and engine, or the difference is shown necessary and stated in docs/native-applications.md; an E2E case builds one program fresh and again on an object hit and compares the images. Files: tools/object-image.f, tools/hb-build-lib.f, lib/object-link.f, the maker (tools/aot-build.f), the hb-build test that owns the object-hit path. Verify: the new case, the hb-build rows. Depends: habu-run-hb-build-6acfec10. Ownership: the object-hit image writer. Claim: agent=kestrel workspace=.jj-ws/r4-relink.
