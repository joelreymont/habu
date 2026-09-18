---
title: Raise the per-definition body text capacity
status: open
priority: 3
issue-type: task
created-at: "2026-09-18T09:02:58.024196+03:00"
---

Problem: a definition's captured body text is bounded by BODYBUF-CAP = 8000 bytes (src/habu/layout.f, the engine's BODY-TEXT capture buffer at DATA+$800), shared by the checker's BODY-BUF (src/habu/verify-source.f E-VS-BODY-CAP) and the native compiler's TEXT-CAP (src/compiler/native/compiler.f E-NCOMP-TEXT); measured 2026-09-18 by the capacity lane: 7972 bytes of body text loads, 7973 refuses, so a definition holding nine 900-byte string literals (aspen/Tender) cannot exist; the refusal is being named by habu-name-the-per-56a594f3, but the ceiling stays. The buffer is boxed in the DATA header (RPKG-CUR at $2780 leaves 62 bytes), so growing it relocates the RPKG/CMM/DOESB/TRUSTED/PKGRESYNC/HIDX/PROT cell band: a DATA-offset ABI change that needs a bridging generation (LESSONS 'a new word in a prefix seam file needs a bridging generation' and the staged-host rule). Acceptance: the capacity chosen with a reason (a multiple that covers realistic literal tables, e.g. 64 KiB) and moved out of the header box (a dedicated region or a mapped buffer), the three layers reading one constant, the bridging landing recorded, the named refusal's regression re-pinned at the new cap, engine size before and after (docs/engine-size.md), byte fixpoint, bootstrap check, test/run.f. Files: src/habu/layout.f, src/habu/habu1.f (EMIT-BCAP), src/habu/verify-source.f, src/compiler/native/compiler.f, bootstrap/cg/*.fs mirrors, docs/. Verify: the regression at the new cap; fixpoint; tools/bootstrap.sh check; test/run.f. Depends: habu-name-the-per-56a594f3. Ownership: engine layout. Claim: unassigned.
