---
title: "The kernel object carries the primitives' checker rows"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-11T10:34:54.925423+02:00"
closed-at: "2026-11-11T10:34:54.969210+02:00"
close-reason: "landed; engine built from the tree equals the 2a engine up to the signature with equal maps; the section decodes to 331 rows (258 global, 64 package, 7 elaborated, 2 unrowed) and ends at the file's end; consumer measure m11 rc 0, warm equal to cold"
---

Problem: privacy step 2b (.jj-ws.noindex/jerry-wasm/.out/privacy-design.md Design 1): a build takes each primitive's checker row from the host hb, never from the tree's src/habu/prims.f. Acceptance: src/arch/arm64/kernel-object.f writes a last section holding every prims.f row in table order (kind, trusted-only, fixed-ABI, stow kind, name, package name, atoms), which link-kernel.f SECTIONS already frames (CHECK-ROWS-AT, CHECK-ROWS-U); nothing reads the rows yet. Files: src/arch/arm64/kernel-object.f, link-kernel.f (comment). Verify: an engine built from the tree equals the one before up to the signature; the object's section decodes to prims.f's 331 rows and ends at the file's end. Depends: 2a. Ownership: the lead. Claim: agent=jerry workspace=.jj-ws.noindex/priv-2a.
