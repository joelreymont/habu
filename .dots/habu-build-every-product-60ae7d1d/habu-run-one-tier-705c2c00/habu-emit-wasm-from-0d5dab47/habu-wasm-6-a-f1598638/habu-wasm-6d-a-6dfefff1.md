---
title: "Wasm 6d: a call site's and a does> slot's form is tested by the emission's machine"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T22:21:35.342816+02:00"
closed-at: "2026-11-13T00:29:17.911514+02:00"
close-reason: "the linker tests a site's form by the emission's machine"
---

Lane s3a (workspace jerry-s3a, on the s2a commit). Each machine's part of LINKER answers whether emission bytes hold its unplaced call at a site (CALL?), its unpatched does> slot (SLOT?) and a whole emission size (SIZE?): arm64 in src/arch/arm64/link.f, Wasm in src/arch/wasm/link-site.f, chosen by machine in link-band.f. The object reader's CALL-CK, SLOT-CK and EMISSION-CK go through them. Acceptance: linker.f names no A64ASM word and no LE:U32 word; arm64 programs and the engine are byte-equal before and after; test/file-objects.f's crafted cases still refuse a moved call site and a moved slot.
