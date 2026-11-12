---
title: "Wasm 6c: address values are read and written through the emission's machine"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T22:11:12.503899+02:00"
closed-at: "2026-11-13T00:29:17.882907+02:00"
close-reason: "the linker reads address values by the emission's machine"
---

Lane s2a (workspace jerry-s2a, on the s1+s2b tip). LINKER reads and writes an emission's address values through its machine's part: ARM64:ADDRESS@ and ADDRESS! over ADDRESS-CARRIER (src/arch/arm64/link.f), WASM:ADDRESS@ and ADDRESS! over WLEB's padded s64 (src/arch/wasm/link-site.f), chosen by NSHADOW:ARCH@ in link-band.f. SITE-VALUE, OWN-FUNCTION, EM-BELOW, EMISSION-CODE+, ADDR-CK and NEMIT-FILL go through them. Acceptance: an arm64 program and engine are byte-equal before and after; a Wasm module is byte-equal; linker.f names no ADDRESS-CARRIER word; EM-BELOW reads a Wasm emission's data addresses as Wasm fields (a cross build's cell refusal names the right routine).
