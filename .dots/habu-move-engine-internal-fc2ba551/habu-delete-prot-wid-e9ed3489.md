---
title: Delete prot-wid-add
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T13:45:54.860796+01:00"
closed-at: "2026-11-18T16:25:59.383734+01:00"
---

Problem: prot-wid-add (src/habu/prims.f, a global row) protects whatever wordlist id a program hands it, a raw duplicate of the package keyword `seal-package`: a program can seal another package's wordlist, and `999999 prot-wid-add` at top level stops the kernel (E-SEAL-WORDLIST) on the installed hb. Loom (~/Work/loom, which no Zen, Tender or Maki file loads) seals its own packages with it (`get-current prot-wid-add`, maki/infer/safetensors.f and lib/ptx/cuda-driver.f); `seal-package` does that, and Loom is not edited from here. It is the only writer of the protected-wid bitmap (src/habu/layout.f PROT-BITS-OFF), so once it goes only the OWNER-API wordlists are protected. Acceptance: prot-wid-add, prot-wid-room (no caller outside tests), the bitmap band and every reader's bit test (habu1.f EMIT-PROTWID, xref.f XREF-WID-PROTECTED?) are gone on arm64 and x86-64 (src/habu/kernel-x64.f is carl's), with the tests that call them; PROT-WID-MAX and PROT-BITS-BYTES, then serving only the policy bitmap, carry policy names. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-pw.
