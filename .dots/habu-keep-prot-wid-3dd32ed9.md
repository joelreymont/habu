---
title: Keep prot-wid-add out of programs
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T20:32:22.663154+02:00"
---

Found by lane sealed (workspace .jj-ws.noindex/jerry-sealed, 2026-10-31). A program on the product engine can call the engine-build word prot-wid-add after the seal: `PROT-WID-MAX prot-wid-add` exits 84 with 'hb: protected-WID id above the bound' (habu1.f BPROTWIDADD; test/seal.f SLV-ASSERT-PWID-OOR pins it). Exit 84 is ENGINE-ERROR:SEAL-PACKAGE, which tools/check.f now reports as E-SEALED-PACKAGE, so this exit is misnamed. Its other callers run only at build time (internal-mark.f SEAL-PACKAGES, native-unit-object.f REC-PROTECT); SUMTYPE/ENUM cannot reach it (PLAN-PREFLIGHT/PREPARE throw 7169 first). Decide: strip the word from the product image (privacy rule: private section + build strip), or give its out-of-range failure its own engine error.
