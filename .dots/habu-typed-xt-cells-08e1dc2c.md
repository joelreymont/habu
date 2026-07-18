---
title: "Typed xt cells: xt<effect> as a storable cell type"
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T21:26:02.475437+02:00"
---

Step 1 of the laundered-execute closure sequenced in habu-checker-exec-of-5923c543. Make xt<effect> an admissible cell type so a variable or buffer can be declared to hold an xt with a known stack effect: extend CHECKER-STORAGE-INFO (src/core/checker.f, which today explicitly rejects quotation cell types) and the TYPED-VARIABLE/TYPED-BUFFER declaration path; the cell type is persistent and monomorphic, a fetch recovers xt<E>, and execute of the fetched value fit-checks E exactly as the direct-tick path does (test/xt-effect-test.f v1-v9 stay green). This removes the erasure point proven in the 5923c543 RCA: today a store into a raw cell freshens the address schema per occurrence, so the bound effect never survives from the store to the fetch. Deliverables: the capability, positive fixtures (store tick, fetch, execute, fit-check), negative fixtures (effect mismatch at execute; storing a non-xt), and doc rows in docs/effects.md. This dot blocks the RSEXEC T-VAR flip (step 3) and the hook migration (step 2) tracked in habu-checker-exec-of-5923c543.
