---
title: Mirror namespace rows in recovery
status: active
priority: 1
issue-type: task
created-at: "2026-07-31T06:35:00.747855+02:00"
---

Source dependency: exact reviewed native E1 namespace-row contract; this stop-the-world branch keeps E1 active until M17, so exact code ancestry enforces ordering. Owner: Gforth recovery engine under the existing ENGINE-EMIT vocabulary. Mirror E1's namespace kind constants, 48-byte namespace rows, exact LNSFIND, absolute prefix creation/reopen, public/private pair allocation, shared OWNER-WID-LIMIT pair and one-WID ceilings, strict snapshot/AOT row validation, owner colon-path validation, and full-prefix storage. Keep the current first-colon qualified lookup in this leaf; recovery last-colon behavior is a separate leaf. No new package owner, parent link, side table, compatibility, schema/version, compact EXT expansion, native source change, or new lint. Write set: bootstrap/cg/forth.fs and tools/bootstrap-codegen-test.f only. Pre-M17 proof is structural source review only. M17 acceptance runs the real no-binary Gforth recovery image and proves native/recovery row and failure parity. Claim: agent=recovery_e1_impl workspace=.jj-ws/habu-mirror-namespace-rows-ff1dc330.
