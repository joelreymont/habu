---
title: Seed the stdlib prefix
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T18:56:46.225140+02:00"
---

Stage A of the seed closure (independent; land FIRST and ALONE): add lib/{prelude,errors,string,memory,vector,adt/option,cad-num-types,cad-num-arithmetic}.f + src/arch/arm64/asm.f to the boot prefix (PFX-LOAD-*/PFX-PATH-*/PFX-PROVIDE-FILES) at the one forced slot: after PFX-LOAD-CORE-FILES (lower-cert-seal.f), before PFX-LOAD-SCRIPT-ARGV. BLAST RADIUS measured: ~600 tree files' requires become no-ops (string.f 548, errors.f 471...) and every consumer silently gets the PRE-SEAL prefix copy - trust status changes inside the friend seal, internal-mark reclassifies, the seal ndict watermark moves 5782 -> ~7xxx. Acceptance: full gate green; byte fixpoint; new boot ndict + watermark recorded as baseline; spot-check a string.f consumer loads zero copies; IBUFSZ +285KB verified fine. Files: src/habu/habu2.f prefix tables. Depends: none.
