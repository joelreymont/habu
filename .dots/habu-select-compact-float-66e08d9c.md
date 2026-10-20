---
title: Select compact float literals
status: open
priority: 2
issue-type: task
created-at: "2026-10-14T17:03:00.446268+03:00"
---

Otto’s zzusage report measures65float literal sites696bytes but30distinct values, plus91d<-x and16x<-d transfers. Inspect real AArch64 floating-immediate/literal-pool and value-lowering opportunities; retain exact binary64 bits, NaN/zero/infinity behavior and stack ABI. No compression. Acceptance: meaningful existing or prewritten floating E2Es, same-source executable/code/size measurement, affected native gate/convergence and Astra review.

Actual21b4/573305bb native literal bodies for1.0,31.0,-0.0,0.3 are16/16/20/32 bytes. Eligible1.0 and31.0 start with FMOV immediates1e6e1000/1e67f000; negative zero/general values retain exact bit materialization. Existing native-chain tier1/inline-primitive suites pass. This verifies the implemented shapes; a broader same-source zzusage application reduction and final combined qualification remain unestablished.
