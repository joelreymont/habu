---
title: "Write a stripped program's DATA as non-zero runs"
status: active
priority: 2
issue-type: task
created-at: "2026-11-06T02:23:26.456768+02:00"
---

Problem: LINKER:WRITE's program for LR-HI is 3,335,676 bytes, 92% of it the DATA image (src/habu/link-data.f DATA-IMAGE), of whose 3,072,680 bytes 64 are non-zero; the start-up copies it all (src/arch/arm64/boot.f COPY-DATA,) into a fresh anonymous DATA that already reads zero (census .jj-ws.noindex/carl-dots/.out/carl-file/fill-report.md). Acceptance: the image is runs [off u64][n u64][n cells] of non-zero cells (gaps of 1-2 zero cells kept), the start-up stores each run at DATA+off; arm64-link-run's programs run as before and each file is smaller than DATA-START; arm64-link-data passes. Provisional (Joel away): the approved S6 design said DATA copied raw; runs keep the bytes raw and drop zero runs (dot habu-s6-l5-provisional-5f8fd488 item 6). Files: src/habu/link-data.f, src/arch/arm64/boot.f, test/compiler/arm64-link-run.f. Verify: bin/hb --load test/run.f -- arm64-link-run arm64-link-data arm64-place linker. Depends: none. Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-runs; brief .jj-ws.noindex/carl-dots/.out/carl-file/brief-runs.md.
