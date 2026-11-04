---
title: "Census what a file's load changes below its marks"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-03T13:38:54.348854+02:00"
closed-at: "2026-11-03T14:07:17.213172+02:00"
close-reason: "measured: .out/carl-file/census-m2/report.md; 2240 owners, 964 segments, diff cost 33 s of a 156 s cold build"
---

Census A for the linker design (design-linker.md sections 3.4, 10 R2): over one cold engine build, per segment, every changed DATA cell and record row below the segment's opening marks, with writer file and owner; counter candidates (owners written by two or more files); mapped owners appended to (M1); copy-and-compare cost (M3). Brief .out/carl-file/brief-census-m2.md; report /tmp/carl-m2/report.md. Nothing lands. Depends: none. Ownership: carl. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-m2.

Result 2026-11-03 (worker, report copied to .out/carl-file/census-m2/report.md with its tsv files and the instrument diff): cold engine build with -o, baseline rc 0 124 s, census rc 0 156 s. M3: DATA here 45,873,072 bytes, ndict 29,111 (records 1,397,328 bytes), 964 segments, copy-and-compare 32.96 s total (largest habu1.f#10 124.7 ms). M2: 2240 owners changed below a segment mark (63 data-claims claims, 1189 host-era records, 987 records), 1790 written by two or more files; top: claims TKA-CELL, BODY-SOURCE, BODYBUF, VVAL-STACK, HIDX-CLAIMS, TOKCP-CELL (205 files each; HIDX-CLAIMS rises once per file). M1: 852 changed owners are mapped-storage heads or counts (HIDX-EFF-BASE, NRX-BASE, USX-BASE 195 files; CHX-BASE, UIX-BASE). Records: 3748 earlier-row changes, 3656 by src/core/internal-mark.f setting byte 16 top bit, 92 at byte 40 (wordlist cell; xref.f 0 to -2, 41). Limits: owner names matched by name only; dropped records charged to the nearest owner below; long spans record only the first cell. Brief defect: hb-build --engine needs -o.
