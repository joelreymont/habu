---
title: Re-measure GB-SIZE-BASELINE-LINUX after TFAM 12 slice 3b
status: open
priority: 2
issue-type: task
created-at: "2026-07-06T22:21:45.455470+02:00"
---

test/gate-build-size.f GB-SIZE-BASELINE-LINUX is still 90304 (pre-TFAM-12-slice-3b), while GB-SIZE-BASELINE-MACOS was bumped 115831 -> 132343 for the slice-3b pass-2 width-aware engine growth (this macOS host measures 132343). The Linux (ELF) candidate cannot be measured on this macOS lane, so the Linux baseline row is stale: on a Linux gate lane GB-SIZE-ENFORCE will fail 'candidate size ratchet: grew past baseline' (~+14% engine growth) until a Linux run re-measures the ELF candidate and commits the new GB-SIZE-BASELINE-LINUX value (baselines are per-target because Mach-O and ELF candidates differ in size). Action: on a Linux/aarch64 lane, build the fixpoint candidate, read FILE-SIZE, and set the GB-SIZE-BASELINE-LINUX constant to the measured ELF size in test/gate-build-size.f (mirror the macOS 132343 bump comment with the Linux measurement + date). Acceptance: the native gate build-size slice is green on Linux with the committed measured baseline.
