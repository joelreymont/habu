---
title: "Wasm 5: profile a program build, and a Wasm build of Maki's viewer"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T21:18:11.835026+02:00"
closed-at: "2026-11-10T21:55:03.338050+02:00"
close-reason: "landed in 1af0a0dd: build-profile profiles a program build; the cold Wasm build's profile is read (checker about half, tier 0 and the Wasm backend 2.4%); its findings are dots d7f6c4cb, 0e4339d0, f1598638"
---

Problem: a Wasm build of Maki's viewer takes about 4 s cold (hb-build OK, real 3.7 to 4.1) and nobody has measured where it goes; tools/build-profile.f profiles only hb's own build (HB-BUILD-CORE:ENGINE). Two dots wait on the numbers: habu-measure-the-dictionary-7a038a28 (the dictionary scan per kernel-word site) and habu-wkmap-and-kernel-7d73d82a. Acceptance: bin/hb --load tools/build-profile.f -- [--target name] [source.f] <out> <text-report> <json-report> profiles a stripped program's build (HB-BUILD-CORE:APP) when given a source and hb's build as today when not; the profile of a Wasm build of Maki's viewer is reported: the top words, the share of WASM:PROVIDER and NAME-AT, and the split between loading and checking, tier 0's generation and the link. Files: tools/build-profile.f, docs/debugging.md. Verify: the profiled module equals the unprofiled one; the old command line still writes its reports. Depends: none. Ownership: jerry. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-prof.
