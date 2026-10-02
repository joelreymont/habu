---
title: Make the effect-store census run as documented
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T12:20:51.628715+02:00"
---

Problem (lane 298 declprobe): tools/effect-store-census.f:36 documents 'bin/hb --load tools/effect-store-census-run.f -- <file>', but on the sealed release engine that run dies rc 70 'E-UNDEFINED: E-PTR' (lead, on the round-4 top with lib/string.f as the argument; log $HOME/.cache/tmp/kestrel-r4/int6/esc/out.log). Its gate row is WHITEBOX-SUITE effect-store-census (test/gate-stdlib-cases.f:1713), and test/effect-store-census-test.f:4 documents the same bin/hb command. Acceptance: either the tool runs on the release engine through public words, or both headers name the whitebox engine and how to get one (WHITEBOX-ENGINE:PROVIDE) and that command works as written. Files: tools/effect-store-census.f, tools/effect-store-census-run.f, test/effect-store-census-test.f.
