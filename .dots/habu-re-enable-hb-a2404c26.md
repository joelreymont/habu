---
title: "Re-enable hb's name stripping"
status: active
priority: 2
issue-type: task
created-at: "2026-11-07T07:59:56.932704+02:00"
---

Problem: Joel 2026-11-07: 'disable name stripping if it's slowing you down and make a dot to re-enable it later'. DECISIONS.md 'privacy' (:517-529) settles the goal: a word is restricted by putting it in a package's private section, the product build strips private names, and this dot tracks re-enabling that for hb. The mechanism the stripping used is gone: tools/native-build-core.f went in 1d97c3f2 and src/core/internal-mark.f no longer holds IMK-SEAL. hb is now linked by hb-build through the linker: src/habu/link-data.f WRITE-PROGRAM (:491-506) takes LINK-ENGINE (:471) for an engine, whose records are all baked into the program with their names (src/habu/link-map.f:219), while LINK-STRIPPED (:420) is the stripped program's path. src/habu/aot-capture.f ACAP-NAMED? (:746) is the x86-64/Wasm capture path's filter. The stripping was dropped because the old seal also dropped public words with no effect row that the linker, a tool loaded into hb from source, calls. docs/architecture.md:90 states that the build strips private words from the delivered hb, which is not true of hb now. Which tests the disabling turned off is not recorded here.
Acceptance: the hb that hb-build --engine writes answers E-UNDEFINED for a private name and keeps every public name, so the linker and other source-loaded tools still run on it; every test the disabling turned off runs again.
Files: src/habu/link-data.f, src/habu/link-map.f, the disabled tests.
Verify: first name the tests the disabling turned off (rg test/ for a skip or unsealed-build note); then hb-build --engine, a private and a public name looked up on the result, those tests, and the full suite.
Depends: none (the privacy decision is settled; habu-move-engine-internal-fc2ba551 relies on this strip). Ownership: jerry (told carl 2026-11-11). Claim: agent=jerry; no workspace yet. The census is carl's running read-only trace (brief .jj-ws.noindex/carl-layout/.out/briefs/trace-private-names.md, report to ~/.cache/tmp/carl-priv/report.md): what refuses a private name today, what LINK-ENGINE writes per record, who looks a private word up by name on the built hb, the tests the disabling turned off. The change is briefed from that report, and Joel's words of 2026-11-11 go into DECISIONS.md "privacy" with it.
