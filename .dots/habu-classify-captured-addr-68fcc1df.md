---
title: "Classify captured address rows by the host's layout, not the source's"
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-12T13:26:05.513162+03:00\""
---

Problem: tools/native-build.f RESET-ADDRESS-ROWS keeps every address row below the SOURCE constant DATA-START as an engine declaration and discards the rest as the retired heap; a tree whose reserved bands grew (habu-size-the-snapshot-1ca5db10 doubles XTCELL-CAP, moving XTCELL-END, STK-OFF and DATA-START up by 256 KiB) makes the host keep its own heap rows that now fall below the new DATA-START, and the capture refuses them: 'aot-capture: declared address target outside its capture window' (measured 2026-09-12 with the seed hb-stdin and with the product engine, both on the rebased lane). The tier stack's move of DATA-START through TIER-PROV:SPANS builds, so the failure depends on which rows the growth uncovers. Acceptance: the build classifies rows by the host's actual heap start (a layout self-description the engine records at boot in an existing spare fixed cell, read by the capture; the source constant only as the fallback for hosts that predate it), stated in docs/bootstrap.md as the rule for landing a reserved-layout change; a regression that builds a tree whose DATA-START moved from a host that did not, through tools/native-build.f, and reaches two-generation gen 5 = gen 4; then the cold-build seed is refreshed from the product and its path documented in docs/maintainer-handoff.md. Files: tools/native-build.f, src/habu/aot-capture.f, src/habu/layout.f, src/habu/habu2.f, docs/bootstrap.md, docs/maintainer-handoff.md. Verify: the regression, the chain, test/run.f. Depends: none. Ownership: hazel. Claim: agent=hazel-worker workspace=.jj-ws/habu-size-the-snapshot-1ca5db10.
