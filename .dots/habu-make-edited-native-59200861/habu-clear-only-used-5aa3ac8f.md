---
title: Clear only used effect graph scratch
status: closed
priority: 2
issue-type: task
created-at: "2026-10-21T11:53:18.957384+03:00"
closed-at: "2026-10-21T12:33:10.778802+03:00"
close-reason: Clear four structural spans and initialize lexical heads only at accepted node starts; discard obsolete scratch copying. Independent Astra review clear. Matching converged engine passes 486/486 native suites, five byte-identical native/map generations, early edited/cold executable+map equality, graph/FILE/type/cold-prefix cases. Frozen Maki cold/warm/fresh images match; warm 3.27–3.28s wall, 2.80s user with 8715 hits/369 compiled versus prior 3.75s/3.28s user observation; separate bracketed candidate comparisons showed 5–13 percent CPU reduction with load variation.
---

Full warm Maki d5c2 profile on qualified a69b3901:8715 hits/369 compiled,3.75wall/3.28user baseline; ASIG-GRAPH-ZERO is13.4% of whole-load/capture/write samples, CK-GRAPH-ROOM16.1% inclusive in full-process report. CK-GRAPH-ROOM clears36 spans per wire byte:4 structural metadata and32 lexical contexts, though lexical heads are used only at accepted node starts. Clear structural spans once and initialize lexical heads at first node acceptance; discard old scratch contents rather than copy on growth. Preserve all malformed graph, lexical scope/domain, shared-node and cached-effect behavior; no epochs, new format or skipped validation. Inspect existing graph/FILE/type E2Es before edits, measure same warm caller wall/CPU, require cold/warm/fresh bytes and matching native/convergence plus independent review for checker changes.
