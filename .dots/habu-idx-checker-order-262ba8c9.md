---
title: Index checker order keys once at handover
status: active
priority: 3
issue-type: task
created-at: "2026-10-22T14:59:32.126208+03:00"
---

Problem: the checker handover (tools/native-build-core.f ~:535, CHECKER-REG:ORDER-* words) recomputes the order key per record: 0.35 s of every warm engine build, 214 exclusive samples. Evidence: `hb-build --engine` builds timed with `/usr/bin/time -l`. Acceptance: order keys computed once per handover and looked up per record; handover time measured before/after; behaviour unchanged. Files: tools/native-build-core.f and the CHECKER-REG order words it calls. Verify: warm engine build timing, test/native-file-auto-e2e.f. Depends: none. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-order brief=carl-dots/.out/carl-file/brief-order.md (on moxyulxx; lands after lane 11).

Measured 2026-10-31 at lane 11 (uyzslxrr), no-change warm engine build 46.0 s wall, dot habu-profile-a-warm-9f10659a, report carl-dots/.out/carl-file/s2prof-report.md: still exists, ~0.30 s of a 0.38 s handover; ORDER-* 298 exclusive samples (ORDER-KEY 113, ORDER-BASE 65, ORDER-FIELD 59, ORDER-SWAP 35, ORDER-MAP 26). Words in src/core/checker.f (package CHECKER-REG): ORDER-BASE/KEY/FIELD 26978-26985, ORDER-SWAP 27018-27023, ORDER-SORT 27027-27033 (insertion sort swapping 6 fields per step), TRANSFER-ROWS 27042-27055, ORDER-MAP 27059-27067 (linear scan per handle, called 27090, 27094), HANDOVER 27160, called from tools/native-build-core.f:580.

First worker stopped before editing: the target sees the source only through the owner ABI's per-symbol SOURCE-ROW (a store walk needs a new ABI callback) and checker.f loads in the kernel prefix before src/lib/sort.f. Lead decision: one heapsort in checker.f's order section for the rows and the ORDER-MAP index (other kernel files carry their own sorts; neither an ABI nor a load-order change is worth 0.3 s). Fresh worker on the revised brief-order.md in carl-order.

txvozqyr (e3c3310a) "Heapsort the checker handover order" on moxyulxx, carl-order: heapsort over (key, tiebreak) pairs in ORDER-P, two sorted (handle, row) index runs searched by ORDER-MAP. Second warm profile run: ORDER-* exclusive 305 -> 13 samples, wall 49 s both; engine build, four profile runs, two-generation (gen 5 == gen 4), types and frame e2e rc 0 on /tmp/carl-order/eng/hb. Opus review running. Lands after lane 11 (rebase onto it).

Opus review of txvozqyr: REVISE for style only. Claims 1-6 confirmed (heapsort bounds, stable tie order, ORDER-MAP lower bound, phases, arena sizing). Lead fold-in in the carl-order working copy: comments cut to card section 9, entry locals on their own line, two blank lines between multiline definitions, TRANSFER-LINK locals line. Unbuilt; the proof reruns after the rebase onto landed lane 11.
