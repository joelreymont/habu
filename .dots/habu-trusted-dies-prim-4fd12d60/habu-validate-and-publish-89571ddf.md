---
title: Validate and publish the combined Habu changes
status: closed
priority: 1
issue-type: task
created-at: "\"2026-09-10T18:03:13.409212+03:00\""
closed-at: "2026-09-16T14:34:48.745685+03:00"
close-reason: "superseded by habu-campaign-c1-finish-1f129a00: Final combined validation of the Habu changes is the campaign's landing step and its handoff notes name workspaces that have moved on"
blocks:
  - habu-deliver-standalone-native-a86d4699
---

Owner: Cedar; integration workspace cedar-source. Depends additionally on the existing type, quotation, cleanup, minimal-PRIM, shared-library, source/docs, target/debug and performance/tooling children. Integrate coherent completed changes only, preserve unrelated working copies and push checked-forth periodically. Final combined validation remains pending: the earlier suite attempt stopped at its first failed pool, and subsequent focused fixes do not constitute a full pass. The existing cedar-polish-full-validation workspace runs a frozen tree while implementation continues. Acceptance: rebuild and self-build the final combined tree, run bin/hb --load test/run.f, run standalone application and resource-lifecycle acceptance, resolve regressions and report actual untested target boundaries. No extra manifests, sign-off ledgers or duplicated acceptance wrappers.

Current handoff (2026-09-11): active integration workspace is cedar-crossing-realpath, NOT the older cedar-source/cedar-polish-full-validation notes above. Current parentb0b90daa; bin/hb SHA1b965ddf frozen /tmp/cedar-class-members-native matches production throughd8b108ae and needs rebuilding for newer RQ/prefix changes. KEEP931a543c and Rowan tier-provenance/routing candidates are not integrated. Use actual jj history and dots rather than assuming workspace binaries match source.

Most recent full suite152pass9fail predates latest repairs. Full runtime regression28.61s passes. JIT Tender0.867s and Maki capacity0.863s, optimized allocator/loop/edge focused tests pass; full optimizing source159.013s and executable242.316s remain far above target. Final required sequence: reviewed compiler correctness/performance/provenance integration; real optimizing selfbuild; bin/hb --load test/run.f (compiler children tier1); full Tender/Maki native build, capture, restore and fresh checked REPL; embedded target handoff with actual boundaries; docs and coherent commits/push. No new test framework or timing ratchets.

Known fixed behavior must remain: local variable named required no longer triggers dynamic-source scanner rejection; package-aware builtin binding; GEOM/PLACER quotation shapes; shared-return and parallel-edge-copy ABI; JIT catch/evaluate stack recovery. No final push yet, checked-forth bookmark remains old. Preserve unrelated default WIP and all unintegrated agents' work.
