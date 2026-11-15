---
title: Measure bare type uses through the public fallback
status: closed
priority: 2
issue-type: task
created-at: "2026-11-06T13:19:27.306571+02:00"
closed-at: "2026-11-06T17:13:11.613625+02:00"
close-reason: "Measured: the counts are in this dot (suite 65 sites, Tender 63, Etch and Maki not loadable). Joel ruled on them 2026-11-06 (docs/forth.md 'Rows, variables, and lexing'): the fallback goes, in the tfb lane after C2."
---

Problem: facts design item 5: how many bare type names in the tree, Etch, Maki and Tender resolve only through the type registry's unique public fallback (src/core/type-family.f:729-746 TFAM-FIND-PUBLIC, reached from TFAM-RESOLVE :787). Joel's question (types follow the word scope rules) needs the count. Acceptance: a list of every fallback hit (type, owning package, file:line) from the suite and the three clients, deduplicated, with totals per repository. Files: src/core/type-family.f (instrumentation only, never landed). Verify: the counts. Depends: none. Ownership: the jerry-tfam workspace. Claim: agent=worker workspace=.jj-ws.noindex/jerry-tfam

Finding 2026-11-06 (probe engine e5c5a021 on 590a52d7; one line per written occurrence, deduplicated by file and offset): hb's own build 3 sites (src/compiler/native/hir-word.f: ctrl, rmove, expand from HIR). Full suite, rc 0, 475 s: 65 sites (/tmp/jerry-tfam/suite-sites.tsv), 7 in library code (hir-word.f 3, net/http.f 2 queue and sem, ws.f 1 and curl.f 1 group), 58 in about 20 test files; by type group (AIO) 25, sem (TASK) 22, queue (QUEUE) 8. Tender with Zen present (tender.f and tenderd.f rc 0, scripts/test.sh rc 1 with 39 failing tests on the probe engine, not compared with the installed hb): 63 Tender sites (/tmp/jerry-tfam/tender2-sites.tsv), 46 in tenderd/api.f and tenderd/auth.f (request, response from HTTP 23 each), matches (DOC) 8, charspan (ADDR) 8, writer (JSON), sem, queue, group 1 each; 0 in Zen's code. Etch: not loadable on current Habu (E-UNDEFINED FS-PATH-CAP at etch src/kicad_read.f:138), 0 hits before the stop. Maki: stops after 34 s with uncaught -30011 after "kernel: worker lost" (its kernel layer is not built), 0 hits in what loaded.
