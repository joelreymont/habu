---
title: A body edit rewrites every object whose walk reaches the word
status: active
priority: 2
issue-type: task
created-at: "2026-11-11T18:52:55.069874+02:00"
---

Problem: src/habu/linker.f lays a WORDS row for every word a part's reach walk reaches (SEE -> NAMED-REC+, :1209-1210) and WORDS-CK (:3127) refuses the object when that word's time changed. A body edit of CORE-FOLD-C (src/core/util.f) rewrites 109 objects besides util.f's two (81 engine, 27 kernel, hb-build end.f); 108 have bytes equal to a cold build's; 104 never spell the word and reach it through CORE-STR=CI (util.f:54). Evidence: ~/.cache/tmp/carl-why (run.sh, an.py, cmp.py, r1/), hand-off .jj-ws.noindex/carl-why/.out/body-edit-hunt-handoff.md. Not known: per object, the seed and the chain of words that reaches CORE-FOLD-C, and whether the run that seeds it executes the host's baked word or the source being built; whether a changer record's whole-piece span (WALK-STORERS :1272-1278, prefix-boundary.f:28 undefine FIRST-RECORD-XT) is what brings it in. Acceptance: the chain is shown for verify.f+0, checker.f and boot.f+1 by a traced linker; then a design. A warm build after the edit rewrites only objects whose bytes change, warm = cold. Files: src/habu/linker.f. Verify: six-edit census (~/.cache/tmp/carl-rca/six). Depends: none. Ownership: linker.f WORDS rows. Claim: agent=carl workspace=.jj-ws.noindex/carl-why (hunt brief .out/briefs/hunt-body-edit-chain.md running; a design follows).
