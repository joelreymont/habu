---
title: Admit layout.f body edits in package gate
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-29T22:58:09.106535+02:00\""
---

Full context: src/habu/layout.f is engine trunk like habu2.f - 240 pre-package global constants whose packaging is blocked on the stage0 using capability (habu-add-using-to-d815f0ab). The relocation work must change the BODIES of two existing globals (SNAP-FORMAT-VERSION bumped 4 to 5, DATA-START rebased to SNAP-RELOC:XTCELL-END), and tools/package-diff-lint.f correctly flags body-changed globals in an unpackaged file. Extend the ENGINE-BODY-EDIT admission from commit a943eb40 (currently exact-path src/habu/habu2.f, keyed on DEF-TAIL-ADDED 0= so body edits pass while NEW globals still fail) to also cover src/habu/layout.f, and pin the behavior in tools/package-diff-lint-test.f with both directions: a body edit to an existing layout.f global passes, a new unpackaged layout.f global still fails with its name in the finding. Falsify both directions by probe before calling it done. This admission retires when habu-give-layout-f-315df2ca finishes packaging the file.

Claim: agent=snapreloc workspace=.jj-ws/habu-relocate-snapshot-region-752042fe
