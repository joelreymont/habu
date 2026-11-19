---
title: Split a SUMTYPE payload, a local and a stored str in a split-str file
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T12:36:48.278614+01:00"
---

Problem: three more places where a file marked split-str does not read str as ^u8 n, found 2026-11-19 on hb a45a6990 beside habu-split-a-stored-fd0b3a8c and not fixed by it: a marked SUMTYPE fails at ;SUMTYPE even with a plain str payload (its variant takes str; the stack holds ^u8 n); a local annotated a:str is not split; and TYPED-VARIABLE V str (or ^str) read with @ gives str, not ^u8 n. Whether the third should split is a design question for the marker's owner. Reproducers: one file per case under /tmp/kevin-habu-split-quot/cases/ on spark; rewrite them into test/split-str-marked.f. Acceptance: each of the three either reads as ^u8 n in a marked file, pinned by test/split-str-test.f, or docs/forth.md Types says a marked file does not split it. Depends: none. Claim: unassigned.
