---
title: Preserve caught value boundaries in native compilation
status: closed
priority: 1
issue-type: task
created-at: "2026-09-13T21:25:00Z"
closed-at: "2026-09-16T14:34:51.500490+03:00"
close-reason: "done: DO-CATCH now keeps the caught window's value boundaries and appends the result code as a separate cell [src/compiler/native/elaborate.f DO-CATCH computes glue with VGLUE-LOW; test/compiler/native-catch.f:270 multicell-payload window case]"
blocks:
  - habu-compile-the-tender-8385810f
---

Owner: cedar-indexed-dictionary. Tender's TENDER:OPEN in src/extract.f fails
native compilation with E-NELAB-MATCH (-8650). The isolated tier1 source load
and a small option<document> reproduction fail on the all-native F product
(source 3e4f0ec5). This is distinct from the recorded-layout capacity issue:
the two MATCH arms retain all correct width, padding and payload-glue facts.
Native one-shot breakpoints show VN=4/VGLUE=4 before DO-CATCH and VN=2/VGLUE=0
at BUNDLE-CK for the following two-cell MATCH. DO-CATCH passed GLUE-NONE to
STAGE-WCALL, erasing the caught option's value boundaries.

RSCATCH unifies both the quotation's input and returning output with the same
caller row. The native boundary therefore retains the input window's grouping
for its results and appends the separate result-code cell. Stack cells still
come back from the actual call: catch restores depth, not contents. MATCH,
finally, checker admission and exception contracts are unchanged.

Acceptance: native-catch executes a family with a multicell payload through
normal and throwing calls, retains a separate multicell prefix, handles an
empty window, and executes the reduced Tender OPEN shape. Existing catch and
MATCH rejection tests must retain their verdicts. The frozen source requires
an actual product rebuild before claiming the all-native path is fixed.

Focused source validation passes native-catch, native-match and native-quot,
including their rejected programs. The expanded native-catch first fails on
unmodified F at CATCH-BUNDLE/-8650. The passing probe uses the supported
PREFIX-REWIND:TO-CORE path, loads the actual compiler source at tier0, installs
its entry, and compiles the tested definitions at tier1. These are functional
results, not an all-AOT optimizer or timing claim. Logs are
/tmp/cedar-match-source-catch-final.{out,err} and
/tmp/cedar-match-source-regression.{out,err}.
