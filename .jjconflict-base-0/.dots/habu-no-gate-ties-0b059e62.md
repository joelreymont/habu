---
title: No gate ties the engine keyword table to the frozen seed
status: open
priority: 2
issue-type: task
created-at: "2026-08-19T16:25:00.056732+02:00"
---

FOUND GAP (trusted-3, 2026-08-19, while probing habu-cast-reads-the-1f151a30). The interpret-loop reader keywords are declared TWICE and nothing checks the two lists agree: src/habu/habu2.f EM-INTERPRET-DEFINE-KEYWORDS (6562-6577, each row a label + CF-ENTRY + KEEP? treeshake entry, e.g. s" trusted:" KEEP? IF LMAIN LABEL@ LKWTRUSTED 8 ['] C-TRUSTED CF-ENTRY THEN) and the frozen gforth recovery seed bootstrap/cg/forth.fs, which carries its own table (forth.fs:2592 LKWTRUSTED @ LBL, s" trusted:" BYTES,). A keyword added to habu2.f and omitted from forth.fs leaves every gate green while tools/bootstrap.sh - the documented recovery when bin/hb is lost or broken (docs/bootstrap.md) - can no longer read the tree. src/core/roles.f is the FIRST entry in bootstrap.sh SRC_COMMON, so the failure lands on the first file of the recovery.

WHY THE EXISTING LINT DOES NOT COVER IT. tools/bootstrap-mirror-lint.f's name oversells its rule. Its actual contract (file header, lines 1-16) is narrower: it fires when an ADT DECLARATION keyword - SUMTYPE / ENUM / PRODUCT / NEWTYPE - appears as a live token in the stage-0 corpus, because gforth stage-0 has no width-aware pass-2 mirror. It has no model of the interpret-loop keyword table and never reads bootstrap/cg/forth.fs. Verified by inspection, not assumed: its green is NOT evidence the mirror is honest.

TODAY'S ONLY PROOF is a manual HABU_ALLOW_BOOTSTRAP=1 tools/bootstrap.sh run, which nothing schedules.

WHAT TO BUILD: a correspondence gate that reads both keyword tables structurally (the CF-ENTRY rows in habu2.f and the BYTES, rows in forth.fs) and refuses any keyword present in one and absent from the other. Structural, not textual: adversarial fixtures must include the spelling hidden in a comment or a string, a duplicated row, rows reordered, and a keyword present under the wrong label. Decide first whether this belongs in bootstrap-mirror-lint.f (second corpus, second model) or as its own lint; either way rename or re-document bootstrap-mirror-lint so its name stops implying a coverage it does not have. Blocked-by nothing. Blocks any future reader-keyword landing.
