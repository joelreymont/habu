---
title: Correct stale type surface in forth docs
status: open
priority: 1
issue-type: task
created-at: "2026-07-28T20:22:42.863024+02:00"
---

Why: docs/forth.md:453-526 still teaches that SUMTYPE/PRODUCT are the live type surface and unified STRUCTURE does not exist, while master has working unified STRUCTURE and ENUM and the hard-cutover contract forbids new legacy declarations - the standards file directly teaches workers to write the wrong types. Exact result: replace ONLY that section with the current rule - new code uses NEWTYPE, unified STRUCTURE, and unified compact/full ENUM; STRUCTURE owns named fields and generated MAKE/UNMAKE/accessors; full ENUM owns payload variants; legacy SUMTYPE, PRODUCT, VALUE-RECORD, low-level structures, and counter enums remain migration-only until deletion and are forbidden in new code; stay honest that executable legacy sites still exist today. Add one concise LESSONS.md entry: agent-facing standards must describe landed interfaces; future or cutover plans belong in design docs. No broad docs rewrite, no other section touched. Owner: docs/forth.md and LESSONS.md. Acceptance: the replaced section names only landed interfaces (verify each named word resolves on master), the legacy names appear only as forbidden-in-new-code migration debt, no other hunks; no gate applies beyond reading. Forbidden: broad rewrite, new sections, speculative future syntax.

Claim: agent=claude workspace=.jj-ws/habu-correct-stale-type (RELEASED 2026-08-21: workspace gone, no live lane - gc)
