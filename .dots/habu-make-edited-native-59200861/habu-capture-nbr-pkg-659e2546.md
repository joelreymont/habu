---
title: Capture NBR package contribution
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T14:29:13.734538+02:00"
blocks:
  - habu-write-nbr-artifact-792cbbb2
---

From the ordinary checked native compilation of the declared ordered NBR input and dependencies, write one unstripped package contribution: source identity, code/DATA/dictionary/WID spans, relocation sites, published checked effects/control facts, and protected WIDs in source order. Reuse AOT coordinate and relocation representations; final-image reachability/compaction in AOT-CAPTURE:CAPTURE is unsuitable. Relevant files: src/habu/aot-capture.f, native publication/capture format owner, checker graph export, and package writer. Acceptance: the E2E producer writes a deterministic artifact retaining private TARGET and every required relocation; a reader can stage it, and malformed coordinate/relocation input refuses. No full engine build until CPU clearance.
