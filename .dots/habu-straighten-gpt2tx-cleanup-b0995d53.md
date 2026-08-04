---
title: Straighten GPT2TX cleanup ladders
status: closed
priority: 2
issue-type: task
created-at: "2026-07-26T22:25:12.215782+02:00"
closed-at: "2026-08-02T16:39:49.961140+02:00"
close-reason: authoritative ancestor 5b0ebb070a5b8ef7c04e2d28772421f796b686c6 deleted the unused GPT2LOAD/GPT2TX/WSTORE/MODELPROV host architecture and suites; retaining the task would resurrect deleted architecture.
blocks:
  - habu-cut-gpt2-model-445a19ff
  - habu-retype-safet-unmap-ed05d42c
  - habu-retype-wstore-disposal-944e0f89
---

Why: the bind transaction's cleanup ladders fold disposal result codes that the total surfaces can no longer produce - CA-PREP-BACK, CA-BUF-BACK, CA-TBL-BACK, the RES-CODE and FOLD-CODE users, and RELINQUISH's fold with its unreachable census-release arm. Behavior: every ladder becomes straight-line total code; RES-CODE/FOLD-CODE and dead arms delete; RELINQUISH simplifies to its total form; the gpt2-bind.f stale rationale paragraphs from 38c9b90f corrected the same way as the WSTORE leaf (no comment may assert a checker acceptance or rejection not measured in the same change). Owner: GPT2TX package only. Dependencies: habu-retype-safet-unmap and habu-retype-wstore-disposal (this leaf consumes both total surfaces). Coordination: the unlanded BIND commit rides above these files; the S6b4 lane rebases and re-verifies after this lands. Acceptance: all four bind suites green on the straight-line ladders with retired assertions named; both diff lints clean; destruction review before merge. Real pre-change defect: CA-PREP-BACK reads TABLE-DISPOSE as total while its type says otherwise - the ladder is correct only because the err arm is a lie.

Reordered 2026-07-26 (resident-order correction): this leaf runs only AFTER the model cutover to the embedded store, so it straightens SURVIVING ladders - it never rewrites resident paths scheduled for deletion.
