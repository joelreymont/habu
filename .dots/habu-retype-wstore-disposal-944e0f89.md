---
title: Retype WSTORE disposal as total
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T22:25:12.209587+02:00"
blocks:
  - habu-make-owned-release-79de2b5c
---

Why: TABLE-DISPOSE and BUILDER-DISPOSE promise result<n,n> but can never mint err (their own comments say the result is the shape, not a wider failure model); BUF-FREE frees the owner record unguarded then catches only the data release, so its err arm reports a failure nothing can act on. With MEM:RELEASE fatal, every one of these results is a false failure model. Behavior: DISPOSE, TABLE-DISPOSE, BUILDER-DISPOSE, BUF-FREE/BUFFER-DISPOSE, MAPPED-DISPOSE, and ALLOC-DISPOSE return plain bytes-given-back n; RESIDENT-DISPOSE is NOT retyped - the resident and its exit are deleted by the store-embedding wave, and totalizing a word scheduled for deletion is disposable work; no catch around any release; the weight-store.f rationale corrected where 38c9b90f landed the measured-false width claim (a field names one typed value; the embedded store declares - the truthful text cites habu-embed-store-in-f8109695 as the resident's retirement). Owner: WSTORE package only. Dependencies: habu-make-owned-release-79de2b5c; coordinate with habu-retype-safet-unmap so MAPPED-DISPOSE consumes the total unmap. Acceptance: weight-store suite green on the total surface, retired err-arm assertions named; both diff lints clean; destruction review before merge. Real pre-change defect: a caught disposal throw after the owner record is gone leaves bytes with no owner and execution continuing - measured in the landed review.
