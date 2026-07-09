---
title: Interpret wide-gate residuals
status: open
priority: 2
issue-type: task
created-at: "2026-07-09T13:30:57.226960+02:00"
---

Residuals of the closed habu-tfam-12-interpret-10b385b1 (DNAME-WIDE interpret gate; engine half + checker-computed marking both landed, GE-INTERP-LAYOUT pins the flag end-to-end). Two documented gaps remain: (1) TRUST-after-publication widening — a top-level s" name" s" effect" TRUST row that WIDENS an already-published word's effect does not mark its dict record (no publish tail follows the record flow; the checker-side fix needs a named-record marking path, e.g. an xref find-by-name + a record-index wide-mark variant), same class as the TRUST-after-execution ordering gap (marking cannot retroactively cover an execution that already happened). (2) raw-xt laundering — fully unchecked top-level code that obtains an xt via find/search-wl and BEXECs it bypasses any static flag by definition (test/type-layout-lower-pending.f TLP-XT is the audited test-only use). Context: checker latch RECW + REC-WIDE-PUBLISH consumed by EM-REC-WIDE-PUBLISH publish tails (habu2.f); diagnostic 'hb: interpret-mode layout value:' rc 70.
