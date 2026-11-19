---
title: "Check a call's nested quotation input in linear time"
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T13:57:57.680549+01:00"
---

Problem: checking a call whose input quotation nests k deep (test/split-str-deep.f QUOT-CALL; k is at most 1023, two levels per quotation under the 2048-level ceiling) still grows faster than k: the installed engine takes 0.024 s at k=256, 0.052 s at 512 and 0.144 s at 1023 (best of five), 2.8x for the last doubling. The one measured cause left is HOLD-ESCAPE?'s trail loop, which every enclosing frame repeats over the entries of the frames inside it (dot habu-check-a-quotation-e198e425). Acceptance: the times per doubling of k stay near 2x from 256 to 1023. Depends: habu-check-a-quotation-e198e425. Claim: unassigned.
