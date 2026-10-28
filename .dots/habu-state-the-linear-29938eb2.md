---
title: State the linear catch rule the checker enforces
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T11:38:54.665512+02:00"
---

Problem: docs/forth.md:2330-2335 says a linear handle cannot cross a quotation-literal catch, but the checker keeps any window cell every throw path left intact (src/core/checker.f:4011-4027 intact masks; test/catch-stale-suite.f:21-30 names the quotation literal's term as carrying that evidence). Probe /tmp/tim-dots/f1.f: OPEN [: W ;] catch with W ( F1:tok -- F1:tok ) 3 throw and CLOSE in both arms certifies (rc 0); moving the handle in W (0 swap 3 throw) is E-STALE-READ at CLOSE. The code and its suite are the contract. Acceptance: docs/forth.md states that a linear handle crosses a quotation-literal or tick catch when every throw path of the caught body keeps it, and is stale otherwise. Files: docs/forth.md. Verify: the probe. Depends: none. Ownership: docs. Claim: unassigned.
