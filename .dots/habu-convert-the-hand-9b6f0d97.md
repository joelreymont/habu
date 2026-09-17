---
title: Convert the hand-judged raw pointer sites
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T18:44:23.885163+03:00"
---

Problem: the raw-storage census (/home/joel/.cache/hazel/scout-pun/census.md) marks 105 rows 'other' (the classifier could not name a mechanical shape) and 78 rows AMBIGUOUS (the reported definition name resolves to more than one declaration), and over-counts rows whose token is cell+, char+ or + (armed, RAW-OK? admits the pointer row where the live rule certifies the scalar row). Acceptance: each row inspected in its body and either converted to a declared form, or recorded as certifying under the live rule (measured on ~/.cache/hazel/engines/raw-rule-gen1), or handed to its directory lane with the form named; the census annotated per row; no TRUSTED: or unchecked seam as a conversion. Files: per the census rows. Verify: raw-rule-gen1 loads of each closure; the affected suites; test/run.f. Depends: the definer lane. Ownership: whoever owns each file. Parent: habu-refuse-a-ptr-5ad2734e. Claim: unassigned.
