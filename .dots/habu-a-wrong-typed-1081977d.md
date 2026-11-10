---
title: A wrong-typed or absent packet field ends or mislabels diag-position-test
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T12:47:37.109783+02:00"
---

Problem: test/diag-position-test.f - after habu-field-ends-the-614c2257 a missing string field is one failure, but a field of the wrong type still ends the run (tools/gate-json-assert-core.f GJA-STRING$ and GJA-INT die), and an absent integer field fails as 'expected N got -1' without the field's name. Reported by the FIELD= worker. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a wrong-typed field and an absent integer field are each one failed assertion that names the field, and the later cases still run. Files: test/diag-position-test.f. Verify: a scratch copy with one field renamed and one retyped reports one failure each and ends at its report. Depends: habu-field-ends-the-614c2257 landed (same words). Ownership: test/diag-position-test.f. Claim: unassigned.

Cases 2026-11-10 (review of habu-field-ends-the-614c2257): test/diag-position-test.f PLACED-IN's 'ELSE T-FAIL THEN' records a failure without T-NEXT and prints no assert line (a call with a span outside the fixture prints F107 twice and miscounts); replacement: ELSE T-NEXT s" expected span lies outside the fixture" T-ASSERT-DETAIL T-LABEL-CLEAR THEN. test/load-reject-diag-test.f ASSERT-IMM-DIAG (:296-305) and :694-697 use readers that die (GJA-REQ, GJA-ASSERT-STR, GJA-ASSERT-INT-FIELD, GJA-OBJ, GJA-DIAG-CONTRACT-ROW end in GJA-FAIL = 1 die), so a missing field or a wrong value ends the whole run: move STR-FIELD= and FIELD-MISSING to one shared place and use them there. Some cases assert absence through INT-FIELD -1 T= (TOP-AT :1043-1046), so naming an absent integer field needs a separate absence check. A stderr line that is not JSON makes GJA-PARSE throw uncaught and ends the run (inferred, not run).
