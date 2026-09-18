---
title: Restore package import depth during source replay
status: closed
priority: 2
issue-type: task
created-at: "2026-09-14T02:03:28.653389+03:00"
closed-at: "2026-09-18T10:10:13.377409+03:00"
close-reason: "landed in cedar's era: test/checker-replay-pkg-state.f exists, registered in test/gate-stdlib-cases.f:1129 (SUITE checker-replay-pkg-state), passes on release e7b1e2dc under the sandbox (2026-09-18), gate 437/437 green on chain BB; the claim was stale"
---

Cedar owns checker package import restoration. Product I loads the three-line
package/import/count reduction, but VERIFY:SOURCE-BUF returns 7141 because
CHECKER-END-PACKAGE leaves CHECKER-USE-OWNED-N unchanged. Evidence:
/tmp/cedar-I-wid-using-{source,verify}.{f,out,err}. This is separate from the
warmed future-shadow defect 0c9fe3d7.

Save the package-entry import depth in the existing CK-USE snapshot header,
which already travels through verifier and declaration rollback frames. A
balanced replay close restores that depth; normal compilation still reads the
engine's depth. Neutral scopes clear their own baseline.

The registered checker-replay-pkg-state fixture covers retained outer imports,
removed inner imports, explicit ;using, real in-scope ambiguity, the COUNT
reduction, throwing replay restoration by freshly compiled callers, and nested
rollback with different package baselines. Old product I fails four new
assertions (/tmp/cedar-I-package-close-old.{out,err}). The actual changed checker
passes when compiled and installed through the tier-1 native source-owner
handover with the normal core/declaration tail
(/tmp/cedar-I-package-close-source.{out,err}; rc 0, window: 0).
The existing using-test native-tier control has the same sole F13 error-code
mismatch with old I and the changed source: expected engine 94, got checker
7140. No ambiguity was accepted (/tmp/cedar-I-package-close-using-{old,source}
.{out,err}); this fixture discrepancy is outside the replay restoration fix.

Independent review and a rebuilt product gate remain with root. Generated maker
certification and the frozen WID collision cases remain end-to-end acceptance.
