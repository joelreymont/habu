---
title: Refuse a build certify that stops early
status: open
priority: 2
issue-type: task
created-at: "2026-10-07T03:47:45.309953+02:00"
---

Problem: the build's blocking certification (tools/build-fixpoint.f BF-CERTIFY-PREFIX, BF-CERTIFY-STAGE2 -> VERIFY:SOURCE-BUF) passes a generated source whose scan stops early: at a word that may read the source after it (src/habu/verify-source.f TOP-OPAQUE, comment above TOP-ACTION) or at a tick the scan cannot order (TICK-REMAINDER). The scan reads nothing after the stop, and the build leaves REPORT-DEFERRALS off, so the rest of the source is never verified and nothing says so; the build's comment promises a type error in emitted engine source cannot warn its way into an installed binary. Today neither phase stops (probe on master e8330f7c: boot prefix 4918 definitions judged, assembled 2331, no stop; ~/.cache/tmp/heron-arm64/evidence/carl-census/probe-master.log). Found reading the census fix (Count the build census from the certify scan). Acceptance: first reproduce in process (row build-fixpoint-source): a generated source holding an opaque top-level word followed by a definition that does not check certifies today; after the fix the blocking certify refuses it E-BUILD-CERTIFY with the stop's token in the report, and a source with no stop certifies as before. Deferred bodies (3 in stage2 today: src/habu/habu2.f C-HOST-SELECT, EM-STARTUP-COLD-BASELINE, EM-STARTUP) are not stops and stay accepted. Files: tools/build-fixpoint.f (the certify words), src/habu/verify-source.f only if the stop is not already observable. Verify: build-fixpoint-source, build-fixpoint-fixtures, certify-generated red then green; a real refresh still certifies. Depends: the census fix on master. Claim: unassigned.
