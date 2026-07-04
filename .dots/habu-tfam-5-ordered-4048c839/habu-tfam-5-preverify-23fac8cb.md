---
title: "TFAM 5: preverify/all-errors ordered-event redrive + support parity"
status: closed
priority: 2
issue-type: task
created-at: "\"2026-07-04T08:53:45.129143+02:00\""
closed-at: "2026-07-04T22:15:00.349671+02:00"
---

Replace path-set closure discovery with ordered-event replay in the consumers. verify-source.f preverify (RECORD-DEFINER? :373) and tools/check-core.f flatten (CHK-MATERIALIZE-FILE/LIST :494, CHK-SCAN-DEPS :426) must redrive each ORIGINAL source-list file per ordered event (stop flattening away original paths). all-errors (tools/check-all-errors-core.f CA-COLLECT-SUPPORT :427) is missing deftype/deflinear/value-record/immediate/EXPORT vs verify-source (asymmetry, census gap4); top-level TRUST replayed by all-errors but NOT verify-source RECORD-DEFINER? (gap5). Drive support replay from the shared ordered event log so package/family metadata loads before metadata-derived signatures and EXPORT rows. --all-errors --source-list replays all prior source-list entries before checking a later file. Do this via the event log, not ad-hoc line collection. Depends on event-log + discovery dots.

VERIFIED DISCHARGED (read-only scout, 2026-07-04, head dbcd3f37): all six clauses landed + test-locked. (1) preverify redrives ORIGINAL files: CHK-RUN-PREVERIFY-ACT check-core.f:1036 branches to CHK-PREVERIFY-ORDER :1030 over CHK-DEP-ORDER, original paths via CHK-DEP$, VERIFY:SOURCE-BUF-IN-SCOPE per file; locked by CKT-TEST-SOURCE-LIST-AUDITED-LIB + SOURCE-LIST-PREVERIFY-DIAG. (2) CHK-SCAN-DEPS gone; flatten temp feeds ONLY the RUN phase; all checker passes (preverify/all-errors/nominal/trust) redrive per original file. (3) support parity full set incl. typefamily/sumtype (CA-COLLECT-SUPPORT :540-558) with fixtures. (4) TRUST parity via skipped-string ring, CAE-TEST-TRUST-SUPPORT. (5) cross-file replay event-ordered (CHK-RUN-ALL-LIST-CURRENT :977); within-file is byte-order collection because the event log has NO definer events — semantically equivalent, see capability dot habu-definer-kind-events. (6) CKT-TEST-SOURCE-LIST-ALL-ERRORS locks prior-entry replay. check-test + check-all-errors-test green on head.
