---
title: Report a shadow refusal once
status: closed
priority: 3
issue-type: task
created-at: "2026-10-03T13:23:51.469151+02:00"
closed-at: "2026-10-15T01:26:45.767572+03:00"
close-reason: Completed or superseded on current master. 343f1370 prevents secondary statement-throw after rendered shadow; f5246ba4 counts/continues multi-error definitions. Current shadow/arity cases assert one packet and continued checking. Read-only source/history evidence checked in backlog review; no new code or broader runtime claim.
---

E-USING-SHADOW-GLOBAL and E-SHADOWED-ARITY throw out of the definition (checker.f ~11079, ~12115) after rendering their record, so tools/check.f --all-errors writes the record and then an E-STATEMENT-THROW span at the definition's ';' (tools/check-all-errors-core.f CA-JSON-THROW), the packet's diagnostic_count overstates by one, and the throw ends checking of the rest of the source (review 436). Acceptance: each shadow refusal is one definition refusal that --all-errors counts and continues past, with one record, seen failing first; goldens updated with the reason.
