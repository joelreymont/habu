---
title: Report a shadow refusal once
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T13:23:51.469151+02:00"
---

E-USING-SHADOW-GLOBAL and E-SHADOWED-ARITY throw out of the definition (checker.f ~11079, ~12115) after rendering their record, so tools/check.f --all-errors writes the record and then an E-STATEMENT-THROW span at the definition's ';' (tools/check-all-errors-core.f CA-JSON-THROW), the packet's diagnostic_count overstates by one, and the throw ends checking of the rest of the source (review 436). Acceptance: each shadow refusal is one definition refusal that --all-errors counts and continues past, with one record, seen failing first; goldens updated with the reason.
