---
title: "Cover check.f's statement-throw JSON in the diagnostic contract"
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T12:03:31.916095+02:00"
---

Problem: tools/check-all-errors-core.f CA-JSON-THROW (~:526 at r4-tbuf dc825869) emits a statement-throw diagnostic that tools/gate-json-assert-core.f diag-contract rejects (rc 1, missing JSON field); no test/gate-diagnostics-lib.f fixture produces that shape, and tools/check-test-lib.f check/statement-throw asserts its fields by substring only. Found by the r4-tbuf worker. Acceptance: decide which side is wrong (the emitted shape or the contract) against docs (the diagnostic JSON contract text); fix that side; a gate-diagnostics fixture produces the throw shape and passes diag-contract, seen failing first; malformed throw rows are refused. Files: tools/check-all-errors-core.f or tools/gate-json-assert-core.f, test/gate-diagnostics-lib.f, test/gate-diagnostics-entry-lib.f. Verify: test/gate-diagnostics.f rc 0, tools/check-test.f rc 0.

Update (review 144 of r4-expand commit 8, wytzutzz 7c6be86e): that commit makes the emitted throw shape match the contract and adds the STATEMENT-THROW fixture (seen failing first), so what remains is the refusal half. diag-contract is weaker than docs/repair-diagnostics.md: GJA-DIAG-SPAN (tools/gate-json-assert-core.f ~:557-560) checks class and suggestion only as a pair from the table and GJA-DIAG-VERDICT admits `uncheckable`, so an E-UNTERMINATED-STRING record with `close_primitive_row`, or a span record with verdict `uncheckable`, passes; GJA-DIAG-INPUT (~:562-565) does not require `rebuild_engine`; no test feeds diag-contract a malformed row. Remaining acceptance: span records must carry verdict `rejected` and the class their code names; the input record must carry `rebuild_engine`; negative rows (wrong class for the code, wrong verdict, a throw record missing throw_code) are refused by diag-contract through its real load path, each seen accepted first. Base: r4-expand commit 9. Files: tools/gate-json-assert-core.f and the test that drives diag-contract.
