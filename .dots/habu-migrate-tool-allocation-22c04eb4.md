---
title: Migrate tool allocation callers
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T14:14:22.905064+02:00"
blocks:
  - habu-migrate-vector-numeric-360069d6
---

Full context: MODEL-CAD-V2-PLAN.md B5 freezes these exact MEM-ALLOC-BYTES tool callers: tools/aot-call-report-test.f, tools/build-fixpoint-test.f, tools/build-fixpoint.f, tools/check-core.f, tools/check-test-lib.f, tools/codegen-role-test.f, tools/codegen-role.f, tools/diag-origin-core.f, tools/diagnose-hb-test.f, tools/examples-test.f, tools/hb-build-lib.f, tools/json-only-test-lib.f, tools/json-only.f, tools/lint/intern.f, tools/lint/text-foundation-test.f, tools/refine-lint-core.f, tools/repair-packet-core.f, tools/repair-packet-test.f, tools/repair-schema-doc-test.f, tools/signature-lint-core.f, tools/stdlib-manifest-test.f, tools/trust-lint.f, and tools/trusted-inventory.f. Fix only those files plus already-associated focused tests to validate CAD byte lengths and call MEM:ALLOC-BYTES; migrate VEC use in tools/lint/intern.f and tools/lint/source-lex.f in this same owned wave after VEC lands. Acceptance: no listed raw allocator call remains, tool output byte-identical, lint/parser capacity errors stay named, and no other caller wave edits these files. Depends on packaged MEM and packaged VEC.
