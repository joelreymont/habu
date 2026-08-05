---
title: Infer linear kinds through polymorphism
status: active
priority: 2
issue-type: task
created-at: "2026-07-13T14:33:27.264362+02:00"
---

Problem: generic copy/drop safety is inferred again at each call from a stored signature. That loses a checked body's actual evidence, cannot distinguish copying a quotation value from executing and dropping its result, and is then compounded when native publication appends a TRUST row that shadows the checked record. Declared row quantifiers and defer assignment also allow specialization to hide behind a generic interface.

Result: add one orthogonal NONLIN bit to existing type-variable kinds. Checked bodies earn it only from operations that actually copy or discard a value; unification propagates it through fields and real layout arguments, stops at pointers, quotations, atoms, rows, and phantom parameters, and rejects concrete owners. Primitive, TRUST, and provisional recurse effects infer it once: exposed ownership positions activate the variable, then nested quotation inputs flip polarity while quote-only variables remain opaque. Store it only on EN-VAR, restore it through the existing trail, and publish/export checked live rows directly. Extend the existing parametricity seal so declared row variables, including nested rows, remain distinct and open. `is` accepts only targets at least as general as the defer scheme, using transient existing maps; typed quotation cells remain closed and monomorphic. Delete LTNT, row kinds, call-time multiplicity, signature reparse, and the duplicate checked TRUST row. Add no registry, annotation, mode, ABI/version, compatibility path, suite, or lint.

Acceptance: checked quotation transport and trusted quotation drop/dup accept owner-producing quotations; checked execute+drop, owner copy/drop, `KEEP`, `BI`, raw-TRUST KEEP, and provisional-recurse KEEP reject. `DIP`, pointer and phantom boundaries, return-row moves, recursive quotation transport, and real `MEM:WITH-BYTES` owner mint/consume accept; fields and real layouts remain owning. Empty `( R -- S )`, row-consuming, nested-row aliasing, concrete-to-generic defer assignment, stronger-kind assignment, and open typed quotation cells reject, while identity rows and sufficiently general defer targets accept. Kinds survive checked publication, return rows, recursion, export, overload rollback, and the native fixpoint. Actual TRUSTED/pretrust publication remains unchanged.

Files: `src/core/checker.f`, `src/core/render.f`, `src/habu/habu2.f`, `bootstrap/cg/forth.fs`, and focused rows in existing checker, memory, export, typed-storage, and compiler-model suites.

Verify: record the current candidate's checked-QCOPY false reject and master's owner-transform reject before broad edits; then run the four production-path probes above, pre-DEFLINEAR/type/row/return/recurse/export/rollback/boundary cases in existing suites, engine and memory suites, checker-model proof, bootstrap, native fixpoint, and the existing ownership/package gates.

Depends: none.

Ownership: type-variable NONLIN kinds, exact checked-effect publication, row/defer parametricity seals, removal of duplicate checked TRUST publication, and focused existing-suite regressions only.

Claim: agent=codex workspace=.jj-ws/habu-infer-linear-kinds-1f77b4c4
