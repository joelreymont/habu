---
title: Validate and publish the combined Habu changes
status: open
priority: 1
issue-type: task
created-at: "2026-09-10T18:03:13.409212+03:00"
blocks:
  - habu-deliver-standalone-native-a86d4699
---

Owner: Cedar. Additional prerequisites: completed type/quotation/cleanup/PRIM migration, shared library handoff, source/docs usability and required target/debug changes. Preserve all unrelated work and land coherent jj changes with periodic pushes; user explicitly forbids accumulating unrelated unfinished features in one commit. Independently reviewed root stored-quotation/lifecycle/capture revisione9fd7e6d passed native-stored-quot, native-catch, native-quot and image-lifecycle on build5; nominal-pointer1eb56c73 also reviewed and focused-tested. Do not call the full suite green: old uniform cedar-validation run ended with86 failures (/tmp/cedar-native-suite-5.out); pointer baseline suite session70735/PID343563 logs /tmp/cedar-pointer-suite.log and is still running. Triage actual failures on one fixed source/binary; retain harness corrections and replace obsolete negative expectations only with executed proof. Rebuild/self-build and run bin/hb --load test/run.f on the final combined tree; report actual failed/untested boundaries and resolve regressions before completed handoff. Remove obsolete bootstrap machinery only after the replacement path works; keep required historical seed bootstrapping distinct from runtime fallback.


Current suite triage (old pointer candidate): shared FIELD-REG/lib/ptx/kernel-abi.f -8392 blocks14 PTX suites; addressed by /root/pointer_review in .jj-ws/cedar-address (spills split four-lane reloc carrier; emitter rejection correct). TAKE/lib/process-pty-handle.f and IR fixture calls fail DSTACK validation -8486, not yet reduced. MULTI-ERR-BEGIN lacks a published effect (NDICT SPELL-ARITY=-1/-1), blocking checker tools. Stale tests still assume fixed512KiB/16cell IR context, reject now-valid scoped locals/inferred quotations/wholly throwing callbacks; replace with valid failure conditions and current accepted semantics. bin/hb-host is absent but host tests still depend on it; tools/ptx/mma-exact-lib.f still requires removed maki/array.f. Manifest/proof timeouts under load remain unvalidated, not established compiler failures. /tmp/cedar-pointer-suite.log and its captures retain details.

Additional existing internal-capability failure: top-level 0 int-mark is currently callable on fd9342de and multi-error candidate; repair final sealing/visibility while keeping authorized native internal compilation. MULTI-ERR-BEGIN/END checked API move fb212dc8 passed rebuilt nine API assertions, all27 all-errors cases, and independent Astra review; integrate it. Source commits are held for combined mandatory native suite; tracking remains published separately.
