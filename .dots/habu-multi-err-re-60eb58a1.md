---
title: Multi-error re-drive re-registers type families (dup)
status: open
priority: 2
issue-type: task
created-at: "2026-07-10T14:59:02.253429+02:00"
---

Bug: tools/check.f --all-errors on a source that declares a SUMTYPE/TYPEFAMILY AND has any checker error emits a SPURIOUS extra E-BAD-DECLARATION 'duplicate family' diagnostic for that family. Repro: printf 'SUMTYPE zrc 0 VARIANT keep n ;VARIANT ;SUMTYPE\n: ZBAD ( n -- zrc ) ;\n' | bin/hb tools/check.f --all-errors --json-errors -> 3 JSON lines: E-MISMATCH(zbad), E-BAD-DECLARATION(zrc duplicate-family, SPURIOUS), plus the redriven E-MISMATCH. A CLEAN SUMTYPE source (no other error) does NOT duplicate — so the double-registration is triggered by the multi-error error-collection RE-DRIVE, not the normal two-phase scan. Root cause: verify-source RECORD-SUMTYPE (src/habu/verify-source.f) calls CHECKER-DEFSUM which registers the family; the --all-errors redrive (rollback + re-run to collect more errors) re-evaluates the SUMTYPE line and re-registers the family WITHOUT rolling back the type-family registry (an SV-rollback gap — the registry is not in TRIAL-SAVE/TRIAL-REST). Fix: make the type-family registry SV-rollback-safe across the multi-error redrive (register-once / roll back registrations between passes), then add a negative regression (--all-errors on SUMTYPE+error must NOT emit duplicate-family). Pre-existing / not caused by the item-13 family render field (render-only). Surfaced by repair-schema-doc-test.f RSD-SRC$ (adds a zrc ADT mismatch under --all-errors) — RSD stays green because the extra line is valid JSON (schema_version:1) and family still appears legitimately.
