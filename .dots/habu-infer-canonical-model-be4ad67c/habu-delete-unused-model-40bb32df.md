---
title: Delete unused model metadata
status: open
priority: 1
issue-type: task
created-at: "2026-07-29T21:09:42.368678+02:00"
blocks:
  - habu-pin-gpt-2-cdb5cfe0
---

Problem: MODELPROV is a 971-line proof and version subsystem with no caller outside its test. Result: delete model-provenance.f, its tests and suite entry, and every MODELPROV proof, mint, and TRUSTED row. Retain exact checkpoint revision and artifact digests only in loader/reference fixtures that consume the model pin constants. Owner: unused MODELPROV source and direct tests only. Production red: package-qualified XREF finds no non-test MODELPROV caller. Acceptance: no MODELPROV symbol or duplicate digest literal remains; model config, GPT-2 load, reference, trust, package, and native fixpoint tests pass. Forbidden: MODEL enum deletion, GPT2TENSOR format deletion, replacement provenance type, manifest, schema, version, compatibility record, or lint. Claim: unassigned.
