---
title: Delete model provenance version
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T20:34:14.871536+02:00"
---

Problem: maki/infer/model-provenance.f stores and folds a provenance schema version even though only one current provenance representation exists. Result: remove the version field, accessors, constants, validation, hash input, old-version diagnostics, and every caller argument; retain exact checkpoint identity, artifact digests, generator identity, and proof semantics. Current representation only; no compatibility branch or alias. Owner: package MODELPROV and direct fixtures. Production red: two otherwise identical provenance values can differ only by an unused compatibility number. Acceptance: the old constructor arity and version names do not resolve; the pinned GPT-2 artifact and reference provenance validate; digest, checkpoint, and generator mutations still reject; focused provenance/reference and exact diff gates pass. Claim: unassigned.
