---
title: Delete model config version
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T20:34:14.770343+02:00"
---

Problem: maki/infer/model-config.f stores, validates, hashes, and publishes a schema version although the product has no installed compatibility contract or old reader; the field is duplicate state and forces speculative version handling into every consumer. Result: remove mcfg.sv, SCHEMA-FIRST, schema validation and folding, E-SCHEMA if no other use remains, and the schema argument from MDLCFG:BUILD; migrate every caller atomically. Current representation only: no default version, legacy arity, alias, upgrade path, or unknown-version branch. Config-key deletion is a separate dependent hard cut. Owner: package MDLCFG and exact callers/tests. Production red: a model with identical semantics but another schema cell follows a compatibility path instead of having no representable old format. Acceptance: old BUILD arity and schema words do not resolve; canonical GPT-2 configuration builds; wrong semantic fields still reject by name; model-config and production GPT2LOAD suites plus exact-diff gates pass. Claim: unassigned.
