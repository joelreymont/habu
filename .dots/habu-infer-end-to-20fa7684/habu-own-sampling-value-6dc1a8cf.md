---
title: Own sampling value types
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T01:41:05.560963+02:00"
---

Why: OPEN-SEQ and closed model arms need sampling values before NEXT-MANY exists. Interface: package SAMPLE owns immutable config and stop-set products. CONFIG validates temperature at least zero, top-k at least one, and top-p greater than zero and at most one, returning configured(config) or refused(config-error). STOPS validates one or two ordered nominal identifiers and returns configured(stop-set) or refused(stop-error); each model arm additionally proves every identifier is below its valid count. Owner: sampling value declarations and construction only. Dependencies: none beyond existing numeric and token identifier types. Production red: the tree has no SAMPLE package, constructors, or public sampling value types. Acceptance: minimum, maximum, and one-outside values select the exact configured or refused arm; stop order is retained; no model-specific constant is stored. Forbidden: selection algorithm, random mutation, engine row, tokenizer, output publication, model registry, default configuration, version, or compatibility path. Smallest owning check: bin/hb --load maki/infer/sampling-types-test.f.
