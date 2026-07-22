---
title: Add generic bounded-copy result family
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-22T15:51:34.318889+02:00\""
---

Why: four in-flight consumers (reentrant JSON-WRITE, model-pack manifest, normalized model config, safetensors owner-threaded reads) each need a bounded-copy result that preserves a possibly-linear owner across both success and refusal; without one shared family each module invents its own bool/sentinel/tuple shape and the ownership-loss class we rejected twice today (safetensors, manifest) keeps reappearing per module. Owned result: package-owned generic result family in lib/adt/copy.f (package COPY): COPY:result<a> with variants copied(a, len) and required(a, len), and COPY:optional<a> adding absent(a); every variant carries the concrete owner type a so a linear owner survives success, too-small refusal, and absence; lengths are typed len; refusal arms perform no write to the destination. Checked negative regressions must prove a linear owner can be neither lost nor duplicated through any arm (construct each variant with a DEFLINEAR owner, prove drop/dup of the carried owner rejects, prove the refusal path returns the same owner). No module-specific bool, sentinel, or duplicate copy-result family may remain in the four consumers once they migrate. Acceptance: family compiles checked with the linearity regressions green; a consumer-shaped fixture threads a linear owner through copied/required/absent and the checker rejects owner loss and duplication in each; the four consumer dots (habu-make-json-write-d0ed2cfb, habu-infer-pack-manifest-27c1030c, habu-infer-pack-normalized-84fc05fa, habu-infer-reentrant-safetensors-d3d3a8a6) reference this family instead of local shapes. Owning gate: new lib/adt/copy-test.f via bin/hb plus the type-linear suite slice. Depends: none; blocks the four consumers' result surfaces. Files: lib/adt/copy.f (new), lib/adt/copy-test.f (new), FILEMAP.md rows.

Claim: agent=copy_result_impl workspace=.jj-ws/copy-result-impl.
