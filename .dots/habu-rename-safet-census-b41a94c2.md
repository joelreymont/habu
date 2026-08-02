---
title: Rename SAFET census to file owner
status: active
priority: 2
issue-type: task
created-at: "2026-08-02T21:50:24.729529+02:00"
---

Why: SAFET:census owns one validated memory-mapped safetensors file plus its tensor index; census misnames an exactly-once released file owner. Exact result: hard-rename the public linear type SAFET:census to SAFET:file in maki/infer/safetensors.f and maki/infer/safetensors-test.f, including all nineteen public effects and every focused test use. Preserve representation, behavior, borrowed spans, and RELEASE exactly-once semantics. Owner: package SAFET and its focused test only. Acceptance: old SAFET:census is undefined; SAFET:file is the sole owner type; safetensors focused and real GPT-2 artifact legs plus typed-local/package exact-diff gates pass. Forbidden: alias, compatibility spelling, wrapper, new type family, version, manifest, lint, registry, behavior change, or caller migration beyond the two exact files. Smallest owning check: bin/hb --load maki/infer/safetensors-test.f. Depends: none. Claim: agent=safet_file_cut workspace=.jj-ws/safet-file-cut.
