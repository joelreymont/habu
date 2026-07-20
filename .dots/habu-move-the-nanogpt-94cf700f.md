---
title: Move the nanoGPT application into maki/examples/nanogpt
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-20T10:32:25.023809+02:00\""
---

Joel 2026-07-20: use a proper directory structure - maki/examples/nanogpt/... maki/ is a flat 228-file directory mixing the ML library with the nanoGPT application and benchmark tooling. Target: maki/ = library only (tensor/CAD/MODEL:/SPEC:/autograd/optimizers/executor/device lowering/RNG/losses/attention math); maki/examples/nanogpt/ = the application (tokenizer.f, data-loader.f, batch-loader.f, from-scratch-model/train + tests, xent-train, gptblock-test, the model composition files and training entry points, corpus fixtures). Boundary rule: if another model would still need it, it is library; if it is specific to this model+dataset, it is maki/examples/nanogpt. Benchmark/report tooling (competitive-*) moves to tools/ or stays per its own dots - decide with evidence, do not fold extra scope in. ONE mechanical change: file moves + every require path + FILEMAP.md + suite-slice registration (maki/test*.f and test/run-lib.f slices) updated together; nothing else mixed in (no-diff-churn). Respect the REQUIRE-MAX situation (raise in flight, habu-raise-require-max-ce5d615f) and compose with habu-centralize-maki-suite-85c0ab18 rather than fighting it - if that dot's slice redesign is ready, land them in sequence, this one purely mechanical. Proof: full cold gate green, filemap/maki-dep/suite-coverage lints green, byte-identical file contents (moves only - verify with content hashing), no require chain broken (the gate build proves it). SERIALIZED: after the current lane wave drains - tokenizer/trainer/gptblock files are claimed by running lanes.

2026-07-20 Joel refined the target: maki/examples/nanogpt (under maki, not top-level examples/) - the example stays inside the library tree; top-level examples/ keeps only non-maki demos.

2026-07-20 serialization released: the trainer wave has fully drained (checkpoint 9bd7cd28 was the last lane).
Claim: agent=nanodir workspace=.jj-ws/fable-nanodir machine=spark (owns the moved app files + maki/test*.f slice registration + FILEMAP + require-path updates; running lanes extprod (src/core) and swbound (tools/ptx) are disjoint - minor FILEMAP merge risk accepted)
