---
title: Reject non-GPT2 bind configuration
status: active
priority: 1
issue-type: task
created-at: "2026-07-22T09:40:20.837945+02:00"
---

Why: `GPT2TX:PREPARE` currently accepts an otherwise valid Llama
`MDLCFG:mcfg` when its common geometry matches the GPT-2 census. That lets a
foreign architecture enter a transaction owned solely by the GPT-2 tensor
vocabulary.

Owner and interface: package `GPT2TX` keeps the existing
`PREPARE ( SAFET:census MDLCFG:mcfg -- prep-result )` interface and existing
`prepared|rejected(census,code)` result. Add public error constant
`E-GX-FAMILY = -5673` and private
`V-FAMILY ( MDLCFG:mcfg -- MDLCFG:mcfg )`. `PLAN` must call `V-FAMILY` as its
first operation. The validator compares `MDLCFG:FAMILY@` with
`MODEL-FAMILY:GPT2` using the derived nominal equality and throws
`E-GX-FAMILY` on mismatch; `PREPARE` converts that through its existing caught
plan into `rejected`.

Dependencies: the landed `MDLCFG:FAMILY@`, `MODEL:family` equality,
`GPT2TENSOR` tensor vocabulary, and `GPT2TX:PREPARE` are sufficient. This leaf
adds no type, package, compatibility path, allocation, or public word other
than the named error constant.

Owned result and write set: only `maki/infer/gpt2-bind.f` and
`maki/infer/gpt2-bind-test.f`. The test builds a valid tiny Llama
configuration and a real production-derived safetensors census through
`TX-LAY`, `SAFET:LOAD`, `GPT2TENSOR:COPY-NAME?`, and `GPT2TENSOR:SHAPE`.
Before `PREPARE`, set the private plan and sum counters to sentinels. The
rejection must leave both sentinels unchanged, proving that no tensor walk,
table creation, prep-block allocation, or weight-store allocation began. The
returned census must retain its count and owners, then succeed in a second
`PREPARE` with the valid GPT-2 configuration and abort cleanly. SAFET, WSTORE,
and GPT2TX live counters must return to their baselines.

Forbidden: checking after counter reset or tensor traversal; reimplementing
architecture identity from dimensions or keys; throwing from the public
entry; consuming the rejected census; adding a new result shape, type,
package, compatibility wrapper, or synthetic validator.

Acceptance: prove the pre-change production path accepts the Llama
configuration; after the change it returns exactly `E-GX-FAMILY` before all
measured work and the same census remains reusable. Run the GPT-2 bind, check,
and allocated suites; exact-diff typed-local and package lints;
`tools/error-code-lint.f`; then `maki/test.f`. Independently review the
implementation and its production-path proof before integration.

Claim: agent=codex-gpt2-family workspace=.jj-ws/habu-infer-gpt2-checked-54b99423
