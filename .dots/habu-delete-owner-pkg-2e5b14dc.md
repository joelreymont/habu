---
title: Delete owner package protection
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T22:10:33.849062+02:00"
---

Remove only ordinary owner-package get-current prot-wid-add registrations from lib/ffi-abi.f, lib/json-read.f, lib/ptx/cuda-driver.f, lib/task.f, maki/infer/safetensors.f, src/core/generated-declaration.f, src/core/generated-declaration-dictionary.f, src/core/generated-declaration-protection.f line owning its package, src/core/type-family.f, and src/habu/xref.f line owning the XREF package. Retain generated-constructor registration in GENERATED-DECL-PROTECTION:COMMIT, XREF-WORDLIST registration, all providers, registry state, C-PACKAGE-PROT-GUARD, protected publication and AOT restoration. Remove associated owner-reopen negatives from lib/json-read-test.f, maki/infer/safetensors-test.f, and test/type-field-owner-suite.f; delete test/seal-package.f and its exact gate enrollment; keep protected-constructor coverage in test/seal.f. Pre-change owning probe: require lib/json-read.f then package JR exits 84. Acceptance before M17: formerly protected owner packages reopen through production paths, generated constructor forgery still rejects 84, exact registration allowlist census, and typed-local/package diff gates. No suite, provider deletion, registry redesign, compatibility, docs, lint, or unrelated seal changes. Claim: agent=owner_pkg_impl workspace=.jj-ws/habu-delete-owner-pkg-2e5b14dc (RELEASED 2026-08-21: workspace gone, no live lane - gc).
