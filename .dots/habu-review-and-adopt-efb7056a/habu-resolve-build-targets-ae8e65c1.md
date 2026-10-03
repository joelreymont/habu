---
title: Resolve build targets through one action model
status: closed
priority: 2
issue-type: task
created-at: "\"2026-10-03T22:32:28.806657+03:00\""
closed-at: "2026-10-04T01:59:16.301801+03:00"
close-reason: Implemented checked target/action records and aliases, separate identity/link/runtime/execution predicates, and guarded native build entries. Independent Astra review and focused correction review passed. Fresh product native suite 593/593 rc 0, generation chain rc 0 with gens 2-5 identical, and check-only Gforth recovery rc 0. Cross-target emission, Linux-host execution and x64 same-ISA execution remain outside this qualification.
---

Problem: tools/build-target.f holds one ambient cell and src/compiler/native/abi.f:31-41 derives the host contract from HB-TARGET-* predicates; there is no ResolvedTarget, alias resolver or separate compatibility predicates (PA-r2 §2-§4, P1b). Acceptance: ExecutionPlatform, CompilerProduct, ResolvedTarget and SameBuildIdentity/LinkCompatible/RuntimeAdmissible/ExecutableHere as checked records under a new package; existing --target labels resolve as aliases; T01-T06 and T15 pass; legacy CTARGET digests unchanged. Files: src/compiler/target/ (new), tools/build-target.f, tools/native-build-args.f, src/os/*/target.f, src/compiler/native/abi.f (host-descriptor split). Verify: full gate; test/compiler/target-resolve.f (new, registered). Depends: habu-add-the-wasm-4c32353e. Ownership: src/compiler/target/, tools/build-target.f. Lane: dave. Claim: dave, delegated P1b implementation on reviewed P1a prerequisite; public integration and independent review remain with the lead.

Implementation decision:

- Add package RTARGET with checked fixed values: ResolvedTarget holds a nominal
  profile and raw selected features; ExecutionPlatform holds the native process
  profile; CompilerProduct holds its executable target, enabled emitter families
  and default output profile; BuildAction holds execution platform and body
  target. BuildIdentity adds the existing action-input digest to ResolvedTarget.
  Profiles own layout, Habu/foreign ABI, runtime and image format; do not repeat
  those facts in each record or change CTARGET wire values and digests.
- Resolve the three existing labels to aarch64-apple-darwin,
  aarch64-unknown-linux-gnu and x86_64-unknown-linux-gnu. Include the adopted
  Windows and Wasm profiles. Windows CORE returns explicit unsupported until
  its ABI is implemented; an unknown ABI is never compatible with another
  unknown ABI. Emitter families describe requested compiler composition, not
  loaded registry rows. Do not invent board profiles for emitter composition.
- Keep four separate predicates: build identity compares target plus existing
  action inputs; link compatibility compares machine/layout/ABI/runtime while
  allowing compatible feature variants; runtime admission fits combined
  requirements to selected semantic features; executable-here checks native
  OS/process ABI/image/runtime. Physical host capability is not target authority.
- Expose RESOLVE, DEFAULT, PROFILE$, ARCH@, FEATURES@, CORE, PRODUCT,
  FOR-COMPILER, FOR-OUTPUT and BODY-TARGET@. Results own fixed values or static
  profile names; resolution borrows its input only for the call. CORE returns a
  checked result rather than substituting a native ABI for an unsupported one.
- Capture the executing platform before the target window through a small
  execution-host owner. Keep NABI:BINDING as a thin compatibility forwarder;
  leave native register/routine constructors at their existing owners.
- Adapt BUILD-TARGET through WITH/CURRENT around the common RUN-READY-RC
  boundary, including direct unit callers. Restore on return and throw; refuse
  nested legacy actions and HOST!/SELECT? mutations during an active action.
  Unknown labels leave pending selection unchanged. Existing fixpoint source
  appenders consume the same action rather than introducing a second manifest.
- Guard output execution by process ABI/image profile as well as architecture.
  Preserve unsupported-writer refusal; never skip smoke and then promote a
  foreign image as checked. Actual cross-target emission remains P3.
- Write the real registered target-resolve E2E before implementation. Cover
  aliases/defaults, unknown nonmutation, Windows compiler/emitter distinction,
  unloaded versus unsupported, same-ISA incompatible ABI, equal link profiles
  with different action digests, target feature limits despite a stronger host,
  and active-action mutation/restoration on normal and throwing paths. Retain
  native-build-entry coverage and repeatable profile/refusal output artifacts.
