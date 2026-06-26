---
title: Model engine-builder asm effects under hard hook
status: open
priority: 2
issue-type: task
created-at: "2026-06-24T17:47:16.441946+02:00"
---

Root cause: generated fixpoint sources now install the hard CHECK! hook, but src/habu/rt.f through src/habu/habu2.f are engine-builder/raw-asm emitters whose label/register/xt-execute effects are not expressible by the current checker. Fix now: tools/build-fixpoint.f and tools/bootstrap.sh emit an explicit 0 set-check / ' HOOK set-check bracket around that audited boundary; target image/sign/layout boundary effects are pinned with TRUST rows. Long-term fix: add typed checker/compiler support for the asm/codegen effect vocabulary so this generated boundary can be removed. Evidence: tools/build-fixpoint-test.f passes after the explicit boundary and trust rows.

2026-06-26 update: the broad generated unchecked span has been thinned. `rt.f`,
`crash.f`, `src/os/image-bytes.f`, and `src/habu/regalloc.f` are checkable in the
generated source. The remaining unchecked spans are now:

- target executable writer/signing (`src/os/<target>/{elf|macho,sign*.f}`),
  because phase-token words such as `ASM-CODE`/`BUILD-IMAGE` assert checker-only
  `asm`/`img` values with no runtime stack value;
- raw primitive/profiler emitters (`src/habu/habu1.f`, `src/habu/prof.f`),
  because register/label roles describe emitted-code effects;
- JIT/compiler emitters (`src/habu/jit.f`, `src/habu/habu2.f`), which still fail
  when the hook is reinstalled.

Evidence: progressive generated-prefix loads identified the first failing files;
`tools/build-fixpoint-test.f` now pins `regalloc.f` between a reinstalled hook
and the JIT unchecked span; native `tools/build-fixpoint-main.f -- stage` reaches
the compiler fixpoint.

2026-06-26 update: target image/sign files are no longer in the generated
unchecked span. `asm`/`img`/`snap` phase tokens are modeled as checked nominal
cells (`ASM-PHASE`, `IMG-PHASE`, `SNAP-PHASE`) and erased/preserved by the next
phase boundary. Linux and macOS target image/sign sources load under the hard
hook, and build-fixpoint/bootstrap now keep target image/sign checked. Remaining
unchecked spans: raw primitive/profiler emitters (`src/habu/habu1.f`,
`src/habu/prof.f`) and JIT/compiler emitters (`src/habu/jit.f`,
`src/habu/habu2.f`).
