# Restart

Habu is a self-hosted native ARM64 Forth with a stack-effect checker. This
note orients a restart on the current line. For build/recovery mechanics see
docs/bootstrap.md, for native debugging see docs/debugging.md, for the
campaign map see docs/roadmap.md, and for durable technical findings see
LESSONS.md (entries are dated).

## Where things stand (2026-09-16, release integration)

The release head of the guard-page line is 6406e0b0 (later commits on the
same chain are docs and dot bookkeeping only). The bookmarks
`cedar/compiler-integration` and `hazel/integration` point at that chain's
tip and the root `bin/hb` is the engine built from it. Its qualification,
recorded in dot habu-qualify-habu-for-9ccd0432:

- Two builds of 6406e0b0 by successive generations are byte-identical
  (5,832,896 bytes), and two builds from different directories are too: the
  engine no longer bakes its build directory (boot require rows are recorded
  by portable name, src/core/include.f REQUIRE-BOOT-OPEN).
- The full gate, `bin/hb --load test/run.f`, is 402 suites, all green, in
  about eight minutes on a quiet machine.
- The Gforth recovery chain (`tools/bootstrap.sh`) reaches "bootstrap check
  OK" on the same sources; the seed mirrors the native emitters for guarded
  stacks, BRUNSTACK, the boot-registry open token and the byte-exact local
  lookup. The retired `bootstrap/habu-cg.fs` code generator is deleted.

What the engine now guarantees:

- The three VM stacks (data, return, loop) sit on guard pages
  (STACK-GUARD:EMIT-MAP in src/habu/rt.f); a fault is named by
  src/habu/crash.f as `hb: stack bounds exceeded (data|return|loop)`, exit
  102. `run-in-stack` admits only a page-aligned, page-sized extent outside
  the DATA region and throws E-STACK-UNGUARDED otherwise; the checker still
  makes underflow in compiled code impossible and the interpreter keeps its
  own depth floor (E-UNDERFLOW, exit 70).
- A local binds a reference only in its declared spelling; word lookup stays
  case-insensitive. Three resolvers agree (src/core/checker.f LOC-REF?,
  src/habu/habu2.f EMIT-LOC-FIND, src/compiler/native/elaborate.f LOCAL-OF)
  and test/compiler/native-local-case.f pins them together.
- Number printers in lib/fmt.f leave the shared string builder alone;
  imgdump `--pc` answers the word that owns a pc in baked and snapshot
  images alike.

## Build and gate

- Rebuild: `bin/hb --load tools/native-build.f -- <out>`, about 96 s on this
  machine; the result is a byte fixpoint of its own source.
- Full gate: `bin/hb --load test/run.f`, about 8 minutes quiet, 402 suites.
- A trivial program starts in about 22-35 ms.
- The stripped AOT DATA image is sparse; the AOT section budget is
  measured by test/aot-data-sites.f.

If `bin/hb` is missing or stale, follow docs/bootstrap.md. For a native
crash, follow docs/debugging.md.

## Next

Joel's order: release quality first, then optimizations. With the release
integrated, the parked lanes resume in this order, each as its own dot:
tier-1 inlining of trivial primitives (habu-inline-trivial-engine-922133ca,
commit e87a1066 in .jj-ws/hazel-inline-prims, verified), the tied
identity-copy elision, the internal profiler with caller attribution
(habu-build-the-internal-4cd07a82), heap table sizing, and the gate wall
clock. Post-release follow-ups from the audit: the retired seed codegen
residue (habu-cut-the-residue-17303e6c), several pcs per imgdump scan
(habu-answer-several-pcs-d63a3fc8), the lifecycle removal race
(habu-serialize-hook-removal-bd32a964). aspen's Tender runtime lanes (C5 in
docs/roadmap.md) develop off this head and land through hazel in one batch.

Work proceeds in small `.jj-ws/hazel-*` workspaces, one per dot, each
landing onto the chain with `jj duplicate`. Run `dot ready` for the open
list; docs/tracker-rebuild.md records how the tracker was rebuilt today.
