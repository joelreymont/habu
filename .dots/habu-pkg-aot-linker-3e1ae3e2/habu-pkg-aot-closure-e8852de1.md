---
title: Package AOT closure
status: active
priority: 1
issue-type: task
created-at: "2026-07-23T21:59:36.655320+02:00"
blocks:
  - habu-pkg-aot-linker-7b8acef6
---

Why: src/habu/aot-closure.f is the private closure engine consumed by the linker, but its record walkers, branch decoder, address classifiers, closure tables, and mutable cursors remain ambient globals. Dependency: AOT-LINK must already own the linker and maker. Owner and interface: reopen package AOT-LINK in src/habu/aot-closure.f. Export nothing; the public AOT-LINK surface remains exactly LINK and RUN. Absorb AOT-BRANCH into the same private owner and replace internal qualified calls with bare calls. Replace direct test access to private closure words with generated source that reopens AOT-LINK inside the forked maker or evaluator; no test-only production export. Preserve direct B and BL decoding, code-entry resolution, closure limits, unsupported-word rejection, relocation, persistent-data pointer classification, absolute-chain rejection, diagnostics, and output bytes. Files: src/habu/aot-closure.f, test/gate-aot-positive-lib.f, test/gate-aot-negative-lib.f, TRUSTED.md and FILEMAP.md only if exact inventory requires them. Checkpoint: real positive and negative AOT baselines plus one complete representative package wrapper through the exact package gate; stop on any unplanned external caller or new public seam. Acceptance: the real tools/hb-build.f path and standalone/resident positive and negative phases retain exact results. Bare REC, CLOSURE, FINDADDR-PTR, CELL-TEXTPTR?, and AOT-BRANCH names reject outside AOT-LINK; their qualified private forms also reject while the package is closed. The only public AOT-LINK words remain LINK and RUN. Package, typed-local, trust, file-map, AOT, fixpoint, and touched native gates pass. Mutation proof: publish AOT-BRANCH and the public-surface check fails; move any closure definition outside AOT-LINK and the package gate fails; remove the generated-source package reopen and the exact private-path fixture fails. Forbidden: public raw tables or pointers, compatibility globals, aliases, copied closure validators, temporary packages, entry-selection changes, process-isolation work, publication hardening, or cache redesign.

Contract correction: src/habu/aot-lib.f is also in scope only to replace its
existing AOT-BRANCH:DIRECT? and AOT-BRANCH:TARGET calls with bare DIRECT? and
TARGET calls inside the already-open AOT-LINK package. No other linker hunk is
owned by this leaf.

Destruction correction: the generated positive fixture must carry the complete
literal identity inventory of every colon, block, create, variable, constant,
value, and defer definition in `src/habu/aot-closure.f`, including multiple
makers on one source line. Every inventoried tail must resolve in the private
`AOT-LINK` wordlist and reject both bare and public-qualified lookup. Check the
exact inventory cardinality; an exact count without the identities is
insufficient. Keep the existing whole-private-wordlist exposure scan. A hostile
mutation that moves the package opener below `AOT-DBASE@` must make the real
positive AOT gate fail even before `package-diff-lint` runs. Do not add a source
parser, production export, test-only alias, package split, or hard-coded total
as a substitute for complete identities. This correction changes only
`test/gate-aot-positive-lib.f`; any production-content change voids the accepted
code review and requires a new blind review.

Claim: agent=aot-closure-pkg workspace=.jj-ws/habu-pkg-aot-closure-e8852de1.
