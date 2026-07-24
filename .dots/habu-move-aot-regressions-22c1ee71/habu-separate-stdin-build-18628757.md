---
title: Separate stdin build entry
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-24T03:18:37.429721+02:00\""
---

Why: both AOT variant builders currently parse src/habu/stdin.f to remove its
final GO call. A copied Forth scanner cannot soundly classify every parsing
word and is the wrong authority.

Owner and interface: keep all stdin metabuild definitions in
src/habu/stdin.f, wrap them in private package STDIN-BUILD, and remove the
top-level GO invocation. Add src/habu/stdin-main.f containing only a reopen of
STDIN-BUILD, the private GO call, and package close. Extend
tools/stdin-closure-lib.f with SDC-ENTRY$ as the fourth canonical path, host
and keyed. BF-EMIT-STDIN-RUN-SOURCE always appends SDC-AOT$, SDC-DRIVER$,
then the caller-supplied entry file; BF-STDIN-SOURCE passes SDC-ENTRY$.
tools/srclist.f and the audited bootstrap emitter produce the same order.

Migrate test/aot-wid-build.f to generate only a private STDIN-BUILD alternate
entry containing PWID-GO and its call. Delete all source reading, GO-tail
parsing, and prefix copying. No public STDIN-BUILD word, parser, compatibility
API, forwarding alias, duplicated GO body, or source-text splice remains.
Update the canonical closure lint, keyed-file inventory, TRUSTED inventory,
FILEMAP, and focused tests for the new role.

Checkpoint: exact native/fixpoint, bootstrap-source, protected-WID,
stdin-closure, repl-lint, package-diff, and typed-local baselines. A
representative private STDIN-BUILD wrapper must pass the package gate. Prove
the assembled normal and variant source order from the real emitters. Stop on
any external caller of a private stdin helper, emitted-engine byte drift,
bootstrap mirror not owned by tools/bootstrap.sh, or need for a second source
manifest.

Acceptance: two private-root normal builds produce the same engine bytes as
verified master. The protected-WID variant retains exact WIDs 300 and 70000
and warm publication rejects. Normal and variant sources each contain stdin
definitions exactly once and exactly one terminal entry call after them.
Missing, duplicated, reordered, commented, string-contained, or wrong-role
entry rows fail the structural closure/build tests. Bare and qualified
STDIN-BUILD helpers reject outside the closed package. stdin-closure lint
proves all four paths reach build-fixpoint, srclist, bootstrap, and run-files
from the canonical manifest.

Files: src/habu/stdin.f, new src/habu/stdin-main.f,
tools/stdin-closure-lib.f, tools/build-fixpoint.f, tools/srclist.f,
tools/stdin-closure-lint.f, tools/bootstrap.sh, test/aot-wid-build.f,
test/run-files.f, the existing focused closure/build/repl tests only where
their production assertions require updates, TRUSTED.md, and FILEMAP.md.
Smallest checks: bin/hb --load tools/stdin-closure-lint.f,
bin/hb --load test/aot-wid-suite.f, and the exact package/typed-local diff
lints. Run bootstrap codegen, build-fixpoint, candidate, native fixpoint,
host/filemap/trust, positive/negative AOT, and touched native gates; root
batches the full native gate.

Claim: agent=stdin_build_impl
workspace=.jj-ws/habu-separate-stdin-build-18628757.
