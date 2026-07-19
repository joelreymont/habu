---
title: Stale bin/hb survives install --force silently
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-19T11:27:20.163506+02:00\""
closed-at: "2026-07-19T12:03:42.353489+02:00"
---

Found 2026-07-19 while acting on the linux-size worker's caveat. Ground truth: the default workspace bin/hb (built 2026-07-18 14:36) is STALE relative to master - engine-affecting commits landed since (REG-PROTECT registry seal in type-family.f, the checker-hook migration 563b2540 with five pre-trust defers in the engine prefix, owner-persist checker changes), and the linux-size worker measured 52194 differing bytes vs a fresh master fixpoint build (both 147648 long). Yet BOTH recovery verbs no-op silently with rc=0 and empty output: 'bin/hb --load <9-lib prefix> tools/build-fixpoint.f -- install --force' leaves bin/hb untouched (same inode, same mtime), and '-- stdin' produces no hb-stdin artifact and no log output. Two failures to explain: (1) WHY the cached-install path treats a stale engine as current under --force - suspicion falls on the content-key cache (recently compacted/indexed, 5d6edc8e; budgets re-derived on the 'healed cache' 76f5e652) validating against a stamp that does not cover the engine-affecting drift, or on --force not reaching the install decision; (2) WHY the stdin verb exits 0 with no output and no artifact when invoked with a closed stdin from the repo root - if it requires an env (HB_TMP?) or an open stdin and silently exits instead of dying with usage, that is an error-masking bug in its own right and must fail closed. Task: root-cause both, fix properly (no workaround: install --force must either install the true fixpoint engine or die loudly; silent rc=0 no-ops are forbidden), install the true master fixpoint as bin/hb (new inode: temp + mv), prove with a byte-compare vs a from-scratch stage-convergence build, and run the full gate with the new engine. Related open dots: habu-split-hb-build-5fc098e2, habu-bind-cache-valid-3b6d1aba.
