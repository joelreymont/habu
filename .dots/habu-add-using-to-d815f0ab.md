---
title: Add using to the stage0 recovery compiler
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-29T22:50:34.706774+02:00\""
---

Full context: capability gap proven by agent pkglayout 2026-07-30. The Gforth recovery host bakes a keyword table into its stage0 engine (bootstrap/cg/forth.fs:2475-2478) that declares package, public, private, ;package but NOT using/;using; LKWUSING and C-USING exist only in src/habu/habu2.f. tools/bootstrap.sh concatenates src/habu/layout.f with habu1.f/habu2.f/jit.f/prof.f/regalloc.f/xref.f into one source interpreted by stage0, so any using line in those files breaks no-binary recovery at the first hop. Stage0 DOES understand qualified NAME:WORD (forth.fs:1379-1380, 3043). Implement using and ;using in the stage0 keyword table and compiler with the same semantics habu2.f gives them, plus a recovery-path test (HABU_BOOTSTRAP_CHECK_ONLY=1 tools/bootstrap.sh green on a source containing using). This unblocks packaging src/habu/layout.f without requalifying ~2500 bare references. Prerequisite for finishing habu-give-layout-f-315df2ca.

Claim: agent=stage0using workspace=.jj-ws/habu-add-using-to-d815f0ab

MEASURED 2026-07-30 (agent stage0using)

Semantics read out of the native engine before porting anything.
- src/habu/habu2.f:4723-4730 states the contract: `using NAME` makes package
  NAME's PUBLIC wordlist visible to bare lookup until the matching `;using`, the
  enclosing `;package`, or the end of the load file. Only the public wordlist id
  joins the search, definitions still land in the current scope, qualified
  NAME:WORD is unchanged, and the used-publics search runs only AFTER the
  open-scope and global chain miss, so an import can never shadow a name that
  already resolves.
- src/habu/habu2.f:4732-4749 (C-USING-NAME-GUARD): consume the next token; no
  token prints `hb: using: missing package name` and exits 89; a token holding a
  ':' prints `hb: using: package name must not contain ':': ` plus the token and
  exits 90.
- src/habu/habu2.f:4752-4764 (C-USING-WID): linear dictionary scan for a package
  record (wid == -1, folded name == token); the record's first cell is the public
  wordlist id. No such package prints `hb: using: unknown package: ` plus the
  token and exits 91.
- src/habu/habu2.f:4767-4778 (C-USING-PUSH): a depth of USE-MAX (16) prints
  `hb: using: too many concurrent usings: ` plus the token and exits 92;
  otherwise USE-WIDS[depth] = wid, the name is mirrored to the checker through
  `checker-using` at the pre-increment depth, then depth++.
- src/habu/habu2.f:4788-4798 (C-END-USING): depth 0 prints
  `hb: ;using without an open using` and exits 93; otherwise depth--.
- src/habu/habu2.f:4808-4871 (EMIT-FIND-USED): one dictionary pass over records
  whose wid is one of USE-WIDS[0..depth); a token containing ':' never resolves
  here; a second distinct match prints `hb: ambiguous bare word resolves in
  multiple used packages: ` plus the token and exits 94.
- Scope ends: src/habu/habu2.f:4676-4677 saves the depth at `package` open and
  4719-4720 restores it at `;package`; src/habu/habu1.f:1287 saves it into the
  evaluate frame and habu2.f:6326-6335 restores it when the frame exits cleanly;
  habu2.f:6247 restores it when a frame unwinds on error; habu2.f:6476/6493 do
  the same across a REPL line.
- src/habu/layout.f:549-572 fixes the data band, and src/core/checker.f:5388
  hard-codes `$9C08 constant CK-USE-DEPTH-OFF`, so the band offsets are a
  cross-file contract: the stage0 engine compiles checker.f during recovery, and
  the checker reads the live depth at that exact DATA offset.

What the recovery compiler now carries (bootstrap/cg/forth.fs): the identical
data band (measured USE-DEPTH-CELL=$9C08, DATA-START=$9CA0 — same numbers as
layout.f and the checker's constant), the six engine-error codes, the `using` /
`;using` / `checker-using` keyword bytes, C-USING-NAME-GUARD / C-USING-WID /
C-USING-PUSH / C-USING / C-END-USING / EMIT-FIND-USED, the checker mirror call,
the used-publics fallback at all three native call sites (interpret dispatch,
compile-time call, `'`), the `package` / `;package` save+restore, and the
evaluate-frame save+restore on the clean, undefined-word and preflight-miss
paths. Stage0 keeps its own local conventions: it exits with the engine-error
status instead of the native catchable compile-die, it has no task-live guard
(neither does its `package`), and it takes no REPL-line snapshot (neither does
its `package`), so the native USE-RPKG-SAVE-CELL offset stays reserved and
unread. The dictionary flag word is assembled exactly like this file's own
EMIT-FIND (stage0 records carry no internal / min-input bands).

Differential proof — the same 15 sources run on the recovery-emitted engine and
on native `bin/hb`; every exit status and diagnostic matched:

  gforth -e "require test/nf.fs s\" F.f\" slurp-file s\" BIN\" FORTH-EXE bye"
  BIN                       vs   bin/hb --load F.f

  case            stage0                              native
  positive        rc=0 out=1                          rc=0 out=1
  after ;using    rc=70 tok=PW                        rc=70 E-UNDEFINED: PW
  private word    rc=70 tok=PHID                      rc=70 E-UNDEFINED: PHID
  ambiguous       rc=94 hb: ambiguous bare word ...   identical
  unknown pkg     rc=91 hb: using: unknown package: NOSUCH   identical
  ':' in name     rc=90 hb: using: package name ...   identical
  ;using alone    rc=93 hb: ;using without an open using      identical
  after ;package  rc=70 tok=PW                        rc=70 E-UNDEFINED: PW
  qualified       rc=0 out=2                          rc=0 out=2
  no name         rc=89 hb: using: missing package name       identical
  17 usings       rc=92 hb: using: too many concurrent usings: P1  identical
  in a : body     rc=0 out=1                          rc=0 out=1
  ' NAME          rc=0 out=1                          rc=0 out=1
  evaluate scope  rc=70 tok=PW                        rc=70 E-UNDEFINED: PW

Recovery-path gate. tools/bootstrap.sh now runs five stage0 engines built from
real `using` sources before the fixpoint stages (bootstrap_using_gate):
test/bootstrap-using-src.f (rc 0, `7 7 7 3 9 BOOTSTRAP-USING-OK`),
-unknown (rc 91), -ambiguous (rc 94), -scope (rc 70; `;package` and evaluate
scope ends), -checker-hook (rc 0; the engine hands the package name to a
stand-in CHECKER-USING). Each case compares the whole of stdout, the whole of
stderr and the exit status. Falsified by mutating the engine, one at a time:
dropping the interpret fallback -> rc 70 on the positive case; disabling the
second-match test -> ambiguous case silently returns 9, rc 0; deleting the
`;package` restore or the evaluate-frame restore -> scope case rc 0 with an
extra `7`. Each mutation reds the gate (measured exit 75).

  HABU_ALLOW_BOOTSTRAP=1 HABU_BOOTSTRAP_CHECK_ONLY=1 GFORTH=gforth \
    HB_TMP=<tmp> tools/bootstrap.sh          -> exit 70

  The five using cases pass; the run then stops where the SAME run on the
  unmodified parent tree stops, with the same message:
  `hook: non-certified definition: install at 'is'`, exit 70, after hb-stage0 is
  built and while it compiles the concatenated source. The failing definition is
  src/habu/xref.f:208 `: INSTALL ( -- ) [: LIVE ;] is PKG-LIVE-XT ;` (line 35938
  of the generated stage2-src, the first `install` containing `is` after the
  check hook is installed at line 20549). This is a PRE-EXISTING break of the
  no-binary recovery path on the base tree, not a regression from this dot —
  proven by running the identical command on a copy of the parent commit
  (baseline exit 70, identical message). It needs its own dot.

Other gates on this tree:
  bin/hb --load tools/bootstrap-codegen-test.f      -> exit 0 (new BCG-USING
    parity clauses across forth.fs, habu2.f, layout.f, engine-error.f,
    checker.f and bootstrap.sh; renaming C-USING-WID in the mirror reds it, exit 1)
  bin/hb --load tools/bootstrap-mirror-lint.f       -> exit 0
  bin/hb --load tools/bootstrap-mirror-lint-test.f  -> exit 1, 41 findings —
    identical on the parent tree (pre-existing, src/compiler/target.f ENUMs)
  bin/hb --load test/using-test.f                   -> exit 0 (native unchanged)
  bin/hb --load tools/typed-local-diff-lint.f -- <jj diff --git>  -> exit 0
  bin/hb --load tools/package-diff-lint.f -- <jj diff --git>      -> exit 1,
    44 findings. 40 are in bootstrap/cg/forth.fs and are NOT specific to this
    change: measured, adding a single trailing comment to the body of the
    existing global word BCOUNT in that file reports
    `E-PACKAGE-OWNERSHIP bootstrap/cg/forth.fs:811:3`. The lint gives
    src/habu/habu2.f a named narrow category (ENGINE-BODY-EDIT?) for exactly
    this reason but has no entry for its Gforth mirror, which is a Gforth
    program and has no `package` word available at all. The other four findings
    are load-bearing globals: CHECKER-USING in the hook fixture must be global
    because the engine finds it with a bare global lookup, BUS-SHADOW and
    BUS-CALLER prove the additive/global-wins rule from true top level, and
    BCG-MAIN is the pre-existing runner whose body gained one call.
  bin/hb --load test/gate-stdlib.f: baseline (before the change) 1 red phase,
    compiler-ir-id. After the change the pool reported 5 (compiler-ir-id,
    trusted-inventory, check-cli-boundary, refine-lint and refine-lint-fixtures,
    the last two kind=TIMEOUT-UNDER-LOAD). Every one of the five is GREEN when
    rerun alone on the changed tree, including compiler-ir-id
    (tools/trusted-inventory-test.f, tools/check-test.f, tools/refine-lint.f,
    tools/refine-lint-test.f, test/compiler/ir-id.f all exit 0). A second full
    run on the changed tree reported 2: compiler-ir-id again and
    check-cli-boundary, whose failure is an uncaught E-PROC-TIMEOUT (-2502), a
    subprocess that ran out of time. The machine was running several agents; no
    phase is reproducibly red on this tree.

Gaps left open: the `checker-using` mirror is proven with a stand-in hook on
stage0, but no recovery build has yet compiled a `using` line in a checked
region with the real src/core/checker.f, because the recovery path stops earlier
at the pre-existing xref.f failure above. The concatenated recovery source has
no file boundaries (tools/bootstrap.sh cats every source into one stdin
program), so a `using` left open at the end of a file leaks into the next file
on the recovery path only; consumers must close with `;using` or keep the import
inside a `package` block.
