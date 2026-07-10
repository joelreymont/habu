---
title: "Compiler: package re-export capability"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T01:10:04.560312+02:00"
---

User-authorized compiler work for maki. Add the EXPORT defining word (bare `EXPORT NAME` in a package public section; no top-level EXPORT exists today - only OBJ:EXPORT+ object rows, no clash) that publishes an EXISTING word - same xt, same checked stack effect, same provenance - into the current package's public wordlist under its own tail, with no forwarding body and zero runtime cost. Fail closed: reject re-export of undefined words, private words from closed packages, and any re-export into/from sealed system packages or generated constructor packages (coordinate with maki-type-families sealing rules when they land; design must not conflict). Checker sees one word, two names; renderer/diagnostics show the defining package. Tests: re-exported word callable via both names with identical checked effect; rejection cases; AOT tree-shake keeps one body; snapshot/rollback safe. Files: src/core dictionary/package code + focused checker tests. EXPORT must participate in preverify/all-errors source replay like other declaration forms (TFAM PLAN item 5 lists EXPORT among replay-support forms). Prereq for: maki packages refactor.

UPDATE 2026-07-07 (TFAM 1-8 landed; this lane is capability-unblocked NOW but
file-collides with the live campaign). Studied the landed sealing + generated
constructor + source-replay seams this dot said to "coordinate with when they
land" — they have ALL landed on maki-type-families:

DEPENDENCY STATUS — every stated prereq is satisfied:
- Sealed system packages (TFAM 2b): TFAM/TYPE/MATCH are sealed via the checker
  friend-arena latch; a post-seal reopen dies E-SEAL-VIOLATION (die code 83,
  checker.f CK-E-SEAL-VIOLATION / CK-SEAL-LATCH-OFF). EXPORT must reject
  re-export INTO or FROM these using that latch.
- Generated constructor packages (TFAM 8): registered WIDs, closed-but-callable.
  Reuse the landed predicates installed in type-family.f: `TFAM-CTOR-PKG?`
  (CTOR-PKG?-XT, reopen reject), `TFAM-CTOR-EXTEND?` (CTOR-EXTEND?-XT,
  closed-package extra-tail reject), `TFAM-CTOR-WORD?` (CTOR-WORD?-XT, undefine
  reject). Re-exporting a generated word (e.g. `RESULT:OK`) under a SECOND public
  name is fine (they are closed but callable — look up/execute/postpone/compile
  allowed); ADDING a tail into a ctor package is not (CTOR-EXTEND? rejects).
- Source replay (TFAM 5): verify-source.f already dispatches declaration records
  (RECORD-TYPEFAMILY/RECORD-SUMTYPE at verify-source.f ~511; INCLUDE-EVALUATE
  audited boundary) and all-errors collects type/export support (CA-ADD-SUPPORT
  family). EXPORT slots in here as one more declaration form — PLAN item 5 lists
  it among replay-support forms and the seam exists.

=> EXPORT does NOT depend on TFAM 9/10/12/14/15. It is buildable today against
the landed 2b/5/8 substrate. Do NOT wait on the ADT runtime rungs.

BUT — HARD COLLISION (this is the real gating constraint): EXPORT edits exactly
the files the TFAM campaign still owns and is actively changing on
maki-type-families — src/core dictionary/package code, src/core/checker.f,
src/habu/verify-source.f, the all-errors tool, bootstrap/cg/forth.fs mirror.
TFAM 9 (construct/MATCH), 14 (ENUM), 15 (PRODUCT) each still add RECORD-* forms
to verify-source.f and rules to checker.f. So EXPORT is capability-unblocked yet
file-blocked: it must NOT be developed in parallel on the same files. Land it
either (a) after the campaign quiesces (9/10/12/14/15 merged), or (b) on the
campaign's own branch as a coordinated commit — never as a separate concurrent
workspace editing checker.f/verify-source.f. Confirm the FOO/;FOO decision
(docs/forth.md §Scope pairs): EXPORT is a single defining word, not a scope
pair, so no `;EXPORT` closer is owed.

EXECUTION PLAN: (1) EXPORT defining word in the src/core package/dictionary
layer: parse-name, resolve the existing xt in scope, publish a second public
tail pointing at the SAME xt with the SAME checked effect + provenance, no
forwarding body, zero runtime cost. (2) Fail-closed rejects wired to the landed
predicates above (undefined word; private word from a closed package; sealed
system package; ctor-package extend). (3) verify-source.f RECORD-EXPORT + the
all-errors support hook. (4) AOT tree-shake keeps one body under two names;
snapshot/rollback restores both names atomically. Tests: dual-name callable with
identical effect; each reject case; AOT single-body; rollback. Gate through the
exact owning `bin/hb --load` path plus the type-family suites (EXPORT must not
perturb sealing/ctor tests). Prereq for: maki packages refactor (spec 3, maki.f
re-export).

================================================================================
AUDIT 2026-07-10 (fable-export workspace, parent b0556262). Full seam map,
resolved design, and slice plan with acceptance criteria. Baseline green: bin/hb
smoke OK (`5 SQ .` -> 25); all-errors `export-support` case currently PASSES only
because it is a stub (see below).

--- CURRENT STATE: EXPORT is SCAFFOLDED, NOT IMPLEMENTED ---
1. Native engine (habu2.f): NO `export` keyword. EM-INTERPRET-DEFINE-KEYWORDS
   (habu2.f:3509) registers package/public/private/end-package/trusted:/defer/
   create/variable/constant/'/char/immediate — no export. So `bin/hb --load` on
   `EXPORT NAME` would treat EXPORT as an ordinary (undefined) word today.
2. Checker (checker.f): NO CHECKER-EXPORT prim/word. No alias mechanism.
3. verify-source.f RECORD-EXPORT (v-s.f:489) is a STUB: it only NEXT-SCANs and
   consumes the name token; it does NOT alias any signature. Dispatched at
   v-s.f:576 (`s" export" STR=CI IF RECORD-EXPORT`).
4. all-errors test CAE-EXPORT-SOURCE$ (check-all-errors-test.f:717) uses TOP-LEVEL
   `EXPORT CAE-EX` where CAE-EX is already a global word, so CAE-EX-USE resolves
   regardless of aliasing — the stub passes trivially. This test must be upgraded
   to a real cross-package alias to actually exercise the capability.
=> No real usage of EXPORT exists anywhere in loaded source; gate is green with a
   no-op stub. The whole capability is unbuilt below the scaffold.

--- DEPENDENCY SEAMS (all landed; reuse, do not redesign) ---
Seal latch: layout.f FRIEND-LATCH-CELL ($20) 0=open/sealed=nonzero; E-SEAL-PACKAGE
  84 (open/reopen), E-SEAL-VIOLATION 83 (protected write). Engine mirrors:
  C-STORE-DEF-NAME (habu2.f:1828) guards publish INTO a protected WID via
  LPROTWIDQ -> exit E-SEAL-PACKAGE; C-QUALIFY-SEAL-GUARD/C-SEAL-MATCH
  (habu2.f:1706/1729) reject NAME:tail into a reserved system package via RESTAB
  (habu2.f:995: tfam/type/match). Checker mirror: CK-SEAL-LATCH-OFF/
  CK-E-SEAL-VIOLATION (checker.f:4280).
Ctor predicates (type-family.f:397/413/420, wired checker.f:4128-4131 via
  CTOR-PKG?-XT/CTOR-WORD?-XT/CTOR-EXTEND?-XT, E-CTOR-PROTECTED 7111):
  CHECKER-USIG-CERT-ADD (checker.f:4412) already runs CTOR-EXTEND? on the NEW
  tail before recording — this IS the "reject adding a tail into a ctor pkg"
  rule. Re-exporting a ctor word under a second tail elsewhere is allowed (only
  the target-side extend is rejected).
Source replay (v-s.f RECORD-* dispatch, RECORD-DEFINER? v-s.f:545) + all-errors
  support (check-all-errors-core.f:181 CHECK-ALL-ERRORS-SUPPORT+, xsup-replay
  test check-all-errors-test.f:758): EXPORT is one more declaration form; the
  slot exists (RECORD-EXPORT), it just needs a real body.

--- KEY REUSE MECHANISMS (the crux findings) ---
Engine resolution: LFIND (a u --) sets x13=source dict-record ptr (0=undefined ->
  reject), x11=xt, x14=flags, honoring package scope (private->public->global).
  A foreign package's PRIVATE word simply will NOT resolve here -> "private from
  closed package" reject falls out of scoped resolution for free.
Dict record layout (48B DREC, layout.f:18): [0]=code ptr (body addr), [8]=body
  len, [16]=DNAME (len + flags: DNAME-EXT bit61, DNAME-WIDE bit62), [24]=inline
  name/ext ptr, [40]=WID. Alias = new record with [0]/[8] COPIED from source (SAME
  body, zero new code), [16]=tail len + copied source flags, [24]=tail bytes,
  [40]=current WID (CUR-CELL). Idiom to copy: C-QUALIFY-DEF nend-path
  (habu2.f:1798) / C-PACKAGE-NEW-RECORD (habu2.f:3393) — C-STORE-NAME, store
  [0]/[8]/[40], NDICT++, LHIDXADD. Publish guard C-STORE-DEF-NAME reused verbatim.
Checker alias (THE decision): CHECKER-EXPORT resolves the source active sym, reads
  its USIG record rows (ER.DIN/DOUT/RIN/ROUT/HASR @, checker.f:3131+ / FIND-USIG
  FEP path checker.f:4264), stashes those 4 arena offsets + hasr into locals
  (BEFORE any arena growth), then interns the NEW name via CHECKER-REC-NAME!
  (current-pkg public tail) and calls E-ADD-EFFECT (checker.f:3426) with the
  stashed rows. E-ADD-EFFECT -> E-BUILD-EFFECT deep-copies rows via E-COPY into a
  FRESH independent scheme for the new sym, recomputes wide?, and updates the
  HIDX-EFF cache. Wrap the CTOR-EXTEND? guard + CHECKER-CERT-DUP? exactly as
  CHECKER-USIG-CERT-ADD (checker.f:4412) does. IDEMPOTENT rule: if the resolved
  new sym already maps to the SAME source effect record, no-op (covers the
  global self-export edge). ROLLBACK-SAFE FOR FREE: the new sym (SYM-N) + new
  USIG record (UEND) sit under the standard RBF-PUSH/POP watermarks
  (checker.f:6772/6796), so scope/candidate/snapshot rollback retires the alias
  atomically — this is the type-family-rollback precedent, no new machinery.
AOT single-body proof: aot-closure.f CLOSURE/SCAN-REC (aot-closure.f:176/191)
  walk reachable records and map referenced body ADDRESSES via FINDADDR. EXPORT
  emits NO body; a call compiled through the alias name emits a BL to the shared
  source body addr, identical machine code to calling the source. Unused alias
  names are never reached; a used alias resolves to the one shared body addr ->
  one copy. Slice 3 must PROVE this with a hb-build closure-diagnostic showing a
  single body for a dual-name program (do not assert by inspection alone).
Reserved-name lint: add `export` to RNL-RESERVED-DEFINER? (reserved-name-lint-
  core.f:156, alongside package/typefamily/enum/product) so `: export ;` fails
  E-RESERVED-DEFINITION; add a negative case to reserved-name-lint-test-lib.f.
  (RESTAB sealing table is for package NAMES, not defn names — do NOT add export
  there.)

--- DESIGN FORKS (resolved) ---
F1 tail vs rename: TAIL-PRESERVING, no rename. `EXPORT SUBSYS:WORD` in
   `package MAKI public` publishes `MAKI:WORD` (maki dot 4655e01a: "curated
   re-export ... users call MAKI:*"). Matches "under its own tail" in the spec.
F2 checker alias representation: FRESH copied scheme via E-ADD-EFFECT (NOT
   cache-only aliasing — a shared record's ER.SYM is the source sym, so a HIDX
   cache miss/rollback would fail to re-find the new sym; NOT render->parse — no
   public "render stored effect to sig string" word exists and round-trip
   fidelity on quotation/named-row schemes is unproven). Copying rows is exact
   and reuses tested E-COPY.
F3 provenance in diagnostics: the copied scheme records under the new name's sym,
   so diagnostics name the alias. "renderer/diagnostics show the DEFINING
   package" is a refinement (store defining-sym backref on the record) — capture
   as a follow-up sub-dot if the base slices land first; base requirement is
   one-word-two-names + faithful effect.
F4 not a scope pair: EXPORT is a single defining word; no `;EXPORT` (docs
   §Scope pairs). Confirmed.

--- SLICE PLAN (each: byte-fixpoint x2 + FULL gate) ---
Two-commit staging per LESSONS ("stage a cross-layer capability"): the checker
word lands stage-compiled first so ANY parent binary builds commit B.

SLICE 1 (commit A — checker capability): checker.f: add CHECKER-EXPORT word +
  `PRIM: CHECKER-EXPORT PE-PTR-U8 PE-IN PE-N PE-IN PRIM;` (mirror CHECKER-UNDEFINE
  checker.f:4004/4648). Implement the F2 alias (resolve source sym+rows, CTOR-
  EXTEND guard, dup/idempotent, E-ADD-EFFECT copy). Focused checker unit test:
  alias a sym; both names FIND-USIG to equal-shape effect; a scope RBF-PUSH/alias/
  RBF-POP retires it; ctor-extend target rejects E-CTOR-PROTECTED.
  ACCEPT: checker.f self-certifies (STATUS count +1 for the new word body);
  byte-fixpoint x2 identical; FULL gate + 7 type suites green; new prim in prop
  census; CHECKER-EXPORT reachable/tested (not dead).
SLICE 2 (commit B — engine keyword + real replay + lint):
  - habu2.f: C-EXPORT (ARM64): C-TASK-LIVE-GUARD; LTOK; LFIND source (x13==0 ->
    undefined reject writing the token + rc); compute tail (strip PKG: via the
    C-QUALIFY-DEF qhas scan idiom); publish alias record copying [0]/[8], set
    [16]=tail+flags, [24]=tail, [40]=CUR; C-STORE-DEF-NAME publish guard (sealed/
    ctor-pkg target reject); NDICT++/LHIDXADD; then HOOK-guarded call to
    CHECKER-EXPORT (LCHKEXPORT via C-FIND-GLOBAL + push source name +
    C-CALL-X11-SAVED, mirror C-CALL-CHECKER-DEFER habu2.f:1340). Register `export`
    in EM-INTERPRET-DEFINE-KEYWORDS + LKWEXPORT label + `export`/`checker-export`
    rows in EMIT-KWDATA (habu2.f:1002). Idempotent same-binding no-op in the
    engine too (global self-export must not dup-die).
  - verify-source.f: RECORD-EXPORT resolves source name and calls CHECKER-EXPORT
    (replace the stub body).
  - reserved-name-lint-core.f:156 + test-lib: reserve `export`.
  ACCEPT: `bin/hb --load` on a package program that does `EXPORT EVAL:RUN` and
  calls MAKI:RUN AND EVAL:RUN with a declared checked effect through EACH name
  runs green; every reject case fails closed with the right code (undefined;
  private-from-closed = undefined at resolve; sealed target = E-SEAL-PACKAGE;
  ctor-extend target = E-CTOR-PROTECTED); `: export ;` = E-RESERVED-DEFINITION;
  all-errors/verify-source replay resolves the alias across files; byte-fixpoint
  x2; FULL gate + maki/test.f (76) + 7 type suites + error-code/namespace/host/
  filemap/dot-dep lints + trusted-inventory strict + typed-local-diff-lint +
  TRUSTED.md rows for C-EXPORT/CHECKER-EXPORT trust.
SLICE 3 (tests + AOT proof + docs): upgrade CAE-EXPORT-SOURCE$ to a real cross-
  package alias called through both names; add native dual-name + each-reject
  fixtures to the checker/engine gate; AOT single-body PROOF via hb-build closure
  diagnostic on a dual-name program; snapshot+rollback alias test; docs/forth.md
  §Packages EXPORT paragraph; STATUS.md count + note; close the dot with ledger.

--- RISKS ---
R1 C-EXPORT is hand-written ARM64 in the self-hosted compiler; a register/encoding
  slip bricks bin/hb + the byte-fixpoint. Mitigate: copy the C-QUALIFY-DEF nend
  publish idiom verbatim; honor the four engine facts (RX LPROT windows, per-leg
  LBCAP, x11 discipline for C-FIND-GLOBAL/CHECKER call, ADR/CBZ bare labels).
R2 tail extraction for `EXPORT PKG:TAIL` must match CHECKER-QUALIFIED? edge rules
  (leading/trailing colon = ordinary name).
R3 idempotent same-binding no-op must be consistent in engine + checker or the
  CAE global case dups.
Each engine/checker edit = a full byte-fixpoint + multi-suite gate cycle; these
  are separate commits, not one session. This audit is the artifact; SLICE 1
  starts the next session.
