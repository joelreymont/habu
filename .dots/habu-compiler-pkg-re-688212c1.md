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
