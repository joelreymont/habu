---
title: "Lex a definer's name as the loader does"
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T05:24:08.650591+02:00"
---

Problem: tools/check.f lexes the operand of a parsing definer with its own token rules, while the loader takes it with parse-name, so the two disagree on a name that starts a comment. Measured by the r4-nomname lane: DEFLINEAR ( loads rc 0 and the linear type ( is reachable in effects, but check.f lexes ( as a COMMENT token and CHK-WORD-TOK? (tools/check-core.f near :689) refuses it; DEFLINEAR \ probably diverges the same way (unmeasured). Related: pprlwrtl 'Skip the operand a parsing keyword takes'. Acceptance: check.f gives every parsing definer's name operand the token the loader's parse-name gives, so DEFLINEAR (, DEFLINEAR \ and the same spellings after the other definers check.f knows get the verdict their real load path gives; a name the loader refuses is still refused; cases through tools/check-test-lib.f written before the code. Files: tools/check-core.f, tools/check-test-lib.f. Verify: tools/check-test.f. Depends: habu-give-deflinear-and-7498dc6d. Ownership: check.f lexing of definer operands.

Also (review 63, on 989d40c0, same class: a definer whose operand is missing): a source ending in `DEFTYPE` makes check.f print `check.f: bad nominal type '` then `hb: uncaught throw code -3300` (E-VEC-BOUNDS: LINT-LEX:TOKEN at index k+1 past the end in CHK-NOM-PROSE / CHK-TYPE-JSON, tools/check-core.f:885), rc 67, no JSON under --json-errors; the loader throws -6001 rc 67. check.f must report a definer with no operand as a located refusal (prose and JSON) for every definer it knows, never index past the token table.
