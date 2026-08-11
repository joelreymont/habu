---
title: A failed defer assignment reports a bare token and rc 70
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-11T09:18:10.134037+02:00\""
---

Found by the fixpkg lane (2026-08-11) while packaging hb-build: 'is' resolves the name it parses through the engine's own lookup, which does not consult used publics, so a bare target under an open using-import fails - and the failure prints ONLY the bare token (e.g. 'HOOK') to stderr with rc 70: no error code, no file, no line, no hint that using does not cover parsing words. Probe on record in the lane report ([: MINE ;] is HOOK vs is DP:HOOK). Fix the diagnostic: the engine's is-resolution failure should name the word, the source location, and state that parsing words resolve outside using-imports; consider whether the checker should reject a bare defer target under an open import outright (same family as habu-checker-defined-answers-1504bbde - scope-explicit resolution for parsing words). Files: engine defer/is implementation (src/habu/habu2.f or src/core), src/core/checker.f. Depends: none.

Checker-refusal half, probed 2026-08-11 before writing any diagnostic, and the
reason this repair is the message and not a new rule:

- The ambiguous case is ALREADY refused. A global `defer DHOOK`, a public
  `DPKG:DHOOK`, and a bare `is DHOOK` under `using DPKG` - with a different word
  in the quotation, so the `is` target is the only bare reference - is stopped by
  the checker at the reference site: `E-USING-SHADOW-GLOBAL habu: bare 'dhook' is
  ambiguous under using ... qualify dpkg:dhook`, throw 7141, rc 67.
- The case that actually produced the bare token is structurally out of the
  checker's reach. With NO global of that name and only a used public `defer`,
  `is DHOOK` is not refused and cannot be: `is` parses its target, so the token
  never becomes a checked reference for the name query to resolve. Making the
  checker see it would mean routing a parsing word's target through reference
  resolution - a real capability, not a bug in this dot.
- The hint's advice is checked, not guessed: `is DPKG:DHOOK` compiles and runs
  (rc 0) on the same program where the bare tail dies. The message names a fix
  that works.

Claim: agent=fixpkg workspace=.jj-ws/habu-fixpoint-pkg
