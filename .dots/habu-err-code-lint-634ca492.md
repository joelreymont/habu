---
title: "error-code-lint: range-aware reservation enforcement"
status: open
priority: 2
issue-type: task
created-at: "2026-07-08T21:17:57.887531+02:00"
---

tools/error-code-lint.f enforces one owner per claimed negative E- code, but E-*-FIRST/E-*-LAST range sentinels (lib/errors.f blocks) are only excluded from claims, not modeled as reservations: a foreign file minting a code INSIDE another subsystem's declared FIRST..LAST range is not flagged until the owning block mints that exact member. Extend the core to parse FIRST/LAST pairs as [first,last] reservations and flag any claim inside a foreign reservation (same allowances as today). Add fixtures to tools/error-code-lint-test.f: foreign claim inside a reserved range flagged; the owning block's own members pass.

## CLOSE-READY

Implemented in tools/error-code-lint-core.f. Reservation model:

- Each `-N constant E-<STEM>-FIRST` / `E-<STEM>-LAST` sentinel is now recorded
  into a reservation table keyed by (stem, declaring-file). Stem = name minus the
  `-FIRST`/`-LAST` suffix; FIRST/LAST for the same stem in the same file pair into
  one `[min,max]` inclusive numeric range (orientation-normalized, so a mislabeled
  pair still works). A reservation is only enforced once BOTH bounds are seen;
  an incomplete pair reserves nothing.
- Ownership is by the declaring file: `ECL-CLAIM-FOREIGN?` flags a claim whose
  code lands inside a reservation's range AND whose file differs from the
  reservation's owning file. `ECL-RES-FINDINGS` runs alongside the existing
  collision pass; both increment `ECL-BAD`.
- Reality check (matches lib/errors.f): every FIRST/LAST pair and every member of
  each 100-code block (-2000..-3999) is declared in lib/errors.f itself, so all
  members are same-file and pass. No FIRST/LAST pair is declared outside
  lib/errors.f. All foreign negative E- claims live at -5000..-7999, outside every
  reserved span. Live strict run: 648 files, 269 claims, 20 reservations,
  0 findings (unchanged clean). No real foreign-range claim existed; nothing had
  to be renumbered or whitelisted.
- Kept all prior allowances (positive codes, sentinels excluded from claims,
  exact (code,name) re-registrations, bootstrap/ not walked).
- Fixtures added to tools/error-code-lint-test.f (MECLT-RESERVATIONS) via new
  two-file helper ECL-COUNT2: foreign claim inside a range flagged (incl. both
  inclusive boundaries); owner's own members pass; foreign claim outside range
  passes; incomplete reservation reserves nothing.

Gates green (true rc captured): error-code-lint-test.f rc=0 (test: ok, live 0
findings); error-code-lint.f rc=0; maki/test.f rc=0 (76 PASS/0 FAIL);
filemap-lint rc=0; dot-dep-lint rc=0; typed-local-diff-lint rc=0.
