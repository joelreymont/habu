---
title: Eliminate foreign package reopenings
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T22:50:09.471706+02:00"
---

Invariant: only the modules assembling an owner may reopen that package and access its private words; consumers call qualified public APIs or use a lexically bounded using import. Reopening another package is not an import. Current production and tests reopen owners such as CAD-NUM, MAKI, PROCESS-PTY, and configuration packages to define consumer-specific wrappers or inspect private state. The process environment module, for example, publishes process-prefixed projections inside CAD-NUM solely to reach private numeric conversions. This makes privacy dependent on convention and lets any source claim owner authority.

Census every package reopening by authenticated source identity and classify legitimate multi-file owner assembly, focused hostile fixtures, white-box tests, and foreign consumer privilege. Replace foreign production reopenings with public typed operations owned at the correct abstraction boundary; use using only for public names. Move white-box tests to public behavior where possible and give any indispensable friend access an explicit narrow checked capability, never ordinary reopen authority. After assembly, seal security- and type-authority owners through the existing sealed-owner capability so later source cannot reopen, publish, tick, postpone, undefine, or mutate private state.

Prove the exact census has zero unapproved foreign reopenings, source relocation cannot gain authority, children and using do not inherit private access, legitimate owner assembly remains deterministic, hostile reopen and every mutation sink reject, rollback leaks no authority, and public callers, snapshots, ahead-of-time compilation, bootstrap, recovery, fixpoint, Maki, PTX, package, host, and full native gates pass. Coordinate with the existing owner-seal syntax, checker, and migration dots rather than inventing a second seal. Measure public API count, compatibility wrappers, dictionary-name bytes, JIT, DATA, CODELEN, and load time before and after; require removal of privilege wrappers with no unexplained growth.
