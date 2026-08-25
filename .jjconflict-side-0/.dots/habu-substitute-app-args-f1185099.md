---
title: Substitute application args in walk
status: open
priority: 2
issue-type: task
created-at: "2026-07-27T18:10:13.519842+02:00"
---

Residual conservatism pinned by the round-six family-schema work (fixture pgen:arg in test/type-family-suite.f): the shared pure walk performs no substitution of application arguments for parameters, so ptr ref<n> - a pointer to a closed application of a generic whose body touches its own parameter under its own pointer - is refused although the bound argument is an ordinary integer. This is the mandated trade while parameter kinds carry no non-linear bound. Owned result, either shape decided at freeze: (a) parameter kinds gain a non-linear bound the walk consults (the direction the round-six contract itself names), or (b) the walk binds an applied family's parameters to the application's argument nodes when crossing a family boundary (pure, threaded like the existing self/under-pointer/boundary-reset parameters - no environment state; note the boundary-reset invariant from the deletion: an applied family's parameter always names an argument root of that same application, which is what makes substitution well-defined). Constraint inherited from the same work: no production grammar can spell a family application with arguments (unified declarers reject parametric heads; the legacy grammar bypasses the query and is deletion-scheduled), so acceptance fixtures are registry-assembled plus real production closes, per the committed pgen:* pattern. Acceptance: pgen:arg's refusal flips to accept with the argument proven ordinary; the laundering cases still reject; per-edge kills preserved; the exponential-walk design dot (habu-bound-family-schema-c136b7d8) decided first or jointly - substitution deepens the walk and must not land against an unbounded cost model.

FROZEN 2026-07-27, shape (b), from the round-three review of candidate
5e631ed1febbfc78 against 212ac99975f2fc27 (blackboard message
20260727-162831.406-codex-0a32 on channel habu-validate-family-schema-34f9e1de).
That review rejected the candidate's Boolean boundary reset as a patch rather
than the long-term-correct fix, and named the shape this dot must implement:
carry an application-argument substitution ENVIRONMENT while walking the applied
family; resolve each PARAM through its exact argument root; and reserve
conservative rejection for parameters that are genuinely unbound, not for every
parameter the walk meets. Note this supersedes the "no environment state"
parenthetical in shape (b) above - the freeze chose an environment carried
through the walk, not a further scalar threaded beside the existing self and
under-pointer parameters. The measured defect the review recorded, in that
candidate's own coordinates: the walk discarded an applied family's parameter
bindings at src/core/type-family.f:1867 and :1873-1875, so a valid
registry-built ref<a> whose member is ptr a, instantiated as ref<n>, is
non-linear by TFAM-CONCRETE-LINEAR? and yet a family containing it was rejected
by both real closes, STRUCTURE pgsaferef and ENUM epgsaferef each returning 7109
where 0 was correct. Those coordinates belong to the candidate lane and are not
master line numbers; re-measure on the tree of the day. The accepted controls in
that candidate changed the family body to plain<a> and therefore never tested
the exact safe binding ref<n>, which is the hole this dot closes. Required
acceptance twins: ref<n> ACCEPTS and ref<linear> REJECTS, each proven through
BOTH real closes rather than through one of them.

Dependency correction, same day, from blackboard message
20260727-163418.305-codex-53f3 on channel habu-validate-family-schema-34f9e1de.
Registry-built fixtures may unit-test the traversal, and that is all they may be
claimed to do; they do not satisfy this behavior's production-path acceptance.
So this dot takes one of two routes and must say which at dispatch: either it
blocks on unified application syntax landing first, so a real close query can be
reached from source; or it pairs every registry case with the real current
producer path that can create the same valid application metadata, which today
means the retained parameterized parser in src/core/sumtype.f - it still builds
argument-bearing SCHEMA-APP nodes, including the `fam<arg,...>` path around
line 1233, and those nodes are valid current metadata rather than corruption.
Synthetic construction on its own never closes this dependency, and no wording
here may imply that it does.
