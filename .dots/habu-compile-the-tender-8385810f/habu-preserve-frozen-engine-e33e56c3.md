---
title: Preserve frozen engine include facts through symlinked roots
status: active
priority: 1
issue-type: task
created-at: "2026-09-14T00:45:10.526415+03:00"
---

Owner: Cedar indexed-dictionary lane. G2 in a checkout with symlinked lib/tools/src
exits 78 at duplicate E-A-FIRST instead of reaching check.f's missing-engine
diagnostic (69). RESOLVE and ENGINE-PROVIDES? both lose lib/errors.f when its
canonical path leaves CWD. Relative, dot-normalized and absolute invocation-root
spellings all reproduce the miss; /tmp/cedar-alias-resolve-before.out and
/tmp/cedar-alias-cli-before.err retain the reduction.

The resolver keeps canonical identity and existing owner-local precedence. For a
fallback or absolute invocation-root request, a missed frozen alias may use the
normalized invocation-root spelling only if that single alias candidate resolves
to the original physical path. This rejects symlink/.. spellings that reach a
different target. No alias filesystem scan or application-name deduplication.
Discovery retains REQUIRE-BASE; ENGINE-PROVIDES? inspects the frozen prefix.

Acceptance: the registered source-root test starts a real child with symlinked
directories and checks portable spelling variants, missing baked source,
discovery isolation, owner-local same-name files and a distinct symlink/.. target.
The existing missing-engine CLI case retains its symlink fixture and exact
diagnostic, adding normalized and absolute entry paths.

Final include.f compiles at tier 1 through native-window-owner-child's actual
replacement-checker handover on G2: rc 0, `include source: ok` / `window: 0`, empty
stderr (/tmp/cedar-alias-source-window-final.{out,err}). This compiles the changed
source; it does not claim the baked G2 resolver changed. On unmodified G2 the
registered source-root test passes its existing checks before the new child
fails at duplicate E-A-FIRST; all three focused CLI spellings return 78 instead
of 69 (/tmp/cedar-alias-source-root-old.{out,err},
/tmp/cedar-alias-cli-focus-old.{out,err}). Root owns independent review and the
composed product build, followed by these real fresh-process acceptance tests.
