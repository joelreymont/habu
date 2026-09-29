\ Select the optimizing compiler from OUTSIDE the file under test.
\
\ A suite whose own assertions are tier-1 facts states the tier itself, with
\ `1 set-tier` after its harness requires and before the code under test; the
\ gate runner prepends nothing. This file is for the other case: a caller that
\ runs ONE unchanged file at both tiers - the `*-aot` twin rows in
\ test/gate-stdlib-cases.f, and the parents that spawn a subject with
\ `--load test/compiler/aot-mode.f <subject>` - where the tier is the caller's
\ variable and not a property of the subject.
\
\ Only code compiled after this file belongs to the tier, so a twin row lists
\ the subject's harness (lib/test.f) BEFORE it: the subject's own require is
\ then a no-op and the harness stays on the default compiler. This file loads
\ nothing itself because some callers pass it inside a native build window
\ (test/primitive-trust.f, test/loop-obligations.f,
\ test/field-proj-boundary.f), where every file loaded is compiled by that
\ window.

package NATIVE-AOT-MODE
1 set-tier
;package
