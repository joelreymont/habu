\ Select the optimizing compiler from OUTSIDE the file under test.
\
\ A suite whose own assertions are tier-1 facts states the tier itself, with
\ `1 set-tier` before its requires; the gate runner prepends nothing. This file
\ is for the other case: a caller that runs ONE unchanged file at both tiers -
\ the `*-aot` twin rows in test/gate-stdlib-cases.f, and the parents that spawn
\ a subject with `--load test/compiler/aot-mode.f <subject>` - where the tier is
\ the caller's variable and not a property of the subject.

package NATIVE-AOT-MODE
1 set-tier
;package
