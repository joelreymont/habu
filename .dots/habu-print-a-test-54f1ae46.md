---
title: Print a test assertion failure on one line
status: active
priority: 2
issue-type: task
created-at: "2026-09-16T12:48:43.077476+03:00"
---

Problem: lib/test.f prints an assertion failure as two lines, 'assert: expected 3' then 'got 9', because the number printer '.' emits a newline; every quoted failure in commit bodies and reports has to be split to match, and a one-line grep of recorded evidence finds nothing (rowan, Maki demonstration chain, 2026-09-16). Acceptance: T= and the other numeric assertions print 'assert: expected 3 got 9' on one line (format the numbers without a trailing newline, lib/fmt.f or a local digit writer that lib/test.f can require without a dependency cycle), the string assertions keep their multi-line form where the strings themselves contain newlines, nothing in test/ or tools/ parses the old two-line form (rg for the patterns and fix any parser found), docs mention the format where the test library is documented. lib/test.f is baked into the engine prefix, so the change needs an engine rebuild to show in child engines. Files: lib/test.f, its test (lib/test-test.f or the nearest), docs/forth.md if it documents the assertion output. Verify: a deliberately failing assertion through a rebuilt bin/hb prints one line. Depends: none. Ownership: hazel line. Claim: unassigned.

Claim: alder, .jj-ws/alder-assert-doc on 1afd910c, remaining documentation only.
Implementation is already on the line: lib/test/assert.f:74-85 uses FMT:.INT;
TAT-TEST-ONE-LINE in lib/test/assert-test.f runs a deliberately failing child
and checks "assert: expected 3 got 9". The complete test-stdlib row passes on
private native gen3 31be3fb0 (/tmp/alder-pty-eintr/logs/test-stdlib.log).
The source search finds old-style emitters in independent whitebox fixtures,
but no parser depending on the split numeric assertion form. docs/stdlib.md's
test framework section now states the existing numeric and string formats.
