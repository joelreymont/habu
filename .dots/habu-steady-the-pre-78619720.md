---
title: Steady the pre-trust-defer fixture
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T23:44:54.605650+02:00"
---

test/pre-trust-defer.f is red from a plain shell on every commit tested back to 03ee73a9 (pre-campaign) — assertions 5-7: 'blanked drain leaves real prefix defers undrained, exits 73', 'undrained names the backstop diagnostic', 'undrained names the real prefix defer TFAM-RESOLVE-XT' — yet it has passed pooled gate-stdlib runs the same day and flaked red in others (2026-08-04: green in one full-gate run, red in the next, same tree). Environment-sensitive: the case spawns a child bin/hb with a blanked drain and asserts exit 73 plus diagnostics; whatever it depends on (tty/stdin state, env, machine load) differs between the pool and an interactive shell and between pool runs. Work: reproduce both outcomes, find the exact environmental dependency with a tool (capture the child's argv/env/fds in both contexts), make the fixture pin its own environment so the same tree gives the same verdict everywhere, and only then trust its verdicts. Until then it cannot serve as a landing signal. Not caused by today's landings — verified by running it at 03ee73a9 with the era-correct seed.
