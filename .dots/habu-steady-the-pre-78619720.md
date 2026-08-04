---
title: Steady the pre-trust-defer fixture
status: active
priority: 2
issue-type: task
created-at: "2026-08-03T23:44:54.605650+02:00"
---

test/pre-trust-defer.f is red from a plain shell on every commit tested back to 03ee73a9 (pre-campaign) — assertions 5-7: 'blanked drain leaves real prefix defers undrained, exits 73', 'undrained names the backstop diagnostic', 'undrained names the real prefix defer TFAM-RESOLVE-XT' — yet it has passed pooled gate-stdlib runs the same day and flaked red in others (2026-08-04: green in one full-gate run, red in the next, same tree). Environment-sensitive: the case spawns a child bin/hb with a blanked drain and asserts exit 73 plus diagnostics; whatever it depends on (tty/stdin state, env, machine load) differs between the pool and an interactive shell and between pool runs. Work: reproduce both outcomes, find the exact environmental dependency with a tool (capture the child's argv/env/fds in both contexts), make the fixture pin its own environment so the same tree gives the same verdict everywhere, and only then trust its verdicts. Not caused by today's landings — verified by running it at 03ee73a9 with the era-correct seed. Same class, same investigation: SUITE stdlib-process-fixtures (tools/hb-cli-contracts-test.f, tools/standalone-load-test.f, test/lint-cli-standalone-load.f) flaked red in two of three pooled full-gate runs on 2026-08-04 while every member was green standalone each time — child-process fixtures under the pool. Fix both with the same environment-pinning discipline.

Findings (2026-08-04, measured):

1. The plain-shell red was NOT environmental and NOT a flake. It reproduces on every
   launcher, including a pooled full-gate run (gate-stdlib --pool-slots 3: RED
   pre-trust-defer kind=exit code=1, same assertions 5-7). Cause: the exit-73
   SEAL-CAPTURE backstop is shadowed by an earlier, equally fail-closed refusal.
   With the drain blanked, src/habu/xref.f INSTALL ([: LIVE ;] is PKG-LIVE-XT) is
   the first CHECKED `is` on an undrained pre-trust defer, has no checker-defer
   row, and the check hook rejects it: exit 70, "hook: non-certified definition:
   install at 'is'". Proof the backstop is alive: blanking that one `is` site, or
   check-hook.f's INSTALL call, makes the same tree exit 73 naming TFAM-RESOLVE-XT
   and 31 more. Fix: one case per guard (70 for the checker, 73 for the backstop
   with the hook blanked, plus a control proving the blanked hook alone boots 0),
   and every child-rc assertion now goes through CHILD-RC, which prints the child's
   own stdout/stderr and the launch context on a mismatch.

2. There IS a real launcher dependency, found from a tty: the capture spawn passed
   -1 for the child's stdin, so the child inherited the fixture's fd 0
   (src/habu/habu1.f SPAWN-DUP2-ACTION skips the dup2 for a negative fd). From a
   terminal the bare child engine found a tty, entered the REPL, and stopped on
   SIGTTOU as a background process group: 21s timeout, E-PROC-TIMEOUT, exit 67.
   The fixture now spawns with an explicit empty stdin, so a pipe, a terminal and a
   pool slot give the same verdict. test/gate-pool.f GT-POOL-SPAWN has the same
   -1 stdin for every pooled job: dotted separately as habu-give-gate-pool-a22663a5.

3. stdlib-process-fixtures does not carry this class - its three named members all
   spawn through RUN-ARGV-STDIN-CAPTURE-OUTCOME (explicit stdin) and time
   identically from a tty and from a pipe - but its flake WAS reproduced and
   named: test/lint-cli-standalone-load.f gives each child a hard 20s budget and
   folds a timeout into "did not exit", and tools/refine-lint.f standalone costs
   ~9s idle but 21.4s and 26.6s at load average 10. Two of four runs of the
   six-member line went red on exactly that assertion while three full gates ran
   alongside. Dotted as habu-un-flake-lint-2535cef6; not fixed here, because the
   repair is a derived budget plus a timeout that reports as a timeout, not a
   luckier constant.

Tool kept: lib/test/spawn-report.f (child-outcome + launch-context reporter) and
tools/launch-context.f (`-- child` reports what a capture-spawned child inherits),
documented in docs/debugging.md.

Claim: agent=steady-fixtures workspace=.jj-ws/habu-steady-the-pre-78619720
