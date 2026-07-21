---
title: Re-verify macOS perf verdict at next quiet window
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T13:03:14.911062+02:00"
---

The native suite perf verdict (test/run.f) on the Mac merge host is currently load-inadmissible: the laptop carries sustained external work (load average 12+, cal-factor observed up to 262 percent), so warm attempts return marginal-fail/inadmissible with correctness=t on every run. Trains landed 2026-07-21 afternoon under this condition with correctness fully proven (three full run.f correctness passes each, fixpoint x2 byte-identical, complete battery) and the perf verdict explicitly waived per the orchestrator with Joel's direction not to wait for a quiet machine. Owed: at the next quiet window run bin/hb --load test/run.f solo on master and record a performance=pass verdict; if it fails QUIET, that is a real regression to RCA against the day's landings (structure-decl/structure-make wiring, SC-QUOT rows, arity gate, decl-event) starting with the per-slice time attributions in the kept capture roots.
