\ cal-spin.f - standalone host calibration probe.
\ schedule-lint: allow-unscheduled - a host calibration probe, not a suite: it
\ MEASURES the box, so a gate row would make a phase verdict host-dependent.
\ test/cal-spin-lib.f documents the pair; skills/habu-host-profiles/SKILL.md runs it.
\
\ Prints the wall-clock milliseconds of ONE run of the gate's fixed calibration
\ spin (test/cal-spin-lib.f CAL-SPIN:MS) in a freshly spawned process, then
\ exits. Run it to read what this box measures the spin at:
\   bin/hb --load test/cal-spin.f
\ That number is what a host profile's reference is derived from, so this is the
\ tool for deriving or checking one. See skills/habu-host-profiles/SKILL.md.
\
\ Why a FRESH process rather than a reading taken inside a long-lived one: the
\ spin is pure integer work with zero in-process drift (twenty back-to-back spins
\ in one process all measure the same ms), so what moves the number is core
\ PLACEMENT. On a heterogeneous host (DGX Spark GB10: 10 Cortex-X925 performance
\ cores at 3.9GHz measure the spin at 87ms, 10 Cortex-A725 efficiency cores at
\ 2.8GHz measure it at 132ms) a process that has been blocking for a while has
\ usually been migrated onto an efficiency core, and a spin started there and
\ finished elsewhere reads a blended time that describes neither core. A freshly
\ spawned CPU-bound child inherits the full (unpinned) affinity mask and is
\ placed on a performance core from its first instruction, so it reads the
\ performance-core figure every time.
\
\ The measurement lives in test/cal-spin-lib.f rather than here because the
\ quiescent ratchet phase (test/json-read-perf-phase.f) brackets its own run with
\ the same spin, and the shared drift tolerance beside it only means one thing if
\ both callers measure the same way.

require test/cal-spin-lib.f
require lib/fmt.f

CAL-SPIN:MS FMT:.U cr
