---
title: Debug running Habu apps by word
status: open
priority: 2
issue-type: task
created-at: "2026-10-14T13:31:31.732175+03:00"
---

Keep after incremental builds and test-suite work. Joel asked to compare native
symbols with improving the built-in debugger for connecting to running apps,
and delegated the choice through $best. Choose LLDB/GDB integration: Habu's
built-in entry traps print the interrupted stack and watches, then resume;
the token stepper runs REPL tokens. Neither provides an attach endpoint or a
paused command loop. External debuggers already own process/thread stopping
and continuation, so Habu should supply its missing language/code facts.

Static symbols for the stored payload alone are insufficient. habu2.f copies
LAOTCODE to the live region, rebases dictionary entries and relocates that
mapping independently of the executable. Publish debugger-readable,
package-qualified word names and spans at their actual live addresses,
including subsequently compiled words. Publish and retire metadata with the
owner of executable code; reuse authoritative dictionary/capture facts.
Use ordinary object symbols where sufficient and minimal name/range debug
records where required; do not mandate full DWARF or dSYM packaging. Preserve
hb.map for internal image analysis.

Acceptance: locally attach LLDB on macOS or GDB on Linux to an already-running
native Habu app; discover a known word, break there by name, remain stopped
until explicitly continued, inspect the selected thread's Forth data stack,
resume and detach while the app continues correctly. Cover captured and newly
compiled code, ASLR and optimized code. Resolve the current LLDB JIT-breakpoint
boundary recorded in docs/debugging.md rather than assuming names fix it.
Source stepping, typed locals, complete Forth backtraces and a custom remote
debugger service are outside this milestone. Relevant owners: debug.f,
debug-watch.f, stepper.f, habu2.f, capture and native image writers.
