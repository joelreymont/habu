---
title: Bind context source diag witness slots
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T18:05:11.530764+02:00"
---

Full context: IR-CTX's SOURCES@ / DIAG@ / WITNESSES@ are typed unbound slots that fail closed with E-IR-CTX-UNBOUND until the source registry, diagnostic sink, and witness allocator modules land (their compiler-IR dots). When each lands, replace its throw with the real typed binder/reader, delete SLOT-UNBOUND from that slot, and keep the E-IR-CTX-UNBOUND negative regression for the remaining unbound slots. Depends on: habu-register-compiler-sources-fd495290 (sources) and the witness/diagnostic dots in the shared-compiler chain.
