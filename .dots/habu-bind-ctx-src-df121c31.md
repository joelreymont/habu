---
title: Bind context source diag witness slots
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T18:05:11.530764+02:00"
---

Full context: IR-CTX's SOURCES@ / DIAG@ / WITNESSES@ are typed unbound slots that fail closed with E-IR-CTX-UNBOUND until the source registry, diagnostic sink, and witness allocator modules land (their compiler-IR dots). When each lands, replace its throw with the real typed binder/reader, delete SLOT-UNBOUND from that slot, and keep the E-IR-CTX-UNBOUND negative regression for the remaining unbound slots. Depends on: habu-register-compiler-sources-fd495290 (sources) and the witness/diagnostic dots in the shared-compiler chain.

Update 2026-07-28 (source registry landed): IR-SOURCE exists (src/compiler/ir/source.f) — the registry is an IR-ARENA arena plus the module key; IR-CTX's HF-SOURCES slot stays unbound because an arena handle is a sealed nominal a stored raw cell cannot re-mint, so the context cannot yet persist the registry reference. Decide the storable form (arena generation serial plus a validating IR-ARENA resolve capability, or linear-ownership handles when they land), bind HF-SOURCES through a validating accessor, keep E-IR-CTX-UNBOUND for the diagnostic and witness slots, and add regressions that a bound slot round-trips while a forged slot value fails closed.
