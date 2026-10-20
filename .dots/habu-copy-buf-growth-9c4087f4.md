---
title: Copy buffer growth in spans
status: open
priority: 1
issue-type: task
created-at: "2026-10-14T22:16:08.645278+03:00"
---

Otto measured src/core/dynamic-storage.f RESERVE copies oldbuffer cellbycell on growth, contributing real Dots dotshow residual33M gap along withlisting/offset. Reproducer dots-profile probe/jp modesdb/plain/cstore/cload; measure growth path separately fromindexdiv/calloverhead. Replace percell reconstruction with existing public byte/span copy boundary where semantics permit, preserve exact allocatedbytecapacity includingfractionaltail, oldmapping lifetime, failureatomicity, overlap/nonoverlap facts andtask/registrycleanup. Coordinate fixed-element cachedbound design habu-cache-dynamic-buf-aaa1f7e3 and density95829178. Inspect existinggrowth/capture/registrytests beforecode, revise genuinefractionaltail gap first; matchingnative/Wasm behavior, same-source measuredgrowth improvement, independent Astra review andcombinednative/convergence checks ifbaked.

Implementation0bfbddef is present. A matched mechanism comparison under frozen21b4/573305bb restores its exact prior cell loop in otherwise identical growth scaffolding:288 growth events copy268434432 bytes. Three interleaved rounds with matched zero-cycle subtraction give median incremental3.311B to1.829B instructions (44.77% fewer),12.334 to6.812 instructions per copied byte,0.25 to0.13user+system seconds. Checksums and full64/128-byte capacities, including fractional element tails, agree. This isolates the copy mechanism, not a historical whole-application speedup. Final combined native qualification remains required.
