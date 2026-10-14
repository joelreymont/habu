---
title: Copy buffer growth in spans
status: open
priority: 1
issue-type: task
created-at: "2026-10-14T22:16:08.645278+03:00"
---

Otto measured src/core/dynamic-storage.f RESERVE copies oldbuffer cellbycell on growth, contributing real Dots dotshow residual33M gap along withlisting/offset. Reproducer dots-profile probe/jp modesdb/plain/cstore/cload; measure growth path separately fromindexdiv/calloverhead. Replace percell reconstruction with existing public byte/span copy boundary where semantics permit, preserve exact allocatedbytecapacity includingfractionaltail, oldmapping lifetime, failureatomicity, overlap/nonoverlap facts andtask/registrycleanup. Coordinate fixed-element cachedbound design habu-cache-dynamic-buf-aaa1f7e3 and density95829178. Inspect existinggrowth/capture/registrytests beforecode, revise genuinefractionaltail gap first; matchingnative/Wasm behavior, same-source measuredgrowth improvement, independent Astra review andcombinednative/convergence checks ifbaked.
