---
title: Leave zero-filled snapshot data out of the image file
status: active
priority: 2
issue-type: task
created-at: "2026-09-18T10:26:28.263918+03:00"
---

Problem: --repl snapshots write canonical DATA verbatim, including internal
zero holes. The baked engine and stripped applications already use grouped
cell bitmaps and unsigned LEB128 values. This is a snapshot size problem;
the measured server capture/write costs about 45 ms, not the reported minutes.
Ownership: snapshot writer. Claim: Alder. Hazel reviews before landing and
the integration chain runs the full gate.

This incorporates habu-store-the-snapshot-203a86a0, closed as a duplicate.
No separate snapshot codec implementation belongs on that dot.

Implemented in .jj-ws/alder-snapshot-compression on 806f0654. The shared pure
IMAGE-CELLS module owns bitmap grouping and canonical cell varints; one ARM64
projection serves baked, stripped and snapshot startup. Snapshot framing is
separate, with no address-cell ABI change. Old AOT/stripped decoder stencils
remain unchanged. Extraction also fixes CELL-V@ reading a tenth byte even for
a one-byte value: the guarded-page fixture was red before and is green now.

Controlled comparison: frozen Tender 4de21b0c on Habu 806f0654 / engine
274f9bea, using private source, engines, HOME, XDG_CACHE_HOME and HB_TMP.
Server image: 43,516,096 -> 16,777,408 B (61.4% smaller). Standalone:
29,163,712 -> 15,663,296 B (46.3% smaller). The engine stays 3,604,672 B.
Snapshot scratch is sized from actual content; the server's stored DATA is
6,081,940 B for 32,851,489 decoded bytes, beyond AOT's fixed value-buffer cap.
The earlier projections on the old PG pair are superseded by these products.

In the quiet startup comparison, 51 runs averaged 12.716622 -> 12.804586 ms
for the engine (within noise), and 34.902446 -> 31.193486 ms for server --help.
The first compressed candidate took 38.920216 ms; full-cell DATA copying with
a byte tail removed that regression. These are warm-file-cache starts, not
serving throughput. At exit_group after --help, RSS is 83,472 -> 57,392 KiB;
peak RSS is 83,472 -> 71,168 KiB. No database-serving RSS claim is made.

Final three private generations are byte-identical, SHA256
3417b6322da994163c75fcb527d26ccc638981cb7422632628905efb6fbb0851.
Check-only bootstrap passes. The compression fixture pins canonical identical
captures, the million-byte hole, zeros overwriting a nonzero baked digit table,
dense fallback/admission, corrupt framing/varints/padding and recapture.
The artifact-row fixtures use the extracted codec's qualified names; their
owning aot-chain-capture row passes after the full gate exposed the stale names.
Both completed gates pass 502/502 on the same registry; the new compression
row passes separately and is included in the committed registry. Baseline:
455.95 s wall / 1747.71 s user. Candidate: 484.36 s wall / 1846.48 s user,
6.2% more wall time and 5.7% more user CPU. An unrelated Odin job briefly
overlapped the candidate run, so the wall comparison is not fully isolated;
the CPU increase remains a measured cost and is not dismissed as load noise.
Those gate timings precede the final emitter refactor. The shared
COPY-INNER, alone took 713 ms to compile; extracting its bitmap-group loop
cuts maker dependency loading from 4.706 to 4.069 s (baseline 3.785 s).
LINK stays about 20 ms. A fresh small stripped build now takes
11.004 -> 11.631 s wall and 10.859 -> 11.498 s user CPU; its image is
byte-identical. Final-tree factor1/factor2/factor3 are also identical to the
gated engine above. Recovery bootstrap, aot-cell-values, snapshot-compression
and aot-chain-capture pass after the refactor. No compiler/checker changes.
Evidence: ~/.cache/habu/snapshot-design/source-806f0654/paired/ (README, products,
hashes, commands, focused checks, startup, RSS and gate logs).

Accepted design and required verification:

1. Use the existing grouped bitmap + unsigned LEB128 grammar. Keep the
   48-byte trailer geometry and distinguish the changed outer format as v10.
   Its offset-32 field is the stored DATA extent; that stream begins with a
   u64 decoded extent followed by the existing G/S, presence map, bitmap and
   values. Preserve named legacy-format refusal rc 80.
2. The bitmap determines the number of values. Bounds, bitmap geometry,
   canonical varints and decoded extent must be validated; malformed images
   refuse rc 79. Alignment zeros follow the values inside the stored extent;
   require them all zero and their length below PROT-PAGE-MAX.
3. Decode into fresh anonymous scratch, validate address-cell and protected-WID
   headers there before mutating live code/DATA, then copy the restored bytes,
   zeros included, and release scratch. Keep the dictionary/code region raw.
   Engine-prefix calculation and relocation subtract the stored extent, not
   the decoded extent. Recapture must not embed the previous snapshot.
4. Factor the shared grammar into one pure module used by both writers and
   readers; do not create a second codec. Do not inherit AOT's fixed buffer cap.
   Do not bump a format that did not change: the address-cell schema stays
   unchanged unless a concrete schema change requires otherwise. The Gforth
   recovery seed's separately documented v3 format is also unchanged.
5. Measure before and after: bin/hb file size, single engine startup time,
   full-gate wall time on the same workload, and RSS of a started server image.
   Include pointer-dense and arbitrary dense DATA; a sparse result alone cannot
   prove the codec is appropriate for dense images. If v10 enlarges or slows
   dense images, retain dense geometry chosen by encoded size at write time,
   with version-based admission, or use a raw-group form if measurements
   justify it. Resolve that choice from measurements, not a runtime flag.
6. Encoding must be canonical. Pin a million-byte hole, exact restored memory
   including zeros over seed-populated cells, malformed framing/varints/pad,
   legacy refusal and recapture. Three private generations with gen2 == gen3
   byte-for-byte prove deterministic artifacts. Run every owning registry row,
   update imgdump and size attribution, and have the integration chain gate it.
7. Optional only after measurement: restore zeros over the seed-populated
   extent rather than touching the entire destination. The current loader
   already maps live DATA private/anonymous and byte-copies the whole saved
   extent into it (EM-MMAP-DATA-REGION / EM-SNAPSHOT-COPY-DATA); restored DATA
   is not presently left as a lazy shared file mapping. Measure RSS and scratch
   peak anyway. The released bin/hb is a grouped-AOT image, not a dense snapshot.

Expected files: src/habu/snap-lib.f, the snapshot loader and shared codec
callers, src/habu/layout.f, tools/imgdump.f, tools/image-size-lib.f,
test/snapshot-writer.f, tools/build-fixpoint-test.f, codec/size fixtures and
docs/native-applications.md. Any address-cells.f change must state why its
format or version ownership needs to change. No compiler/checker changes.
