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

Held at Joel's direction. Candidate 7289a859 must not be duplicated or chained
while the deployment shape and allocation causes are addressed. Tender's
server now defaults to stripped; its current feb55f7d / Habu 806f0654 build is
2,228,416 bytes with 143,113 carried DATA bytes and no dictionary. The earlier
16,777,408-byte candidate server and its roughly 6 MB encoded DATA were a
full REPL snapshot, not the deployment product. Root DATA reachability work
and its unused initialized-array reduction are on 5b7d02bb.

The dense baseline snapshot's final compiler registries were read offline:
SYMS reserves 2,621,440 bytes for 65,536 rows with 33,319 used; USIGS reserves
4,521,984 with 4,519,552 content bytes; NORET reserves 1,179,648 with
1,144,440 used. A symbol table's capacity, live compiler metadata and an
application's runtime storage are different causes and must not be called
one 6 MB preallocation. Old APH 2 MiB and IMC 4 MiB page reservations are
already RUN-MAPPED allocations in the current Tender source. The snapshot
census assigns unnamed storage to its preceding named word: CONN-LIVE's
10 MB interval was largely these persisted registries, not a connection buffer.

Hazel's source review also found an unresolved donor-format bug in 7289a859:
ENCODE-DATA compares two source constants, so its supposed old-donor guard
cannot detect an old running engine. Loading the new maker source on that
donor can write a v10 image that its baked loader refuses (exit 79). Resolve
the actual donor-format contract before this held change can be accepted;
the finding has not been waived or fixed by this evidence update.

This incorporates habu-store-the-snapshot-203a86a0, closed as a duplicate.
No separate snapshot codec implementation belongs on that dot.

Measured baseline: frozen Tender 4de21b0c, published PG source 133d7cf8 / engine
a7eb1731, built in private copies. Server image 43,712,704 B, snapshot DATA
32,827,688 B; standalone image 29,360,320 B, DATA 19,091,448 B. These are the
two current build targets; the old request for three images is obsolete.
Calculated existing grouped-codec payloads are 4,995,407 / 4,482,520 B;
maximal byte runs are 5,734,773 / 4,157,601 B. Trimming only trailing zeros
saves 287,652 / 3,945 B. Including extent framing and alignment predicts about
15.93 / 14.81 MB products before decoder growth; these are estimates, not
completed compressed images. The values alone exceed AOT's 4 MiB VAL-CAP on
both subjects, so snapshot scratch must be sized from its encoded content.
Evidence: ~/.cache/habu/snapshot-design/source-45608866/ and
~/.cache/habu/link-time/source-45608866/.

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
