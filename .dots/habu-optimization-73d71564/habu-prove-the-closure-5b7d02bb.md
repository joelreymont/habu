---
title: Prove the closure walker shakes at word grain
status: active
priority: 1
issue-type: task
created-at: "2026-09-22T11:30:53.090387+03:00"
---

Problem: Joel (2026-09-22): 'dead code should be eliminated during tree shaking. this is not being done correctly' - unproved either way. Probe: a stripped program whose MAIN calls one word of lib/string.f, sized against the 65,728 B stripped hello (~/.cache/tender/habu-gaps/stripped-determinism/hello-a): growth by about one word means word granularity, growth by the file or the package means the walker keeps more than MAIN reaches; then tools/hb-build-report.f's per-package split of Tender's bin/tenderd 1,783,176 code bytes, naming the packages that own them. Acceptance: both measurements recorded with the engine sha, the walker's granularity stated, the per-package table for bin/tenderd; a walker defect gets its fix and bin/tenderd is re-measured. Verification: the probe images and the report output under ~/.cache/tender/habu-gaps/closure-grain/, then Tender's python3 scripts/habu.py build --server. Ownership: alder. Claim: agent=alder workspace=.jj-ws/alder-size-probes.


Initial code-closure measurement on the same engine: stripped hello is 2,760
code bytes; adding two unreachable arithmetic words and an unreachable word
with a literal leaves code at 2,760 bytes; calling `STR=` from `lib/string.f`
raises code to 2,976 bytes. The code result is consistent with word-grain
reachability. The unreachable literal still adds DATA bytes (464 to 480), so
data reachability is not yet proven word-granular. `--size-report` also shows that the current stripped report has image-wide code/data classes, not a per-package code table; the requested Tender package split therefore needs a report change or a separate attribution probe.

Current owner: Alder. The historical `alder-data-root` workspace is retired;
current reproduction artifacts are in `~/.cache/tmp/habu-opt-round2/`.

Current macOS reproduction on source
88243cb4f4dc6297a1f365538c6629cdb4f536da and engine SHA256
2daeb34544e8c081209f2437485d76f606c61ca5ac7eafe8cc28ddf438845639
confirms that unreachable initialized DATA still travels. Empty MAIN and
empty MAIN plus a private, unreferenced 65,536-byte BUFFER filled with 65 at
build time both emit 2,156 code bytes. The unused array raises value payload
from 427 to 74,155 bytes (+73,728), bitmap from 153 to 1,179 bytes, and the
signed executable from 33,276 to 99,324 bytes. Both build and run with exit 0.
The real inputs, executable artifacts, hashes and reports are retained in
`~/.cache/tmp/habu-opt-round2/`; failure modes were recorded in
`data-reachability-plan.md` before creating the reproduction. This evidence
does not measure how much of Maki's warm snapshot is dead.

The responsible boundary still admits every in-window DATA address in
MAPPED-DATA, roots every nonzero declared quotation cell before CLOSURE, and
serializes every nonzero cell in BUILD-SPARSE-DATA. ADDRESS-CELLS records
pointer-bearing cells, not complete allocation extents or object lifetimes.
AOT-OWNED has explicit fresh/carried extents for named engine claims only.
Use actual allocation provenance to establish unreachable objects; the nearest
dictionary name and a zero value are not ownership or liveness proofs.

The bounded owner-interface design is recorded in
`~/.cache/tmp/habu-opt-round2/data-object-design.md`. CREATE/ALLOT do not retain
general allocation extents; sized BUFFER and typed-storage definers know sizes
while declaring but do not publish an authoritative object table. Ordinary
pointer storage and deferred-word relative-offset backing references show why
address-relocation rows alone are not a complete edge graph. Record exact
extent and reference/escape policy at storage introduction, retire that identity
on rollback/reuse, and conservatively retain unclassified spans/escapes. The
first dead-DATA omission can preserve all live addresses, mapping size and HERE;
it does not require a new codec or heap compaction. Extents alone are insufficient.

Current priority: fix DATA reachability before snapshot compression. The defect
is broader than quotation-cell roots. On Habu 806f0654 / engine SHA256
274f9bea9aa8a047ccefb2de3d80ab45a82fe24230f17a63be6a067bdb383cbd,
an empty MAIN has 2,444 code bytes, 464 carried DATA bytes and a 65,728-byte file.
Adding a private 65,536-byte array filled with byte 65 at load time, which MAIN
never reaches, leaves code at 2,444 but raises carried DATA to 74,192 and the
file to 131,264. Both stripped builds and executables exit 0. The 73,728-byte
DATA growth is exactly 8,192 unused cells encoded in nine bytes each. No
quotation or stored code pointer is involved. BUILD-SPARSE-DATA in aot-lib.f
walks every nonzero cell in the latched application window, independently of
code reachability. The existing sparse encoding does not remove dead DATA.

The root fix must emit reachable DATA objects and literals, following declared
pointer/quotation edges and retaining unknown escapes conservatively. It must
preserve interior pointers, aliases, mutable reachable arrays, initial contents,
and all relocation rows consistently. Do not clear a table merely because its
current values look unused, shrink a runtime limit, or compress the same dead
objects. Acceptance includes the unused-array reduction above and the existing
unused quotation-cell fixture, with positive cases for reachable aliases and
interior addresses. The compiler/checker seams remain Hazel-owned.

Normal current Tender default server build, from a private export of feb55f7d
on that same Habu pair: 2,228,416 bytes = code 2,037,592 / names 0 / DATA
143,113 / padding 34,573 / other 13,138. SHA256
dc85225bfbb485b2c154e37017602c389a5d9c8e74242fd0558566d4cb27b03f.
The restored DATA extent is 5,821,808 bytes. An offline decode partitions all
143,113 value bytes exactly: NSTR owner header 17, literal row tables 12,554,
literal arena 117,295, remaining DATA 13,247. The pool reserves 655,408 bytes
(40 header + 131,072 row tables + 524,296 arena); its 3,174 literals use
103,865 body bytes. These are all interned literals, not a measured live-only
set. The stripped runtime has no compiler lookup use for the row tables.

The remaining 5,166,400 bytes of DATA address extent include fixed application
columns and library scratch. Examples checked against declarations: four
212,992-byte DOC-OF/CODE-OF/MAIN-OF/HELD-OF arrays for 13 run slots, and
PROC-CMD's fixed argument/environment/input/output/error buffers. Zero
reservations already contribute no value payload and start as demand-zero
pages; reducing them is not equivalent to saving that many file or resident
bytes. Generated code is 91.4% of the current file, so code generation remains
the main route toward the parent's 500 KB target.

Aspen confirms that Tender already defaults the server to stripped and reports
green binary DB/HTTP and MemoryDenyWriteExecute migrate/serve checks on this
Habu pin; their logs were read in stripped-default-lane. The independent run
above verifies the current source's normal build and size. It does not claim a
new independent database gate or a speed comparison across source versions.

Evidence and checked Habu probes:
`~/.cache/habu/data-root/source-806f0654/` (README.md, current-build.log,
current-size.txt, strip-data.tsv, window-census.tsv, empty-size.txt,
unused-size.txt, and the sources and executables). The census's nearest named
word is only a locator: anonymous pools and does> storage between named
variables must be attributed from their actual declarations, not that label.

The package measurement is complete for Tender `08a1c3b6` with its specified
Habu source `cedar/pg` `df231977` and engine SHA256
`a7eb1731ec8169cf1b3881db3bce03919e09b1b7521147d44f0a2c88f541507d`.
The build wrapper's actual entry is `server/entry.f`, which loads
`server/main.f` and defines MAIN. All sources and the host were copied privately.
The unchanged stripped path still refuses MUNMAP-XT at data-off 11909760
(rc 70, existing `habu-run-image-lifecycle-bcb472b5`). Calling the existing
IMAGE-LIFECYCLE:PREPARE before BUILD-NATIVE lets the diagnostic link finish
(rc 0); this does not repair the production driver or prove server readiness.

That image is 2,031,808 bytes, including 1,863,460 code bytes (91.7%). The
10,342 closure members total 1,860,984 bytes; startup/glue accounts for the
other 2,476. Every member is attributed to one of 157 packages. The largest:

| Package | Members | Code bytes |
| --- | ---: | ---: |
| RULE | 856 | 297,940 |
| DOCX | 525 | 147,032 |
| SCRAPE | 779 | 113,536 |
| DOC | 536 | 87,352 |
| TENDER | 337 | 84,024 |
| global words | 565 | 68,840 |
| XLSX | 308 | 66,860 |
| XLSX-WRITE | 285 | 66,160 |
| JSON | 272 | 61,736 |
| HTTP | 345 | 55,788 |

Full table, raw member rows, image, engine sidecar, size report and Habu probes:
`~/.cache/tender/habu-gaps/closure-grain/source-08a1c3b6-engine-a7eb1731/`.
The image SHA256 is
`bbc3728116e202a039c8500e425508e70b7018a54cfbdd5dd844b37f5cae518e`.
The probe reads final BLEN values after LINK, so its own literals cannot enter
the capture. Named rows map through live package wordlist IDs. The 53 anonymous
rows match the engine's own sidecar by exact start and body extent, using that
sidecar's package IDs. No nearest-owner attribution or unexplained remainder.

Controlled capture comparison: identical Tender `5a876b0f`, release engine
`5db92eca` versus compacted candidate `3c7570bd` (`fd4880d7` on `1ba6e264`).
Both images have 9,425 members and 1,724,484 code bytes, byte-identical across
the full code range. The 131,072-byte engine reduction does not reduce this
already-stripped application. The two files differ only in four carried DATA
integers, each lower by the 66,456-byte removed engine code extent; their
semantic owners remain unidentified. This is not whole-image identity.
Evidence is in adjacent directory `source-5a876b0f-engines-5db92eca-3c7570bd/`.
Do not compare its size to the newer source as a compiler regression.

Remaining walker defect, reduced on candidate engine `3c7570bd`: empty MAIN
has 2,424 code bytes. Adding an uncalled arithmetic word leaves that unchanged.
Storing the same word in a TYPED-VARIABLE quotation cell that MAIN never reads
raises code to 3,172 (+748), carried DATA from 464 to 468, and relocation rows
from zero to one. The retained bodies are UNUSED (212), throw (396), and
(LREPLROUTE) (84); relocation startup adds another 56 bytes. All three probes
build and run with status 0. ELF padding makes all three files 65,728 bytes,
so file length alone hides this result. The source, images and member/size
reports are in the controlled-comparison directory above.

The cause is `aot-closure.f` COLLECT-XT-CELLS / XT-CELL-ROOTS: every nonzero
declared quotation cell in the captured window roots its target before MAIN
is walked. It proves conservative DATA roots, not package-level code retention.
Deleting XT-CELL-ROOTS alone is incorrect: the writer still serializes those
cells and relocates their values. A fix must prove the initial cell cannot be
observed from entry/startup, follow reachable DATA references and declared
pointer edges, retain unknown aliases/escapes, and omit a dead cell's carried
pointer and relocation consistently. No name-based deletion or unchecked
pointer assumption. This dot stays active for that fix; the ordinary
word-grain and package probes are complete. The production remeasurement
after bcb472b5 is recorded below.

Priority evidence for this remaining DATA-root gap: a read-only query after
the valid prepared image was written rebuilt the graph from MAIN while
ignoring ALL thirteen stored quotation roots. No image was produced from that
incomplete graph. It removes at most 124 members / 17,124 body bytes (0.92% of
this image's code): SCRAPE 16,776, SECRETS 320, global words 28. Much of that
bound is necessary runtime behavior: `server/scrape/ingest.f:1544` reads the
nine RUN-QT slots for TASK:ACTIVATE, `server/secrets.f:48/55/62` calls MEMORY-*
defers, and `lib/process.f:309` calls PROC-REAP-ARM. Thus this gap cannot explain
the server's roughly 1.86 MB of code; broader code-generation work remains the
size priority. Probe, root list and removed-member table are `root-bound*` in
the current-pair evidence directory. The reduced unused-cell defect remains
valid and open.

Production driver remeasurement completed on frozen Tender 4de21b0c with
Cedar's reconciled PG/PREPARE source 36f8d059d8d3 and engine SHA256
f42251c47062ea0595f2e933d22a2f627c03f01447c54218919948d9b7e05794.
Private exported source and engine copies ran the unmodified command
`python3 scripts/habu.py build --server --stripped -o <private-output>`:
rc 0, 63.50 s wall / 62.99 s user / 0.30 s system. No diagnostic PREPARE
injection. Result: 2,162,880 B, including 1,951,640 code, 140,178 carried
DATA, 58,062 padding and 13,000 other bytes. SHA256
98783b66ee385867e59c5e6d832f948aed98d0e1791b2d38139f4f1ff2234d47.
The executable reaches its usage dispatcher and returns the expected rc 2
for --help; this does not prove server/HTTP/database readiness.

Evidence: ~/.cache/habu/closure-grain/source-4de21b0c-engine-f42251c4/.
The image has the same size classes as the prepared old-pair diagnostic on
this Tender source, but different code bytes; no byte identity or speedup
claim is made. Cedar's pending download tree on Tender 985a866a still refuses
an undeclared pointer on this Habu pair, a separate application revision under
Cedar/Aspen investigation. The unused quotation-cell root defect above remains
open; the production PREPARE remeasurement is no longer outstanding.
