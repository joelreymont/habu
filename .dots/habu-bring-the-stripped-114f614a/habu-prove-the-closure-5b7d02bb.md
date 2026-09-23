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

Current owner: alder, workspace `.jj-ws/alder-closure-attribution`.

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
pointer assumption. This dot stays active for that fix and the production
Tender remeasurement after bcb472b5; the ordinary word-grain and package probes
are complete.

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
