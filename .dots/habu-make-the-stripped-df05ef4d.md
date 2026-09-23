---
title: Make the stripped link linear in the closure size
status: active
priority: 3
issue-type: task
created-at: "2026-09-22T09:02:57.342889+03:00"
---

Claim: agent=alder workspace=.jj-ws/alder-link-time, based on 45608866.
Measurement complete: the member binary search and single-copy fixes below
already landed, and the present stripped LINK takes 0.504 seconds. No new
linker change is warranted by the reported build delay. Awaiting Hazel's
closure review; the compiler load, not LINK or the snapshot writer, is the
measured expensive phase. Tender build latency takes priority over thin-binary
measurements.

Problem (measured by the reach study, recorded beside src/habu/aot-lib.f TEXT-ADR,): OLD>NEW and MEMBER-AT in src/habu/aot-lib.f scan every closure member per relocated instruction, so the stripped link is quadratic in NCLO. A build past 1 MiB of code costs minutes at test scale, which is why no fixture pins the far-ADR case and why Tender's standalone link time grows with its closure (Tender standalone: 1.36 MB of code, built by aspen). Acceptance: measure the link time of the Tender standalone build and of tools/hb-build-test.f's largest stripped fixture before; replace the per-instruction member scans with a lookup filled once per link (a member table sorted by entry with binary search, or a per-offset map over the closure); every image byte-identical before and after (cmp on the hb-build-test fixtures and the Tender image); link times reported after; then state in TEXT-ADR,'s comment whether a >1 MiB fixture is affordable and, if it is, add it (a stripped image past the ADR window pinned by tools/hb-build-test.f). The link is a compiler pass, so the before/after measurement is required, not optional. Files: src/habu/aot-lib.f, tools/hb-build-test.f. Verify: tools/hb-build-test.f; test/run.f; Tender's build with aspen. Depends: none. Ownership: hazel.

Before (aspen, hazel/integration a98ae8d1, engine 804041a8): standalone bin/tender 1,507,520 bytes links in 54.9 s wall (54.5 s user); server bin/tenderd 2,031,808 bytes in 69.3 s; both rc 0. On ec37691e (both still refused) 50 s / 61 s. Logs: ~/.cache/tender/habu-gaps/stripped-life-hook/strip-standalone-3.log and strip-server-3.log, binaries beside them.

Landed: 5312ac87 (member table sorted by entry, binary search) and 4b96c081 (nested closure member copied once), integrated at 9ba97aae; every hb-build-test fixture byte-identical, maker chain 45.2 s -> 42.8 s user. Open until aspen reports Tender's standalone link time on 9ba97aae or later (before: 54.9 s wall on a98ae8d1).

Current measurement (Alder): Tender main 4de21b0c; private export of PG source
133d7cf8 with its published a7eb1731 engine. Aspen clarified that the reported
10-11 minutes was the ordinary `build --server` (--repl) path, estimated from
neighboring log timestamps; those logs have no elapsed timer. The exact current
command, with private source, engine, HOME, cache and HB_TMP, passed in 57.22 s
wall / 56.71 s user and wrote a 43,712,704-byte image. The delay did not
reproduce. A phase diagnostic measured application load/compilation at 51.973 s
and the entire capture preparation/persistence/header/canonicalization/write
at 44.54 ms. The writer is not the bottleneck on this subject.

The same server's prepared stripped build passed in 56.19 s wall / 55.76 s user,
writing 2,162,880 bytes, including 1,951,640 code bytes. The diagnostic PREPARE
call is necessary only because this published PG pair predates its production
landing. No PG or Tender source was changed. The profiled stripped run also
passed (56.15 s wall / 55.78 s user): application load 53.462 s, linker load
2.144 s, LINK 0.504 s. Of 503 link samples, the largest named work is sparse
DATA serialization and process-map checks; OLD>NEW takes 15 inclusive samples.
The profile's diagnostic definitions change its image, so this is timing
evidence, not a before/after image-equivalence claim. No server runtime test
was performed.

The exact 1,100-word generated chain shape from tools/hb-build-test.f, measured
on green 45608866 / engine 3da80b23, has 1,104 closure members: load 1.169 s,
linker load 3.807 s, LINK 26.616 ms; total 5.02 s wall / 4.95 s user, rc 0.
Its 65,728-byte image (46,772 code bytes) executes and prints chain=ok.
The existing TEXT-ADR, comment correctly identifies compilation, not relocation,
as the cost of a >1 MiB synthetic chain. No fresh implementation or gate was
needed for this measurement-only follow-up to the landed lookup fix.

The ordinary stripped standalone command also passed on the green line itself
(Tender 4de21b0c, Habu 45608866 / 3da80b23): 50.73 s wall / 50.32 s user;
1,507,520-byte image, 1,393,248 code bytes. This supplies the post-landing
Tender standalone measurement requested above. It is not a controlled speedup
comparison with the older 54.9 s source/engine pair.

Application-load profile: IR-ARENA:RD@ 9.4% exclusive samples, RD-SIZE 4.5%,
PROT-SPAN 3.5%, RD-FIND 1.6%. This supports the existing a143be45 compiler-reader
dot; 67.3% is profiler `other`, so these named rows are not a complete cost
account. Inclusive sample counts overlap and must not be added.

Durable evidence: ~/.cache/habu/link-time/source-45608866/ (source archives,
exact Habu probe programs, baseline logs, timings, full text/JSON profiles,
byte-accounting reports and the plain images). No linker implementation changed.
