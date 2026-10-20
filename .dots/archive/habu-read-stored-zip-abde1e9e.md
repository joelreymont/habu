---
title: Read stored ZIP entries without libzip
status: closed
priority: 2
issue-type: task
created-at: "2026-10-15T07:57:34.475574+03:00"
closed-at: "2026-10-15T08:30:52.038371+03:00"
close-reason: "All-STORE unencrypted archives now read the owned snapshot through shared bounded input/CRC/drain logic; mixed/encrypted archives retain eager libzip. Independent Astra review and16 focused libzip parity probes CLEAR after ZIP64 repairs. Installed source ZIP/lifecycle/framing/count/offset/concurrent/XML, stripped ZIP/XML/TMPDIR refusal, REPL XML and compressed Kaizen Office source/native pass. Exact public engine/map/XML unchanged. Frozen Tender55f/Kaizend175 six-open native workload passes: observed incremental physical7.28147888 to4.96875 MiB (31.76% lower), user14.80 to14.42s, sys0.54 to0.31s; faults1 after. This is one controlled run with32KiB threshold margin; requested repeatability work continues separately, no broad memory guarantee."
---

The unchanged all-STORE Office workload still peaks at 7.48463 MiB above baseline. libzip is acquired for every archive although the owned bounded snapshot, directory and CRC machinery can read STORE entries directly. Select one archive backend from central metadata: native only when every original member is STORE and unencrypted; otherwise preserve eager libzip OPEN/read behavior. Keep public acceptance/refusal boundaries, count/index identity, CRC/drain/child/source/commit/save lifetimes and prefix/ZIP64 framing. Add only distinct existing-fixture behavior gaps before production edits, run affected ZIP source/native/REPL/lifecycle/concurrent and framing consumers, obtain independent Astra review, merge/push and qualify an immutable source-only release on the unchanged installed engine before the unchanged Office physical-footprint comparison.
