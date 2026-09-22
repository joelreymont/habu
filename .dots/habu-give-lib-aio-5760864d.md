---
title: Give lib/aio.f a verb vocabulary and one submission shape
status: open
priority: 2
issue-type: task
created-at: "2026-09-22T16:48:07.899972+03:00"
---

Requested by Joel: make sure aio is well-factored and has a beautiful vocabulary. Read in full at 6aa16d76 (1,159 lines). Vocabulary: POLL-ADD leaks the io_uring op name where every other operation is a verb (READ, WRITE, ACCEPT, CONNECT, CANCEL, AWAIT) - rename to POLL; LOOP-START/LOOP-STOP/LOOP-RUNNING? read better as START/STOP/RUNNING? under the AIO: prefix; GROUP-MAX and MAX-OPS are public words shadowing private constants of the same name (`: MAX-OPS MAX-OPS ;`) - make the two capacities public constants with one naming order (GROUP-MAX, OPS-MAX). Factoring: the five *-STAGE bodies repeat the room-check-and-claim prologue (`1 SQ-ROOM? … REC-CLAIM … idx 0 < …`), the four submit wrappers and CANCEL-HANDLE repeat RUNNING-CHECK ENSURE-SCRUB lock/stage/release/throw/mint; XFER-OP and SOCK-OP map KIND to OP by if-chains where one KIND>OP table covers all eight kinds; AIO-ALIGN8 and ZERO-CELLS, hand-roll what lib/string.f BUFFER: allots; LE32@/LE32!/LE64@/LE64! are the fifth private copy in lib (ffi-abi.f:308, net/udp4.f:96, net/tcp4.f:140, crypto/evp.f:149) - one shared little-endian accessor module they all require. Consumers of the renamed words: lib/net/curl.f, lib/net/udp4.f, lib/net/tcp4.f, lib/serial.f, lib/pty.f, lib/signal.f, lib/process-pty-io.f, their tests, test/process-pty-*-smoke.f, test/engine-candidate-test.f, docs/aio.md (13 mentions), docs/threads.md; Tender requires lib/aio.f (aspen gets the migration). Acceptance: lib/aio-test.f and every consumer test green, docs updated, the public surface listed in the commit body. Ownership: hazel.
