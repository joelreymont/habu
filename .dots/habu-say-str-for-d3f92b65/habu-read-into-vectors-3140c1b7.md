---
title: Read into vectors, not caller buffers
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T23:12:49.095634+01:00"
---

Problem: Census: /tmp/jerry-p1/rows/pubrows.py (public rows holding ^u8 n or ^u8 NUM:byte-len). Words that fill a caller's ^u8 n buffer: src/lib/fs.f READ-ALL, READ-AT, READ-SIZED; fs-anchored.f FD-PATH; fs-list.f NAMES; process-pty-io.f AWAIT-BYTES; genio-core.f READ and DEVICE's read slots; aio.f READ, AWAIT-XFER; net/tcp4.f READ, TRY-READ, READ-EXACT (and private EXACT-STEP); net/tls.f READ. Words whose row says str for a buffer they write: net/tcp4.f RECEIVE-RAW, TRY-CHUNK, READ-CHUNK; test/wasm/data-test.f READ-DUMP (its dst). Joel 2026-11-19: it is either str or vector. Design: .out/designs/design-readvec.md (convention, each word's row, lanes 1-13). Acceptance: each appends to or fills a VEC:vector the caller owns; callers move with it; consumer owners are told before the install. Depends: none. Claim: unassigned.
