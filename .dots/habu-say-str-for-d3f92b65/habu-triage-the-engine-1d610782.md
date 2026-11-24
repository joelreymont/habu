---
title: "Triage the engine's ^u8 n public rows"
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T23:12:49.125757+01:00"
---

Problem: Census: /tmp/jerry-p1/rows/pubrows.py (public rows holding ^u8 n or ^u8 NUM:byte-len). Engine, compiler and arch words: habu/link-code.f PATCH, PLACE; address-carrier.f SET-MOVABS; escape.f SPAN; compiler/digest.f SLOT!, SLOT@, SLOTS-COPY; compiler/native/dict.f SPELL-START, FIXED-VALUE; core/checker.f TOKEN$, TOKEN!, SPELLED, ROOM; checker-owner-guard.f VALIDATE; rejection.f POSITION; core/bytes.f BYTE+, BYTE@, BYTE-COPY; arch/*/link.f PATCH, ADDRESS!; arm64/icode.f CODE-BYTE+; wasm/kernel-words.f COPY-BYTES, SHA-BLOCKS; os/env-base.f ZBYTE@, ZBYTE!, ZPTR+; os/image-bytes.f M-BYTE+, M-BYTE@; tools/source-discovery.f EMIT. Some n are offsets, not lengths. Acceptance: every row whose pair is bytes says str, span or vector; an address and an offset stay and are named so. Depends: none. Claim: unassigned.
