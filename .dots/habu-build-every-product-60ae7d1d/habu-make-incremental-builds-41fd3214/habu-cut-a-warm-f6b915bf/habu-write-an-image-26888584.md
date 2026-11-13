---
title: "Write an image's bytes a span at a time"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T04:37:15.093256+02:00"
closed-at: "2026-11-13T04:46:31.149659+02:00"
---

Problem: LINKER:WRITE takes 338 ms of a warm engine build: src/os/macos/macho.f assembles the image a byte at a time (src/os/image-bytes.f M-BYTES-LEN :155 and M-ZEROS-LEN call IMG-M8 per byte), each c! through the store guard. Acceptance: a span is copied, and zeros filled, with one room check and one guarded copy; the image's bytes are unchanged; LINKER:WRITE's time falls. Depends: none. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-img.
