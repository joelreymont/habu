---
title: Write private byte buffers as vectors
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T23:34:33.386041+01:00"
---

Problem: Joel 2026-11-19: "no agent should be using "^u8 n" anymore! it's either str or vector". Batch lanes leave private words that write a caller's ^u8 n buffer: src/lib/net/tcp4-test.f FILL-PATTERN, src/lib/net/test-issuer-test.f PUT and DECODED, src/lib/net/model-test.f APPEND and XS, src/lib/siphash-test.f SIPT-ZERO, src/lib/serial-test.f PEER-EXACT, src/lib/serial-xmodem.f READ-CHUNK and READ-REST, src/lib/pty-harness.f READ-CHUNK and its public ROOM$ (a str handed out as write room)., src/lib/net/ws-frame.f BE! ( n str -- ), which writes into its str, src/lib/net/test-issuer.f RAW, src/lib/uri.f SLASH-AT, DOT-SEGMENTS, SQUEEZE and SEGMENT ( ^u8 n n n -- n n ), src/lib/unicode.f SIGMA! and SIGMAS!, which write into a str in place; static buffers read back through >str: src/lib/test/suite.f ITEM-NAME$, src/lib/test/spawn-report.f MARK$, src/lib/tree-copy.f DEST$, src/lib/test/stderr-test.f FILE$. Writes through a str's data: src/lib/xattr.f GET (`buf SPAN:$` handed to fgetxattr), src/lib/xml/write.f, xml/scalar.f and xml/source.f (`VEC:SPAN SPAN:$ STR:DATA`). src/lib/zip.f COPY-INPUT and FILL-BUFFER (and COPY-ATTEMPT and REPLACE-FILL, which call them) write into their str, and SOURCE-STEP and INPUT-BYTES take a ^u8 n destination; src/lib/zlib.f's public DEFLATE-RAW, INFLATE-RAW, DEFLATE-FRAMED and INFLATE-FRAMED write into their second str. Acceptance: each writes into a VEC:vector<u8> its caller owns; no private row names a ^u8 n destination. Depends: none. Claim: unassigned.
