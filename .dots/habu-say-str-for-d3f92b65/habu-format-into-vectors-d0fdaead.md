---
title: Format into vectors, not caller buffers
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T23:12:49.106036+01:00"
---

Problem: Census: /tmp/jerry-p1/rows/pubrows.py (public rows holding ^u8 n or ^u8 NUM:byte-len). Words that format into a caller's ^u8 n buffer or take text as ^u8 n: src/lib/date.f FORMAT-YMD, FORMAT-EPOCH-UTC, WIDTH!, N; f64-text.f FORMAT; unicode.f FOLD, LOWER; string-option.f BUF-APPEND, BUF-APPEND-C; process-argv.f PROC-ARGV-PROGRAM+; byte-edit.f WRITE; float32-buffer.f UNPACK; core/path.f PATHZ; core/sha256.f SHA256-FD-IN, SHA256-FD-HEX-IN; pty.f OPEN; net/httpc.f OPEN; net/curl.f METHOD!; ffi-abi.f DLOPEN; process-env.f PROC-ENV-VEC-NAME-IDX, PROC-ENV-VEC-HAS-NAME?. Acceptance: text in is str, output goes to a VEC:vector; callers move with it. Depends: none. Claim: unassigned.
