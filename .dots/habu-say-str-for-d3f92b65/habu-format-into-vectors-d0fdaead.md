---
title: Format into vectors, not caller buffers
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T23:12:49.106036+01:00"
---

Problem: Census: /tmp/jerry-p1/rows/pubrows.py (public rows holding ^u8 n or ^u8 NUM:byte-len). Words that format into a caller's ^u8 n buffer or take text as ^u8 n: src/lib/date.f FORMAT-YMD, FORMAT-EPOCH-UTC, WIDTH!, N; f64-text.f FORMAT; unicode.f FOLD, LOWER; string-option.f BUF-APPEND, BUF-APPEND-C; process-argv.f PROC-ARGV-PROGRAM+; byte-edit.f WRITE; float32-buffer.f UNPACK; core/path.f PATHZ; core/sha256.f SHA256-FD-IN, SHA256-FD-HEX-IN; pty.f OPEN; net/httpc.f OPEN; net/curl.f METHOD!; ffi-abi.f DLOPEN; process-env.f PROC-ENV-VEC-NAME-IDX, PROC-ENV-VEC-HAS-NAME?; src/compiler/native/dict.f NDICT:FIXED-VALUE (:91) and NDICT:SPELL-START (:149), whose callers split a str with `str>`. Still marked split-str because of it: src/lib/span.f (private SET-BYTES writes the bytes its str names; FILL passes an address and count), src/lib/process-tree.f (RECORD? hands its str to the FFI PID-INFO, which fills it), src/core/sha256.f (ZFILL and BMOVE write their buffers) and src/habu/intern.f (CLEAR-CELLS writes its buffer). Acceptance: text in is str, output goes to a VEC:vector; callers move with it. Depends: none. Claim: unassigned.
