---
title: "Build a program whose FUNCTION: row takes a str"
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T10:15:24.758515+01:00"
---

Problem: src/lib/ffi-abi.f SPLIT-BODY switches the checker to split-str mode by setting CHECKER-SPLIT-STR, but in a build that name binds the tree's own copy of the cell, not the running checker's, so a generated FUNCTION: body (`^u8 n` locals for a str, LOCALS/ARG-CELLS) is checked in the includer's mode and fails in every unmarked file: zip-ffi.f:23 (file-objects Q, ZP), secret/linux.f:34 (every Zen program requiring secret.f), net/tls.f, net/datagram.f, sqlite.f, unicode.f, crypto/evp.f, serial.f, xattr.f, test/ffi-struct-arg.f (ffi-cabi). Acceptance: the generated body binds a str parameter as one str local and SPLIT-BODY is deleted; the marked files with str rows (aio.f, aio-macos.f, net/tcp4.f, zlib.f, fd.f, process-tree.f) are unmarked; a program build of each library above passes; a test builds a program with an unmarked str FUNCTION: row; ffi-cabi and file-objects are green. Maki's marked src/worker.f has a str row: tim is told before the install. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-zipffi and .jj-ws.noindex/jerry-ffiunm.
