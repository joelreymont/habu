---
title: "Build a program whose FUNCTION: row takes a str"
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T10:15:24.758515+01:00"
---

Problem: src/lib/ffi-abi.f SPLIT-BODY switches the checker to split-str mode by setting CHECKER-SPLIT-STR, but in a build that name binds the tree's own copy of the cell, not the running checker's, so a generated FUNCTION: body (`^u8 n` locals for a str, LOCALS/ARG-CELLS) is checked in the includer's mode and fails in every unmarked file: zip-ffi.f:23 (file-objects Q, ZP), secret/linux.f:34 (every Zen program requiring secret.f), net/tls.f, net/datagram.f, sqlite.f, unicode.f, crypto/evp.f, serial.f, xattr.f, test/ffi-struct-arg.f (ffi-cabi). Acceptance: the generated body takes each str parameter apart itself (`>r str> r>`, which checks in both modes) and SPLIT-BODY is deleted; a program build of each library above, and of the marked fd.f, process-tree.f, net/tcp4.f and zlib.f, passes; ffi-cabi and file-objects are green. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-zipffi.
