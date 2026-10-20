---
title: Read files beneath anchored directories
status: active
priority: 1
issue-type: task
created-at: "2026-10-15T01:45:26.450119+03:00"
---

The per-call work region must align libc struct stat storage to a cell after the pathname buffer; its field offsets alone do not establish base alignment.

Tender CAS external-reader review demonstrated a path race on public hb: lstat a regular files/<prefix>/<digest> entry, replace it with a symlink, then FS:READ-ALL reopens the pathname and reads outside the configured root. Matching digest/length does not establish root confinement. Provide a shared checked Habu API that acquires directory descriptors without following symlinks, resolves each relative component beneath those descriptors without following symlinks, rejects traversal, checks regular-file type on the opened final fd, and reads bounded bytes from that same fd. Support macOS and Linux at the responsible library or syscall boundary; preserve errno and cleanup every descriptor on success and failure. Establish the exact guarantee and limits (including concurrent renames and hard links) from platform semantics; do not claim guarantees an openat chain cannot provide. Prefer existing FD, MEM and FFI APIs over a new framework. Write real filesystem E2E cases before code: deterministic symlink replacement at final and intermediate components, opened-file type, overflow, and cleanup. Independent Astra review and focused source/native consumer artifact required; full native/convergence only if engine or ABI changes require them. Habu ownership only; Tender migration belongs to its owner. Design first, with no READ-ALL compatibility change or application workaround.
