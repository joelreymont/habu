---
title: Expose public native credential write and erase
status: open
priority: 2
issue-type: task
created-at: "2026-10-22T09:20:01.236760+03:00"
---

Zen (phil) asked for public SECRET:WRITE and SECRET:ERASE beside SECRET:READ (src/lib/secret.f). Consumers that provision a key at setup, and hermetic tests that must create a dummy keychain or Secret Service item from the reading process so no prompt appears, today can only reopen the private package SECRET words (MAC-OPEN, REQUEST-*, NATIVE-CLOSE) and copy the glue in src/lib/secret-test.f, which couples them to private names of a pinned release. The fixture already writes and removes items on both backends, so the native calls exist; the work is a checked public interface (identifier validation as READ, error codes, overwrite semantics, no key material in diagnostics), the src/lib/secret.f header, and moving secret-test.f onto the public words.
