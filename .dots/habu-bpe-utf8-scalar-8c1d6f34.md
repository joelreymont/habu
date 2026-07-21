---
title: "BPE: strict reentrant UTF-8 scalars"
status: closed
priority: 1
issue-type: task
created-at: "2026-07-21T19:22:10+02:00"
closed-at: "2026-07-21T22:44:11.506589+02:00"
close-reason: The strict reentrant UTF-8 decoder and malformed-input property coverage passed the full native gate on b925e8e427ab.
---

Problem: `BPE-CP@` and the matcher use package-global cursor/scratch cells and
lack direct malformed/boundary coverage. Nested or concurrent tokenization can
clobber scanner state, and a parity fixture can pass without proving the decoder
failure policy.

Acceptance: provide one small checked package-owned UTF-8 scalar decoder whose
state is passed explicitly and whose outcome distinguishes a valid scalar from
the exact GPT-2 byte-fallback case. Every malformed or truncated sequence
returns the single raw lead byte, consumes and advances exactly one byte, and
leaves every following byte for later decoding. It has no mutable global cursor, return
buffer, or shared scratch. Validate continuation shape, shortest form,
surrogates, U+10FFFF, truncation, and remaining-length arithmetic before reading
each byte. Direct tests cover every width, first/last scalar of each width,
overlong forms, lone and excess continuations, every truncation point,
surrogates, values above U+10FFFF, arbitrary invalid bytes, adjacent calls,
nested scans, and two interleaved scanner states. Failure never reads past the
span, including when the lead byte is the final byte.

Files: one UTF-8 scalar decoder package and its focused test file, plus manifest
and `FILEMAP.md` rows. Verify direct decoder properties, canaries, malformed
corpus fuzz/property cases, package/typed-local/host/filemap/dot lints, and the
owning native test gate.

Dependencies: none. Ownership: UTF-8 byte-to-scalar/fallback decoding and
reentrant scanner state only. Unicode categories and BPE chunk grammar remain
with their exact dots.

Claim: agent=enumcert_impl workspace=.jj-ws/habu-bpe-utf8-scalar-8c1d6f34 machine=spark
