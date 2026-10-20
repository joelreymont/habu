---
title: Format fixed-width hex bit patterns
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T22:18:59.491656+03:00"
---

Maki needs exactly 16 uppercase zero-padded hexadecimal digits for 64-bit IEEE754 bits and hash keys, including negative cell values. Seven callers currently repeat a 16-nibble shift/mask loop in step-source.f, weld-source.f, trim-source.f and edit.f. Existing FMT:SB-HEX is variable-width and rejects negatives. Add FMT:SB-HEX-BITS ( n width -- ) in src/lib/fmt.f for 0..16 digits of the low bits; width 0 emits nothing and invalid widths refuse before writing with E-FMT-DOMAIN. Inspect existing FMT behavior coverage before implementation; verify negative/all-one bits, zero, narrower width, and invalid widths through its real source/native consumers. Document the public signature and publish only in a matching qualified release. Requested by Jorge; no rush, after incremental-build correctness.
