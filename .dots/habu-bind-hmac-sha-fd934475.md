---
title: Bind HMAC-SHA-1 through the EVP binding
status: open
priority: 2
issue-type: task
created-at: "2026-09-20T00:14:44.113311+03:00"
---

Problem (aspen, Tender login second factor): lib/crypto/evp.f declares EVP_sha256 only (FUNCTION: SHA-256 EVP_sha256) and exposes one keyed digest, HMAC-SHA256 ( ptr u8 n ptr u8 n ptr u8 n -- ) writing MAC-BYTES (32). RFC 6238 TOTP for Login.gov (SAM.gov) enrols SHA-1 authenticators, so the server cannot sign in there: no SHA-1 exists in the engine or the library. Acceptance: FUNCTION: SHA-1 EVP_sha1 ( -- n ) beside SHA-256 and a public HMAC-SHA1 ( ptr u8 n ptr u8 n ptr u8 n -- ) with the same shape, refusals (E-OPERAND on a short out span, E-MAC on a wrong digest length) and WRITES-BYTES contract as HMAC-SHA256, writing MAC1-BYTES (20); lib/crypto/evp-test.f pins RFC 2202 case 1 (key 0x0b x20, 'Hi There' -> b617318655057264e28bc0b6fb378c8ef146be00) and the RFC 6238 appendix B SHA-1 row (seed 12345678901234567890, T = 59 -> HMAC over the 8-byte big-endian counter 1, dynamic truncation, 94287082) computed with the new word and lib-side truncation in the test, plus the short-span refusal; docs/stdlib.md 'Authenticated encryption and HMAC' names it. No SHA-1 use anywhere else; no new dlopen (libcrypto is already bound). Files: lib/crypto/evp.f, lib/crypto/evp-test.f, docs/stdlib.md. Verify: bin/hb --load lib/crypto/evp-test.f; the crypto-evp row. Depends: none. Ownership: lib (alder). Claim: unassigned.
