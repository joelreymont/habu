---
title: Bind libcrypto for authenticated encryption and HMAC
status: open
priority: 1
issue-type: task
created-at: "2026-09-17T02:30:56.894977+03:00"
---

Problem: the Tender server must store platform cookie jars encrypted at rest with a service key and sign its session cookies (docs/backend.md SESSIONS, AUTH), and Habu has SHA-256 but no authenticated cipher or keyed MAC. Acceptance: package CRYPTO in lib/crypto/evp.f over libcrypto.so.3 declared through the FUNCTION: declarer: RANDOM-BYTES ( ptr u8 -- ) via RAND_bytes; SEAL ( key ptr u8 nonce ptr u8 aad ptr u8 plain ptr u8 out ptr u8 -- n ) and OPEN ( ... -- open-result ) as AES-256-GCM through EVP_CIPHER_CTX_new / EVP_EncryptInit_ex / EVP_EncryptUpdate / EVP_EncryptFinal_ex / EVP_CIPHER_CTX_ctrl (tag get/set) / EVP_CIPHER_CTX_free with the 16-byte tag appended to the ciphertext, OPEN answering a named refusal on a bad tag and never a partial plaintext; HMAC-SHA256 ( key ptr u8 msg ptr u8 out ptr u8 -- ) via HMAC(); every context freed on every path including a throw; a caller-owned output span of the stated size, refused when too small; tests with published vectors (NIST GCM test case with AAD, RFC 4231 HMAC-SHA-256 cases 1-4, a tampered-tag OPEN refusal, a round trip of 1 MiB), docs/crypto.md, a CRYPTO error block in lib/errors.f. Files: lib/crypto/evp.f, lib/crypto/evp-test.f, docs/crypto.md, lib/errors.f, test/gate-stdlib-cases.f. Verify: the tests, engine-suite, error-code-lint. Depends: the FFI declarer. Ownership: lib/crypto/. Claim: agent=aspen workspace=.jj-ws/habu-bind-libcrypto
