# Authenticated encryption and HMAC over libcrypto

[`lib/crypto/evp.f`](../lib/crypto/evp.f) binds OpenSSL 3's libcrypto through
the `FUNCTION:` declarer so Habu can seal bytes at rest and sign a short token.
It is Habu on Linux AArch64 with glibc and `libcrypto.so.3`; every other target
is rejected with `CRYPTO:E-PLATFORM` before anything is allocated. Habu already
has SHA-256 in the engine — this package adds the two primitives SHA-256 alone
cannot provide: an authenticated cipher and a keyed MAC.

The engine's own digest streams through a context the caller owns: a span of
`SHA256-CTX-BYTES` bytes passed to `SHA256-BEGIN`, `SHA256-FEED` and
`SHA256-END`, one per digest, so two tasks hash at once by holding one each.
Hashing a file is the same bargain one level up: `SHA256-FILE-IN` and
`SHA256-FILE-HEX-IN` take a FILE CONTEXT, a span of `SHA256-FILE-CTX-BYTES`
holding a digest context, the digest, the read buffer and the path, so two tasks
digest two files at once by holding one each. `SHA256-IN
( ptr u8 ptr u8 n ptr u8 -- )` — context, bytes, out — is those three calls in
one for a caller that already holds every byte, over the same kind of context.
No digest word keeps storage of its own: the only static digest cells left in
the engine are `TF-SHA-CTX` and `SHA-DIGEST` in
[`src/core/type-family-sha.f`](../src/core/type-family-sha.f), the checker's own,
single-task because the checker is.

The package carries no key management: no derivation, no rotation, no storage
format, no nonce counter. It moves one message.

## Values and operations

`CRYPTO:code` is a nominal cell type carrying one of this package's own throw
codes, and `unseal-result` is the decryption outcome. Byte spans are `ptr u8 n`.
`KEY-BYTES` (32), `NONCE-BYTES` (12), `TAG-BYTES` (16) and `MAC-BYTES` (32) are
the sizes a caller allocates against.

| Operation | Inputs | Result |
| --- | --- | --- |
| `RANDOM-BYTES` | writable span | |
| `SEAL` | key, nonce, associated data, plaintext, writable span | `n`, bytes written |
| `UNSEAL` | key, nonce, associated data, sealed record, writable span | `unseal-result` |
| `HMAC-SHA256` | key, message, writable span | |

Every input span is borrowed for its own call and nothing is retained: the
package holds no key, no message and no context between calls.

`RANDOM-BYTES` fills the span from `RAND_bytes`. An empty span is a caller
mistake, not a no-op, and a generator that reports failure throws
`CRYPTO:E-RANDOM` rather than leaving the span half written.

`SEAL` is AES-256-GCM. The key must be exactly `KEY-BYTES` and the nonce exactly
`NONCE-BYTES`; anything else is `CRYPTO:E-OPERAND`. The output span receives the
ciphertext followed by the 16-byte tag, so it must hold the plaintext length plus
`TAG-BYTES` and is refused when it is smaller. `SEAL` answers how many bytes it
wrote, which is always that sum. The associated data is authenticated in place
and never copied into the output; empty associated data is legal.

**A nonce must never repeat under one key.** GCM's security collapses when it
does — two messages under one key and nonce leak their XOR and the authentication
key itself. Draw each nonce with `RANDOM-BYTES` or count it, and store it beside
the record.

`UNSEAL` is the reverse. Its ciphertext span is what `SEAL` wrote, tag included,
so the plaintext is `TAG-BYTES` shorter and the output span must hold that much.
The result requires an exhaustive match:

| Variant | Payload | Meaning |
| --- | --- | --- |
| `ok` | `len` | The plaintext, `len` bytes, is in the caller's span |
| `failed` | `code` | The record did not authenticate; the span is cleared |

`failed` carries `CRYPTO:E-TAG` when the tag does not authenticate the ciphertext
and its associated data — a wrong key, a wrong nonce, different associated data
and a flipped bit anywhere in the record all arrive here — and `CRYPTO:E-OPEN`
when libcrypto refused the decryption itself. On either arm the bytes the caller
would have read as the message are cleared first, so a partial or unauthenticated
plaintext is never visible. **Branch on the result.** A caller that ignores it
reads zeros, not forged plaintext, but it is still reading a message that was
never sent.

`HMAC-SHA256` writes the digest into a span of at least `MAC-BYTES`. The key may
be any length; libcrypto folds a longer one with SHA-256 itself. A MAC is a
signature, not a cipher: it authenticates a token, it does not hide it.

## A sealed record

A stored record is `nonce || ciphertext || tag`, with the row's own identity as
associated data. The identity is authenticated without being encrypted, so a
record moved to another row — a cookie jar re-filed under a second account —
fails to open even though its bytes are untouched.

```forth
require lib/crypto/evp.f

package JAR

using CRYPTO

$10000 constant PLAIN-CAP
NONCE-BYTES PLAIN-CAP + TAG-BYTES + constant RECORD-CAP

create RECORD-BUF RECORD-CAP allot

public

\ ( key row-id plaintext -- record-bytes )
: STORE ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: key ku:n row ru:n plain pu:n :}
   RECORD-BUF NONCE-BYTES RANDOM-BYTES
   key ku RECORD-BUF NONCE-BYTES row ru plain pu
   RECORD-BUF NONCE-BYTES + RECORD-CAP NONCE-BYTES -
   SEAL NONCE-BYTES + ;


\ ( key row-id record plaintext-span -- unseal-result )
: LOAD ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- CRYPTO:unseal-result )
   {: key ku:n row ru:n record du:n out ou:n :}
   key ku record NONCE-BYTES row ru
   record NONCE-BYTES + du NONCE-BYTES -
   out ou UNSEAL ;

;using
;package
```

`STORE` draws a fresh nonce into the front of the record and seals straight into
the rest of it, so the stored bytes are one span with nothing to reassemble.
`LOAD` reads the nonce back out of the record's own head. Both pass the row id as
associated data, which binds the record to its row: a 16-byte message stores as a
44-byte record, opens back under its own id, and answers `failed` with
`CRYPTO:E-TAG` under any other.

## Failures

`CRYPTO:E-OPERAND` rejects a key or nonce of the wrong length, an output span too
small for what the operation writes, and a sealed record shorter than one tag.
`CRYPTO:E-RANDOM` reports a generator failure. `CRYPTO:E-SEAL`, `CRYPTO:E-OPEN`
and `CRYPTO:E-MAC` report a libcrypto step that refused, which on a working
installation means the library or the environment is broken. `CRYPTO:E-TAG` is
not thrown: it is the code `UNSEAL` hands back on the `failed` arm.
`CRYPTO:E-PLATFORM` rejects a non-Linux target. A missing libcrypto symbol is
package FFI's `E-FFI-DLSYM`, named where the first call stands.

The `EVP_CIPHER_CTX` every `SEAL` and `UNSEAL` allocates is freed on every path,
including a throw: the context lives in a per-task slot and the operation runs
under `finally`, so a failure anywhere between the allocation and the answer
still frees it. The slot holds at most one live context and a non-empty slot is a
named failure rather than a silent leak.

## Declarations

Every foreign function is declared with `FUNCTION:` (see `lib/ffi-abi.f`).
`EVP_CIPHER_CTX*`, `EVP_CIPHER*` and `EVP_MD*` are opaque — nothing in the
package dereferences one — so they are declared `n`: AAPCS64 passes a pointer and
an integer in the same register. Only a span Habu or the callee really reads or
writes is declared `ptr u8`, which is what the bounded call guards.

Three symbols carry two argument shapes each, under different Habu names, because
a declaration states one extent and a bounded call guards exactly the span a
`ptr u8` declares — a NULL has to be a `n` argument. `EVP_EncryptInit_ex` selects
the cipher with a NULL key and IV (`ENC-CIPHER`) and then installs the key with a
NULL cipher (`ENC-KEY`); `EVP_EncryptUpdate` takes the associated data with a NULL
output (`ENC-AAD`) and the plaintext with a real one (`ENC-UPDATE`);
`EVP_CIPHER_CTX_ctrl` writes the tag out for one selector (`CTL-TAG-OUT`) and
reads it in for another (`CTL-TAG-IN`). The decryption side mirrors all three.

Constants are written as `/usr/include/openssl/evp.h` writes them, so each is
checkable against the header on sight. `EVP_CTRL_GCM_SET_IVLEN`,
`EVP_CTRL_GCM_GET_TAG` and `EVP_CTRL_GCM_SET_TAG` are the AEAD selectors `0x9`,
`0x10` and `0x11` under their GCM spelling.

`EVP_EncryptFinal_ex` and `EVP_DecryptFinal_ex` are given a one-block scratch
buffer rather than the caller's span. GCM is a stream cipher and emits nothing
there; the package asserts that the final call wrote zero bytes instead of adding
a count that must always be zero.

## Tests

`lib/crypto/evp-test.f` runs published vectors through the public words. The
AES-256-GCM cases are Test Case 13 (empty message, no associated data) and Test
Case 16 (60-byte message with 20 bytes of associated data) of McGrew and Viega's
GCM specification, the submission NIST SP 800-38D adopted; the HMAC-SHA-256 cases
are RFC 4231 test cases 1 to 4. Vectors are written as their tables print them,
in hex groups, and the loader refuses to run past a target buffer's capacity.

Beyond the vectors: a flipped tag bit, a flipped ciphertext bit and altered
associated data each answer `failed` with `CRYPTO:E-TAG` and a cleared output
span, and the untouched record still opens afterwards; a one-mebibyte message
seals and opens unchanged through one owned mapping; every operand refusal
differs from a working call in exactly one operand; and 20,000 seal/unseal rounds
must not raise the process's peak resident set, which one leaked context per
round would do by megabytes.
