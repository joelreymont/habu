---
title: Refuse negative lengths in the rest of lib/string.f
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-09-30T23:27:05.170066+02:00\\\"\""
closed-at: "2026-10-01T04:13:57.555074+02:00"
close-reason: "Landed in change qruvkryo 'Refuse negative lengths in every string word': every remaining lib/string.f length word refuses -1 and the minimum cell on guard-page bytes, every case failing on the unfixed tree; string, pty-harness, float and 32 more test files rc 0; Fable review ACCEPT."
---

Problem: lane r4-neg (change morwurxk) put one guard, STR-CHECK-LENS, on STR=, STR=CI, STARTS-WITH?, ENDS-WITH? and FIND-SUB. The other lib/string.f words that take a caller length still answer for a negative one: STR-DIGITS? and STR-DIGITS<= answer true; STR-PARSE-POS and STR-PARSE-NEG answer SOME 0; STR>NUMBER? with -1 reads byte 0 and after a sign answers SOME 0, and with the minimum cell after a sign `u 1-` wraps and the scan reads past the span (an out-of-bounds read); INDEX-OF, COUNT-CHAR and SPLIT-NEXT answer NONE, 0 or no more fields; LTRIM and RTRIM turn a negative length into an empty string. lib/pty-harness.f TOOK moves the read cursor back for a negative count and past BUF-CAP for a count past the room. Acceptance: every lib/string.f word that takes a caller length refuses a negative one with E-STR-BOUNDS before it reads a byte, through the one named guard; TOOK refuses a count outside 0..room with its module's named error; a caller census of the words whose callers pass computed lengths (LTRIM/RTRIM first, e.g. tools/unicode/class-verify.f:183), with any caller that can pass a negative fixed at its source; tests (-1, the minimum cell) written first through the public words. Files: lib/string.f, lib/pty-harness.f, their tests, callers the census names. Verify: the string and pty-harness suites, the census callers' tests, a converged engine rebuild (string.f is in PFX-BASELIB). Depends: habu-refuse-negative-and-90e942d5. Ownership: lib/string.f length guards and TOOK. Claim: agent=kestrel workspace=.jj-ws/r4-neg.
