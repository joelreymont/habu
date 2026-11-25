---
title: Write into vectors, not caller spans
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T12:52:38.576758+01:00"
---

Problem: public words write into a caller's fixed SPAN:span<u8> and answer a count: src/lib/fd.f:249 READ-EXACT; base64.f ENCODE, DECODE, ENCODE-URL, DECODE-URL (:190-232); base85.f:90 DECODE; uri.f PATH>FILE, FORM-ENCODE, RESOLVE, FILE>PATH (:409-536); fs-anchored.f:284 READ-FILE-AT; fs-identity.f:69 IDENTITY; xattr.f:80 GET. Joel 2026-11-19: it is either str or vector; uri.f FORM-DECODE+ and QUERY-ENCODE already append to a vector. Acceptance: each appends to a VEC:vector the caller owns under .out/designs/design-readvec.md's convention; callers move with it; consumer owners are told before the install. Depends: none. Claim: unassigned.
