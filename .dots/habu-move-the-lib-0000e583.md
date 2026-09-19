---
title: Move the lib buffer owners onto spans
status: open
priority: 2
issue-type: task
created-at: "2026-09-18T11:33:02.848629+03:00"
---

Problem: band 1 of habu-bound-pointers (habu-bound-pointers-with-719ba3f4): every writable buffer under lib/ that is create-and-allot storage copied into by BYTE-COPY behind a hand-written capacity check, or indexed by arithmetic over hostile bytes, becomes a span. Acceptance, one commit per file group (split at dispatch): lib/byte-buffer.f (BUF header's data as a span; DATA-FIELD's cast goes with the record design, not here), the string builder's destination in lib/string.f, lib/fs.f read buffers, lib/json-read.f and lib/xml/state.f indexed readers on SPAN:C@ / SPAN:SUB, lib/net/ and lib/process*.f destination buffers, lib/serial-xmodem.f; each hand-written 'u cap > if E-... throw then' before a copy deleted in favour of SPAN:COPY with its E-SPAN-CAPACITY, the public effect changed from ( ptr u8 n ) to ( span u8 ) only where the parameter is a destination, each public change swept across loom, maki, kiba, radar and Tender with rg (never loaded) and announced to the owner before landing; the file's own suite unchanged in what it asserts plus one overrun refusal per converted destination; tools/lint/bare-copy-lint.f run over lib/ after each group with the count recorded. Files: the lib groups above and their tests. Verify: their suites; test/run.f. Depends: habu-add-the-span-94d52f2a. Ownership: lib. Parent: habu-campaign-c2-mem-c3d7662b. Claim: unassigned.

Landed: c621b61f (lib/byte-buffer.f BUF data as a span, lib/fs.f read buffers; bare-copy-lint count recorded in its body), gate CP 453/453, integrated 88e43ad5. Remaining, in order: the string builder destination in lib/string.f (group 1 rest), then json-read/xml readers, net/process destinations, serial-xmodem, one commit per group as above.
Also (audit 2026-09-19): lib/fs.f:366 JOIN-PATH mints dst FS-PATH-CAP SPAN:MAKE over a caller's bare ptr u8 as a transitional adapter; the string.f group retires it by taking a span from the caller.
