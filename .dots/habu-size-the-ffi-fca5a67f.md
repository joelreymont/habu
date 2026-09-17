---
title: "Size the FFI declaration table for a server's bindings"
status: open
priority: 1
issue-type: task
created-at: "2026-09-17T03:49:04.868586+03:00"
---

Problem: package FFI keeps one process-wide table of $40 declared foreign functions (lib/ffi-abi.f FN-MAX) and FN-REGISTER refuses the 65th with E-FFI-ARITY naming nothing; libpq (18) + TCP4 (11) + task (5) + libcurl (15) leave 14 rows and CRYPTO declares 19, so the Tender server cannot load its cookie-jar sealing (measured 2026-09-17: the four libraries plus 14 dummy declarations load, 15 throw). Acceptance: the table holds at least $100 declarations (a named constant with its memory cost stated in the header), a full table is refused with its own named FFI code whose diagnostic names the symbol and the Habu word being declared, a test declares past the ceiling in a child engine and asserts the code and the symbol in stderr, and lib/net/tcp4.f, lib/net/curl.f, lib/db/pq.f, lib/crypto/evp.f and lib/task.f load together in one image (a test requires all five). Files: lib/ffi-abi.f, lib/ffi-test.f, lib/errors.f, docs/stdlib.md. Verify: lib/ffi-test.f, the five-library load, engine-suite, error-code-lint on a rebuilt engine. Depends: none. Ownership: lib/ffi-abi.f. Claim: agent=aspen workspace=.jj-ws/habu-size-the-ffi
