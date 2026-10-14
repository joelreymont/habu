---
title: Use DATA-relative native addresses
status: open
priority: 2
issue-type: task
created-at: "2026-10-14T16:55:46.775372+03:00"
---

Otto measures1,703 absolute address chains, about1,610 into DATA although x20 holds its base, consuming20,156 bytes. Use the actual native ABI and relocation/range facts to select smaller DATA-relative addressing where valid; preserve out-of-range and non-DATA references and relocation semantics. Estimated6–8KB savings are unverified. Acceptance: runnable unchanged-source applications and measured code/size reduction with correct address cases; focused E2E, native gate, convergence and Astra review. No codec.
