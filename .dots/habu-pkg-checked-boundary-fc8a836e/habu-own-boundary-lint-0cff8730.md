---
title: Own boundary lint API
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-23T03:47:22.647698+02:00\""
closed-at: "2026-07-25T14:39:37.537569+02:00"
close-reason: "Landed in master@origin as commit 2f20da1978fe 'Integrate CHECK cutover', reachable from 79c50e5a9dbf; squashed atomically with habu-cut-over-check-ac1b7cdf exactly as both contracts required. tools/checked-boundary-lint-core.f opens package CHECKED-BOUNDARY-LINT and publishes only RESET, JSON!, STRICT!, OUT-FD!, FILE and FINISH; tools/checked-boundary-lint-test-lib.f reopens the package for the private lifecycle probes including the zero-byte scan case; tools/check-test-lib.f no longer reads any provider-private UB state."
---

Why: the checked-boundary provider and its six caller operations are global, and its file mapping can outlive the scan state on success or failure. Package ownership and mapping lifetime must land with every caller on one green tree.

Exact result: package CHECKED-BOUNDARY-LINT owns tools/checked-boundary-lint-core.f and publishes only RESET ( -- ), JSON! ( bool -- ), STRICT! ( bool -- ), OUT-FD! ( fd -- ), FILE ( ptr u8 n -- ), and FINISH ( -- ). Rename those six operations to their short tails. Keep every UB-prefixed helper, buffer, cell, constant, and other word private without otherwise renaming it. Update BOUNDARY-LINT-CLI, CBLT, and CHECK to use only qualified public calls. Zero-byte FILE uses a non-owned empty span and still runs UB-SCAN. Positive files validate the byte-allocation role and perform read plus scan inside MEM:WITH-BYTES. Clear every source, token, and previous-token span derived from the mapping before release on success and throw; the primary error takes precedence over cleanup errors. Preserve borrowed path lifetime, reset state, finding order and count, JSON and prose bytes, strict behavior, output descriptor behavior, and FINISH throws.

Test seam: tools/checked-boundary-lint-test-lib.f reopens package CHECKED-BOUNDARY-LINT and defines private test-only lifecycle probes beside the private UB state. Execute those probes before closing that test package; export no production or test bridge. They prove zero-byte scan execution and mapped-span clearing plus release on success and throw. Remove every UB private-state access from tools/check-test-lib.f; CHECK proves public empty SOURCE/RUN success and uses a nonempty production mutation to prove the boundary phase is wired. Review confirms the same unconditional phase serves both lengths. Do not add provider-private access or public surface merely to distinguish two publicly identical zero-byte outcomes.

Atomic delivery: implement and squash this provider change with habu-cut-over-check-ac1b7cdf in workspace .jj-ws/habu-cut-over-check-ac1b7cdf. Neither change may land or close independently. In shared files, this dot owns provider qualifications and removal of private UB accesses; the CHECK dot owns session logic and public CHECK tests.

Forbidden: aliases, forwarding globals, extra public words, exported storage, copied policy, registry changes, zero-byte allocation, dangling mapping spans, or a test-only public hook. Pre-change proof: the exact combined diff reports E-PACKAGE-OWNERSHIP for UB-SCAN and CHECKED-BOUNDARY-LINT-FILE; the public nonempty SOURCE mutation fails when CHK-RUN-BOUNDARY is removed. Acceptance: exact combined typed-local and package diff checks pass; CHECK and CBLT suites pass; the real boundary command and real CHECK stdin, file, list, and missing-engine paths remain exact; zero-byte scan and success/throw mapping lifecycle probes pass; all six public calls resolve; every private and legacy provider name rejects; removing the package owner produces the measured findings; host lint and file-map lint pass.

Files: tools/checked-boundary-lint-core.f, tools/checked-boundary-lint.f, tools/checked-boundary-lint-test-lib.f, tools/check-core.f, and tools/check-test-lib.f. Depends: the locally closed habu-pkg-boundary-lint-353528aa. Ownership: provider package, six-word API, mapping lifetime, caller qualification, and private provider lifecycle proofs. Claim: agent=check_cutover workspace=.jj-ws/habu-cut-over-check-ac1b7cdf.
