---
title: Package internal-word-gate helpers
status: active
priority: 2
issue-type: task
blocks:
  - habu-add-grammar-fixture-9a2e7f44
created-at: "2026-07-27T10:46:50.186114+02:00"
---

Why: `test/internal-word-gate.f` defines 85 `IWG-*` helpers at global scope.
Its declaration fixtures are source strings executed by child processes, so
the grammar-fixture category does not apply to these definitions.

Owner: package `INTERNAL-WORD-GATE` in `test/internal-word-gate.f`.

Behavior: move every file-owned helper into that package with short private
tails. The package exports zero words. A private `ACTION` word returns the
checked runner quotation; evaluate `ACTION` before `;package`, then execute the
quotation immediately after the close. Preserve all fixture strings byte for
byte. Remove the file's obsolete grammar-fixture admission only after
`habu-add-grammar-fixture-9a2e7f44` lands, so the package gate observes the real
post-admission boundary.

Forbidden: a public runner, a forwarding global, nested packages, changed
fixture text, or another package-lint exemption.

Acceptance: the exact owning `bin/hb --load test/internal-word-gate.f` path has
identical output and exit status; public-signature inspection reports zero
exports; a representative helper-body edit passes the package diff lint only
inside the package; the narrowed grammar table contains no row for this file;
both diff lints and the standard-library slice pass.

Rejected reference evidence: commit
`526c93d103491a8e62a42f75d9825d24b0ca7bd9` has the correct implementation and
zero-export contract but was rejected solely because its source comments are
far larger than the behavior they explain.

Accepted successor: commit
`137b82387534b9ad6340746a3f92695e4d900e94`, whose parent is the rejected
`526c93d103491a8e62a42f75d9825d24b0ca7bd9`, changes comments only. Executable
bytes and all 297 child fixture literals are unchanged, public-signature
inspection reports zero exports, and the exact owning and diff-lint gates pass.
The dot remains active until `137b82387534b9ad6340746a3f92695e4d900e94`
lands and passes integration gates.

Claim: agent=iwg-prose-r3 workspace=.jj-ws/iwg-prose-r3
