---
title: Name an open locals group in source discovery
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T10:50:07.909255+02:00"
---

Problem (r4-strjson, 615280b7): tools/source-discovery.f:267 SD-LOCAL-GROUP throws E-DISC-UNTERM when a `{:` group has no closing `:}`, so tools/check.f reports an open locals group as 'check.f: discovery rejected: unterminated string', rc 70, with no location, and under --json-errors it stays a prose line (the lexer finds no string defect to place). Fixture: $HOME/.cache/tmp/kestrel-r4-strjson/fx/loc.f. Acceptance: an open `{:` group is refused under its own name (what the loader says for it), located at the `{:`, as a JSON record under --json-errors in every file mode and on stdin, the record accepted by diag-contract and documented in docs/repair-diagnostics.md if it is a new class; the string refusal keeps its own name; a case through tools/check-test-lib.f seen to fail first. Files: tools/source-discovery.f, tools/check-core.f, tools/check-all-errors-core.f, tools/check-test-lib.f, docs as needed. Base: after 615280b7 lands.
