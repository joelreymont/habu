---
title: dot CLI re-quotes created-at on close
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T18:49:20.767636+02:00"
---

Closing a dot rewrites its frontmatter with created-at double-quoted: created-at: '"..."' (escaped quotes nested inside quotes) — see .dots/habu-tfam-5-ordered-4048c839/habu-tfam-5-c-bfa575d2.md and habu-tfam-5-event-d7618516.md after the TFAM-5 redrive close. Cause: the close path re-serializes frontmatter by quoting the already-quoted stored value instead of the raw string. Fix the dot CLI serializer to parse/emit YAML scalars idempotently; add a close-then-reopen round-trip test proving created-at is byte-stable. Repair the two mangled files in the same change.

NOTE (2026-07-04): the dot CLI is an external compiled binary (/opt/homebrew/bin/dot, Mach-O arm64), not repo code — the fix belongs to the dot CLI project, not habu. Kept open as a tracked external defect; the two mangled files in .dots/archive were left as-is (closed, content intact). If the dot CLI source repo is available, fix the frontmatter serializer there (parse/emit YAML scalars idempotently + close/reopen round-trip test).

## Worked (2026-07-07, from head 671c15a7)

Reproduced on the deployed CLI (dots 0.6.4, brew joelreymont/tap): a
scratch add+off outside the repo yields created-at: '"..."' on close.
Live defect, not stale.

Root cause (source repo available at ~/Work/dots): parseFrontmatter
stored created-at/closed-at/close-reason/assignee/issue-type RAW,
including the on-disk YAML quotes - only title went through
parseYamlValue - and serializeFrontmatter re-quoted the already-quoted
value on every rewrite (src/storage.zig).

Fix landed upstream on branch fix-frontmatter-requote (commit 1c6109d,
local, not pushed): every scalar now parses through a shared parseScalar
(parseYamlValue + owned-buffer tracking via ParseResult.allocated_scalars),
making parse+emit round-trips byte-stable. Tests per this dot's spec:
"close-then-reopen round-trip keeps created-at byte-stable" (also proves
full-file reopen identity) and "parse unquotes every scalar frontmatter
field" - both red against the old parser, green with the fix (zig test,
0.16.0; type-checked under 0.15.2 with -fno-emit-bin).

The two mangled files were repaired in this change
(.dots/habu-tfam-5-ordered-4048c839/habu-tfam-5-c-bfa575d2.md,
habu-tfam-5-event-d7618516.md); no other tracked .dots file carries the
nested-quote pattern.

Habu-Only determination: the dot CLI is external workflow infrastructure
(same class as jj/rg), installed via a brew tap and used across projects;
it is NOT habu-repo host glue, so no Habu-native rewrite is warranted and
no repo shell/python is implicated.

OPEN residual (this dot stays open pinned to deployment):
- The DEPLOYED binary still re-quotes; every dot close keeps mangling
  archive frontmatter until it is upgraded. Deployment needs a dots
  release: the dots repo currently builds on this box with neither
  toolchain (zig 0.15.x cannot link under the macOS 26 SDK; installed
  zig 0.16 needs the repo+deps Io/API migration - ohsnap dep chain still
  0.15-only). Migrate the dots repo to 0.16, tag, bump the tap, brew
  upgrade, then rerun the scratch add/off repro and close this dot.
