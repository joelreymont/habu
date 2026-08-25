---
title: Reject re-quoted dot frontmatter
status: open
priority: 3
issue-type: task
created-at: "2026-07-26T09:44:10.432939+02:00"
---

Problem: every rewrite of an existing dot file by the external dot CLI (a compiled binary at ~/.local/bin/dot, source outside this repo) adds a quoting layer to quoted frontmatter scalars: created-at: "2026-07-22T..." becomes "\"2026-07-22T...\"" after one rewrite and gains another layer on the next. Evidence: the 2026-07-26 metadata-wave closure diff on habu-consume-registry-events-efe7fe5e.md (one layer added) and habu-enum-generate-named-1f3261a3.md (now two layers). The serializer re-quotes the raw stored string instead of round-tripping the parsed value, so tracker metadata corrupts cumulatively with every claim edit, reopen, or closure. Invariant: parse-then-serialize of a dot file is byte-stable for unchanged fields, and the repo gate refuses trees where it is not. Exact behavior, repo side (the binary fix is upstream and must be reported to the dot CLI owner): tools/dot-dep-lint.f gains a finding for any frontmatter scalar whose value begins with a backslash-escaped quote (an over-quoted layer), so the gate fails closed before the corruption spreads; a checked Habu repair tool normalizes every affected field across .dots/ to exactly one canonical quoting layer. Acceptance: a fixture dot with a double-quoted created-at fails dot-dep-lint with the new finding; the repair pass brings the whole tree to 0 findings and a second repair run is a byte-identical no-op; hostile fixtures include an escaped quote legitimately inside a title string, which must NOT be flagged. Files: tools/dot-dep-lint.f and its test, the repair tool under tools/. Verify: dot-dep-lint suite plus a full-tree run. Depends: none. Ownership: tracker frontmatter integrity only.

Sole owner as of 2026-07-27. A second dot,
habu-normalize-dot-created-744ca41f, was minted for the same defect and is
deleted in this wave as a duplicate of this one - it covered only the
created-at field and the one-off file cleanup, while this dot already owns the
writer defect for every quoted frontmatter scalar plus the gate that keeps it
from coming back.

Writer path now identified. The live reproduction is recorded as a lesson in
LESSONS.md - "`dot on` still double-quotes an already quoted `created-at`
value" - landed as a0142465 "Record clear-name and dot-status lessons". Read
the reproduction there rather than here; this dot is the contract that closes
it. Two facts belong here because that lesson does not carry them:

- `dot add` writes created-at correctly, in the single-quoted form. Measured
  2026-07-27 by minting a fresh dot with the current binary and reading its
  bytes back. So the add path is not the culprit, and the defect sits on the
  front-matter rewrite path shared by `dot on`, `dot off`, and the claim edits.
  That is exactly the serializer behavior described above: an already-quoted
  stored string is quoted again instead of being round-tripped through the
  parsed value.
- Current population, measured on this tree on 2026-07-27 with an exact
  line-anchored match on `created-at` values beginning with a backslash-escaped
  quote, byte-verified with `od` on a sample: 305 of 1314 dot files carry the
  doubled form. The figure was 304 on cd743607, and the increase is the writer
  still firing, which is the point - a cleanup without the writer fix regrows.

So the fix contract is both halves, and neither half alone closes this dot:
repair the writer, and normalize the affected files (305 today, and re-measure
at the time of the fix rather than trusting this number). The writer half is
upstream in the compiled binary at ~/.local/bin/dot and must be reported to and
fixed by its owner; the repo half is the gate and the repair tool described
above.

Required regression, and it must exercise the real tool rather than a model of
it: run `dot on` against a fixture dot and assert the created-at bytes are
unchanged afterwards. A test that only inspects the repair tool's output would
pass while the writer keeps corrupting the tree. This dot stays open until that
regression exists, which is what the lesson above instructs.
