# Framed diff artifacts

Repository lints consume `.hbdiff` artifacts, never unframed line streams. Create
one artifact and share it across consumers:

```text
bin/hb --load tools/diff-capture.f -- /tmp/change.hbdiff @- @
bin/hb --load tools/typed-local-diff-lint.f -- /tmp/change.hbdiff
bin/hb --load tools/kernel-perf-lint.f -- /tmp/change.hbdiff
```

`tools/diff-capture.f` first snapshots the working-copy commit with jj, resolves
one operation ID, and runs every revision resolution, metadata query, and raw
diff at that operation. The resolved full commit IDs are stored in the header;
later operation or working-copy changes cannot alter the artifact. A spawn
failure propagates its process error. A nonzero jj completion, successful
command with stderr, invalid resolved ID, metadata/schema violation, raw-section
violation, or output-write failure aborts without publishing the destination.

## Binary contract

All integers are unsigned little-endian `u64` values whose top bit must be zero.
Every length is checked before pointer arithmetic. The reader authenticates and
validates the complete artifact before emitting an event.

```text
header:
  "HABUDIF2" | version:u8=1 | reserved[7]=0
  from_len:u64 | from_id[from_len]
  to_len:u64   | to_id[to_len]

section:
  'S' | status:u8 | form:u8 | body_present:u8
  old_present:u8 | new_present:u8 | reserved:u16=0
  old_len:u64 | old_path[old_len]
  new_len:u64 | new_path[new_len]
  raw_len:u64 | raw_git_section[raw_len]

trailer:
  'T' | section_count:u64 | sha256[32]
```

The SHA-256 covers every byte before the digest, including the trailer marker
and declared section count. The section count and trailer position must be
exact. Status is `MODIFIED`, `ADDED`, `REMOVED`, `RENAMED`, or `COPIED`. Form is
`TEXT`, `BINARY`, `MODE`, `EMPTY`, `PURE`, or `GITLINK`. `body_present` is part
of the authenticated declaration and must agree with the checked raw form.

Paths are length-prefixed repository identities, not lines or quoted shell
tokens. The supported domain is every `RepoPath` representable by jj 0.37 on
the host. Spaces, tabs, LF, CR, quotes, backslashes, ` b/`, and ` and ` remain
raw bytes. NUL is rejected because it is outside jj's repository-path domain.
No claim is made for filesystem byte strings that the host cannot create.

## Reader contract

`DIFF:OPEN` verifies the digest, header, scalar bounds, every section, and the
trailer before returning. `DIFF:NEXT?` emits checked diff events and an explicit
presence boolean; false consumes the exact trailer and closes the reader.
Calling it again is an error. Section metadata is available while iterating via
`DIFF:SECTION-STATUS`, `DIFF:SECTION-FORM`, `DIFF:SECTION-BODY?`,
`DIFF:SECTION-OLD?`/`DIFF:SECTION-OLD$`,
`DIFF:SECTION-NEW?`/`DIFF:SECTION-NEW$`, `DIFF:SECTION-RAW$`,
`DIFF:SECTION-INDEX`, and `DIFF:SECTION-COUNT`. Header identities are available
through `DIFF:FROM$` and `DIFF:TO$`.

The raw Git-format parser is private implementation machinery. Public
line-at-a-time reset/line/finish entry points do not exist: they cannot prove
section length, form, identity, count, or artifact integrity.
