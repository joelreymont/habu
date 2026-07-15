# Bulk Diff Side Content

`tools/bulk-diff-scan.f` is the AOT entry for a Jujutsu external directory-diff
tool. Its invocation is:

```text
bulk-diff-scan LEFT RIGHT /absolute/path/to/metadata.jsonl
```

`LEFT` and `RIGHT` may be relative to Jujutsu's external-tool working
directory. The metadata path is absolute because Jujutsu runs the tool from a
temporary directory. Successful stdout is exactly one binary artifact. Failure
stdout is empty; stderr is one JSON object with `phase`, `row`, `side`,
`path_hex`, and `code`.

The scanner creates no child processes. It parses the ordered metadata once,
lstats each declared node, streams regular files once, reads symlink targets
without following them, skips absent and git-submodule bodies, and emits rows
in metadata order. A regular-file body is binary exactly when a NUL occurs in
its first 8000 bytes. Size and SHA-256 always cover the complete body.

## Metadata

Metadata is JSONL with a final LF and one nine-element array per changed row:

```text
[status,old_path,old_type,old_exec,old_conflict,new_path,new_type,new_exec,new_conflict]
```

Types are `""`, `"file"`, `"symlink"`, and `"git-submodule"`. Empty type is
the absent side and requires an empty path. Conflict rows reject. Paths are
decoded as length-bearing JSON strings; LF, CR, space, tab, quote, and backslash
are data, not delimiters. Absolute paths, NUL, empty segments, `.`, and `..`
reject. The artifact header binds the exact metadata byte length and SHA-256.

## Artifact v1

All integers are unsigned 64-bit little-endian values whose top bit must be
zero. Reserved bytes must be zero. Digests are raw 32-byte SHA-256 values.

Header, 112 bytes:

| Offset | Size | Field |
|---:|---:|---|
| 0 | 8 | `HABUSIDE` |
| 8 | 1 | version, `1` |
| 9 | 7 | reserved |
| 16 | 8 | row count |
| 24 | 8 | payload size |
| 32 | 32 | artifact digest |
| 64 | 8 | metadata byte length |
| 72 | 32 | metadata digest |
| 104 | 8 | reserved |

The payload is the ordered row sequence. Each row begins with a 40-byte header:

| Offset | Size | Field |
|---:|---:|---|
| 0 | 1 | `R` |
| 1 | 7 | reserved |
| 8 | 8 | zero-based ordinal |
| 16 | 8 | complete row size |
| 24 | 8 | old-side frame size |
| 32 | 8 | new-side frame size |

Each side is a 64-byte header followed by exact path bytes:

| Offset | Size | Field |
|---:|---:|---|
| 0 | 1 | `S` |
| 1 | 1 | present, `0` or `1` |
| 2 | 1 | kind: absent `0`, file `1`, symlink `2`, gitlink `3` |
| 3 | 1 | binary, `0` or `1` |
| 4 | 4 | reserved |
| 8 | 8 | complete body size |
| 16 | 8 | path size |
| 24 | 8 | reserved |
| 32 | 32 | body digest |
| 64 | path size | exact repository path bytes |

Body bytes are not embedded. Absent and gitlink body sizes and digests are
zero. Only a regular file may set binary. Present empty files and empty symlink
targets carry SHA-256 of the empty byte string.

Trailer, 64 bytes:

| Offset | Size | Field |
|---:|---:|---|
| 0 | 8 | `EDISUBAH` |
| 8 | 1 | version, `1` |
| 9 | 7 | reserved |
| 16 | 8 | repeated row count |
| 24 | 8 | complete artifact size |
| 32 | 32 | repeated artifact digest |

The artifact digest hashes the header and payload, treating header bytes
32..63 as zero. The trailer is excluded.

## Reader API

Load `tools/diff-side-content-read.f` and use package `DIFF-CONTENT`:

```text
VALIDATE          ( artifact -- count )
VALIDATE-BINDING  ( artifact metadata -- count )
COUNT             ( -- count )
ROW-SELECT        ( ordinal -- )
ROW-NEXT?         ( -- bool )
OLD/NEW-PRESENT?  ( -- bool )
OLD/NEW-KIND      ( -- content-kind )
OLD/NEW-BINARY?   ( -- bool )
OLD/NEW-CONTENT-SIZE ( -- n )
OLD/NEW-PATH      ( destination capacity -- length )
OLD/NEW-DIGEST    ( destination -- )
METADATA-SIZE     ( -- n )
METADATA-DIGEST   ( destination -- )
```

Validation copies the artifact into reader-owned geometric storage. Path and
digest accessors copy out; no internal pointer is exposed. Forward selection is
linear over the row sequence, and backward selection restarts the cursor.

Focused validation:

```sh
HB_TMP=/tmp/habu-diff-side bin/hb --load tools/diff-side-content-test.f
HB_TMP=/tmp/habu-bulk-diff bin/hb --load tools/bulk-diff-scan-test.f
```
