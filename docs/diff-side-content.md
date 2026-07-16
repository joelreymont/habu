# Diff Side Content

`tools/diff-side-content.f` writes one deterministic ordered artifact containing
old/new side identities and content facts. `tools/diff-side-content-read.f`
authenticates that artifact before exposing any row. The format stores paths,
kind, size, binary classification, and SHA-256; it never embeds body bytes.

## Metadata

The codec treats metadata as opaque bytes. The artifact header binds its exact
length and SHA-256, and `VALIDATE-BINDING` requires the caller's metadata bytes
to match. The integration owner separately validates the metadata schema and
cross-checks every decoded row identity.

Repository paths are length-bearing bytes. LF, CR, space, tab, quote, and
backslash are data, not delimiters. Absolute paths, NUL, empty segments, `.`,
and `..` reject.

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

## Writer API

Load `tools/diff-side-content.f` and use package `DIFF-CONTENT`:

```text
START       ( output capacity row-count metadata -- )
ROW-BEGIN   ( ordinal old-side-size new-side-size -- )
SIDE-BEGIN  ( present kind path body-size -- )
SIDE-CHUNK  ( bytes -- )
SIDE-END    ( -- )
ROW-END     ( -- )
FINISH      ( -- artifact )
```

Rows must be emitted in zero-based order with exactly two sides. `SIDE-CHUNK`
may be called repeatedly; its complete byte count must equal the declared body
size. It hashes every byte and classifies a file as binary when a NUL occurs in
the first 8000 bytes. Absent and gitlink sides accept no body bytes and receive
the canonical zero digest.

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
```
