# Framed diff artifacts

Repository lints consume `.hbdiff` artifacts, never unframed line streams. Create
one artifact and share it across consumers:

```text
bin/hb --load tools/diff-capture.f -- /tmp/change.hbdiff @- @
bin/hb --load tools/typed-local-diff-lint.f -- /tmp/change.hbdiff
bin/hb --load tools/kernel-perf-lint.f -- /tmp/change.hbdiff
```

`tools/diff-capture.f` remains deliberately fail-closed until the reviewed bulk
adapter installs the production provider. The command above is the activation
contract; there is no legacy content fallback.

`tools/diff-capture.f` obtains one operation ID with one atomic `jj op log`
command, then runs every revision resolution, metadata query, raw diff, and
content query at that operation. The resolved full commit IDs are stored in the
header; later operation or working-copy changes cannot alter the artifact.
Content identity, body presence, binary form, and mode change are derived
independently of the raw diff, then checked against it before framing.

Command diagnostics retain the phase, argv, outcome, exit status, command throw
code, independent stdout/stderr capture codes, stdout, and stderr. Process
outcome is recorded before either diagnostic file is loaded; both loads run even
when one fails. A zero exit status with stderr is successful. Spawn/open failures
propagate their exact errors, and nonzero jj completion becomes
`E-DIFF-CAPTURE`. Argv, stdout, and stderr are hexadecimal JSON strings with
explicit encoding fields, so arbitrary command and output bytes cannot
invalidate the report. Hex bytes stream directly into the growable JSON writer;
diagnostic size is not bounded by shared string scratch and rendering cannot
replace the recorded capture outcome. `DIFF-CAPTURE:REPORT$` returns the
structured JSON diagnostic.
Capture-level outcome is ready before temporary-root creation or jj discovery;
command fields are omitted and `command_present` is false until a command starts.
`DIFF-CAPTURE:COMMAND?` guards command accessors, which fail closed while absent.
Invalid resolved IDs, metadata/schema violations, raw-section violations,
cleanup failures, or output-write failures abort without publishing the
destination. The existing destination is never removed before capture and
survives primary, cleanup, and publish failures byte-for-byte. The temporary
tree is cleaned before the one atomic publish. A primary failure combined with
a cleanup failure rethrows the primary code while preserving both codes in the
typed capture outcome, accessors, and JSON diagnostic.

The CLI delivers the complete JSON object plus its trailing LF through a checked
full-write loop. Partial writes advance until all bytes are delivered; zero,
negative, oversized, or throwing writes fail. `DIFF-REPORT` keeps the original
capture code, report-delivery code, and typed unattempted/delivered/render-failed/
write-failed outcome in separate fields. A render or write failure therefore
never replaces the capture failure; the CLI still rethrows the original code.
The inherited diagnostic descriptor is borrowed and remains open.

Metadata declares side presence through its tree-entry kind. Capture normalizes
an absent side to an empty path; parsing rejects an absent side with a path or
executable bit, a present side without a path, and status/path combinations that
do not agree. Producer, frame writer, and authenticated reader all call the same
nonempty, NUL-free repository-path validator. Authentication never substitutes
for this schema check.

Content capture has no default or per-row fallback. Production must install one
checked provider with `DIFF-CAPTURE:CONTENT-PROVIDER! ([ -- ] --)` before
`RUN`/`RUN-IN`; an unbound provider fails with `E-DIFF-CAPTURE` after metadata
validation and before framing. The provider runs once per capture. During that
call, `CONTENT-METADATA$` exposes the exact metadata bytes,
`CONTENT-METADATA-PATH$` exposes their existing capture-owned file, and
`CONTENT-ROW-COUNT` exposes the required row count. After authenticating a
single bulk side-content artifact against those metadata bytes, the provider
calls `CONTENT-ROW!` exactly once for every zero-based row:

```text
CONTENT-ROW! ( row old-size old-binary old-digest old-digest-u
               new-size new-binary new-digest new-digest-u -- )
```

Each digest length must be 32 bytes; sizes must be nonnegative; absent and
gitlink sides must have zero size and cannot be binary; only files may be
binary. Duplicate, missing, out-of-range, or noncanonical rows fail closed.
Provider metadata/count access and row mutation also fail outside the active
provider call. The reviewed bulk adapter is therefore an explicit production
dependency rather than a caller opt-in or legacy fallback.

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
  'S' | status:u8 | form:u8 | body_present:u8 | mode_present:u8
  old_present:u8 | new_present:u8 | reserved:u8=0
  old_len:u64 | old_path[old_len]
  new_len:u64 | new_path[new_len]
  raw_len:u64 | raw_git_section[raw_len]

trailer:
  'T' | section_count:u64 | sha256[32]
```

Both header identities are canonical 40-byte lowercase hexadecimal jj commit
IDs. The operation ID used to pin capture is not stored in the artifact.

The SHA-256 covers every byte before the digest, including the trailer marker
and declared section count. The section count and trailer position must be
exact. Status is `MODIFIED`, `ADDED`, `REMOVED`, `RENAMED`, or `COPIED`. Form is
`TEXT`, `BINARY`, `MODE`, `EMPTY`, `PURE`, or `GITLINK`. `body_present` and
`mode_present` are independent authenticated declarations and must agree with
the checked raw form. Thus a rename or copy may carry both a text/binary body
and a mode change without losing either fact.

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
`DIFF:SECTION-MODE?`,
`DIFF:SECTION-OLD?`/`DIFF:SECTION-OLD$`,
`DIFF:SECTION-NEW?`/`DIFF:SECTION-NEW$`,
`DIFF:SECTION-INDEX`, and `DIFF:SECTION-COUNT`. Header identities are available
through `DIFF:FROM$` and `DIFF:TO$`.

`DIFF:OPEN` copies the input into a reusable geometrically grown owned buffer;
the caller may mutate or release its input immediately. Every public byte view
is copied into reusable view storage, so mutating it cannot corrupt the
authenticated artifact. Accessor and non-file event views remain valid until
the next such byte-returning call. A `FILE` event has independent storage and
remains valid through its section, until the next `FILE` event. Reopening reuses
capacity when possible.

Consumers load artifact files through `DIFF-FILE:LOAD`, which allocates from the
exact file size and requires an exact read. It has no fixed 16 MiB ceiling.
