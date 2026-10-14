# JSON Tooling

`tools/json.f` is a bounded Habu-native JSON foundation for tools that run with
`bin/hb`.

Load it before the tool that uses it:

```sh
bin/hb --load src/lib/errors.f src/lib/memory.f tools/json.f my-tool.f -- args...
```

## Parser

`JSON-PARSE ( ptr u8 u -- node )` parses one complete JSON value and throws named
errors on failure:

- `E-JSON-SYNTAX`
- `E-JSON-CAPACITY`
- `E-JSON-TYPE`

`JSON-ERROR$ ( -- ptr u8 u )` and `JSON-ERR-POS @` expose the last parser error
message and byte offset.

`JSON-PARSE-TRY ( ptr u8 u -- node kind code )` runs the same parser through a
checked stack-preserving recovery path. On success it returns
`node JSON-PARSE-OK 0`; on failure it returns `-1 JSON-PARSE-THROW code` while
leaving `JSON-ERROR$` and `JSON-ERR-POS @` set for diagnostics.

The parser accepts objects, arrays, strings, numbers, `true`, `false`, and
`null`. Strings are decoded into UTF-8, including `\uXXXX` escapes and surrogate
pairs. Numbers are validated with the JSON grammar and stored as their original
lexeme.

Core accessors:

- `JSON-KIND ( node -- kind )`
- `JSON-COUNT ( node -- u )`
- `JSON-STRING$ ( node -- ptr u8 u )`
- `JSON-NUMBER$ ( node -- ptr u8 u )`
- `JSON-BOOL@ ( node -- bool )`
- `JSON-NULL? ( node -- bool )`
- `JSON-ARR@ ( arr-node idx -- node )`
- `JSON-OBJ@ ( obj-node idx -- key-ptr key-u value-node )`
- `JSON-GET ( obj-node key-ptr key-u -- node|-1 )`

## JSONL

`src/lib/jsonl.f` owns JSONL framing for memory and files. Its checked
`JSONL:cursor` API returns physical lines as `option<JSONL:line>`; each line
contains its byte span and one-based physical number. `JSONL:NEXT` skips empty
lines, and `JSONL:READER` opens a `JR:reader` on the current borrowed line.
Only one reader may be live per cursor. Call `JR:CLOSE` before opening another
reader, advancing or closing the cursor, or reusing its source or reader
backing storage. The memory source must remain live until `JSONL:CLOSE`;
`JSONL:WITH-FILE` closes on success and throw.

`tools/json.f` uses that memory cursor for its DOM object iterator. It trims
JSON whitespace and applies the following parser policy to each framed line:

`JSONL-START-STRICT ( ptr u8 u -- )` initializes strict iteration over
newline-separated input. `JSONL-NEXT-OBJECT ( -- node|-1 )` returns the next
object row, skips blank rows, throws parser errors for invalid JSON/prose rows,
and throws `E-JSON-TYPE` for valid non-object rows.

`JSONL-START-SKIP ( ptr u8 u -- )` initializes skip-invalid/prose iteration.
`JSONL-NEXT-OBJECT` returns the next valid object line, skipping blank lines,
prose, invalid JSON syntax, and valid non-object JSON values. `JSONL-START`
remains a compatibility alias for `JSONL-START-SKIP`.

Skip mode only recovers syntax/prose rows. Capacity and type failures remain
hard parser errors and are rethrown by `JSONL-NEXT-OBJECT`.

Skipped rows are part of the iterator contract and can be inspected with
`JSONL-SKIPPED ( -- u )`.

`JSONL-NEXT-ROW ( -- node kind code found? )` exposes the lower-level cursor
outcome. `kind` is `JSONL-ROW-JSON`, `JSONL-ROW-BLANK`, `JSONL-ROW-ERROR`, or
`JSONL-ROW-EOF`; `code` is the parser throw code for `JSONL-ROW-ERROR` and `0`
otherwise. `JSONL-LINE# ( -- u )` reports the physical row number for the
current row, including final partial rows without a trailing newline, and
`JSONL-LINE$ ( -- ptr u8 u )` reports the trimmed current row bytes.

Returned nodes are valid until the next `JSONL-NEXT-OBJECT` or
`JSONL-NEXT-ROW` call, because each line parse reuses the parser DOM tables and
string arena.

## JSONL files

Use `JSONL:OPEN` or `JSONL:WITH-FILE` from `src/lib/jsonl.f` to frame file
input. Pass an inclusive byte limit for each physical line. The cursor reads
8192-byte chunks and grows the current line as needed; final partial lines
are returned once, EOF is idempotent, and early `JSONL:CLOSE` is allowed.
Call `JSON-PARSE` on a returned line when a tool needs the DOM parser.

## Writer

The separate checked `JSON-WRITE:` vocabulary supports caller-owned fixed or
growable `BUF` output; see [stdlib.md](stdlib.md#json-write).

`JSON-WRITE ( node -- ptr u8 u )` emits compact JSON for a parsed node. It escapes
control bytes, quotes, and backslashes; non-ASCII UTF-8 bytes are emitted as
UTF-8.

Manual writer helpers share the same output buffer:

- `JSONW-RESET`
- `JSONW-RAW`
- `JSONW-STRING`
- `JSONW-KEY`
- `JSONW-OBJECT-START`, `JSONW-OBJECT-END`
- `JSONW-ARRAY-START`, `JSONW-ARRAY-END`
- `JSONW-COMMA`

## Bounds

The parser string arena starts at `JSON-STR-BOOT-CAP` and grows in
`JSON-STR-GRAIN` OS-backed spans as needed. This lets large JSONL benchmark rows
carry long prompts, responses, and bundles without a fixed string cap.

The DOM and writer structures remain explicitly bounded by `JSON-MAX-NODES`,
`JSON-MAX-ITEMS`, `JSON-MAX-PAIRS`, `JSON-OUT-CAP`, and `JSON-MAX-DEPTH` in
`tools/json.f`. Exceeding any fixed table cap, overflow guard, or OS allocation
throws `E-JSON-CAPACITY`.
