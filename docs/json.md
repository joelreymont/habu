# JSON Tooling

`tools/json.f` is a bounded Habu-native JSON foundation for tools that run with
`bin/hb`.

Load it before the tool that uses it:

```sh
bin/hb --load tools/json.f my-tool.f -- args...
```

## Parser

`JSON-PARSE ( ptr u8 u -- node )` parses one complete JSON value and throws named
errors on failure:

- `E-JSON-SYNTAX`
- `E-JSON-CAPACITY`
- `E-JSON-TYPE`

`JSON-ERROR$ ( -- ptr u8 u )` and `JSON-ERR-POS @` expose the last parser error
message and byte offset.

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

`JSONL-START-STRICT ( ptr u8 u -- )` initializes strict iteration over
newline-separated input. `JSONL-NEXT-OBJECT ( -- node|-1 )` returns the next
object row, skips blank rows, throws parser errors for invalid JSON/prose rows,
and throws `E-JSON-TYPE` for valid non-object rows.

`JSONL-START-SKIP ( ptr u8 u -- )` initializes skip-invalid/prose iteration.
`JSONL-NEXT-OBJECT` returns the next valid object line, skipping blank lines,
prose, invalid JSON syntax, and valid non-object JSON values. `JSONL-START`
remains a compatibility alias for `JSONL-START-SKIP`.

Skipped rows are part of the iterator contract and can be inspected with
`JSONL-SKIPPED ( -- u )`.

The returned node is valid until the next `JSONL-NEXT-OBJECT` call, because each
line parse reuses the parser DOM tables and string arena.

## Writer

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
