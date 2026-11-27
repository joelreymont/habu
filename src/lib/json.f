\ json.f - package JSON: a checked pull reader, an emit-only writer, a copy
\ of a JSON:value into the writer and a comparison of two values.
\ Reading is strict RFC 8259: no comments, trailing commas or single quotes.
\ Structure, escapes and Unicode scalar encoding are validated as the cursor
\ advances; values decode on demand. Every E-JSON-READ code and E-MEMBER is
\ thrown through REASON:THROW with the text
\ `L:C: expected <e>, found <f>; <fix>`, line and byte column counted from the
\ source's start.
\ PARSE checks a text as one JSON value and answers a JSON:value: one value
\ inside that text, borrowed, valid while the text is unchanged, as a str is.
\ PARSE rejects malformed text and an object that repeats a member name, at any
\ depth, with the place. TEXT$ answers the value as written. REJECT rejects a
\ value at its place with a program's own expected text and fix. GET, FIND and
\ AT find a member by decoded name or a value by RFC 6901 pointer. EACH and
\ EACH-MEMBER walk an array or object, and EACH-WHILE and EACH-MEMBER-WHILE
\ stop after the first body that answers false; STRING=, WITH-STRING, >N and
\ >BOOL read scalars; STRING? OBJECT? ARRAY? NUMBER? BOOL? NULL? test a
\ value's kind, INTEGER? whether >N reads it, and INTEGRAL? whether a number
\ has no fractional part, whatever its size.
\ READ-LISTED reads a closed object one member at a time,
\ each member its list names once, one whose name ends in `?` at most once
\ and keyed without it, and no other unless the list's last name is `*`,
\ which skips the others, repeated or not: from the object's `{` or a member
\ value's last token it answers the key's index in the list, with the reader
\ on the value's first token, or -1 at the `}`.
\ EACH-LISTED walks a parsed object by such a list, with READ-LISTED's
\ rejections, handing the body each listed member's value and its index.
\ READ-REWIND puts a reader on a value's last token back on its first, given
\ the span READ-ANY-SPAN$ answered for that value.
\ READ-REPEAT answers some index, from 0 in document order, of the first
\ top-level key of an object that repeats one before it once decoded, nested
\ objects' keys not counted: READ-FIND-KEY takes the first copy where
\ JSON.parse keeps the last. A first value other than an object throws
\ E-JSON-READ-STATE before the rest is read.
\ Writing goes through the nominal `^JSON:writer`, minted only by the typed
\ storage definers; one never opened (the definer's zero image) or already
\ closed rejects every operation with E-JSON-WRITE-STATE.
\ Every emitter answers its writer and appends all of its bytes or none; a
\ document that hit a rejection is incomplete until RESET or CLOSE.
\ WRITE-STRING, WRITE-KEY and WRITE-STRING-PART take UTF-8 text, else
\ E-JSON-WRITE-BYTE, as is a negative indent; WRITE-RAW and
\ WRITE-FIELD-RAW copy JSON text as given.
\ WRITE-INDENT! sets the spaces per level: 0, compact, at open; RESET keeps it.
\ WRITE-ASCII! true makes WRITE-STRING, WRITE-KEY and WRITE-STRING-PART write
\ DEL and every scalar above U+007F as \uXXXX, one above U+FFFF as its UTF-16
\ pair, for an HTTP header; off at open; RESET keeps it.
\ When indented, a START or COMMA puts the next key, value, RAW or START on a
\ new line indented by depth times width; an END closes on its own line, except
\ `{}` and `[]`; string parts never break.
\ A COMMA right after a START writes nothing; after a value it writes the
\ comma. Between WRITE-STRING-START and WRITE-STRING-END only string parts
\ and the end are taken. These are E-JSON-WRITE-STATE, rejected before any byte
\ is written: a COMMA before any token, after a comma or a key, or inside an
\ open string; an END with no open container, after a comma or a key, or
\ inside an open string; any other token inside an open string; a string part
\ or end outside one.
\ WRITE-KEY writes `"k": ` when indented and `"k":` when compact.
\ WRITE-STRING-PART appends whole UTF-8 scalars, escaped.
\ COPY-VALUE writes a JSON:value through the writer's tokens, keeping member
\ order and numeric lexemes, and answers the writer.
\ VALUE= answers whether two texts, each one JSON value, hold the same value,
\ whatever their member order, spacing and escapes; numbers are equal when their
\ mathematical values are, as JSON Schema defines equality: 1, 1.0, 1e0 and
\ 10e-1 are equal, -0 equals 0, and 0.1 differs from 0.10000000000000001.
\ NUMBER-COMPARE orders two number lexemes by value, none when an exponent
\ leaves a signed cell.
\ POINTER-SPAN finds the value an RFC 6901 JSON Pointer names in a text.
\ STORAGE CLASS. CALLER-OWNED: the caller owns the JSON:cursor record
\ (zero-filled typed storage owns nothing, and READ-DISPOSE leaves it so) and the
\ source span, and keeps both live and exclusive until READ-CLOSE or
\ READ-FINISH: a promise the checker cannot prove. The caller owns the writer
\ record and its output. A JSON:value owns nothing.
\ No module state.
require src/lib/json/read.f
require src/lib/json/write.f
require src/lib/json/copy.f
require src/lib/json/equal.f
require src/lib/json/pointer.f
require src/lib/json/value.f

package JSON
seal-package
;package
