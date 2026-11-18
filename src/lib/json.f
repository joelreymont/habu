split-str
\ json.f - package JSON: a checked pull reader, an emit-only writer, a copy
\ between them and a comparison of two values.
\ Reading is strict RFC 8259: no comments, trailing commas or single quotes.
\ Structure, escapes and Unicode scalar encoding are validated as the cursor
\ advances; values decode on demand. Every E-JSON-READ code and E-MEMBER is
\ thrown through REASON:THROW with the text
\ `L:C: expected <e>, found <f>; <fix>`, line and byte column counted from the
\ source's start. READ-LISTED reads a closed object one member at a time,
\ each member its list names once, one whose name ends in `?` at most once
\ and keyed without it, and no other unless the list's last name is `*`,
\ which skips the others, repeated or not: from the object's `{` or a member
\ value's last token it answers the key's index in the list, with the reader
\ on the value's first token, or -1 at the `}`.
\ READ-REWIND puts a reader on a value's last token back on its first, given
\ the span READ-ANY-SPAN$ answered for that value.
\ READ-REPEAT answers some index, from 0 in document order, of the first
\ top-level key of an object that repeats one before it once decoded, nested
\ objects' keys not counted: READ-FIND-KEY takes the first copy where
\ JSON.parse keeps the last. A first value other than an object throws
\ E-JSON-READ-STATE before the rest is read.
\ Writing goes through the nominal `^JSON:writer`, minted only by the typed
\ storage definers; one never opened (the definer's zero image) or already
\ closed refuses every operation with E-JSON-WRITE-STATE.
\ Every emitter answers its writer and appends all of its bytes or none; a
\ document that hit a refusal is incomplete until WRITE-RESET or WRITE-CLOSE.
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
\ and the end are taken. These are E-JSON-WRITE-STATE, refused before any byte
\ is written: a COMMA before any token, after a comma or a key, or inside an
\ open string; an END with no open container, after a comma or a key, or
\ inside an open string; any other token inside an open string; a string part
\ or end outside one.
\ WRITE-KEY writes `"k": ` when indented and `"k":` when compact.
\ WRITE-STRING-PART appends whole UTF-8 scalars, escaped.
\ COPY-VALUE copies one value from the reader's current token into the writer,
\ given a decode buffer at least as large as the source value; escaped strings
\ can never decode to more source bytes. It writes the value through the
\ writer's tokens, keeping member order and numeric lexemes, and answers the
\ reader at the end of that value and the writer.
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
\ record, its output and COPY-VALUE's decode buffer. No module state.
require src/lib/json/read.f
require src/lib/json/write.f
require src/lib/json/copy.f
require src/lib/json/equal.f
require src/lib/json/pointer.f

package JSON
seal-package
;package
