\ json.f - package JSON: a checked pull reader, an emit-only writer and a copy
\ between them.
\ Reading (src/lib/json/read.f) is strict RFC 8259: no comments, trailing commas
\ or single quotes. Structure, escapes and Unicode scalar encoding are validated
\ as the cursor advances; values decode on demand.
\ Writing (src/lib/json/write.f) goes through the nominal `ptr JSON:writer`,
\ minted only by the typed storage definers; one never opened (the definer's
\ zero image) or already closed refuses every operation with E-JSON-WRITE-STATE.
\ Every emitter answers its writer and appends all of its bytes or none; a
\ document that hit a refusal is incomplete until WRITE-RESET or WRITE-CLOSE.
\ WRITE-STRING and WRITE-KEY take UTF-8 text, else E-JSON-WRITE-BYTE; WRITE-RAW
\ and WRITE-FIELD-RAW copy JSON text as given.
\ COPY-VALUE (src/lib/json/copy.f) copies one value from the reader's current
\ token into a compact writer, given a decode buffer at least as large as the
\ source value; escaped strings can never decode to more source bytes. It
\ answers the reader at the end of that value and the writer; the output is
\ compact and keeps member order and numeric lexemes.
\ STORAGE CLASS. CALLER-OWNED: the caller owns the JSON:cursor record
\ (zero-filled typed storage owns nothing, and READ-DISPOSE leaves it so) and the
\ source span, and keeps both live and exclusive until READ-CLOSE or
\ READ-FINISH: a promise the checker cannot prove. The caller owns the writer record, its output and COPY-VALUE's
\ decode buffer. No module state.
require src/lib/json/read.f
require src/lib/json/write.f
require src/lib/json/copy.f

package JSON
private
get-current prot-wid-add
public
get-current prot-wid-add
;package
