\ input.f - the text the interpreter is reading, for a migration that takes its
\ definition from there instead of from a string. One concern: where the engine's
\ own reader is in the source it is running, and moving it past what a migration
\ consumed.
\
\ WHY A MIGRATION NEEDS THIS AT ALL. A recording unit has to be open before the
\ definition and closed after it, so the migration's own word must be running
\ while the engine compiles it - and the only way to compile a definition inside
\ a running word is to hand its text to `evaluate`. The text is the stream, and
\ the extent of the definition in that stream is not something this file may
\ decide: a second lexer would have to know what a comment, a string body and a
\ quotation are, and would read `;` inside any of them as the end. So the whole
\ tail of the stream is handed over and the ENGINE's reader decides: it stops the
\ stream where it finished the definition, and the interpreter is put back at
\ that byte afterwards.
\
\ WHAT IT DOES NOT DO. It never reads a byte to decide anything. The one question
\ it asks about the text - is there a definition next - it asks with `parse-name`,
\ the engine's own token reader, and about one token.

require lib/prelude.f
require lib/errors.f

package NINP

private

\ ---- the two engine cells ----------------------------------------------------
\ The interpreter's cursor and its stream end are DATA header cells at fixed byte
\ offsets, which is the shape src/habu/layout.f states them in and the shape the
\ engine's own reader of them (src/habu/stepper.f) uses. A byte offset into that
\ header is the boundary docs/forth.md § Checker & type model names: the cell
\ holds a byte pointer, so it is reached as a pointer field and read and written
\ with ordinary `@` and `!` from there.
\ Retirement: habu-model-the-interpreter-f450db18 - a modeled cursor the checker
\ can express deletes both rows, and this file's only unchecked lines with them.
TRUSTED: INP-FIELD ( -- ptr ptr u8 )
   data-base INP-CELL + 0 ptr-field ;

TRUSTED: INE-FIELD ( -- ptr ptr u8 )
   data-base INE-CELL + 0 ptr-field ;

: INP@ ( -- ptr u8 )    INP-FIELD @ ;
: INE@ ( -- ptr u8 )    INE-FIELD @ ;
: INP! ( ptr u8 -- )    INP-FIELD ! ;

$3A constant COLON-CH

\ ---- what one armed stream remembers -----------------------------------------
variable ARMED                       \ a migration is reading this stream
variable CLOSED                      \ a definition finished inside it
PTR-VARIABLE RESUME-AT               \ the byte after that definition

\ Is a definition next? Asked with the engine's own token reader, so what counts
\ as one token here is what counts as one for the interpreter. The reader
\ CONSUMES what it reads, and the cursor goes back either way: the caller hands
\ the whole tail - this token included - to `evaluate`, and a refusal leaves the
\ stream exactly where it found it.
: COLON-CK ( -- )
   INP@ {: at:ptr :}                \ typed-local-lint: allow-bare-local - at keeps the ptr u8 byte-span role
   parse-name {: t:ptr tu:n :}      \ typed-local-lint: allow-bare-local - t keeps the ptr u8 byte-span role
   at INP!
   tu 1 <> if E-NINP-DEF throw then
   t c@ COLON-CH <> if E-NINP-DEF throw then ;

public

\ The tail of the stream, from the definition that follows it.
: DEF$ ( -- ptr u8 n )
   COLON-CK
   INP@ INE@ over - ;

\ Armed for ONE definition, by the migration that is about to evaluate the tail
\ above. A second arm would mean two migrations reading one stream, which is the
\ same statement E-NMIGRATE-STATE makes about two migrations.
: ARM ( -- )
   ARMED @ 0<> if E-NINP-STATE throw then
   1 ARMED !
   0 CLOSED ! ;

\ The reader finished a definition. An armed stream ENDS here: the evaluate that
\ is running the tail has compiled what the migration asked for, and everything
\ after it belongs to the interpreter that will be resumed, not to this unit.
\ Unarmed - which is every unit whose text came from a string - it says nothing.
: CLOSE ( -- )
   ARMED @ 0= if exit then
   INP@ RESUME-AT !
   1 CLOSED !
   INE@ INP! ;

\ Give the stream back. A definition that closed inside it was consumed by this
\ migration, so the interpreter goes on at the byte after it - on the failing
\ path too, where the engine has already published the word and only the chain
\ refused it. A stream no definition closed in is left exactly where it was.
: RELEASE ( -- )
   0 ARMED !
   CLOSED @ 0= if exit then
   0 CLOSED !
   RESUME-AT @ INP! ;

\ Where the last stream this module cut was put back to. It is published because
\ it is the one fact about this module a test can hold against a source it built
\ itself: the byte after that source's `;`, counted rather than described.
: RESUMED ( -- ptr u8 )
   RESUME-AT @ ;

private

get-current prot-wid-add

public
get-current prot-wid-add

;package
