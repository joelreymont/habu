\ check-all-errors.f - CLI wrapper for all-errors checker diagnostics.
\ Load after lib/string.f, lib/memory.f, lib/vector.f, lib/fs.f,
\ lib/process.f, lib/process-argv.f,
\ tools/lint/text.f, tools/lint/token.f, tools/lint/lib.f,
\ tools/lint/json-writer.f, tools/lint/source-lex.f,
\ tools/check-all-errors-core.f, and lib/argv.f.

package CHECK-ALL-ERRORS-CLI
private

\ Capture sizes are a command decision, not a core one: the core writes into
\ whatever buffers its caller hands it.
$10000 constant ERR-CAP
$10000 constant OUT-CAP
74 constant RC-IO

create ERR-BUF ERR-CAP allot
create OUT-BUF OUT-CAP allot

: WRITE-FD ( n ptr u8 n -- ) {: fd:n a:ptr u:n :}
   u 0= IF exit THEN
   fd a u write u <> IF s" check-all-errors: write failed" RC-IO die THEN ;

: FLUSH ( -- )
   2 CHECK-ALL-ERRORS:OUT$ WRITE-FD ;

: EXEC ( -- )
   s" tools/check-all-errors.f [--json-errors] --label name source" ARGV:USAGE!
   ARGV:PARSE
   ARGV:REQUIRE-LABEL
   1 ARGV:EXPECT-POS-EXACT
   OUT-BUF OUT-CAP
   ERR-BUF ERR-CAP CHECK-ALL-ERRORS:BUFFERS!
   ARGV:JSON? CHECK-ALL-ERRORS:JSON!
   ARGV:LABEL$ 0 ARGV:POS$ CHECK-ALL-ERRORS:FILE ;

: RUN ( -- )
   [: EXEC ;] catch
   FLUSH
   dup 0= IF drop exit THEN
   throw ;

RUN

;package
