\ check-all-errors.f - CLI wrapper for all-errors checker diagnostics.
\ Load after lib/string.f, lib/memory.f, lib/vector.f, lib/fs.f,
\ lib/process.f, lib/process-argv.f,
\ tools/lint/text.f, tools/lint/token.f, tools/lint/lib.f,
\ tools/lint/json-writer.f, tools/lint/source-lex.f,
\ tools/check-all-errors-core.f, and lib/argv.f.

package CHECK-ALL-ERRORS-CLI
private

create ERR-BUF CA-DEFAULT-ERR-CAP allot
create OUT-BUF CA-DEFAULT-OUT-CAP allot

: FLUSH ( -- )
   2 CHECK-ALL-ERRORS-OUT$ CA-WRITE ;

: EXEC ( -- )
   s" tools/check-all-errors.f [--json-errors] --label name source" ARGV:USAGE!
   ARGV:PARSE
   ARGV:REQUIRE-LABEL
   1 ARGV:EXPECT-POS-EXACT
   OUT-BUF CA-DEFAULT-OUT-CAP
   ERR-BUF CA-DEFAULT-ERR-CAP CHECK-ALL-ERRORS-BUFFERS!
   ARGV:JSON? CHECK-ALL-ERRORS-JSON!
   ARGV:LABEL$ 0 ARGV:POS$ CHECK-ALL-ERRORS-FILE ;

: RUN ( -- )
   [: EXEC ;] catch
   FLUSH
   dup 0= IF drop exit THEN
   throw ;

RUN

;package
