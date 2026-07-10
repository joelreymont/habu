\ trust-lint.f - CLI wrapper for TRUSTED.md drift lint.
\ Load after tools/date.f, lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f,
\ tools/lint/text.f, tools/lint/token.f, tools/lint/lib.f,
\ tools/trust-lint-core.f, and tools/argv.f.
\ Run: bin/hb --load tools/date.f lib/errors.f lib/string.f lib/memory.f lib/fs.f
\ tools/lint/text.f tools/lint/token.f tools/lint/lib.f
\ tools/trust-lint-core.f tools/argv.f tools/trust-lint.f -- [ROOT] [TODAY]
\ Or:  bin/hb --load ... tools/trust-lint.f -- source-only SOURCE [ROOT] [TODAY]
\ Or:  bin/hb --load ... tools/trust-lint.f -- source-list ROOT [TODAY] SOURCE...

require tools/date.f
require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/trust-lint-core.f
require tools/argv.f

$40000 constant TL-ARGV-STR-CAP
$80000 constant TL-ARGV-FILE-CAP        \ src/core/checker.f grew past $40000; largest-source watermark

variable TL-ARGV-STR-BUF-A
variable TL-ARGV-FILE-BUF-A
variable TL-ARGV-SOURCE-ONLY
variable TL-ARGV-SOURCE-LIST
variable TL-ARGV-SOURCE-FIRST
variable TL-ARGV-SOURCE-A
variable TL-ARGV-SOURCE-U

: TL-ARGV-STR-BUF ( -- ptr u8 )
   TL-ARGV-STR-BUF-A @ 0= if
      TL-ARGV-STR-CAP MEM-ALLOC-BYTES drop
      TL-ARGV-STR-BUF-A 0 ptr-field !
   then
   TL-ARGV-STR-BUF-A 0 ptr-field @ ;

: TL-ARGV-FILE-BUF ( -- ptr u8 )
   TL-ARGV-FILE-BUF-A @ 0= if
      TL-ARGV-FILE-CAP MEM-ALLOC-BYTES drop
      TL-ARGV-FILE-BUF-A 0 ptr-field !
   then
   TL-ARGV-FILE-BUF-A 0 ptr-field @ ;

: TL-ARGV-SOURCE-A-FIELD ( -- ptr ptr u8 )
   TL-ARGV-SOURCE-A 0 ptr-field ;

: TL-ARGV-SOURCE-A@ ( -- ptr u8 )
   TL-ARGV-SOURCE-A-FIELD @ ;

: TL-ARGV-SOURCE-A! ( ptr u8 -- )
   TL-ARGV-SOURCE-A-FIELD ! ;

: TL-ARGV-BAD-TODAY ( ptr u8 n -- )
   s" BAD-TODAY today argument invalid `" TL-OUT
   TL-OUT
   s" `" TL-OUT TL-NL
   1 throw ;

: TL-ARGV-CONFIG-TODAY ( n -- ) {: idx :}
   idx ARGV-POS$ 2dup PARSE-YMD 0= if drop TL-ARGV-BAD-TODAY then
   TRUST-LINT-TODAY!
   2drop ;

: TL-ARGV-CONFIG-SOURCE ( -- )
   ARGV-POS# 2 < if s" wrong number of positional arguments" ARGV-FAIL then
   ARGV-POS# 4 > if s" wrong number of positional arguments" ARGV-FAIL then
   1 ARGV-POS$ TL-ARGV-SOURCE-U ! TL-ARGV-SOURCE-A!
   ARGV-POS# 2 > if 2 ARGV-POS$ TRUST-LINT-ROOT! else s" ." TRUST-LINT-ROOT! then
   ARGV-POS# 3 > if 3 TL-ARGV-CONFIG-TODAY else TRUST-LINT-TODAY-NOW then ;

: TL-ARGV-CONFIG-SOURCE-LIST-TODAY? ( -- bool )
   ARGV-POS# 4 < if TL-FALSE exit then
   2 ARGV-POS$ 2dup PARSE-YMD 0= if drop 2drop TL-FALSE exit then
   TRUST-LINT-TODAY!
   2drop
   3 TL-ARGV-SOURCE-FIRST !
   TL-TRUE ;

: TL-ARGV-CONFIG-SOURCE-LIST ( -- )
   ARGV-POS# 3 < if s" wrong number of positional arguments" ARGV-FAIL then
   1 ARGV-POS$ TRUST-LINT-ROOT!
   2 TL-ARGV-SOURCE-FIRST !
   TL-ARGV-CONFIG-SOURCE-LIST-TODAY? if exit then
   TRUST-LINT-TODAY-NOW ;

: TL-ARGV-CONFIG-ROOT ( -- )
   0 2 ARGV-EXPECT-POS
   ARGV-POS# 0 > if 0 ARGV-POS$ TRUST-LINT-ROOT! else s" ." TRUST-LINT-ROOT! then
   ARGV-POS# 1 > if 1 TL-ARGV-CONFIG-TODAY else TRUST-LINT-TODAY-NOW then ;

: TL-ARGV-CONFIG ( -- )
   s" tools/trust-lint.f [ROOT] [TODAY] | source-only SOURCE [ROOT] [TODAY] | source-list ROOT [TODAY] SOURCE..." ARGV-USAGE!
   ARGV-PARSE
   0 TL-ARGV-SOURCE-ONLY !
   0 TL-ARGV-SOURCE-LIST !
   ARGV-POS# 0 > if
      0 ARGV-POS$ s" source-only" STR= if
         -1 TL-ARGV-SOURCE-ONLY !
         TL-ARGV-CONFIG-SOURCE
         exit
      then
      0 ARGV-POS$ s" source-list" STR= if
         -1 TL-ARGV-SOURCE-LIST !
         TL-ARGV-CONFIG-SOURCE-LIST
         exit
      then
   then
   TL-ARGV-CONFIG-ROOT ;

: TL-ARGV-BUFFERS ( -- )
   TL-ARGV-STR-BUF TL-ARGV-STR-CAP
   TL-ARGV-FILE-BUF TL-ARGV-FILE-CAP
   TRUST-LINT-BUFFERS! ;

: TRUST-LINT-SOURCE ( -- )
   TL-ARGV-SOURCE-A@ TL-ARGV-SOURCE-U @ TRUST-LINT-SOURCE-FILE ;

: TRUST-LINT-SOURCE-LIST ( -- )
   TRUST-LINT-RESET
   TL-ARGV-SOURCE-FIRST @ begin dup ARGV-POS# < while
      dup ARGV-POS$ TRUST-LINT-SOURCE+
      1+
   repeat drop
   TRUST-LINT-SOURCES-FINISH ;

: TL-MAIN ( -- )
   TL-ARGV-BUFFERS
   TL-TRUE TL-REPORT-SUCCESS!
   TL-ARGV-CONFIG
   TL-ARGV-SOURCE-LIST @ if TRUST-LINT-SOURCE-LIST exit then
   TL-ARGV-SOURCE-ONLY @ if TRUST-LINT-SOURCE else TRUST-LINT then ;

TL-MAIN
