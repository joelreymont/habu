\ verify-diagnostics.f - schema-1 fault records for quiet source verification.
\ Returned bytes belong to this package and last until its next record.

require lib/fs.f
require lib/json-write.f
require lib/source-lex.f
require src/habu/verify-source.f

package VERIFY-DIAGNOSTICS
private

create OUT BUF:HDR-BYTES allot
TYPED-VARIABLE W JSON-WRITE:writer
variable READY

66 constant MISSING-SOURCE
74 constant UNREADABLE-SOURCE

: START ( -- ptr JSON-WRITE:writer )
   READY @ 0= if
      OUT 256 BUF:N>BLEN BUF:INIT
      1 READY !
   then
   W OUT JSON-WRITE:OPEN-BUF JSON-WRITE:OBJECT-START
   s" schema_version" 1 JSON-WRITE:FIELD-U JSON-WRITE:COMMA ;

: END$ ( ptr JSON-WRITE:writer -- ptr u8 n )
   JSON-WRITE:OBJECT-END JSON-WRITE:$ ;

: ORIGIN ( ptr u8 n -- n n ) {: src:ptr at:n :}
   1 0 at 0 ?do
      src i + c@ 10 = if drop 1+ i 1+ then
   loop
   at swap - 1+ ;

: TOKEN-END ( ptr u8 n n -- n ) {: src:ptr u:n at:n :}
   at begin dup u < if src over + c@ 32 > else false then while
      1+
   repeat ;

: LEX-CODE$ ( -- ptr u8 n ptr u8 n ptr u8 n )
   LINT-LEX:ERROR-KIND@ LINT-LEX:MALFORMED-REGISTRY = if
      s" E-MALFORMED-REGISTRY-ROW" s" close_primitive_row"
      s" Close the primitive-axiom row opened at this token: a bare row reads PRIM: name effect... PRIM;, and a package row reads PPRIM: package name effect... PPRIM; or CLOSE-PRIVATE." exit
   then
   s" E-UNTERMINATED-STRING" s" close_string"
   s" Close the string literal before the definition ends." ;

: LOADER-CODE$ ( n -- ptr u8 n ptr u8 n ptr u8 n ) {: rc:n :}
   rc MISSING-SOURCE = if
      s" E-MISSING-SOURCE" s" fix_load_path"
      s" No file is at the path this loader word names. Correct the path, or create the file." exit
   then
   rc UNREADABLE-SOURCE = if
      s" E-UNREADABLE-SOURCE" s" make_source_readable"
      s" The file this loader word names cannot be read. Make it readable, or correct the path." exit
   then
   s" E-LOADER-FORM" s" literal_loader_form"
   s" Load a file by a literal path of at most 1024 bytes, as written and as resolved, through a loader word no definition redefines or retires, or list this file in tools/dynamic-tail-manifest.f." ;

public

\ Scan the supplied source and render its first lexical fault, or an empty span.
: LEX-RECORD$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: name:ptr nameu:n src:ptr srcu:n :}
   src srcu LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= if s" " exit then
   LINT-LEX:ERROR-BYTE@ {: at:n :}
   src srcu at TOKEN-END at - {: len:n :}
   LEX-CODE$ {: code:ptr codeu:n class:ptr classu:n sug:ptr sugu:n :}
   START
   s" code" code codeu JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" repair_class" class classu JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" verdict" s" rejected" JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" token" src at + len JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" file" name nameu JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" line" LINT-LEX:ERROR-LINE@ JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" column" LINT-LEX:ERROR-COL@ JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" byte_start" at JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" byte_end" at len + JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" suggestion" sug sugu JSON-WRITE:FIELD-S
   END$ ;

: THROW-RECORD$ ( n n ptr u8 n ptr u8 n -- ptr u8 n )
   {: rc:n at:n name:ptr nameu:n src:ptr srcu:n :}
   src at ORIGIN {: line:n col:n :}
   src srcu at TOKEN-END {: last:n :}
   START
   s" code" s" E-STATEMENT-THROW" JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" repair_class" s" unknown_rejection" JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" verdict" s" rejected" JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" token" src at + last at - JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" file" name nameu JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" line" line JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" column" col JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" byte_start" at JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" byte_end" last JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" throw_code" rc JSON-WRITE:FIELD-INT JSON-WRITE:COMMA
   s" suggestion" s" Inspect the token, signature, and raw stack evidence." JSON-WRITE:FIELD-S
   END$ ;

: LOADER-RECORD$ ( n n n ptr u8 n ptr u8 n -- ptr u8 n )
   {: rc:n at:n len:n src:ptr srcu:n name:ptr nameu:n :}
   src at ORIGIN {: line:n col:n :}
   rc LOADER-CODE$ {: code:ptr codeu:n class:ptr classu:n sug:ptr sugu:n :}
   START
   s" code" code codeu JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" repair_class" class classu JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" verdict" s" rejected" JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" token" src at + len JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" file" name nameu JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" line" line JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" column" col JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" byte_start" at JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" byte_end" at len + JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" suggestion" sug sugu JSON-WRITE:FIELD-S
   END$ ;

\ The composition keeps the scanned source and stop location after its scope closes.
: COMPOSE-FAULT-RECORD$ ( rc -- ptr u8 n )
   {: rc:rc :}
   rc RC>N E-DISC-UNTERM = if
      VERIFY:SOURCE-COMPOSE-STOPPED$ VERIFY:SOURCE-COMPOSE-STOPPED-SOURCE$
      LEX-RECORD$ dup 0<> if exit then 2drop
      rc RC>N VERIFY:TOKEN-BYTE@ VERIFY:SOURCE-COMPOSE-STOPPED$
      VERIFY:SOURCE-COMPOSE-STOPPED-SOURCE$ THROW-RECORD$ exit
   then
   rc RC>N VERIFY:E-SOURCE-READ = if
      VERIFY:FAULT-TARGET$ FILE? if UNREADABLE-SOURCE else MISSING-SOURCE then
   else rc RC>N then
   VERIFY:TOKEN-BYTE@ VERIFY:FAULT-LEN@
   VERIFY:SOURCE-COMPOSE-STOPPED-SOURCE$
   VERIFY:SOURCE-COMPOSE-STOPPED$
   LOADER-RECORD$ ;

;package
