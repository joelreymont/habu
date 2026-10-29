\ repair-packet-core.f - normalize checker JSONL into an LLM repair packet.

require src/lib/memory.f
require src/lib/argv.f
require src/lib/jsonl.f
require src/lib/json-write.f
require src/lib/json-copy.f
require src/lib/string.f
require tools/diag-code.f
require src/lib/fs.f

\ Checked CLI packet builder. Raw fd primitives are used through checked effects.

74 constant RP-E-IO
64 constant RP-E-USAGE
$40000 constant RP-IN-CAP
$8000 constant RP-OUT-CAP

create RP-OUT RP-OUT-CAP allot
create RP-TEXT RP-IN-CAP allot
TYPED-VARIABLE RP-READER JR:cursor
TYPED-VARIABLE RP-W JSON-WRITE:writer
TYPED-VARIABLE RP-SOURCE ptr u8
TYPED-VARIABLE RP-ROW ptr u8

variable RP-IN-A
variable RP-FD
variable RP-RD
variable RP-LEN
variable RP-N
variable RP-SOURCE-U
variable RP-POS
variable RP-ROW-U
variable RP-ROW-OBJECT

: RP-PTR-U8-FIELD ( ptr a -- ptr ptr u8 )
   0 ptr-field ;

: RP-PTR-U8@ ( ptr a -- ptr u8 )
   RP-PTR-U8-FIELD @ ;

: RP-PTR-U8! ( ptr u8 ptr a -- )
   RP-PTR-U8-FIELD ! ;

: RP-IN ( -- ptr u8 )
   RP-IN-A @ 0= if RP-IN-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop RP-IN-A RP-PTR-U8! then
   RP-IN-A RP-PTR-U8@ ;

: RP-FAIL ( ptr u8 n n -- )
   {: a:ptr u:n code:n :}
   a u code die ;

: RP-READ-FILE ( ptr u8 n -- ptr u8 n )
   {: a:ptr u:n :}
   a u FS:>PATHZ 0 0 open RP-FD !
   RP-FD @ 0 < if s" repair-packet: cannot open input" RP-E-IO RP-FAIL then
   0 RP-LEN !
   begin
      RP-LEN @ RP-IN-CAP >= if
         RP-FD @ close
         s" repair-packet: input too large" RP-E-IO RP-FAIL
      then
      RP-FD @ RP-IN RP-LEN @ + RP-IN-CAP RP-LEN @ - read RP-RD !
      RP-RD @ 0 >
   while
      RP-LEN @ RP-RD @ + RP-LEN !
   repeat
   RP-RD @ 0 < if
      RP-FD @ close
      s" repair-packet: read failed" RP-E-IO RP-FAIL
   then
   RP-FD @ close
   RP-IN RP-LEN @ ;

: RP-USAGE ( -- )
   s" usage: tools/repair-packet.f checker-jsonl.err" RP-E-USAGE RP-FAIL ;

: RP-INPUT$ ( -- ptr u8 n )
   ARGV:COUNT 1 <> if RP-USAGE then
   0 ARGV:TOK$ ;

: RP-ROW$ ( -- ptr u8 n )
   RP-ROW @ RP-ROW-U @ ;

: RP-ROW! ( ptr u8 n -- )
   RP-ROW-U ! RP-ROW ! ;

: RP-FIND ( ptr u8 n -- JR:reader bool )
   {: key:ptr ku:n :}
   RP-READER RP-ROW$ JR:INIT
   JR:NEXT drop
   key ku JR:FIND-KEY ;

: RP-REQ ( ptr u8 n -- JR:reader )
   RP-FIND if exit then
   JR:CLOSE
   s" repair-packet: missing diagnostic field" RP-E-IO RP-FAIL ;

: RP-STRING$ ( JR:reader -- ptr u8 n )
   JR:TOKEN JR:T-STR <> if
      JR:CLOSE s" repair-packet: expected string field" RP-E-IO RP-FAIL
   then
   RP-TEXT RP-IN-CAP JR:STR {: len:n :}
   JR:CLOSE RP-TEXT len ;

\ The record's code names its shape and the field a refused record adds
\ (tools/diag-code.f).
: RP-CODE$ ( -- ptr u8 n )
   s" code" RP-REQ RP-STRING$ ;

\ A deferral or a warning is no refusal: the packet neither counts it nor comes
\ from it.
: RP-REFUSAL? ( -- bool )
   RP-CODE$ DIAG-CODE:REFUSAL? ;

: RP-VALIDATE ( -- )
   RP-READER RP-ROW$ JR:INIT
   JR:NEXT {: kind:n :}
   begin JR:NEXT dup JR:T-END <> while drop repeat drop
   JR:CLOSE
   kind JR:T-OBJ = RP-ROW-OBJECT ! ;

: RP-OBJECT? ( -- bool )
   [: RP-VALIDATE ;] catch
   dup 0= if drop RP-ROW-OBJECT @ 0<> exit then
   dup E-JR-MALFORMED = over E-JR-STRING = or
   over E-JR-ESCAPE = or over E-JR-SURROGATE = or
   over E-JR-TRAILING = or over E-JR-NUMBER = or
   over E-JR-EOF = or over E-JR-COLON = or
   over E-JR-COMMA = or if drop false exit then
   throw ;

: RP-SCAN-START ( ptr u8 n -- )
   RP-SOURCE-U ! RP-SOURCE !
   0 RP-POS ! ;

: RP-NEXT? ( -- bool )
   begin
      RP-SOURCE @ RP-SOURCE-U @ RP-POS @ JSONL:SPLIT-LINE
      0= if 2drop drop false exit then
      RP-POS !
      TRIM RP-ROW!
      RP-ROW-U @ 0 > if RP-OBJECT? if true exit then then
   again ;

: RP-COUNT ( ptr u8 n -- n )
   RP-SCAN-START
   0 RP-N !
   begin RP-NEXT? while
      RP-REFUSAL? if RP-N @ 1+ RP-N ! then
   repeat
   RP-N @ ;

: RP-FIRST ( ptr u8 n -- )
   RP-SCAN-START
   begin RP-NEXT? while
      RP-REFUSAL? if exit then
   repeat
   s" repair-packet: no diagnostics" RP-E-IO RP-FAIL ;

: RP-REQ-STR ( ptr u8 n -- )
   RP-REQ RP-STRING$ {: a:ptr u:n :}
   RP-W a u JSON-WRITE:STRING drop ;

: RP-OPT-STR ( ptr u8 n -- )
   RP-FIND 0= if JR:CLOSE RP-W JSON-WRITE:NULL drop exit then
   JR:TOKEN JR:T-NULL = if JR:CLOSE RP-W JSON-WRITE:NULL drop exit then
   RP-STRING$ {: a:ptr u:n :}
   RP-W a u JSON-WRITE:STRING drop ;

: RP-NUMBER ( JR:reader -- )
   JR:TOKEN dup JR:T-INT <> swap JR:T-FLOAT <> and if
      JR:CLOSE s" repair-packet: expected number field" RP-E-IO RP-FAIL
   then
   JR:SPAN$ {: a:ptr u:n :}
   JR:CLOSE
   RP-W a u JSON-WRITE:RAW drop ;

: RP-REQ-NUM ( ptr u8 n -- )
   RP-REQ RP-NUMBER ;

: RP-OPT-NUM ( ptr u8 n -- )
   RP-FIND 0= if JR:CLOSE RP-W JSON-WRITE:NULL drop exit then
   RP-NUMBER ;

: RP-REQ-STR-FIELD ( ptr u8 n -- )
   {: key:ptr ku:n :}
   RP-W key ku JSON-WRITE:KEY drop
   key ku RP-REQ-STR ;

: RP-OPT-STR-FIELD ( ptr u8 n -- )
   {: key:ptr ku:n :}
   RP-W key ku JSON-WRITE:KEY drop
   key ku RP-OPT-STR ;

: RP-REQ-NUM-FIELD ( ptr u8 n -- )
   {: key:ptr ku:n :}
   RP-W key ku JSON-WRITE:KEY drop
   key ku RP-REQ-NUM ;

: RP-OPT-NUM-FIELD ( ptr u8 n -- )
   {: key:ptr ku:n :}
   RP-W key ku JSON-WRITE:KEY drop
   key ku RP-OPT-NUM ;

: RP-PACKET-START ( -- )
   RP-W RP-OUT RP-OUT-CAP JSON-WRITE:OPEN
   JSON-WRITE:OBJECT-START
   s" schema_version" JSON-WRITE:KEY s" 1" JSON-WRITE:RAW
   JSON-WRITE:COMMA s" kind" JSON-WRITE:KEY
   s" habu_repair_packet" JSON-WRITE:STRING drop ;

: RP-PACKET-END ( n ptr u8 n -- ptr u8 n )
   {: count:n text:ptr textu:n :}
   RP-W JSON-WRITE:COMMA s" diagnostic_count" JSON-WRITE:KEY
   count JSON-WRITE:U
   JSON-WRITE:COMMA s" instruction" JSON-WRITE:KEY
   text textu JSON-WRITE:STRING JSON-WRITE:OBJECT-END
   JSON-WRITE:$ ;

: RP-RETURN-STACK ( -- )
   RP-ROW$ {: row:ptr rowu:n :}
   RP-W s" return_stack" JSON-WRITE:KEY drop
   s" return_stack" RP-REQ
   JR:TOKEN JR:T-OBJ <> if
      JR:CLOSE s" repair-packet: return_stack is not object" RP-E-IO RP-FAIL
   then
   JR:VALUE-SPAN$ {: nested:ptr nestedu:n :} JR:CLOSE
   nested nestedu RP-ROW!
   RP-W JSON-WRITE:OBJECT-START drop
   s" expected" RP-OPT-STR-FIELD
   RP-W JSON-WRITE:COMMA drop
   s" actual" RP-OPT-STR-FIELD
   RP-W JSON-WRITE:OBJECT-END drop
   row rowu RP-ROW! ;

: RP-DEF-PACKET ( n -- ptr u8 n )
   {: count:n :}
   RP-PACKET-START
   RP-W JSON-WRITE:COMMA drop s" word" RP-REQ-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" token" RP-REQ-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" token_index" RP-REQ-NUM-FIELD
   RP-W JSON-WRITE:COMMA drop s" file" RP-REQ-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" line" RP-REQ-NUM-FIELD
   RP-W JSON-WRITE:COMMA drop s" column" RP-REQ-NUM-FIELD
   RP-W JSON-WRITE:COMMA drop s" byte_start" RP-REQ-NUM-FIELD
   RP-W JSON-WRITE:COMMA drop s" byte_end" RP-REQ-NUM-FIELD
   RP-W JSON-WRITE:COMMA drop s" declared_effect" RP-OPT-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" declared_effect_source" RP-OPT-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" inferred_effect" RP-REQ-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" expected" RP-OPT-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" actual" RP-OPT-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" family" RP-OPT-STR-FIELD
   RP-W JSON-WRITE:COMMA drop RP-RETURN-STACK
   RP-W JSON-WRITE:COMMA drop s" code" RP-REQ-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" repair_class" RP-REQ-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" reason" RP-OPT-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" suggestion" RP-REQ-STR-FIELD
   RP-W JSON-WRITE:COMMA s" source_excerpt" JSON-WRITE:KEY drop
   s" definition_source" RP-REQ-STR
   count s" Fix the definition so it certifies. Output only corrected Habu code."
   RP-PACKET-END ;

: RP-DECL-PACKET ( n -- ptr u8 n )
   {: count:n :}
   RP-PACKET-START
   RP-W JSON-WRITE:COMMA drop s" decl" RP-REQ-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" family" RP-REQ-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" token" RP-REQ-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" file" RP-REQ-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" code" RP-REQ-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" repair_class" RP-REQ-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" reason" RP-REQ-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" suggestion" RP-REQ-STR-FIELD
   count s" Repair the type-family declaration. Output only corrected Habu code."
   RP-PACKET-END ;

\ throw_code is the code a statement threw, and null for any other span.
: RP-SPAN-PACKET ( n -- ptr u8 n )
   {: count:n :}
   RP-PACKET-START
   RP-W JSON-WRITE:COMMA drop s" token" RP-REQ-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" file" RP-REQ-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" line" RP-REQ-NUM-FIELD
   RP-W JSON-WRITE:COMMA drop s" column" RP-REQ-NUM-FIELD
   RP-W JSON-WRITE:COMMA drop s" byte_start" RP-REQ-NUM-FIELD
   RP-W JSON-WRITE:COMMA drop s" byte_end" RP-REQ-NUM-FIELD
   RP-W JSON-WRITE:COMMA drop s" code" RP-REQ-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" throw_code" RP-OPT-NUM-FIELD
   RP-W JSON-WRITE:COMMA drop s" repair_class" RP-REQ-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" suggestion" RP-REQ-STR-FIELD
   count s" Fix the source at this token so it checks. Output only corrected Habu code."
   RP-PACKET-END ;

: RP-INPUT-PACKET ( n -- ptr u8 n )
   {: count:n :}
   RP-PACKET-START
   RP-W JSON-WRITE:COMMA drop s" file" RP-REQ-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" line" RP-REQ-NUM-FIELD
   RP-W JSON-WRITE:COMMA drop s" column" RP-REQ-NUM-FIELD
   RP-W JSON-WRITE:COMMA drop s" code" RP-REQ-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" repair_class" RP-REQ-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" suggestion" RP-REQ-STR-FIELD
   count s" Rebuild bin/hb to check this source; no code change answers this diagnostic."
   RP-PACKET-END ;

\ A record carries all four place fields, when its token locates in the file,
\ or none.
: RP-PLACE ( -- )
   s" line" RP-FIND 0= if JR:CLOSE exit then JR:CLOSE
   RP-W JSON-WRITE:COMMA drop s" line" RP-REQ-NUM-FIELD
   RP-W JSON-WRITE:COMMA drop s" column" RP-REQ-NUM-FIELD
   RP-W JSON-WRITE:COMMA drop s" byte_start" RP-REQ-NUM-FIELD
   RP-W JSON-WRITE:COMMA drop s" byte_end" RP-REQ-NUM-FIELD ;

: RP-STORAGE-PACKET ( n -- ptr u8 n )
   {: count:n :}
   RP-PACKET-START
   RP-W JSON-WRITE:COMMA drop s" word" RP-REQ-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" token" RP-REQ-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" reason" RP-REQ-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" file" RP-REQ-STR-FIELD
   RP-PLACE
   RP-W JSON-WRITE:COMMA drop s" code" RP-REQ-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" repair_class" RP-REQ-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" suggestion" RP-REQ-STR-FIELD
   count s" Fix the storage declaration so its definer accepts it. Output only corrected Habu code."
   RP-PACKET-END ;

\ The field a refused record's code adds to its token, when it names one.
: RP-EVIDENCE ( -- )
   RP-CODE$ DIAG-CODE:EVIDENCE {: key:ptr keyu:n :}
   keyu 0= if exit then
   RP-W JSON-WRITE:COMMA drop key keyu RP-REQ-STR-FIELD ;

: RP-RECORD-PACKET ( n -- ptr u8 n )
   {: count:n :}
   RP-PACKET-START
   RP-W JSON-WRITE:COMMA drop s" token" RP-REQ-STR-FIELD
   RP-EVIDENCE
   RP-W JSON-WRITE:COMMA drop s" file" RP-REQ-STR-FIELD
   RP-PLACE
   RP-W JSON-WRITE:COMMA drop s" code" RP-REQ-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" repair_class" RP-REQ-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" suggestion" RP-REQ-STR-FIELD
   count s" Fix the statement that names this token so it loads. Output only corrected Habu code."
   RP-PACKET-END ;

: RP-USING-PACKET ( n -- ptr u8 n )
   {: count:n :}
   RP-PACKET-START
   RP-W JSON-WRITE:COMMA drop s" token" RP-REQ-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" file" RP-REQ-STR-FIELD
   RP-PLACE
   RP-W JSON-WRITE:COMMA s" used_packages" JSON-WRITE:KEY drop
   s" used_packages" RP-REQ
   JR:TOKEN JR:T-ARR <> if
      JR:CLOSE s" repair-packet: used_packages is not array" RP-E-IO RP-FAIL
   then
   RP-W RP-TEXT RP-IN-CAP JSON-COPY:VALUE drop JR:CLOSE
   RP-W JSON-WRITE:COMMA drop s" code" RP-REQ-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" repair_class" RP-REQ-STR-FIELD
   RP-W JSON-WRITE:COMMA drop s" suggestion" RP-REQ-STR-FIELD
   count s" Qualify this token as PKG:WORD for the package word meant, or rename the collision. Output only corrected Habu code."
   RP-PACKET-END ;

: RP-PACKET ( n -- ptr u8 n ) {: count:n :}
   RP-CODE$ DIAG-CODE:SHAPE MATCH DIAG-CODE:shape
      definition OF count RP-DEF-PACKET ENDOF
      declaration OF count RP-DECL-PACKET ENDOF
      storage OF count RP-STORAGE-PACKET ENDOF
      source-span OF count RP-SPAN-PACKET ENDOF
      deferral OF s" repair-packet: a deferral has no packet" RP-E-IO RP-FAIL ENDOF
      input OF count RP-INPUT-PACKET ENDOF
      record OF count RP-RECORD-PACKET ENDOF
      warning OF s" repair-packet: a warning has no packet" RP-E-IO RP-FAIL ENDOF
      using-refusal OF count RP-USING-PACKET ENDOF
   ;MATCH ;

: RP-MAIN ( -- )
   RP-INPUT$ RP-READ-FILE 2dup RP-COUNT >r RP-FIRST r> RP-PACKET type cr ;
