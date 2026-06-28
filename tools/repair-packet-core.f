\ repair-packet-core.f - normalize checker JSONL into an LLM repair packet.
\ Load after lib/errors.f, lib/memory.f, tools/argv.f, and tools/json.f.

\ Checked CLI packet builder. Raw fd primitives are used through checked effects.

74 constant RP-E-IO
64 constant RP-E-USAGE
1024 constant RP-PATH-CAP
$40000 constant RP-IN-CAP
32 constant RP-NUM-CAP

create RP-PATH RP-PATH-CAP allot
create RP-NUM RP-NUM-CAP allot

variable RP-IN-A
variable RP-FD
variable RP-RD
variable RP-LEN
variable RP-N
variable RP-NUM-I
variable RP-NODE

: RP-PTR-U8-FIELD ( ptr a -- ptr ptr u8 )
   0 ptr-field ;

: RP-PTR-U8@ ( ptr a -- ptr u8 )
   RP-PTR-U8-FIELD @ ;

: RP-PTR-U8! ( ptr u8 ptr a -- )
   RP-PTR-U8-FIELD ! ;

: RP-IN ( -- ptr u8 )
   RP-IN-A @ 0= if RP-IN-CAP MEM-ALLOC-BYTES drop RP-IN-A RP-PTR-U8! then
   RP-IN-A RP-PTR-U8@ ;

: RP-COPY ( ptr u8 ptr u8 n -- )
   {: a:ptr dst:ptr u:n :}
   0 begin dup u < while
      dup a + c@ over dst + c!
      1+
   repeat drop ;

: RP-FAIL ( ptr u8 n n -- )
   {: a:ptr u:n code:n :}
   a u code die ;

: RP-PATHZ ( ptr u8 n -- ptr u8 )
   {: a:ptr u:n :}
   u 1+ RP-PATH-CAP > if s" repair-packet: path too long" RP-E-IO RP-FAIL then
   a RP-PATH u RP-COPY
   0 RP-PATH u + c!
   RP-PATH ;

: RP-READ-FILE ( ptr u8 n -- ptr u8 n )
   {: a:ptr u:n :}
   a u RP-PATHZ 0 0 open RP-FD !
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
   ARGV-COUNT 1 <> if RP-USAGE then
   0 ARGV-TOK$ ;

: RP-COUNT ( ptr u8 n -- n )
   JSONL-START
   0 RP-N !
   begin
      JSONL-NEXT-OBJECT dup -1 <>
   while
      drop
      RP-N @ 1+ RP-N !
   repeat
   drop
   RP-N @ ;

: RP-FIRST ( ptr u8 n -- n )
   JSONL-START
   JSONL-NEXT-OBJECT dup -1 = if
      drop
      s" repair-packet: no diagnostics" RP-E-IO RP-FAIL
   then ;

: RP-NULL ( -- )
   s" null" JSONW-RAW ;

: RP-U ( n -- )
   {: u:n :}
   RP-NUM-CAP RP-NUM-I !
   u 0= if 48 JSONW-C exit then
   u begin dup 0 > while
      dup 10 mod 48 +
      RP-NUM-I @ 1- RP-NUM-I !
      RP-NUM RP-NUM-I @ + c!
      10 /
   repeat drop
   RP-NUM RP-NUM-I @ + RP-NUM-CAP RP-NUM-I @ - JSONW-RAW ;

: RP-REQ ( n ptr u8 n -- n )
   JSON-GET dup -1 = if
      s" repair-packet: missing diagnostic field" RP-E-IO RP-FAIL
   then ;

: RP-REQ-STR ( n ptr u8 n -- )
   RP-REQ dup JSON-KIND J-STR <> if
      drop
      s" repair-packet: expected string field" RP-E-IO RP-FAIL
   then
   JSON-STRING$ JSONW-STRING ;

: RP-OPT-STR ( n ptr u8 n -- )
   JSON-GET dup -1 = if drop RP-NULL exit then
   dup JSON-KIND J-NULL = if drop RP-NULL exit then
   dup JSON-KIND J-STR <> if
      drop
      s" repair-packet: expected optional string field" RP-E-IO RP-FAIL
   then
   JSON-STRING$ JSONW-STRING ;

: RP-REQ-NUM ( n ptr u8 n -- )
   RP-REQ dup JSON-KIND J-NUM <> if
      drop
      s" repair-packet: expected number field" RP-E-IO RP-FAIL
   then
   JSON-NUMBER$ JSONW-RAW ;

: RP-REQ-STR-FIELD ( n ptr u8 n -- )
   {: root:n key:ptr ku:n :}
   key ku JSONW-KEY
   root key ku RP-REQ-STR ;

: RP-OPT-STR-FIELD ( n ptr u8 n -- )
   {: root:n key:ptr ku:n :}
   key ku JSONW-KEY
   root key ku RP-OPT-STR ;

: RP-REQ-NUM-FIELD ( n ptr u8 n -- )
   {: root:n key:ptr ku:n :}
   key ku JSONW-KEY
   root key ku RP-REQ-NUM ;

: RP-RETURN-STACK ( n -- )
   {: root:n :}
   s" return_stack" JSONW-KEY
   root s" return_stack" RP-REQ RP-NODE !
   RP-NODE @ JSON-KIND J-OBJ <> if s" repair-packet: return_stack is not object" RP-E-IO RP-FAIL then
   JSONW-OBJECT-START
   RP-NODE @ s" expected" RP-OPT-STR-FIELD
   JSONW-COMMA
   RP-NODE @ s" actual" RP-OPT-STR-FIELD
   JSONW-OBJECT-END ;

: RP-PACKET ( n n -- ptr u8 n )
   {: root:n count:n :}
   JSONW-RESET
   JSONW-OBJECT-START
   s" schema_version" JSONW-KEY s" 1" JSONW-RAW
   JSONW-COMMA s" kind" JSONW-KEY s" habu_repair_packet" JSONW-STRING
   JSONW-COMMA root s" word" RP-REQ-STR-FIELD
   JSONW-COMMA root s" token" RP-REQ-STR-FIELD
   JSONW-COMMA root s" token_index" RP-REQ-NUM-FIELD
   JSONW-COMMA root s" file" RP-REQ-STR-FIELD
   JSONW-COMMA root s" line" RP-REQ-NUM-FIELD
   JSONW-COMMA root s" column" RP-REQ-NUM-FIELD
   JSONW-COMMA root s" byte_start" RP-REQ-NUM-FIELD
   JSONW-COMMA root s" byte_end" RP-REQ-NUM-FIELD
   JSONW-COMMA root s" declared_effect" RP-OPT-STR-FIELD
   JSONW-COMMA root s" declared_effect_source" RP-OPT-STR-FIELD
   JSONW-COMMA root s" inferred_effect" RP-REQ-STR-FIELD
   JSONW-COMMA root s" expected" RP-OPT-STR-FIELD
   JSONW-COMMA root s" actual" RP-OPT-STR-FIELD
   JSONW-COMMA root RP-RETURN-STACK
   JSONW-COMMA root s" code" RP-REQ-STR-FIELD
   JSONW-COMMA root s" repair_class" RP-REQ-STR-FIELD
   JSONW-COMMA s" reason" JSONW-KEY RP-NULL
   JSONW-COMMA root s" suggestion" RP-REQ-STR-FIELD
   JSONW-COMMA s" source_excerpt" JSONW-KEY root s" definition_source" RP-REQ-STR
   JSONW-COMMA s" diagnostic_count" JSONW-KEY count RP-U
   JSONW-COMMA s" instruction" JSONW-KEY
   s" Fix the definition so it certifies. Output only corrected Habu code." JSONW-STRING
   JSONW-OBJECT-END
   JSON-OUT-BUF JSON-OUT-LEN @ ;

: RP-MAIN ( -- )
   RP-INPUT$ RP-READ-FILE 2dup RP-COUNT >r RP-FIRST r> RP-PACKET type cr ;
