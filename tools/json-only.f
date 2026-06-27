\ json-only.f - CLI wrapper for JSON diagnostic line filtering.
\ Run with argv/json support:
\   bin/hb --load lib/errors.f lib/memory.f tools/argv.f tools/json.f \
\     tools/json-only-core.f tools/json-only.f -- stderr-file

\ Checked CLI wrapper. Raw fd primitives are used through checked effects.

1024 constant JSON-ONLY-PATH-CAP
$40000 constant JSON-ONLY-IN-CAP

create JSON-ONLY-PATH JSON-ONLY-PATH-CAP allot

variable JSON-ONLY-FD
variable JSON-ONLY-LEN
variable JSON-ONLY-RD
variable JSON-ONLY-IN-A

: JSON-ONLY-PTR-U8-FIELD ( ptr a -- ptr ptr u8 )
   0 ptr-field ;

: JSON-ONLY-PTR-U8@ ( ptr a -- ptr u8 )
   JSON-ONLY-PTR-U8-FIELD @ ;

: JSON-ONLY-PTR-U8! ( ptr u8 ptr a -- )
   JSON-ONLY-PTR-U8-FIELD ! ;

: JSON-ONLY-ALLOC-IN ( -- ptr u8 )
   JSON-ONLY-IN-CAP MEM-ALLOC-BYTES drop ;

: JSON-ONLY-IN ( -- ptr u8 )
   JSON-ONLY-IN-A @ 0= if JSON-ONLY-ALLOC-IN JSON-ONLY-IN-A JSON-ONLY-PTR-U8! then
   JSON-ONLY-IN-A JSON-ONLY-PTR-U8@ ;

: JSON-ONLY-COPY-BYTES ( ptr u8 ptr u8 n -- )
   {: a:ptr dst:ptr u :}
   0 begin dup u < while
      dup a + c@ over dst + c!
      1+
   repeat drop ;

: JSON-ONLY-PATHZ ( ptr u8 n -- ptr u8 )
   {: a:ptr u :}
   u 1+ JSON-ONLY-PATH-CAP > IF s" json-only: path too long" JSON-ONLY-E-IO die THEN
   a JSON-ONLY-PATH u JSON-ONLY-COPY-BYTES
   0 JSON-ONLY-PATH u + c!
   JSON-ONLY-PATH ;

: JSON-ONLY-DST ( -- ptr u8 )
   JSON-ONLY-IN JSON-ONLY-LEN @ + ;

: JSON-ONLY-ROOM ( -- n )
   JSON-ONLY-IN-CAP JSON-ONLY-LEN @ - ;

: JSON-ONLY-READ-CHUNK ( -- )
   JSON-ONLY-FD @ JSON-ONLY-DST JSON-ONLY-ROOM read JSON-ONLY-RD ! ;

: JSON-ONLY-READ-FILE ( ptr u8 n -- ptr u8 n )
   {: a:ptr u :}
   a u JSON-ONLY-PATHZ 0 0 open JSON-ONLY-FD !
   JSON-ONLY-FD @ 0 < IF s" json-only: cannot open input" JSON-ONLY-E-IO die THEN
   0 JSON-ONLY-LEN !
   begin
      JSON-ONLY-LEN @ JSON-ONLY-IN-CAP >= IF
         JSON-ONLY-FD @ close
         s" json-only: input too large" JSON-ONLY-E-IO die
      THEN
      JSON-ONLY-READ-CHUNK
      JSON-ONLY-RD @ 0 >
   while
      JSON-ONLY-LEN @ JSON-ONLY-RD @ + JSON-ONLY-LEN !
   repeat
   JSON-ONLY-RD @ 0 < IF
      JSON-ONLY-FD @ close
      s" json-only: read failed" JSON-ONLY-E-IO die
   THEN
   JSON-ONLY-FD @ close
   JSON-ONLY-IN JSON-ONLY-LEN @ ;

: JSON-ONLY-USAGE ( -- )
   2 s" usage: tools/json-only.f stderr-file" JSON-ONLY-WRITE
   2 JSON-ONLY-LF$ JSON-ONLY-WRITE
   ARGV-E-USAGE throw ;

: JSON-ONLY-INPUT$ ( -- ptr u8 n )
   ARGV-COUNT 1 <> IF JSON-ONLY-USAGE THEN
   0 ARGV-TOK$ ;

: JSON-ONLY-MAIN ( -- )
   JSON-ONLY-DEFAULT-FDS
   JSON-ONLY-INPUT$ JSON-ONLY-READ-FILE JSON-ONLY-FILTER ;

JSON-ONLY-MAIN
