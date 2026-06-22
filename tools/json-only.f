\ json-only.f - emit JSON diagnostic object lines, else pass stderr through.
\ Run with argv/json support:
\   bin/hb --load tools/argv.f tools/json.f tools/json-only.f -- stderr-file

\ Checked CLI filter. Raw fd primitives are used through checked effects.

10 constant JSON-ONLY-LF-C
74 constant JSON-ONLY-E-IO
1024 constant JSON-ONLY-PATH-CAP
$40000 constant JSON-ONLY-IN-CAP

create JSON-ONLY-LF 1 allot
create JSON-ONLY-PATH JSON-ONLY-PATH-CAP allot
create JSON-ONLY-IN JSON-ONLY-IN-CAP allot

variable JSON-ONLY-FD
variable JSON-ONLY-LEN
variable JSON-ONLY-RD

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
      JSON-ONLY-FD @ JSON-ONLY-IN JSON-ONLY-LEN @ + JSON-ONLY-IN-CAP JSON-ONLY-LEN @ - read JSON-ONLY-RD !
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

: JSON-ONLY-WRITE ( n ptr u8 n -- )
   {: fd a:ptr u :}
   u 0= IF exit THEN
   fd a u write u <> IF s" json-only: write failed" JSON-ONLY-E-IO die THEN ;

: JSON-ONLY-LF$ ( -- ptr u8 n )
   JSON-ONLY-LF-C JSON-ONLY-LF c!
   JSON-ONLY-LF 1 ;

: JSON-ONLY-STDERR ( ptr u8 n -- )
   {: a:ptr u :}
   2 a u JSON-ONLY-WRITE ;

: JSON-ONLY-STDOUT-LINE ( -- )
   1 JSONL-LA@ JSONL-LU @ JSON-ONLY-WRITE
   1 JSON-ONLY-LF$ JSON-ONLY-WRITE ;

: JSON-ONLY-NEXT? ( -- bool )
   JSONL-NEXT-OBJECT dup -1 = IF drop 0 0= 0= ELSE drop 0 0= THEN ;

: JSON-ONLY-START? ( ptr u8 n -- bool )
   JSONL-START
   JSON-ONLY-NEXT? ;

: JSON-ONLY-EMIT-FOUND ( -- )
   begin
      JSON-ONLY-STDOUT-LINE
      JSON-ONLY-NEXT? 0=
   until ;

: JSON-ONLY-FILTER ( ptr u8 n -- )
   2dup JSON-ONLY-START? IF
      2drop
      JSON-ONLY-EMIT-FOUND
   ELSE
      JSON-ONLY-STDERR
   THEN ;

: JSON-ONLY-USAGE ( -- )
   2 s" usage: tools/json-only.f stderr-file" JSON-ONLY-WRITE
   2 JSON-ONLY-LF$ JSON-ONLY-WRITE
   ARGV-E-USAGE throw ;

: JSON-ONLY-INPUT$ ( -- ptr u8 n )
   ARGV-COUNT 1 <> IF JSON-ONLY-USAGE THEN
   0 ARGV-TOK$ ;

: JSON-ONLY-MAIN ( -- )
   JSON-ONLY-INPUT$ JSON-ONLY-READ-FILE JSON-ONLY-FILTER ;

JSON-ONLY-MAIN
