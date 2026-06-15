\ json-only.f - emit JSON diagnostic object lines, else pass stderr through.
\ Load with argv/json support:
\   cat tools/argv.f tools/json.f tools/json-only.f > /tmp/json-only.f
\   bin/hb /tmp/json-only.f stderr-file

\ Tool driver boundary: raw file descriptors plus wrapper exit behavior. The
\ JSON parser loaded before this file stays checked.
0 set-check

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

: JSON-ONLY-COPY-BYTES {: a dst u :} ( a dst u -- )
   0 begin dup u < while
      dup a + c@ over dst + c!
      1+
   repeat drop ;

: JSON-ONLY-PATHZ {: a u :} ( addr u -- zaddr )
   u 1+ JSON-ONLY-PATH-CAP > IF s" json-only: path too long" JSON-ONLY-E-IO die THEN
   a JSON-ONLY-PATH u JSON-ONLY-COPY-BYTES
   0 JSON-ONLY-PATH u + c!
   JSON-ONLY-PATH ;

: JSON-ONLY-READ-FILE ( addr u -- file-addr file-u )
   {: a u :}
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

: JSON-ONLY-WRITE ( fd addr u -- )
   {: fd a u :}
   u 0= IF exit THEN
   fd a u write u <> IF s" json-only: write failed" JSON-ONLY-E-IO die THEN ;

: JSON-ONLY-LF$ ( -- addr u )
   JSON-ONLY-LF-C JSON-ONLY-LF c!
   JSON-ONLY-LF 1 ;

: JSON-ONLY-STDERR ( addr u -- )
   {: a u :}
   2 a u JSON-ONLY-WRITE ;

: JSON-ONLY-STDOUT-LINE ( -- )
   1 JSONL-LA @ JSONL-LU @ JSON-ONLY-WRITE
   1 JSON-ONLY-LF$ JSON-ONLY-WRITE ;

: JSON-ONLY-NEXT? ( -- f )
   JSONL-NEXT-OBJECT dup -1 = IF drop 0 ELSE drop -1 THEN ;

: JSON-ONLY-START? ( addr u -- f )
   JSONL-START
   JSON-ONLY-NEXT? ;

: JSON-ONLY-EMIT-FOUND ( -- )
   begin
      JSON-ONLY-STDOUT-LINE
      JSON-ONLY-NEXT? 0=
   until ;

: JSON-ONLY-FILTER ( addr u -- )
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

: JSON-ONLY-INPUT$ ( -- addr u )
   ARGV-COUNT 1 <> IF JSON-ONLY-USAGE THEN
   0 ARGV-TOK$ ;

: JSON-ONLY-MAIN ( -- )
   JSON-ONLY-INPUT$ JSON-ONLY-READ-FILE JSON-ONLY-FILTER ;

JSON-ONLY-MAIN
