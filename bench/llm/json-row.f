\ json-row.f - Habu-native JSON string emitter for LLM benchmark rows.
\
\ Load after lib/errors.f. The bundled driver modes are:
\   bin/hb BUNDLE string TEXT
\   bin/hb BUNDLE file PATH

34 constant BQ-DQ
8 constant BQ-BS
9 constant BQ-TAB
10 constant BQ-LF
12 constant BQ-FF
13 constant BQ-CR
32 constant BQ-SP
92 constant BQ-BACKSLASH
64 constant BQ-ARGV-USAGE
$1000 constant BQ-IN-CAP
$1000 constant BQ-OUT-CAP

create BQ-IN BQ-IN-CAP allot
create BQ-OUT BQ-OUT-CAP allot

variable BQ-FD
variable BQ-RD
variable BQ-WR
variable BQ-OFF
variable BQ-OUT-LEN

: BQ-SAME? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   u v <> if 0 0= 0= exit then
   0 begin dup u < while
      dup a + c@ over b + c@ <> if drop 0 0= 0= exit then
      1+
   repeat drop 0 0= ;

: BQ-WRITE-ALL ( ptr u8 n -- ) {: a:ptr u :}
   0 BQ-OFF !
   begin BQ-OFF @ u < while
      1 a BQ-OFF @ + u BQ-OFF @ - write BQ-WR !
      BQ-WR @ 0 <= if E-FS-IO throw then
      BQ-WR @ u BQ-OFF @ - > if E-FS-IO throw then
      BQ-OFF @ BQ-WR @ + BQ-OFF !
   repeat ;

: BQ-FLUSH ( -- )
   BQ-OUT-LEN @ 0 > if
      BQ-OUT BQ-OUT-LEN @ BQ-WRITE-ALL
      0 BQ-OUT-LEN !
   then ;

: BQ-RESET ( -- )
   0 BQ-OUT-LEN ! ;

: BQ-C ( n -- ) {: c :}
   BQ-OUT-LEN @ BQ-OUT-CAP >= if BQ-FLUSH then
   c BQ-OUT BQ-OUT-LEN @ + c!
   BQ-OUT-LEN @ 1+ BQ-OUT-LEN ! ;

: BQ-HEX ( n -- n )
   dup 10 < if 48 + else 55 + then ;

: BQ-U00 ( n -- ) {: c :}
   BQ-BACKSLASH BQ-C
   117 BQ-C
   48 BQ-C
   48 BQ-C
   c 4 rshift BQ-HEX BQ-C
   c $F and BQ-HEX BQ-C ;

: BQ-ESC-C ( n -- ) {: c :}
   c BQ-DQ = if BQ-BACKSLASH BQ-C BQ-DQ BQ-C exit then
   c BQ-BACKSLASH = if BQ-BACKSLASH BQ-C BQ-BACKSLASH BQ-C exit then
   c BQ-BS = if BQ-BACKSLASH BQ-C 98 BQ-C exit then
   c BQ-FF = if BQ-BACKSLASH BQ-C 102 BQ-C exit then
   c BQ-LF = if BQ-BACKSLASH BQ-C 110 BQ-C exit then
   c BQ-CR = if BQ-BACKSLASH BQ-C 114 BQ-C exit then
   c BQ-TAB = if BQ-BACKSLASH BQ-C 116 BQ-C exit then
   c BQ-SP < if c BQ-U00 exit then
   c BQ-C ;

: BQ-BYTES ( ptr u8 n -- ) {: a:ptr u :}
   0 begin dup u < while
      dup a + c@ BQ-ESC-C
      1+
   repeat drop ;

: BQ-STRING ( ptr u8 n -- )
   BQ-DQ BQ-C
   BQ-BYTES
   BQ-DQ BQ-C ;

: BQ-EMIT-STRING ( ptr u8 n -- )
   BQ-RESET
   BQ-STRING
   BQ-FLUSH ;

: BQ-CLOSE-THROW ( n -- ) {: code :}
   BQ-FD @ close
   code throw ;

: BQ-READ-CHUNK ( -- n )
   BQ-FD @ BQ-IN BQ-IN-CAP read BQ-RD !
   BQ-RD @ 0 < if E-FS-IO BQ-CLOSE-THROW then
   BQ-RD @ BQ-IN-CAP > if E-FS-IO BQ-CLOSE-THROW then
   BQ-RD @ ;

: BQ-EMIT-FILE ( ptr u8 n -- ) {: path:ptr u :}
   BQ-RESET
   path u path0 open-rd BQ-FD !
   BQ-FD @ 0 < if E-FS-OPEN throw then
   BQ-DQ BQ-C
   begin
      BQ-READ-CHUNK dup 0 > while
      BQ-IN swap BQ-BYTES
   repeat drop
   BQ-FD @ close
   BQ-DQ BQ-C
   BQ-FLUSH ;

: BQ-USAGE ( -- )
   s" usage: bench/llm/json-row.f string TEXT | file PATH" BQ-ARGV-USAGE die ;

: BQ-MAIN ( -- )
   SCRIPT-ARGC 2 <> if BQ-USAGE then
   0 SCRIPT-ARGV$ s" string" BQ-SAME? if 1 SCRIPT-ARGV$ BQ-EMIT-STRING exit then
   0 SCRIPT-ARGV$ s" file" BQ-SAME? if 1 SCRIPT-ARGV$ BQ-EMIT-FILE exit then
   BQ-USAGE ;

: BQ-AUTO ( -- )
   SCRIPT-ARGC 0 > if BQ-MAIN then ;

BQ-AUTO
