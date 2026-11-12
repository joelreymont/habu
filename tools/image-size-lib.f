\ image-size-lib.f - where the bytes of a Mach-O program the linker wrote went,
\ read from its load commands (tools/macho-read.f) and the <program>.map beside
\ it (tools/image-names.f), whose rows tile __text from its first byte. Each
\ byte falls in one class, and MEASURE dies unless the classes sum to the file's
\ length:
\ - code: the map's code rows, which end where its first pad or region row starts;
\ - names: the names, records and record-list rows, what the program holds to
\   find a word by its name;
\ - data written and data zero: the data and data-list rows; data zero is the
\   zero cells inside the DATA image's runs, data written the rest, the runs'
\   headers included;
\ - padding: the pad rows, the gap after the load commands up to __text, the
\   gap after __text to its segment's end and __DATA_CONST past __got;
\ - other: the header and load commands, __got and __LINKEDIT.
\ An ELF executable or a Wasm module is refused by name.

require src/lib/le.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/fs.f
require tools/image-names.f
require tools/macho-read.f

package IMAGE-SIZE

74 constant RC

DYNAMIC-BUFFER IMG n
variable ILEN
variable CODE-SIZE
variable NAME-SIZE
variable WRITTEN-SIZE
variable ZERO-SIZE
variable PAD-SIZE
variable OTHER-SIZE

: IMG@ ( -- ptr u8 )
   0 IMG BYTE-VIEW ;

: READ-IMAGE ( ptr u8 n -- )
   {: path:ptr pathu:n :}
   path pathu FS:FILE-SIZE {: size:n :}
   size CELL < if s" image-size: image too small" RC die then
   size CELL 1- + CELL / IMG-RESERVE
   size ILEN !
   path pathu IMG@ size FS:READ-ALL size <> if
      s" image-size: short read" RC die
   then ;

: REFUSAL$ ( -- ptr u8 n )
   IMG@ LE:U32@ {: magic:n :}
   magic $464C457F = if
      s" image-size: an ELF executable, which this report does not read" exit
   then
   magic $6D736100 = if
      s" image-size: a Wasm module, which this report does not read" exit
   then
   s" image-size: not a Habu Mach-O program" ;

: REFUSE ( -- )
   IMG@ ILEN @ MACHO-READ:MAGIC? 0= if REFUSAL$ RC die then ;

\ The room MAP-CHECK's line needs, the longest built around the path.
: MAP-ROOM ( ptr u8 n -- ptr u8 n )
   dup s" image-size: no map .map" nip + SB-CAP > if
      s" image-size: the program path is too long for its map path" RC die
   then ;

: MAP$ ( ptr u8 n -- ptr u8 n )
   MAP-ROOM SB-RESET SB-APPEND s" .map" SB-APPEND SB$ ;

: MAP-CHECK ( ptr u8 n -- )
   {: path:ptr pathu:n :}
   path pathu MAP$ FS:FILE? if exit then
   SB-RESET s" image-size: no map " SB-APPEND
   path pathu SB-APPEND s" .map" SB-APPEND SB$ RC die ;

: CONTAINER ( -- )
   MACHO-READ:COMMANDS-END MACHO-READ:GOT-BYTES + MACHO-READ:LINK-BYTES +
   OTHER-SIZE !
   MACHO-READ:TEXT-OFF MACHO-READ:COMMANDS-END -
   MACHO-READ:TEXT-LIMIT MACHO-READ:TEXT-END - +
   MACHO-READ:DATA-BYTES MACHO-READ:GOT-BYTES - +
   PAD-SIZE ! ;

: IMG-U64@ ( n -- n ) IMG@ + LE:U64@ ;

\ A run is [off u64][n u64][n cells]. Answers the file offset after the run
\ at `at` and the bytes of its zero cells.
: DATA-RUN ( n n -- n n )
   {: at:n end:n :}
   at 16 + end > if
      s" image-size: a DATA run's header passes its end" RC die then
   at 8 + IMG-U64@ {: len:n :}
   len 1 < len end at - 16 - 8 / > or if
      s" image-size: a DATA run passes its end" RC die then
   at 16 + len 8 * +
   0 len 0 ?do at 16 + i 8 * + IMG-U64@ 0= if 8 + then loop ;

\ The bytes of the zero cells in the runs at file offsets [at, end).
: DATA-ZEROS ( n n -- n )
   {: at:n end:n :}
   end MACHO-READ:TEXT-END > if
      s" image-size: the DATA image passes __text" RC die then
   0 at begin dup end < while end DATA-RUN rot + swap repeat drop ;

: DATA-REGION ( -- n n )
   s" data" IMAGE-NAMES:REGION {: start:n len:n :}
   start 0 < len 0 <= or if
      s" image-size: the map frames no DATA image" RC die then
   start len ;

: CODE-CLASS ( -- )
   IMAGE-NAMES:CODE-END {: end:n :}
   end IMAGE-NAMES:CODE-ATTR 0= if
      s" image-size: the map's code rows do not tile the code" RC die then
   end CODE-SIZE ! ;

: NAME-CLASS ( -- )
   s" names" IMAGE-NAMES:KIND-BYTES s" records" IMAGE-NAMES:KIND-BYTES +
   s" record-list" IMAGE-NAMES:KIND-BYTES + NAME-SIZE ! ;

: DATA-CLASSES ( n n -- )
   {: start:n len:n :}
   MACHO-READ:TEXT-OFF start + {: at:n :}
   at at len + DATA-ZEROS ZERO-SIZE !
   len s" data-list" IMAGE-NAMES:KIND-BYTES + ZERO-SIZE @ - WRITTEN-SIZE ! ;

: MAP-CLASSES ( -- )
   DATA-REGION {: start:n len:n :}
   CODE-CLASS
   NAME-CLASS
   s" pad" IMAGE-NAMES:KIND-BYTES PAD-SIZE +!
   start len DATA-CLASSES ;

: SUMS ( -- )
   CODE-SIZE @ NAME-SIZE @ + WRITTEN-SIZE @ + ZERO-SIZE @ +
   PAD-SIZE @ + OTHER-SIZE @ + ILEN @ <> if
      s" image-size: classes do not sum to the file length" RC die then ;

: TAB ( -- ) 9 emit ;

\ Tenths of a percent, so a 0.1% row is still visible beside a 35% one.
: SHARE ( n -- )
   {: bytes:n :}
   bytes 1000 * ILEN @ / {: tenths:n :}
   tenths 10 / FMT:.U 46 emit  tenths 10 mod FMT:.U ;

: ROW ( ptr u8 n n -- )
   {: name:ptr nameu:n bytes:n :}
   name nameu type TAB bytes FMT:.U TAB bytes SHARE cr ;

public

: MEASURE ( ptr u8 n -- )
   {: path:ptr pathu:n :}
   path pathu READ-IMAGE
   REFUSE
   IMG@ ILEN @ MACHO-READ:OPEN
   path pathu MAP-CHECK
   path pathu IMAGE-NAMES:LOAD
   CONTAINER
   MAP-CLASSES
   SUMS ;

: TOTAL-BYTES ( -- n ) ILEN @ ;
: CODE-BYTES ( -- n ) CODE-SIZE @ ;
: NAME-BYTES ( -- n ) NAME-SIZE @ ;
: DATA-WRITTEN ( -- n ) WRITTEN-SIZE @ ;
: DATA-ZERO ( -- n ) ZERO-SIZE @ ;
: PAD-BYTES ( -- n ) PAD-SIZE @ ;
: OTHER-BYTES ( -- n ) OTHER-SIZE @ ;

: REPORT ( -- )
   s" class" type TAB s" bytes" type TAB s" percent" type cr
   s" code" CODE-BYTES ROW
   s" names" NAME-BYTES ROW
   s" data-written" DATA-WRITTEN ROW
   s" data-zero" DATA-ZERO ROW
   s" padding" PAD-BYTES ROW
   s" other" OTHER-BYTES ROW
   s" total" TOTAL-BYTES ROW
   IMAGE-NAMES:REPORT-CODE ;

: .SUMMARY ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   s" size: " type name nameu type
   s"  " type TOTAL-BYTES FMT:.U s"  bytes = code " type CODE-BYTES FMT:.U
   s" , names " type NAME-BYTES FMT:.U
   s" , data " type DATA-WRITTEN FMT:.U s"  written + " type DATA-ZERO FMT:.U
   s"  zero, padding " type PAD-BYTES FMT:.U
   s" , other " type OTHER-BYTES FMT:.U cr ;

: RUN ( -- )
   SCRIPT-ARGC 1 <> if
      s" usage: <engine> --load tools/engine-size.f -- <program>" 64 die
   then
   0 SCRIPT-ARGV$ MEASURE
   REPORT ;

;package
