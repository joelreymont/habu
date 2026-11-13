\ image-size-lib.f - where the bytes of a program the linker wrote went, read
\ from its container and the <program>.map beside it, whose rows tile the code
\ from its first byte. Each byte falls in one class, and
\ MEASURE dies unless the classes sum to the file's length:
\ - code: the map's code rows, which end where its first pad or region row starts;
\ - names: the names, records and record-list rows, what the program holds to
\   find a word by its name;
\ - data written and data zero: the data and data-list rows; data zero is the
\   zero cells inside the DATA image's runs, data written the rest, the runs'
\   headers included;
\ - padding: the pad rows; in Mach-O the gap after the load commands up to
\   __text, the gap after __text to its segment's end and __DATA_CONST past
\   __got; in ELF the bytes before the code no header or table holds and the
\   gap after the code to its load's end;
\ - other: in Mach-O the header and load commands, __got and __LINKEDIT; in
\   ELF the header, the program headers, the dynamic-linking tables and the
\   read-write load's dynamic section and slots.
\ An ELF records no length for the code, so the code ends where the map's rows
\ do and its classes sum by construction; ELF-TEXT-END checks that end instead.
\ A Wasm module is refused by name.

require src/lib/le.f
require src/lib/fmt.f
require src/lib/fs.f
require tools/image-names.f
require src/os/macos/macho.f
require src/os/linux/elf.f

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

: ELF? ( -- bool ) IMG@ ILEN @ ELF:MAGIC? ;

: REFUSAL$ ( -- ptr u8 n )
   IMG@ LE:U32@ $6D736100 = if
      s" image-size: a Wasm module, which this report does not read" exit
   then
   s" image-size: not a Habu program" ;

: REFUSE ( -- )
   ELF? IMG@ ILEN @ MACHO:MAGIC? or 0= if REFUSAL$ RC die then ;

: OPEN ( -- )
   ELF? if IMG@ ILEN @ ELF:OPEN exit then
   IMG@ ILEN @ MACHO:OPEN ;

: MAP-CHECK ( ptr u8 n -- )
   IMAGE-NAMES:MAP$ 0= if
      2drop s" image-size: the program path is too long for its map path" RC die
   then {: map:ptr mapu:n :}
   map mapu FS:FILE? if exit then
   2 s" image-size: no map " write drop
   map mapu RC die ;

\ The bytes the map's rows tile from the code's first byte.
: MAP-BYTES ( -- n )
   IMAGE-NAMES:CODE-END
   s" pad" IMAGE-NAMES:KIND-BYTES + s" data" IMAGE-NAMES:KIND-BYTES +
   s" data-list" IMAGE-NAMES:KIND-BYTES + s" names" IMAGE-NAMES:KIND-BYTES +
   s" records" IMAGE-NAMES:KIND-BYTES +
   s" record-list" IMAGE-NAMES:KIND-BYTES + ;

: ZEROS? ( n n -- bool ) {: at:n end:n :}
   end at ?do IMG@ i + c@ 0<> if false unloop exit then loop true ;

\ The text's load pads the code with zeros to the next page.
: ELF-TEXT-END ( -- n )
   ELF:TEXT-OFF MAP-BYTES + {: end:n :}
   end ELF:TEXT-OFF >= end ELF:TEXT-LIMIT <= and
   ELF:TEXT-LIMIT end - ELF:TEXT-ALIGN < and
   end ELF:TEXT-LIMIT ZEROS? and 0= if
      s" image-size: the map's rows do not end at the code's last page" RC die
   then end ;

: TEXT-OFF ( -- n )
   ELF? if ELF:TEXT-OFF exit then MACHO:TEXT-OFF ;

: TEXT-END ( -- n )
   ELF? if ELF-TEXT-END exit then MACHO:TEXT-END ;

: MACHO-CONTAINER ( -- )
   MACHO:COMMANDS-END MACHO:GOT-BYTES + MACHO:LINK-BYTES +
   OTHER-SIZE !
   MACHO:TEXT-OFF MACHO:COMMANDS-END -
   MACHO:TEXT-LIMIT MACHO:TEXT-END - +
   MACHO:DATA-BYTES MACHO:GOT-BYTES - +
   PAD-SIZE ! ;

: ELF-CONTAINER ( -- )
   ELF:HEADERS-BYTES ELF:TABLES-BYTES + {: head:n :}
   head ELF:RW-BYTES + OTHER-SIZE !
   ELF:TEXT-OFF head - ELF:TEXT-LIMIT TEXT-END - +
   PAD-SIZE ! ;

: CONTAINER ( -- )
   ELF? if ELF-CONTAINER exit then MACHO-CONTAINER ;

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
   end TEXT-END > if
      s" image-size: the DATA image passes the code" RC die then
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
   TEXT-OFF start + {: at:n :}
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
   OPEN
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
