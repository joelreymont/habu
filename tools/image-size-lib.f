split-str
\ image-size-lib.f - where the bytes of a program the linker wrote went, read
\ from its container and the <program>.map beside it, whose rows tile the code
\ from its first byte. Each byte falls in one class, and
\ MEASURE dies unless the classes sum to the file's length:
\ - code: the map's code rows, which end where its first pad or region row starts;
\ - names: the names, records and record-list rows, what the program holds to
\   find a word by its name;
\ - data written and data zero: the data and data-list rows and, in ELF, the
\   carrier's load; in Mach-O data zero is the zero cells inside the DATA
\   image's runs, data written the rest, the runs' headers included; in ELF
\   the run table is data written, and the carrier's zero cells are data zero
\   and the rest data written;
\ - padding: the pad rows; in Mach-O the gap after the load commands up to
\   __text, the gap after __text to its segment's end and __DATA_CONST past
\   __got; in ELF the bytes before the code no header or table holds, the
\   gap after the code to its load's end and the gap after the read-write
\   load up to the carrier's;
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
require src/core/exit-status.f

package IMAGE-SIZE

DYNAMIC-BUFFER IMG n
variable ILEN
variable CODE-SIZE
variable NAME-SIZE
variable WRITTEN-SIZE
variable ZERO-SIZE
variable PAD-SIZE
variable OTHER-SIZE

: IMG@ ( -- ^u8 )
   0 IMG BYTE-VIEW ;

: READ-IMAGE ( str -- )
   {: path:^ pathu:n :}
   path pathu FS:FILE-SIZE {: size:n :}
   size CELL < if s" image-size: image too small" EXIT-STATUS:DATAERR die then
   size CELL 1- + CELL / IMG-RESERVE
   size ILEN !
   path pathu IMG@ size FS:READ-ALL size <> if
      s" image-size: short read" EXIT-STATUS:IOERR die
   then ;

: ELF? ( -- bool ) IMG@ ILEN @ ELF:MAGIC? ;

: REFUSAL$ ( -- str )
   IMG@ LE:U32@ $6D736100 = if
      s" image-size: a Wasm module, which this report does not read" exit
   then
   s" image-size: not a Habu program" ;

: REFUSE ( -- )
   ELF? IMG@ ILEN @ MACHO:MAGIC? or 0= if REFUSAL$ EXIT-STATUS:DATAERR die then ;

: OPEN ( -- )
   ELF? if IMG@ ILEN @ ELF:OPEN exit then
   IMG@ ILEN @ MACHO:OPEN ;

: MAP-CHECK ( str -- )
   IMAGE-NAMES:MAP$ 0= if
      2drop s" image-size: the program path is too long for its map path" EXIT-STATUS:FULL die
   then {: map:^ mapu:n :}
   map mapu FS:FILE? if exit then
   2 s" image-size: no map " write drop
   map mapu EXIT-STATUS:NOINPUT die ;

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
      s" image-size: the map's rows do not end at the code's last page" EXIT-STATUS:DATAERR die
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
   ELF:CARRIER drop ELF:TEXT-LIMIT - ELF:RW-BYTES - +
   PAD-SIZE ! ;

: CONTAINER ( -- )
   ELF? if ELF-CONTAINER exit then MACHO-CONTAINER ;

: IMG-U64@ ( n -- n ) IMG@ + LE:U64@ ;

\ A run is [off u64][n u64][n cells]. Answers the file offset after the run
\ at `at` and the bytes of its zero cells.
: DATA-RUN ( n n -- n n )
   {: at:n end:n :}
   at 16 + end > if
      s" image-size: a DATA run's header passes its end" EXIT-STATUS:DATAERR die then
   at 8 + IMG-U64@ {: len:n :}
   len 1 < len end at - 16 - 8 / > or if
      s" image-size: a DATA run passes its end" EXIT-STATUS:DATAERR die then
   at 16 + len 8 * +
   0 len 0 ?do at 16 + i 8 * + IMG-U64@ 0= if 8 + then loop ;

\ The bytes of the zero cells at file offsets [at, at + len).
: ZERO-CELLS ( n n -- n )
   {: at:n len:n :}
   0 len 0 ?do at i + IMG-U64@ 0= if 8 + then 8 +loop ;

\ The bytes of the zero cells in the runs at file offsets [at, end).
: DATA-ZEROS ( n n -- n )
   {: at:n end:n :}
   0 at begin dup end < while end DATA-RUN rot + swap repeat drop ;

: DATA-REGION ( -- n n )
   s" data" IMAGE-NAMES:REGION {: start:n len:n :}
   start 0 < len 0 <= or if
      s" image-size: the map frames no DATA image" EXIT-STATUS:DATAERR die then
   start len ;

: CODE-CLASS ( -- )
   IMAGE-NAMES:CODE-END {: end:n :}
   end IMAGE-NAMES:CODE-ATTR 0= if
      s" image-size: the map's code rows do not tile the code" EXIT-STATUS:DATAERR die then
   end CODE-SIZE ! ;

: NAME-CLASS ( -- )
   s" names" IMAGE-NAMES:KIND-BYTES s" records" IMAGE-NAMES:KIND-BYTES +
   s" record-list" IMAGE-NAMES:KIND-BYTES + NAME-SIZE ! ;

\ An ELF image's data rows are its run table, all data written.
: DATA-CLASSES ( n n -- )
   {: start:n len:n :}
   TEXT-OFF start + {: at:n :}
   at len + TEXT-END > if
      s" image-size: the DATA image passes the code" EXIT-STATUS:DATAERR die then
   ELF? if 0 else at at len + DATA-ZEROS then ZERO-SIZE !
   len s" data-list" IMAGE-NAMES:KIND-BYTES + ZERO-SIZE @ - WRITTEN-SIZE ! ;

\ An ELF image's carrier: its zero cells are data zero, the rest data written.
: CARRIER-CLASSES ( -- )
   ELF:CARRIER {: at:n len:n :}
   at len ZERO-CELLS {: zero:n :}
   zero ZERO-SIZE +!  len zero - WRITTEN-SIZE +! ;

: MAP-CLASSES ( -- )
   DATA-REGION {: start:n len:n :}
   CODE-CLASS
   NAME-CLASS
   s" pad" IMAGE-NAMES:KIND-BYTES PAD-SIZE +!
   start len DATA-CLASSES
   ELF? if CARRIER-CLASSES then ;

: SUMS ( -- )
   CODE-SIZE @ NAME-SIZE @ + WRITTEN-SIZE @ + ZERO-SIZE @ +
   PAD-SIZE @ + OTHER-SIZE @ + ILEN @ <> if
      s" image-size: classes do not sum to the file length" EXIT-STATUS:DATAERR die then ;

: TAB ( -- ) 9 emit ;

\ Tenths of a percent, so a 0.1% row is still visible beside a 35% one.
: SHARE ( n -- )
   {: bytes:n :}
   bytes 1000 * ILEN @ / {: tenths:n :}
   tenths 10 / FMT:.U 46 emit  tenths 10 mod FMT:.U ;

: ROW ( str n -- )
   {: name:^ nameu:n bytes:n :}
   name nameu type TAB bytes FMT:.U TAB bytes SHARE cr ;

public

: MEASURE ( str -- )
   {: path:^ pathu:n :}
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

: .SUMMARY ( str -- )
   {: name:^ nameu:n :}
   s" size: " type name nameu type
   s"  " type TOTAL-BYTES FMT:.U s"  bytes = code " type CODE-BYTES FMT:.U
   s" , names " type NAME-BYTES FMT:.U
   s" , data " type DATA-WRITTEN FMT:.U s"  written + " type DATA-ZERO FMT:.U
   s"  zero, padding " type PAD-BYTES FMT:.U
   s" , other " type OTHER-BYTES FMT:.U cr ;

: RUN ( -- )
   SCRIPT-ARGC 1 <> if
      s" usage: <engine> --load tools/engine-size.f -- <program>" EXIT-STATUS:USAGE die
   then
   0 SCRIPT-ARGV$ MEASURE
   REPORT ;

;package
