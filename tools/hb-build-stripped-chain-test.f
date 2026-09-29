\ hb-build-stripped-chain-test.f - checked fixture for tools/hb-build-lib.f:
\ what a stripped image carries by name or by size - the baked constants a
\ program prints, parses and hashes with, a closure past any fixed table size,
\ and the path scratch a file open reaches. tools/hb-build-test-lib.f lists
\ the other hb-build rows.
\ Run: bin/hb --load tools/hb-build-stripped-chain-test.f

require tools/hb-build-test-lib.f

\ The shared fixture's words are private words of the library's package, so
\ this row reopens it the way tools/hb-build-test-lib.f does.
package HB-BUILD-CLI

: HBT-PPH-SRC ( -- ptr u8 n )
   HBT-PPH-SRC-BUF HBT-PPH-SRC-U @ ;

: HBT-PPH-OUT ( -- ptr u8 n )
   HBT-PPH-OUT-BUF HBT-PPH-OUT-U @ ;

: HBT-CHAIN-SRC ( -- ptr u8 n )
   HBT-CHAIN-SRC-BUF HBT-CHAIN-SRC-U @ ;

: HBT-CHAIN-OUT ( -- ptr u8 n )
   HBT-CHAIN-OUT-BUF HBT-CHAIN-OUT-U @ ;

: HBT-OPENP-SRC ( -- ptr u8 n )
   HBT-OPENP-SRC-BUF HBT-OPENP-SRC-U @ ;

: HBT-OPENP-OUT ( -- ptr u8 n )
   HBT-OPENP-OUT-BUF HBT-OPENP-OUT-U @ ;

\ THE THREE BAKED CONSTANTS AN ORDINARY PROGRAM READS, in one program: printing
\ an integer reaches lib/fmt.f INT>NUM and its copy of STR-MIN-I64$, parsing one
\ reaches STR-PARSE-POS and STR-MAX-I64$, hashing reaches SHA-256's KK and HH0
\ in src/core/sha256.f, whose digest context is this program's own. Those tables
\ live in baked files, below every capture window, and each one of them refused
\ this program before
\ src/habu/aot-owned-cells.f named them (measured: caller=INT>NUM
\ target=STR-MIN-I64$, caller=STR-PARSE-POS target=STR-MAX-I64$, caller=SHA256
\ target=SHA-U). The digest is SHA-256 of "abc" from FIPS-180, so the pinned
\ stdout below proves the CARRIED bytes arrived, not merely that the image ran:
\ a zeroed KK or HH0 answers a different digest, and a zeroed bound table parses
\ nothing.
: HBT-PPH-SRC$ ( -- ptr u8 n )
   SB-RESET
   S\" require lib/fmt.f\ncreate PPH-DIG $20 allot\ncreate PPH-HEX $40 allot\n" SB-APPEND
   S\" create PPH-CTX SHA256-CTX-BYTES allot\n" SB-APPEND
   S\" : MAIN ( -- )\n   42 FMT:.INT cr\n   s\" 123\" STR-PARSE-POS MATCH option\n" SB-APPEND
   S\"      none OF s\" none\" type cr ENDOF\n     some OF FMT:.INT cr ENDOF\n   ;MATCH\n" SB-APPEND
   S\"    PPH-CTX s\" abc\" PPH-DIG SHA256-IN\n   PPH-DIG PPH-HEX SHA256>HEX\n   PPH-HEX $40 type cr ;\n" SB-APPEND
   SB$ ;

: HBT-PPH-EXPECTED$ ( -- ptr u8 n )
   SB-RESET
   s" 42" SB-APPEND 10 SB-APPEND-C
   s" 123" SB-APPEND 10 SB-APPEND-C
   s" ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad" SB-APPEND 10 SB-APPEND-C
   SB$ ;

\ OPENING A FILE BY PATH, the smallest program that reaches the engine's path
\ scratch: PATH0 (src/core/util.f) zero-terminates the path into PZB and hands
\ `open` that buffer, so PATH0's compiled code spells out an address below every
\ window. Reported from Tender, where it refused every entry point before
\ anything else - `caller=PATH0 target=PZB` - and answered by the FRESH-BYTES
\ claim in src/habu/aot-owned-cells.f. The printed line runs AFTER the close, so
\ the pinned stdout proves the image opened and closed the file rather than
\ merely exiting zero.
: HBT-OPENP-SRC$ ( -- ptr u8 n )
   SB-RESET
   S\" variable FD\n: MAIN ( -- )\n   s\" /dev/null\" PATH0 0 0 open FD !\n" SB-APPEND
   S\"    FD @ 0 < IF s\" openpath: cannot open /dev/null\" 74 die THEN\n" SB-APPEND
   S\"    FD @ close\n   s\" opened\" type cr ;\n" SB-APPEND
   SB$ ;

: HBT-OPENP-EXPECTED$ ( -- ptr u8 n )
   S\" opened\n" ;

\ A PROGRAM WHOSE CLOSURE IS ITS OWN SIZE. HBT-CHAIN-N words, each calling the
\ next and reaching no library, are exactly HBT-CHAIN-N + 1 closure members, so
\ this fixture measures the table sizing and nothing else. 1100 is past the 1024
\ rows the tables were once cut to by a constant - the wall that refused every
\ Tender entry point (dot habu-size-the-stripped-b2932715) and, measured on the
\ engine before the sizing landed, this very chain at `last_added_word='CW76'`.
\ The image prints the chain's value rather than merely exiting, so a closure
\ that built but lost a member is a wrong number and not a silent pass. The
\ source is generated here because 1101 definitions is 50 KB of fixture; the
\ builder holds one line at a time (SB-CAP is 1 KB).
1100 constant HBT-CHAIN-N
: HBT-SB-U+ ( n -- ) {: n:n :}
   n 0 < if E-STR-BOUNDS throw then
   n 10 >= if n 10 / recurse then
   n 10 mod STR-ZERO + SB-APPEND-C ;

: HBT-CHAIN-WORD! ( n -- ) {: i:n :}
   SB-RESET
   s" : CW" SB-APPEND i HBT-SB-U+ s"  ( n -- n ) " SB-APPEND
   i 0 > if s" CW" SB-APPEND i 1- HBT-SB-U+ s"  " SB-APPEND then
   s" dup 0 < if drop 0 then 1 + ;" SB-APPEND 10 SB-APPEND-C
   HBT-CHAIN-SRC SB$ APPEND-FILE ;

: HBT-CHAIN-SRC! ( -- )
   HBT-CHAIN-SRC s" " WRITE-ALL
   HBT-CHAIN-N 0 ?do i HBT-CHAIN-WORD! loop
   SB-RESET
   s" : MAIN ( -- ) 0 CW" SB-APPEND HBT-CHAIN-N 1- HBT-SB-U+
   s"  " SB-APPEND HBT-CHAIN-N HBT-SB-U+
   s\"  = if s\" chain=ok\" type cr then ;" SB-APPEND 10 SB-APPEND-C
   HBT-CHAIN-SRC SB$ APPEND-FILE ;

: HBT-CHAIN-EXPECTED$ ( -- ptr u8 n )
   SB-RESET
   s" chain=ok" SB-APPEND 10 SB-APPEND-C
   SB$ ;

\ ... a program that PRINTS an integer, PARSES one and HASHES a string builds
\ stripped, because every baked constant those three reach is carried by name,
\ and the image's stdout is the proof that the bytes travelled.
: HBT-STRIPPED-PRINT-PARSE-HASH ( -- )
   HBT-PPH-SRC HBT-PPH-SRC$ WRITE-ALL
   HBT-PPH-OUT HBT-REMOVE-FILE?
   HBT-PPH-SRC HBT-PPH-OUT HBT-HBB-PREPARE-AOT HBT-HBB-BUILD-OUT
   HBT-PPH-OUT FILE? TTRUE
   HBT-PPH-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 <> if HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   errn 0 T=
   HBT-RUN-OUT outn HBT-PPH-EXPECTED$ T$= ;

\ ... and a program whose closure is larger than any constant the linker used to
\ carry builds and runs: the tables are sized from the program (aot-closure.f
\ CLO-CAPACITY).
: HBT-STRIPPED-CHAIN ( -- )
   HBT-CHAIN-SRC!
   HBT-CHAIN-OUT HBT-REMOVE-FILE?
   HBT-CHAIN-SRC HBT-CHAIN-OUT HBT-HBB-PREPARE-AOT HBT-HBB-BUILD-OUT
   HBT-CHAIN-OUT FILE? TTRUE
   HBT-CHAIN-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 <> if HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   errn 0 T=
   HBT-RUN-OUT outn HBT-CHAIN-EXPECTED$ T$= ;

\ ... a stripped image OPENS A FILE BY PATH, because the path scratch PATH0
\ terminates into is claimed fresh with its byte length. Before that claim this
\ program - Tender's minimal reproducer - was refused with `outside the restored
\ span caller=PATH0 target=PZB`, and so was every entry point that reaches a file.
: HBT-STRIPPED-OPEN-PATH ( -- )
   HBT-OPENP-SRC HBT-OPENP-SRC$ WRITE-ALL
   HBT-OPENP-OUT HBT-REMOVE-FILE?
   HBT-OPENP-SRC HBT-OPENP-OUT HBT-HBB-PREPARE-AOT HBT-HBB-BUILD-OUT
   HBT-OPENP-OUT FILE? TTRUE
   HBT-OPENP-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 <> if HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   errn 0 T=
   HBT-RUN-OUT outn HBT-OPENP-EXPECTED$ T$=
   HBT-OPENP-OUT HBT-REMOVE-FILE? ;

\ Public so the driver below runs it with the package CLOSED: the subtests
\ drive real builds, which resolve names in whatever package scope is open.
public
: HBT-STRIPPED-CHAIN-MAIN ( -- )
   T-RESET
   HBT-PREPARE
   HBT-STRIPPED-PRINT-PARSE-HASH
   HBT-STRIPPED-CHAIN
   HBT-STRIPPED-OPEN-PATH
   CLEANUP-RUN
   HBT-ROOT EXISTS? TFALSE
   T-REPORT
   s" hb-build-stripped-chain-test: ok" type cr ;

;package

HB-BUILD-CLI:HBT-STRIPPED-CHAIN-MAIN
