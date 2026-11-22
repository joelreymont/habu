split-str
\ An engine started from a long working directory within PATH-CAP loads its
\ entry, and from that directory and a short one an entry written as a `./`
\ chain past 8 KB that resolves within PATH-CAP, through `hb P`, `--load`, an
\ include, a require and tools/check.f; one started from a working directory
\ that does not resolve, here one removed before the engine starts, rejects it
\ with one record. It does not show a working directory past PATH-CAP or
\ FS:REMOVE-TREE over such paths: no test makes a path past PATH-CAP
\ (test/AGENTS.md). The children's logs are kept in the tree, which the process
\ removes at exit.
require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/process-cwd.f
require src/lib/engine-candidate.f
require test/pcap-result.f

package DEEP-CWD-E2E
private

$4000 constant IO-CAP
create ROOT PATH-CAP allot variable ROOT-U
create ENTRY PATH-CAP allot variable ENTRY-U
create ENGINE PATH-CAP allot variable ENGINE-U
create MID PATH-CAP allot variable MID-U
create GONE PATH-CAP allot variable GONE-U
create PATH PATH-CAP allot
create OUT IO-CAP allot
create ERR IO-CAP allot
variable OUT-U
variable ERR-U
variable RC

: ROOT$ ( -- ^u8 n ) ROOT ROOT-U @ ;
: ENTRY$ ( -- str ) ENTRY ENTRY-U @ ;
: ENGINE$ ( -- ^u8 n ) ENGINE ENGINE-U @ ;
: MID$ ( -- str ) MID MID-U @ ;
: GONE$ ( -- str ) GONE GONE-U @ ;

: CHILD$ ( -- str )
   S\" package DEEP-CWD-CHILD\npublic\n: RUN ( -- ) s\q deep cwd: ok\q type cr ;\n;package\nDEEP-CWD-CHILD:RUN\n" ;

\ CHAIN is 4200 `./` segments before entry.f: 8407 bytes written, the entry's
\ own path once resolved. inc.f includes it and req.f requires it.
4200 constant DOTS
DOTS 2 * 7 + constant CHAIN-BYTES
create CHAIN CHAIN-BYTES allot
create SRC CHAIN-BYTES 16 + allot variable SRC-U
create CHECKER PATH-CAP allot variable CHECKER-U

: CHAIN$ ( -- str ) CHAIN CHAIN-BYTES ;

: CHAIN! ( -- )
   DOTS 0 ?do
      [char] . CHAIN i 2 * + c!  [char] / CHAIN i 2 * 1 + + c!
   loop
   s" entry.f" {: a:^ u:n :}  a CHAIN DOTS 2 * + u BYTE-COPY
   SOURCE-ROOT:CWD$ s" tools/check.f" CHECKER FS:JOIN-PATH CHECKER-U ! ;

: SRC+ ( str -- ) {: a:^ u:n :}
   a SRC SRC-U @ + u BYTE-COPY  u SRC-U +! ;

\ Writes the file name in dir, a word w and the chain its one line.
: LOADER! ( str str str -- )
   {: dir:^ diru:n name:^ nameu:n w:^ wu:n :}
   0 SRC-U !  w wu SRC+  s"  " SRC+  CHAIN$ SRC+  S\" \n" SRC+
   dir diru name nameu PATH FS:JOIN-PATH {: pathu:n :}
   PATH pathu SRC SRC-U @ FS:WRITE-ALL ;

: LOADERS! ( str -- ) {: dir:^ diru:n :}
   dir diru s" inc.f" s" include" LOADER!
   dir diru s" req.f" s" require" LOADER! ;

\ TAIL is three 200-byte segments, and MID is ROOT$/deep/TAIL.
200 constant SEG-BYTES
SEG-BYTES 1 + 3 * 1 - constant TAIL-BYTES
create TAIL TAIL-BYTES allot

: TAIL$ ( -- str ) TAIL TAIL-BYTES ;

: TAIL! ( -- )
   TAIL-BYTES 0 ?do
      [char] d i 1 + SEG-BYTES 1 + mod 0= if drop [char] / then TAIL i + c!
   loop ;

: SETUP ( -- )
   s" habu-deep-cwd" FS:MKTEMP-DIR {: a:^ u:n :}
   a u FS:CLEANUP-TREE+
   a ROOT u BYTE-COPY u ROOT-U !
   ENGINE-CANDIDATE:PATH$ SOURCE-ROOT:CANONICAL TTRUE {: e:^ eu:n :}
   e ENGINE eu BYTE-COPY eu ENGINE-U !
   ROOT$ s" entry.f" ENTRY FS:JOIN-PATH ENTRY-U !
   ENTRY$ CHILD$ FS:WRITE-ALL
   ROOT$ s" gone" GONE FS:JOIN-PATH GONE-U !
   GONE$ FS:MAKE-DIR
   TAIL!
   ROOT$ s" deep" PATH FS:JOIN-PATH {: pathu:n :}
   PATH pathu TAIL$ MID FS:JOIN-PATH MID-U !
   MID$ FS:MAKE-DIRS
   MID$ s" entry.f" PATH FS:JOIN-PATH PATH swap CHILD$ FS:WRITE-ALL
   CHAIN!  ROOT$ LOADERS!  MID$ LOADERS! ;

: SAVE ( ^u8 n ^u8 n -- ) {: rel:^ relu:n data:^ size:n :}
   ROOT$ rel relu PATH FS:JOIN-PATH {: pathu:n :}
   PATH pathu data size FS:WRITE-ALL ;

: ENGINE-ARGV+ ( -- )
   ENGINE$ PROC-ARGV+
   s" --load" PROC-ARGV+
   ENTRY$ PROC-ARGV+ ;

: RUN-CHILD ( str str -- )
   {: prog:^ progu:n cwd:^ cwdu:n :}
   PROC-ENV-INHERIT-MISSING
   prog progu cwd cwdu
   OUT IO-CAP >LEN ERR IO-CAP >LEN PROCESS:WAIT-FOREVER
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE
   PCAP-RESULT:>N RC ! ERR-U ! OUT-U ! ;

: KEEP ( ^u8 n ^u8 n -- ) {: out:^ outu:n err:^ erru:n :}
   out outu OUT OUT-U @ SAVE
   err erru ERR ERR-U @ SAVE ;

: FITS ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   ENGINE-ARGV+
   s" /usr/bin/env" MID$ RUN-CHILD
   s" fits.out" s" fits.err" KEEP
   RC @ 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   s" the entry loads from a long directory within PATH-CAP" T-LABEL
   RC @ 0 T=
   OUT OUT-U @ s" deep cwd: ok" CONTAINS? TTRUE ;

\ Each child is its own engine: each reader resolves the chain at its start.
: ARGV ( -- ) PROC-CWD:ARGV-ENV-CWD-RESET  ENGINE$ PROC-ARGV+ ;
: LOAD-ARGV ( str -- ) ARGV  s" --load" PROC-ARGV+  PROC-ARGV+ ;

: RAN ( ^u8 n ^u8 n -- ) {: cwd:^ cwdu:n what:^ whatu:n :}
   s" /usr/bin/env" cwd cwdu RUN-CHILD
   RC @ 0<> if cwd cwdu type cr OUT OUT-U @ type ERR ERR-U @ type then
   what whatu T-LABEL  RC @ 0 T= ;

: SAID ( -- )
   s" and runs it" T-LABEL
   OUT OUT-U @ s" deep cwd: ok" CONTAINS? TTRUE ;

: CHAINED ( str -- ) {: cwd:^ cwdu:n :}
   ARGV  CHAIN$ PROC-ARGV+
   cwd cwdu s" `hb P` takes an entry written past 8 KB" RAN SAID
   CHAIN$ LOAD-ARGV
   cwd cwdu s" `hb --load P` takes it" RAN SAID
   s" inc.f" LOAD-ARGV
   cwd cwdu s" an include of it loads it" RAN SAID
   s" req.f" LOAD-ARGV
   cwd cwdu s" a require of it loads it" RAN SAID
   CHECKER CHECKER-U @ LOAD-ARGV  s" --" PROC-ARGV+  CHAIN$ PROC-ARGV+
   cwd cwdu s" tools/check.f checks it" RAN ;

: CHAINS ( -- ) ROOT$ CHAINED  MID$ CHAINED ;

\ The record of a working directory that does not resolve, PATH-CAP its limit.
create WANT IO-CAP allot variable WANT-U

: WANT+ ( str -- ) {: a:^ u:n :}
   a WANT WANT-U @ + u BYTE-COPY  u WANT-U +! ;

: DIGITS+ ( n -- ) {: v:n :}
   v 10 >= if v 10 / recurse then
   v 10 mod [char] 0 + WANT WANT-U @ + c!  1 WANT-U +! ;

: GONE-WANT! ( -- )
   0 WANT-U !
   s" error[E-SOURCE-ROOT]: the working directory does not resolve to a searchable directory" WANT+
   S\" \n  = limit: " WANT+  PATH-CAP DIGITS+
   S\"  bytes\n  = help: run hb from a directory that exists, that you may search, and whose path fits the limit\n\n" WANT+ ;

: REMOVED ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" -c" PROC-ARGV+
   S\" rmdir \q$PWD\q && exec \q$0\q --load \q$1\q" PROC-ARGV+
   ENGINE$ PROC-ARGV+ ENTRY$ PROC-ARGV+
   s" /bin/sh" GONE$ RUN-CHILD
   s" gone.out" s" gone.err" KEEP
   s" a removed working directory exits 74" T-LABEL
   RC @ 74 T=
   s" and prints nothing on stdout" T-LABEL
   OUT-U @ 0 T=
   s" and writes its record" T-LABEL
   GONE-WANT!  ERR ERR-U @ WANT WANT-U @ T$= ;

public

: RUN ( -- )
   T-RESET SETUP FITS CHAINS REMOVED T-REPORT ;

;package

DEEP-CWD-E2E:RUN
