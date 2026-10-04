\ main.f - the engine's command line in Habu: which argv entries name source
\ files and how each is loaded, as the assembly reads them (src/habu/habu2.f:
\ the flag table FLAGTAB-DATA and its matcher LFLAGMATCH, the refusal
\ LSRCBADFLAG, C-SOURCE-FILE-PREFIX, C-SOURCE-FIND-SEP, C-SOURCE-FILE-LOOP and
\ C-SOURCE-APPEND-ARG, the reader LSRCRD and the shebang rewrite LSHBANG).
\
\ argv[1] picks the route. `--load` and `--build` take the files after it up
\ to the first `--`; what follows that `--` is the program's own arguments
\ (src/os/script-argv.f), whatever it looks like. Any other argument that
\ starts with `-` is refused with the usage line, exit 64. Anything else,
\ `--` included, is the one program file, and the rest are its arguments.
\ Past argv[1] only `--` is special: `--load a.f --x` loads a file named `--x`.
\
\ Every route builds one stream of source, as the assembly does. The root
\ stream runs through the unrestricted outer interpreter; only the individual
\ files named by script-required enter the loader's closed source boundary.
\ `--load` writes a `s" <path>" script-required` row per file, so the registry
\ loads each in its own include frame and skips a file the engine already
\ holds. The program file, and the first file of `--build`, are read raw into
\ the stream after a `s" <path>" provided` row, their `#!` line made a
\ comment; the files after the first of `--build` get `script-required` rows,
\ as the engine measured does it (`--build a.f missing.f` prints a.f's output,
\ then include's "cannot open").
\
\ MAIN reads the process's argv and stdin. RUN takes a vector instead, so a
\ test can hand it one it built.

require lib/string.f
require src/core/bytes.f
require src/habu/layout.f
require src/core/include.f
require src/os/env-base.f
require src/habu/interpret.f
require src/habu/repl.f
require lib/fmt.f

package ENGINE-MAIN

private

2 constant ERR-FD
64 constant RC-USAGE             \ habu2.f LSRCBADFLAG
74 constant RC-SOURCE            \ habu2.f LSRCRD, SRC-SFAIL: the source cannot be had
$2D constant DASH
$23 constant HASH
$21 constant BANG
$5C constant BACKSLASH
$20 constant BLANK

create NL $0A c,

ENUM route load build plain ;ENUM

: LOAD$ ( -- ptr u8 n ) s" --load" ;
: BUILD$ ( -- ptr u8 n ) s" --build" ;
: SEP$ ( -- ptr u8 n ) s" --" ;

\ Argument idx of an argv vector, a NUL-terminated string.
: ARG ( ptr ptr u8 n -- ptr u8 n )
   ptr-field @ dup ZLEN ;

\ ---- refusals ---------------------------------------------------------------
\ Each writes the engine's text on descriptor 2 and exits; like the engine's
\ own diagnostics, a write that fails is not reported, the exit is.
: SAY ( ptr u8 n -- ) {: a:ptr u:n :}
   ERR-FD a u write drop ;

\ habu2.f LSRCBADFLAG: the argument, then the usage line, which lists the
\ flags the route test below knows.
: USAGE ( ptr u8 n -- )
   s" hb: unknown flag: " SAY  SAY  NL 1 SAY
   s" usage: bin/hb " SAY  LOAD$ SAY  s"  " SAY  BUILD$ SAY  s"  " SAY  SEP$ SAY
   s"  [file.f]  (source on stdin)" RC-USAGE die ;

\ No file to load: `--load` or `--build` with nothing before the end or a
\ `--`. The assembly reaches its arena-overflow exit here (SRC-SFAIL) and says
\ the buffer is full; this keeps the status and names the cause.
: NO-FILES ( -- )
   s" hb: no source files" RC-SOURCE die ;

: CANNOT-OPEN ( ptr u8 n -- )
   s" hb: cannot open " SAY  RC-SOURCE die ;

: CANNOT-READ ( -- )
   s" hb: cannot read source" RC-SOURCE die ;

: FULL ( -- )
   s" hb: source prefix buffer full" RC-SOURCE die ;

: CANNOT-MAP ( -- )
   s" hb: cannot map the source arena" RC-SOURCE die ;

\ ---- the route ---------------------------------------------------------------
: ROUTE ( ptr u8 n -- route ) {: a:ptr u:n :}
   a u LOAD$ STR= if construct route load exit then
   a u BUILD$ STR= if construct route build exit then
   a u SEP$ STR= if construct route plain exit then
   u 0 > if a c@ DASH = if a u USAGE then then
   construct route plain ;

\ The first `--` from argv[2] on, or argc: where a list's files end.
: LIST-END ( ptr ptr u8 n -- n ) {: argv:ptr argc:n :}
   argc 2 ?do
      argv i ARG SEP$ STR= if i unloop exit then
   loop
   argc ;

\ The argv indices [first, end) of the files a route loads.
: FILES ( ptr ptr u8 n route -- n n ) {: argv:ptr argc:n r:route :}
   r MATCH route
      plain OF 1 2 ENDOF
      load OF 2 argv argc LIST-END ENDOF
      build OF 2 argv argc LIST-END ENDOF
   ;MATCH ;

\ ---- the stream ---------------------------------------------------------------
\ habu2.f C-SOURCE-FILE-LOOP and C-SOURCE-APPEND-ARG: one row per file, a
\ newline between files, all of it in one arena before any of it runs. The
\ bytes stay mapped for the life of the process, as the assembly's boot
\ source arena does.
PTR-VARIABLE ARENA
variable USED

: ROOM ( -- n )
   SOURCE-ARENA-CAP USED @ - ;

: CURSOR ( -- ptr u8 )
   ARENA @ USED @ + ;

: APPEND ( ptr u8 n -- ) {: a:ptr u:n :}
   u ROOM > if FULL then
   a CURSOR u BYTE-COPY
   USED @ u + USED ! ;

\ habu2.f LAPPPROV and LAPPREQ: `s" <path>" <word>`.
: ROW ( ptr u8 n ptr u8 n -- ) {: path:ptr u:n word:ptr w:n :}
   S\" s\" " APPEND  path u APPEND  S\" \" " APPEND  word w APPEND  NL 1 APPEND ;

\ One read at the cursor; whether the file has ended. A full arena refuses
\ before the read that would have found the end, as LSRCRD does.
: STEP ( n -- bool ) {: fd:n :}
   ROOM 0 = if FULL then
   fd CURSOR ROOM read {: got:n :}
   got 0 < if CANNOT-READ then
   USED @ got + USED !
   got 0 = ;

\ habu2.f LSHBANG: a file that starts `#!` starts a line comment instead.
: SHEBANG ( ptr u8 n -- ) {: a:ptr u:n :}
   u 2 < if exit then
   a c@ HASH <> if exit then
   a 1 + c@ BANG <> if exit then
   BACKSLASH a c!
   BLANK a 1 + c! ;

\ habu2.f LSRCRD: the whole file at the cursor. The path is NUL-terminated, as
\ every argv string is.
: FILE ( ptr u8 n -- ) {: path:ptr u:n :}
   path open-rd {: fd:n :}
   fd 0 < if path u CANNOT-OPEN then
   USED @ {: from:n :}
   begin fd STEP until
   fd close
   ARENA @ from + USED @ from - SHEBANG ;

: START-STREAM ( -- )
   SOURCE-ARENA-CAP map-anon 0 <> if drop CANNOT-MAP then ARENA !
   0 USED ! ;

\ The stream for files [first, end); raw says whether the first is read raw.
: STREAM ( ptr ptr u8 n n bool -- ptr u8 n ) {: argv:ptr first:n end:n raw:bool :}
   START-STREAM
   end first ?do
      i first > if NL 1 APPEND then
      argv i ARG
      i first = raw and if
         2dup s" provided" ROW FILE
      else
         s" script-required" ROW
      then
   loop
   ARENA @ USED @ ;

: STDIN ( -- ptr u8 n )
   START-STREAM
   begin 0 STEP until
   ARENA @ USED @ SHEBANG
   ARENA @ USED @ ;

\ Root source can change the user's stack by any amount. The outer loop owns
\ that effect; the checked caller cannot declare a fixed result row for it.
TRUSTED: RUN-TEXT ( ptr u8 n -- )
   OUTER:INTERPRET ;

\ APP-ENTRY holds an executable token supplied by the image writer. Keep the
\ cell intact: SCRIPT-ARGC uses its nonzero value to choose the app convention.
TRUSTED: APP-ACTION ( n -- [ -- ] ) ;

TRUSTED: APP-RUN ( -- )
   data-base APP-ENTRY:XT-CELL + @ APP-ACTION execute ;

: APP? ( -- bool )
   data-base APP-ENTRY:XT-CELL + @ 0<> ;

\ The x86 kernel calls this reporter with the throw code on the data stack.
\ It has no machine-side exit hook; run and clear that hook before any report.
TRUSTED: HOOK-ACTION ( n -- [ -- ] ) ;

TRUSTED: EXIT-HOOK ( -- )
   data-base EXIT-HOOK-CELL + {: slot:ptr :}
   slot @ {: xt:n :}
   0 slot !
   xt 0<> if xt HOOK-ACTION execute then ;

: THROW-REPORT ( n -- )
   SB-RESET
   s" hb: uncaught throw code " SB-APPEND
   FMT:SB-INT
   10 SB-APPEND-C
   2 SB$ write drop ;

: REPORT ( n -- )
   {: code:n :}
   EXIT-HOOK
   code 1 >= code 255 <= and if NULL$ code die then
   code THROW-REPORT
   data-base REFUSAL-ABI:CODE-CELL + @ {: refusal:n :}
   refusal 0<> code refusal = and if NULL$ 70 die then
   NULL$ UNCAUGHT-RC die ;

TRUSTED: REPORT-PTR ( -- ptr [ n -- ] )
   data-base ENGINE-MAIN:REPORT-CELL + ;

: LIST? ( route -- bool )
   MATCH route
      load OF true ENDOF
      build OF true ENDOF
      plain OF false ENDOF
   ;MATCH ;

public

\ The snapshot writer clears this process-owned mmap pointer in copied DATA.
: ARENA-CELL ( -- n )
   ARENA BYTE-VIEW data-base BYTE-VIEW - ;

\ Load what argv names, in order. argv[0] is the program and is not read; a
\ vector with nothing after it has no file to load (the assembly sends that
\ one to the REPL before it reaches its file list).
: RUN ( ptr ptr u8 n -- ) {: argv:ptr argc:n :}
   argc 2 < if NO-FILES then
   argv 1 ARG ROUTE {: r:route :}
   argv argc r FILES {: first:n end:n :}
   first end >= if NO-FILES then
   r MATCH route
      load OF false ENDOF
      build OF true ENDOF
      plain OF true ENDOF
   ;MATCH {: raw:bool :}
   argv first end raw STREAM RUN-TEXT ;

private

\ Unknown leading flags are checked before any stdin read. Explicit source
\ lists never consume stdin, including when stdin is a pipe.
: ROUTED ( -- )
   APP? if
      APP-RUN
      TTY? if REPL-ENABLE OUTER:REPL else STDIN RUN-TEXT then
      exit
   then
   ARGC 1 > if
      1 ARGV$ ROUTE LIST? if
         ARGV-BASE ARGC RUN exit
      then
   then
   TTY? if
      ARGC 1 > if ARGV-BASE ARGC RUN exit then
      REPL-ENABLE OUTER:REPL exit
   then
   STDIN {: a:ptr u:n :}
   u 0 > if a u RUN-TEXT exit then
   ARGC 1 > if ARGV-BASE ARGC RUN else a u RUN-TEXT then ;

public

\ Normal completion follows die's deliberate exit path, which runs the hook.
: MAIN ( -- )
   ROUTED
   NULL$ 0 die ;

\ The x86 linker copies the captured reporter from its fixed code cell into
\ protected UNCGH-CELL before it publishes the image. ARM boot installs its
\ assembly uncaught reporter separately.
: INSTALL ( -- )
   HB-TARGET-LINUX-X86-64? if ['] REPORT REPORT-PTR xt! then
   ['] MAIN data-base ENGINE-MAIN:XT-CELL + xt! ;

;package
