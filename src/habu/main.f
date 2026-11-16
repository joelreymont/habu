\ main.f - the engine's command line in Habu.
\ `--load` and stdin build one source stream, interpreted once, open, through
\ SOURCE-ROOT:RUN-ROOT; the program file of `hb prog.f` is a loaded file the
\ loader reads and runs (SOURCE-ROOT:RUN-PROGRAM). MAIN reads the
\ process's argv and stdin.

require src/lib/error.f
require src/lib/string.f
require src/lib/fs.f
require src/core/bytes.f
require src/habu/layout.f
require src/core/include.f
require src/core/rejection.f
require src/os/env-base.f
require src/habu/interpreter.f
require src/habu/repl.f

package COMMAND-LINE

private

64 constant RC-USAGE             \ an unknown flag or no file (USAGE, NO-FILES)
74 constant RC-SOURCE            \ the source cannot be had
$2D constant DASH
$23 constant HASH
$21 constant BANG
$5C constant BACKSLASH
$20 constant BLANK

create NL $0A c,

ENUM route load plain ;ENUM

: LOAD$ ( -- ^u8 n ) s" --load" ;
: SEP$ ( -- ^u8 n ) s" --" ;

\ Argument idx of an argv vector, a NUL-terminated string.
: ARG ( ^^u8 n -- ^u8 n )
   ptr-field @ dup ZLEN ;

\ The open record has no place: no source holds an argument or stdin.
: REJECT ( n -- ) {: rc:n :}
   REJECTION:KEEP  NULL$ rc die ;

\ The usage help lists the routes ROUTE knows.
: USAGE-HELP ( -- )
   REJECTION:HELP
   s" usage: hb file.f [args ...], hb --load file.f ... [-- args ...], or hb with the program on stdin"
   REJECTION:EMIT-STR ;

\ The unknown flag a u.
: USAGE ( ^u8 n -- ) {: a:^ u:n :}
   s" E-HB-USAGE" REJECTION:OPEN
   s" unknown flag `" REJECTION:EMIT-STR  a u REJECTION:EMIT-STR  96 REJECTION:EMIT1
   USAGE-HELP  RC-USAGE REJECT ;

\ No file to load: `--load` with nothing before the end or a `--`.
: NO-FILES ( -- )
   s" E-HB-USAGE" REJECTION:OPEN
   s" no source file to load" REJECTION:EMIT-STR
   USAGE-HELP  RC-USAGE REJECT ;

\ A read of the program on stdin failed with errno e, 0 for none.
: CANNOT-READ ( n -- ) {: e:n :}
   s" E-UNREADABLE-SOURCE" REJECTION:OPEN
   s" cannot read the program on stdin" REJECTION:EMIT-STR
   e 0<> if
      s" : " REJECTION:EMIT-STR  e SOURCE-ROOT:ERRNO-WHY$ SOURCE-ROOT:UNREAD-WHY
   then
   REJECTION:HELP
   s" give hb a readable program on stdin, or name a file: `hb file.f`" REJECTION:EMIT-STR
   RC-SOURCE REJECT ;

\ argv[1] picks it. `--load` takes the files after it up to the first `--`;
\ what follows is the program's arguments (src/os/script-argv.f).
\ Any other argument starting with `-` is an unknown flag (USAGE).
\ Anything else, `--` included, is the program file. Past argv[1] only `--` is
\ special: `--load a.f --x` loads a file named `--x`.
: ROUTE ( ^u8 n -- route ) {: a:^ u:n :}
   a u LOAD$ STR= if construct route load exit then
   a u SEP$ STR= if construct route plain exit then
   u 0 > if a c@ DASH = if a u USAGE then then
   construct route plain ;

\ The first `--` from argv[2] on, or argc: where a list's files end.
: LIST-END ( ^^u8 n -- n ) {: argv:^ argc:n :}
   argc 2 ?do
      argv i ARG SEP$ STR= if i unloop exit then
   loop
   argc ;

\ The argv indices [first, end) of the files a route loads.
: FILES ( ^^u8 n route -- n n ) {: argv:^ argc:n r:route :}
   r MATCH route
      plain OF 1 2 ENDOF
      load OF 2 argv argc LIST-END ENDOF
   ;MATCH ;

\ The root source stream, USED bytes of it. It grows only before it runs, so
\ the address STREAM and STDIN answer holds for the rest of the process.
DYNAMIC-BUFFER TEXT u8
variable USED

\ Bytes one read asks for: a Linux pipe's default capacity.
$10000 constant READ-SIZE

: APPEND ( ^u8 n -- ) {: a:^ u:n :}
   USED @ u + TEXT-RESERVE
   a USED @ TEXT u BYTE-COPY
   USED @ u + USED ! ;

\ One `s" <path>" <word>` row. Through script-required the registry loads
\ each file in its own include frame and skips one the engine holds.
: ROW ( ^u8 n ^u8 n -- ) {: path:^ u:n word:^ w:n :}
   S\" s\" " APPEND  path u APPEND  S\" \" " APPEND  word w APPEND  NL 1 APPEND ;

\ One read past USED; whether the file has ended.
: STEP ( n -- bool ) {: fd:n :}
   USED @ READ-SIZE + TEXT-RESERVE
   fd USED @ TEXT READ-SIZE read {: got:n :}
   got 0 < if FS:ERRNO CANNOT-READ then
   USED @ got + USED !
   got 0 = ;

\ A file that starts `#!` starts a line comment instead.
: SHEBANG ( ^u8 n -- ) {: a:^ u:n :}
   u 2 < if exit then
   a c@ HASH <> if exit then
   a 1 + c@ BANG <> if exit then
   BACKSLASH a c!
   BLANK a 1 + c! ;

: START-STREAM ( -- ) 0 USED ! ;

\ The program file's argv string, 0 off the plain route: PROGRAM-FILE-CELL
\ (src/habu/layout.f), from which src/core/include.f derives the program's
\ canonical path and its directory, the top-level root.
: PROGRAM-ARG ( -- ^^u8 )
   data-base PROGRAM-FILE-CELL + 0 ptr-field ;

\ The stream for files [first, end): one row per file, a newline between.
: STREAM ( ^^u8 n n -- ^u8 n ) {: argv:^ first:n end:n :}
   START-STREAM
   end first ?do
      i first > if NL 1 APPEND then
      argv i ARG s" script-required" ROW
   loop
   0 TEXT USED @ ;

: STDIN ( -- ^u8 n )
   START-STREAM
   begin 0 STEP until
   0 TEXT USED @ SHEBANG
   0 TEXT USED @ ;

\ The root stream runs open, as the engine's loop runs it: a stream that
\ leaves cells still exits 0 (src/core/include.f SOURCE-ROOT:RUN-ROOT).
: RUN-TEXT ( ^u8 n -- ) SOURCE-ROOT:RUN-ROOT ;

\ APP-ENTRY holds an executable token supplied by the image writer. Keep the
\ cell intact: SCRIPT-ARGC uses its nonzero value to choose the app convention.
CAST: APP-ACTION ( n -- [ -- ] )

: APP-RUN ( -- )
   data-base APP-ENTRY:XT-CELL + @ APP-ACTION execute ;

: APP? ( -- bool )
   data-base APP-ENTRY:XT-CELL + @ 0<> ;

\ argv[0] is the program; a vector with nothing after it has no file to load
\ (the assembly sends that one to the REPL before it reaches its file list).
: RUN ( ^^u8 n -- ) {: argv:^ argc:n :}
   argc 2 < if NO-FILES then
   argv 1 ARG ROUTE {: r:route :}
   argv argc r FILES {: first:n end:n :}
   first end >= if NO-FILES then
   r MATCH route
      load OF argv first end STREAM RUN-TEXT ENDOF
      plain OF
         argv first ARG over PROGRAM-ARG ! SOURCE-ROOT:RUN-PROGRAM
      ENDOF
   ;MATCH ;

\ Runs a source. A code from 1 to 255 is an exit status the program chose; any
\ other ends the process with the record of the throw nothing caught.
: RUN-SOURCE ( [ -- ] -- )
   {: q :}
   q ERROR:CAUGHT {: rc:n :}
   rc 0= if exit then
   REJECTION:RELEASE
   rc 0 >  rc 256 <  and if rc throw then
   rc ERROR:UNCAUGHT
   NULL$ UNCAUGHT-RC die ;

\ An application's MAIN takes the arguments, then stdin is the source. Off
\ one, an argument names what runs and stdin is its data; else stdin is source.
: ROUTED ( -- )
   APP? if
      [: APP-RUN ;] RUN-SOURCE
      TTY? if INTERPRETER:REPL else [: STDIN RUN-TEXT ;] RUN-SOURCE then
      exit
   then
   ARGC 1 > if [: ARGV-BASE ARGC RUN ;] RUN-SOURCE exit then
   TTY? if INTERPRETER:REPL exit then
   [: STDIN RUN-TEXT ;] RUN-SOURCE ;

public

\ Normal completion follows die's deliberate exit path, which runs the hook.
: MAIN ( -- )
   ROUTED
   NULL$ 0 die ;

\ Install MAIN as the entry the engine boot calls.
: INSTALL ( -- )
   ['] MAIN data-base ENGINE-MAIN:XT-CELL + xt! ;

;package
