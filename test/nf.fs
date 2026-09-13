\ nf.fs — native-Forth (Part F) build/run/capture harness. Reusable by tests and
\ from the REPL. Builds a standalone Mach-O from a habu source line, runs it, and
\ captures stdout. Use NFX interactively; NF-RUN + NF= in checked stdout tests.

require ../bootstrap/cg/forth.fs

\ Scratch paths. These were four fixed /tmp names, so two lanes running the
\ gforth-hosted checks at the same time overwrote each other's binary and each
\ other's captured stdout — the harness would then compare one lane's output
\ against the other lane's build (docs/archive/lessons-2026h1.md records the
\ same hazard). The directory is HB_TMP when the caller set one, which
\ tools/bootstrap.sh always does, so the periodic check keeps its artifacts in
\ the scratch dir it already owns; otherwise /tmp with this process's pid
\ appended, which is what a bare `gforth test/bootstrap-wide-memory.fs` gets.
128 constant NF-PATH-CAP

create NF-BIN NF-PATH-CAP allot   variable NF-BIN-U
create NF-OUT NF-PATH-CAP allot   variable NF-OUT-U
create NF-SRC NF-PATH-CAP allot   variable NF-SRC-U
create NF-RPL NF-PATH-CAP allot   variable NF-RPL-U

\ A shell-quoted path needs two wrapper quotes and, in the worst case, four
\ bytes for every input byte (the portable '\'' spelling of one apostrophe).
\ NF-REPL-CMD$ carries three such arguments plus its 21 bytes of redirection
\ syntax, so the combined buffer is sized from the path contract it serves.
NF-PATH-CAP 4 * 2 + constant NF-ARG-CAP
21 constant NF-REPL-SYNTAX-CAP
NF-ARG-CAP 3 * NF-REPL-SYNTAX-CAP + constant NF-CMD-CAP
create NF-CMD NF-CMD-CAP allot    variable NF-CMD-U

: NF-HB-TMP ( -- a u )  s" HB_TMP" getenv ;

: NF-ROOT ( -- a u )
   NF-HB-TMP dup 0> if exit then  2drop  s" /tmp" ;

\ Digits of this process's pid, or nothing when HB_TMP already names a private
\ directory. The pictured-numeric buffer is transient, so callers copy at once.
: NF-SUFFIX ( -- a u )
   NF-HB-TMP nip 0> if s" " exit then
   getpid s>d <# #s [char] - hold #> ;

: NF-APPEND ( buf uv a u -- )
   {: buf uv a u :}
   uv @ u + NF-PATH-CAP > abort" nf.fs: scratch path exceeds NF-PATH-CAP"
   a  buf uv @ +  u move
   u uv +! ;

: NF-BUILD ( buf uv a u -- )            \ buf := "<root>/<name><suffix>"
   {: buf uv a u :}
   0 uv !
   buf uv NF-ROOT   NF-APPEND
   buf uv s" /"     NF-APPEND
   buf uv a u       NF-APPEND
   buf uv NF-SUFFIX NF-APPEND ;

NF-BIN NF-BIN-U s" nf-bin"  NF-BUILD
NF-OUT NF-OUT-U s" nf-out"  NF-BUILD
NF-SRC NF-SRC-U s" nf-src"  NF-BUILD
NF-RPL NF-RPL-U s" nf-repl" NF-BUILD

: NF-BIN$ ( -- a u )  NF-BIN NF-BIN-U @ ;
: NF-OUT$ ( -- a u )  NF-OUT NF-OUT-U @ ;
: NF-SRC$ ( -- a u )  NF-SRC NF-SRC-U @ ;
: NF-RPL$ ( -- a u )  NF-RPL NF-RPL-U @ ;

: NF-CMD, ( a u -- )
   {: a u :}
   NF-CMD-U @ u + NF-CMD-CAP > abort" nf.fs: scratch command exceeds NF-CMD-CAP"
   a  NF-CMD NF-CMD-U @ +  u move
   u NF-CMD-U +! ;

\ Append one complete shell argument. The command contains only fixture-owned
\ paths; quoting each as one argument keeps whitespace and shell punctuation in
\ HB_TMP from changing argv or the surrounding redirections.
: NF-ARG, ( a u -- )
   s" '" NF-CMD,
   bounds ?do
      i c@ [char] ' = if s" '\''" NF-CMD, else i 1 NF-CMD, then
   loop
   s" '" NF-CMD, ;

: NF-RUN-CMD$ ( -- a u )
   0 NF-CMD-U !
   NF-BIN$ NF-ARG,  s"  > " NF-CMD,  NF-OUT$ NF-ARG,  s"  2>/dev/null" NF-CMD,
   NF-CMD NF-CMD-U @ ;

: NF-REPL-CMD$ ( -- a u )
   0 NF-CMD-U !
   NF-RPL$ NF-ARG,  s"  < " NF-CMD,  NF-SRC$ NF-ARG,
   s"  > " NF-CMD,  NF-OUT$ NF-ARG,  s"  2>/dev/null" NF-CMD,
   NF-CMD NF-CMD-U @ ;

2variable NFOUT
: NF-RUN ( src-a src-u -- )            \ build native Forth on src, run, capture stdout
   NF-BIN$ FORTH-EXE
   NF-RUN-CMD$ system
   NF-OUT$ slurp-file NFOUT 2! ;
: NF= ( a u -- f )  NFOUT 2@ compare 0= ;

: NFX ( src-a src-u -- )               \ build+run+show (interactive: `s" 5 SQ ." NFX`)
   2dup cr ." nf< " type  NF-RUN  cr ." nf> " NFOUT 2@ type ;

: NF-REPL ( src-a src-u -- )           \ build a stdin REPL, pipe src in, capture stdout
   NF-SRC$ w/o create-file throw {: fh :}
   fh write-file throw  fh close-file throw
   NF-RPL$ FORTH-REPL-EXE
   NF-REPL-CMD$ system
   NF-OUT$ slurp-file NFOUT 2! ;
