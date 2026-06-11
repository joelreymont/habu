\ exec.fs — write the in-memory Mach-O to disk, ad-hoc sign it, and (optionally)
\ run it. Pure Forth + gforth's built-in file I/O and `system` — no FFI, no C,
\ and no external `codesign` (sign.fs embeds the ad-hoc CodeDirectory itself).

require sign.fs

create CMD$ 512 allot   variable CMD#
: c+ ( c -- )  CMD$ CMD# @ + c!  1 CMD# +! ;
: cs+ ( addr u -- )  bounds ?do i c@ c+ loop ;
: cmd( ( -- )  0 CMD# ! ;
: )run ( -- wstatus )  CMD$ CMD# @ system  $? ;
: WSTAT>RC ( wstatus -- code )  8 rshift $FF and ;

: WRITE-EXE ( addr u -- )            \ write current MBUF[0..MLEN] to filename
   w/o create-file throw >r
   MBUF MLEN @ r@ write-file throw
   r> close-file throw ;

s" cg: chmod failed"     exception constant E-CHMOD

: CHMODX ( addr u -- )
   cmd(  s" chmod +x '" cs+  cs+  s" '" cs+  )run  if E-CHMOD throw then ;

: BASENAME ( a u -- a2 u2 )          \ strip directory: text after the last '/'
   {: a u :}  a u + {: e :}  a {: s :}
   a begin dup e < while
        dup c@ [char] / = if  dup 1+ to s  then  1+
     repeat drop
   s  e s - ;

\ Build current ICODE -> self-signed runnable executable at `filename`.
: EMIT-EXE ( addr u -- )
   2dup BASENAME SIG-ID 2!            \ ad-hoc identifier = binary basename
   BUILD-MACHO                        \ reserves the signature area in __LINKEDIT
   CODESIG                            \ fill it: embedded ad-hoc CodeDirectory
   2dup WRITE-EXE
   CHMODX ;

\ --- crash diagnostics: caf-built binaries install an in-binary signal handler
\ (crash.fs) that dumps the faulting registers to stderr and exit(134), so a crash
\ self-diagnoses. If a binary dies from a signal anyway (handler not installed, or
\ a re-fault), name the signal so it isn't a silent exit-0. ---
: SIG-NAME ( sig -- a u )
   dup  4 = if drop s" SIGILL"  exit then
   dup 11 = if drop s" SIGSEGV" exit then
   dup 10 = if drop s" SIGBUS"  exit then
   dup  5 = if drop s" SIGTRAP" exit then
   dup  6 = if drop s" SIGABRT" exit then
   dup  8 = if drop s" SIGFPE"  exit then
   drop s" signal" ;
: CRASH-CHECK {: pa pu ws -- ws :}         \ name the signal if ws says killed by one
   ws $7F and {: sig :}
   sig if
      cr ." *** caf-built binary killed by " sig SIG-NAME type ."  (signal " sig 0 .r ." )"
      ."  path=" pa pu type cr
   then  ws ;

\ Build + run, returning the decoded process exit code (0..255).
: RUN-EXE ( addr u -- code )
   2dup {: pa pu :} EMIT-EXE          \ EMIT-EXE consumes addr u; keep pa pu for the run
   cmd(  [char] ' c+  pa pu cs+  [char] ' c+  )run  pa pu rot CRASH-CHECK  WSTAT>RC ;
