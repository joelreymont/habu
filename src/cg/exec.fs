\ exec.fs — write the in-memory Mach-O to disk, ad-hoc sign it, and (optionally)
\ run it. Pure Forth + gforth's built-in file I/O and `system` — no FFI, no C.

require macho.fs

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

s" cg: codesign failed"  exception constant E-CODESIGN
s" cg: chmod failed"     exception constant E-CHMOD

: ADHOC-SIGN ( addr u -- )           \ ad-hoc sign; throw on failure (no silent fallback)
   cmd(  s" codesign -f -s - '" cs+  cs+  s" ' 2>/dev/null" cs+  )run
   if E-CODESIGN throw then ;

: CHMODX ( addr u -- )
   cmd(  s" chmod +x '" cs+  cs+  s" '" cs+  )run  if E-CHMOD throw then ;

\ Build current ICODE -> signed runnable executable at `filename`.
: EMIT-EXE ( addr u -- )
   BUILD-MACHO
   2dup WRITE-EXE
   2dup CHMODX
   ADHOC-SIGN ;

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
   2dup EMIT-EXE  2dup {: pa pu :}
   cmd(  [char] ' c+  pa pu cs+  [char] ' c+  )run  pa pu rot CRASH-CHECK  WSTAT>RC ;
