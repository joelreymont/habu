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

: ADHOC-SIGN ( addr u -- wstatus )
   cmd(  s" codesign -f -s - '" cs+  cs+  s" ' 2>/dev/null" cs+  )run ;

: CHMODX ( addr u -- wstatus )
   cmd(  s" chmod +x '" cs+  cs+  s" '" cs+  )run ;

\ Build current ICODE -> signed runnable executable at `filename`.
: EMIT-EXE ( addr u -- )
   BUILD-MACHO
   2dup WRITE-EXE
   2dup CHMODX drop
   ADHOC-SIGN drop ;

\ Build + run, returning the decoded process exit code (0..255).
: RUN-EXE ( addr u -- code )
   2dup EMIT-EXE
   cmd(  [char] ' c+  cs+  [char] ' c+  )run  WSTAT>RC ;
