\ main-argv-child.f - the child side of test/main-argv.f: it builds an argv
\ vector, argv[0] then this run's script arguments, and hands it to
\ src/habu/main.f ENGINE-MAIN:RUN.
\
\   bin/hb --load src/habu/main.f test/main-argv-child.f -- <args>
\
\ loads what `bin/hb <args>` loads, through RUN instead of the assembly.

require src/os/script-argv.f
require src/habu/main.f

package MAIN-ARGV-CHILD

16 constant ARGS-CAP

ARGS-CAP PTR-U8-TABLE ARGS

public

: VECTOR ( -- ptr ptr u8 n )
   SCRIPT-ARGC 1 + ARGS-CAP > if s" main-argv-child: too many arguments" 2 die then
   0 ARGV ARGS 0 ptr-field !
   SCRIPT-ARGC 0 ?do i SCRIPT-ARGV ARGS i 1 + ptr-field ! loop
   ARGS SCRIPT-ARGC 1 + ;

\ The argv probe itself is a closed --load unit, while RUN leaves root source's
\ arbitrary stack effect in place for real process startup.
TRUSTED: CLEAR-STACK ( -- )
   depth 0 ?do drop loop ;

;package

\ Outside any package, as at boot.
MAIN-ARGV-CHILD:VECTOR ENGINE-MAIN:RUN
MAIN-ARGV-CHILD:CLEAR-STACK
