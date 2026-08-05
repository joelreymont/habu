\ aot.f - thin AOT maker entry.
\
\ Load after src/habu/aot-lib.f.

package AOT-LINK

\ The AOT maker compiles the user program with its own interpret loop, so it must
\ install the sumtype constructor eval hook the stdin/REPL path gets from
\ include.f (src/core/include.f binds `[: INCLUDE-EVALUATE ;] is TDECL-EVAL-XT`).
\ Without it, any `SUMTYPE ...` declaration in an AOT source dies rc 76
\ ("sumtype: constructor eval hook not installed") before it can lower a matched
\ definition. `evaluate` compiles the generated constructor bodies into the
\ maker dictionary exactly as the engine does at interpret level. Named boundary:
\ source-string metaprogramming (`evaluate`) is outside checked inference.
\ Retirement: habu-builder-trust-rows-c5d41af6.
TRUSTED: AOT-CTOR-EVAL ( ptr u8 n -- ) evaluate ;
: AOT-CTOR-EVAL-INSTALL ( -- ) [: AOT-CTOR-EVAL ;] is TDECL-EVAL-XT ;
AOT-CTOR-EVAL-INSTALL
-1 TDECL-EVAL-ARMED !

\ --- preseeded test entry argv (tools/hb-build.f --preseed-entry / --preseed-seed):
\ argv[3] = selected entry word (default MAIN), argv[4] = seed cells as big-endian
\ u64 hex, 16 chars per cell, bottom-of-stack first.
\ src/habu/aot-lib.f EMIT-SEED materializes the cells before entry.
variable AOT-HEXACC
: AOT-HEXNIB ( n -- n ) {: c:n :}
   c 48 >= c 58 < and IF c 48 - EXIT THEN
   c 97 >= c 103 < and IF c 87 - EXIT THEN
   c 65 >= c 71 < and IF c 55 - EXIT THEN
   s" aot: bad preseed hex digit" 74 die ;
: AOT-HEX16 ( ptr u8 -- n ) {: a:ptr :}      \ 16 hex chars big-endian -> u64
   0 AOT-HEXACC !
   0 BEGIN dup 16 < WHILE
      AOT-HEXACC @ 4 lshift  over a + c@ AOT-HEXNIB or  AOT-HEXACC !
      1 +
   REPEAT drop
   AOT-HEXACC @ ;
: AOT-SEED-HEX ( ptr u8 n -- ) {: a:ptr u:n :}
   u 16 mod 0 <> IF s" aot: preseed hex not cell-aligned" 74 die THEN
   SEED-RESET
   0 BEGIN dup u < WHILE
      a over + AOT-HEX16 SEED+
      16 +
   REPEAT drop ;
: AOT-PRESEED-ARGS ( -- )
   ARGC 3 > IF 3 ARGV$ ENTRY-NAME! THEN
   ARGC 4 > IF 4 ARGV$ AOT-SEED-HEX THEN ;

\ Named boundary: installs the user-source checker hook; `set-check` is a
\ compiler-control op the checker rejects inside a checked body (top-level only).
\ Retirement: cap:checker-hook-identity.
TRUSTED: INSTALL-USER-HOOK ( -- )
   LOWER-CERT-HOOK:INSTALL
   ['] USER-HOOK set-check ;

public

: RUN ( -- )
   AOT-RUNTIME-ARGS
   AOT-PRESEED-ARGS
   READ-PROG
   SENTSET
   INSTALL-USER-HOOK
   AOT-PB@ data-base INP-CELL + !
   AOT-PB@ PN @ + data-base INE-CELL + ! ;

;package

AOT-LINK:RUN
