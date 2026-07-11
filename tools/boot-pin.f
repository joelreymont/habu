\ boot-pin.f - boot-prefix content pin: compute / verify the digest of the exact
\ checker/core source the small stdin engine re-reads at process start.
\
\ Background. `bin/hb` is the source-loading engine (docs/bootstrap.md): at
\ process start it re-reads the boot-prefix files from the checkout and re-parses
\ them (src/habu/habu2.f PFX-LOAD-BASE-FILES via LSRCRD), so an engine
\ built/certified against source revision A can boot against an edited revision B
\ — the boot-time reload TOCTOU. The build-time half (BF-PIN in
\ tools/build-fixpoint.f) pins the prefix across a single build; this tool pins it
\ across the build -> deploy boundary.
\
\ Why a tool, not a digest baked into the image + auto-checked at every boot: the
\ engine bakes ONLY primitives (the #PL registry); every colon word — the whole
\ checker/core and any generated `constant` — is re-read at boot and only
\ primitives are seeded into the boot dictionary. So a digest emitted as a
\ `constant` into the generated stage source is re-evaluated, not baked, and is
\ not name-resolvable before the (re-read) checker runs. An autonomous per-boot
\ verifier would need a NEW metacompiler capability (a build-computed baked-data
\ primitive / AOT-seeded blob) AND could not fail-closed by default without
\ deadlocking self-hosting (the rebuild command itself boots bin/hb re-reading
\ the drifted prefix). See dot habu-boot-pin-bake for the full RCA. This tool is
\ the feasible enforcement: capture the certified digest at build (`print`) and
\ fail-closed at deploy/CI (`verify <hex>`), fully checked, no boot-path risk.
\
\ The combined digest is a manifest hash: SHA-256 over the ordered per-file
\ SHA-256 digests of the boot-prefix files, in PFX-LOAD-BASE-FILES order then
\ PFX-LOAD-SCRIPT-ARGV. The order MUST match src/habu/habu2.f; the cross-check
\ test (test/boot-pin-test.f) enforces the path list against that source.

32 constant BP-LEN                  \ SHA-256 digest bytes
64 constant BP-HEX-LEN              \ SHA-256 hex digest chars
$400 constant BP-MAN-INIT            \ initial manifest bytes; grows with prefix rows
70 constant BP-DRIFT-RC             \ verify-drift exit code (checker-reject convention)
64 constant BP-USAGE-RC
1024 constant BP-PATH-CAP           \ max resolved prefix-file path
47 constant BP-SLASH

create BP-DIGEST  BP-LEN allot                    \ expected digest (set by BP-DIGEST-HEX!)
create BP-GOT     BP-LEN allot                    \ recomputed on-disk digest
create BP-FILE-DG BP-LEN allot                    \ per-file digest scratch
create BP-MAN-BOOT BP-MAN-INIT allot
PTR-VARIABLE BP-MAN-P   BP-MAN-BOOT BP-MAN-P !
variable BP-MAN-CAP BP-MAN-INIT BP-MAN-CAP !
create BP-HEXOUT  80 allot
create BP-ROOT    BP-PATH-CAP allot               \ optional path prefix (--root)
create BP-PATHBUF BP-PATH-CAP allot
variable BP-ROOT-U
variable BP-MAN-U
variable BP-OK
variable BP-SET

: BP-MANIFEST ( -- ptr a ) BP-MAN-P @ ;

: BP-MAN-ENSURE ( n -- ) {: need:n :}
   need BP-MAN-CAP @ <= if exit then
   need 0 <= if s" boot-pin: manifest capacity overflow" BP-DRIFT-RC die then
   BP-MAN-CAP @ $3FFFFFFFFFFFFFFF <= if BP-MAN-CAP @ 2 * need max else need then
   {: cap:n :}
   BP-MAN-P @ BP-MAN-CAP @ cap ARENA-BYTES-GROW BP-MAN-P !
   cap BP-MAN-CAP ! ;

: BP-TRUE ( -- bool )  0 0= ;
: BP-FALSE ( -- bool )  0 0= 0= ;

\ ---- expected-digest hex parse --------------------------------------------
: BP-NIB ( c -- n ) {: c:n :}
   c 48 >= c 58 < and if c 48 - exit then                 \ 0-9
   c 97 >= c 103 < and if c 87 - exit then                \ a-f
   c 65 >= c 71 < and if c 55 - exit then                 \ A-F
   0 ;

: BP-HEX-BYTE ( ptr u8 -- n ) {: a:ptr :}
   a c@ BP-NIB 16 *  a 1 + c@ BP-NIB + ;

: BP-DIGEST-HEX! ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u BP-HEX-LEN <> if BP-FALSE exit then
   BP-LEN 0 do
      a i 2 * + BP-HEX-BYTE  BP-DIGEST i + c!
   loop
   BP-TRUE BP-SET !  BP-TRUE ;

\ ---- optional root prefix (--root <dir>): hash a prefix at another tree -----
: BP-ROOT! ( ptr u8 n -- ) {: a:ptr u:n :}
   u BP-PATH-CAP > if s" boot-pin: --root too long" BP-USAGE-RC die then
   a BP-ROOT u BYTE-COPY  u BP-ROOT-U ! ;

: BP-PATH ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   BP-ROOT-U @ 0= if a u exit then
   BP-ROOT-U @ 1 + u + BP-PATH-CAP > if s" boot-pin: path too long" BP-USAGE-RC die then
   BP-ROOT BP-PATHBUF BP-ROOT-U @ BYTE-COPY
   BP-SLASH BP-PATHBUF BP-ROOT-U @ + c!
   a BP-PATHBUF BP-ROOT-U @ 1 + + u BYTE-COPY
   BP-PATHBUF BP-ROOT-U @ 1 + u + ;

\ ---- on-disk boot-prefix manifest hash -------------------------------------
: BP-ACC ( ptr u8 n -- )                                  \ hash one file, append its digest
   BP-PATH BP-FILE-DG SHA256-FILE 0 <> if BP-FALSE BP-OK ! exit then
   BP-MAN-U @ BP-LEN + BP-MAN-ENSURE
   BP-FILE-DG BP-MANIFEST BP-MAN-U @ + BP-LEN BYTE-COPY
   BP-MAN-U @ BP-LEN + BP-MAN-U ! ;

: BP-OS-TARGET$ ( -- ptr u8 n )
   HB-TARGET-MACOS? if s" src/os/macos/target.f" exit then
   s" src/os/linux/target.f" ;

: BP-OS-LAYOUT$ ( -- ptr u8 n )
   HB-TARGET-MACOS? if s" src/os/macos/layout.f" exit then
   s" src/os/linux/layout.f" ;

\ Canonical boot-prefix path list — the single source of truth. Order MUST match
\ src/habu/habu2.f PFX-LOAD-BASE-FILES then PFX-LOAD-SCRIPT-ARGV; the cross-check
\ test enforces membership + count against that source. The hash and the test
\ sandbox-copy both drive this, so the list lives in exactly one place.
: BP-EACH ( [ ptr u8 n -- ] -- ) {: q :}  \ typed-local-lint: allow-bare-local - quotation bound as ordinary local (docs/forth.md)
   s" src/core/util.f" q execute
   s" src/core/structures.f" q execute
   s" src/core/checker.f" q execute
   s" src/core/lower-cert-base.f" q execute
   s" src/core/type-schema.f" q execute
   s" src/core/type-family.f" q execute
   s" src/core/render.f" q execute
   s" src/core/sumtype.f" q execute
   s" src/core/layout-buffer.f" q execute
   s" src/core/layout-valid.f" q execute
   s" src/core/check-hook.f" q execute
   s" src/core/structures-effects.f" q execute
   s" src/core/roles.f" q execute
   s" src/core/bytes.f" q execute
   BP-OS-TARGET$ q execute
   BP-OS-LAYOUT$ q execute
   s" src/habu/layout.f" q execute
   s" src/os/env-base.f" q execute
   s" src/core/include.f" q execute
   s" src/core/enums.f" q execute
   s" src/core/exec-vector.f" q execute
   s" src/core/sha256.f" q execute
   s" src/core/type-family-sha.f" q execute
   s" src/core/combinators.f" q execute
   s" src/habu/xref.f" q execute
   s" src/core/layout-buffer-seal.f" q execute
   s" src/core/lower-cert-seal.f" q execute
   s" src/os/script-argv.f" q execute ;

: BP-HASH ( -- )                                          \ -> BP-GOT (+ BP-OK)
   BP-TRUE BP-OK !  0 BP-MAN-U !  [: BP-ACC ;] BP-EACH
   BP-MANIFEST BP-MAN-U @ BP-GOT SHA256 ;

: BP-HASH-HEX ( ptr a -- ) {: dst:ptr :}                  \ hex of on-disk prefix -> dst (64 chars)
   BP-HASH BP-GOT dst SHA256>HEX ;

: BP-MATCH? ( -- bool )
   BP-HASH
   BP-OK @ 0= if BP-FALSE exit then
   BP-GOT BP-LEN BP-DIGEST BP-LEN CORE-STR= ;

\ ---- CLI -------------------------------------------------------------------
: BP-DIAG$ ( -- ptr u8 n )
   s" boot-pin: on-disk boot prefix does not match expected digest (drift from certified checker/core source)" ;

: BP-USAGE ( -- )
   s" usage: tools/boot-pin.f [print | verify <64-hex>]" BP-USAGE-RC die ;

: BP-PRINT ( -- )
   BP-HEXOUT BP-HASH-HEX
   BP-OK @ 0= if s" boot-pin: unreadable boot-prefix file" BP-DRIFT-RC die then
   BP-HEXOUT BP-HEX-LEN type cr ;

: BP-VERIFY ( ptr u8 n -- )
   BP-DIGEST-HEX! 0= if BP-USAGE then
   BP-MATCH? if s" boot-pin: OK" type cr exit then
   BP-DIAG$ BP-DRIFT-RC die ;

: BP-ROOT-OPT ( n -- )                                    \ argv idx of "--root": take idx+1 as root
   1 + dup SCRIPT-ARGC >= if BP-USAGE then SCRIPT-ARGV$ BP-ROOT! ;

: BP-SCAN-ROOT ( -- )
   0 begin dup SCRIPT-ARGC < while
      dup SCRIPT-ARGV$ s" --root" CORE-STR= if BP-ROOT-OPT exit then
      1 +
   repeat drop ;

: BOOT-PIN-MAIN ( -- )
   BP-SCAN-ROOT
   SCRIPT-ARGC 0= if BP-PRINT exit then
   0 SCRIPT-ARGV$ s" print" CORE-STR= if BP-PRINT exit then
   0 SCRIPT-ARGV$ s" verify" CORE-STR= if
      SCRIPT-ARGC 2 < if BP-USAGE then
      1 SCRIPT-ARGV$ BP-VERIFY exit
   then
   BP-USAGE ;
