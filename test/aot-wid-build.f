\ aot-wid-build.f - build a protected-WID variant of the stdin engine.
\
\ Run as `bin/hb --load test/aot-wid-build.f` with HB_TMP pointing at a private
\ directory; on success it writes an `hb-pwid` engine into that directory. The
\ variant is identical to the shipped `bin/hb` except that its ahead-of-time
\ (AOT) section carries a protected-WID registry with two entries (word-list ids
\ 300 and 70000). Nothing in production is touched: the registry is baked ONLY
\ into this throwaway variant, through the same capture-buffer serialize the real
\ metabuild uses (AOT-CAPTURE's private PWID-PUT), so the shipped engine keeps an
\ empty registry.
\
\ How the registry is injected without editing production source: the stdin
\ metabuild driver src/habu/stdin.f ends with `STDIN-DRIVER:RUN`. This builder
\ verifies that terminal call, drops it, and appends a fixture that writes two
\ protected word-list ids into the capture buffer between capturing the REPL and
\ emitting the image. The rest of
\ the build reuses tools/build-fixpoint.f exactly as the normal stdin build does.
\
\ Its companion test/aot-wid-suite.f spawns this builder in a child process and
\ then probes the resulting hb-pwid to prove the protected-WID registry is
\ restored at engine startup, before any batch program runs.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/vector.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-fork.f
require lib/source.f
require lib/build.f
require lib/codesign.f
require lib/content-key.f
require lib/date.f
require tools/build-fixpoint.f

package AOT-WID-BUILD

: STDIN-SRC-PATH ( -- ptr u8 n ) s" src/habu/stdin.f" ;

$4000 constant SRC-CAP             \ src/habu/stdin.f is ~4 KB
$4000 constant DRV-CAP             \ driver = source minus terminal RUN + injection

create SRC-BUF SRC-CAP allot   variable SRC-U
create DRV-BUF DRV-CAP allot   variable DRV-U
create DRV-PATH-BUF FS-PATH-CAP allot   variable DRV-PATH-U

: DRV-PATH$ ( -- ptr u8 n )
   DRV-PATH-BUF DRV-PATH-U @ ;

: DRV-PATH! ( -- )                 \ <HB_TMP>/pwid-driver.f
   BF-TMP$ s" pwid-driver.f" DRV-PATH-BUF JOIN-PATH DRV-PATH-U ! ;

: READ-STDIN-SRC ( -- )
   STDIN-SRC-PATH SRC-BUF SRC-CAP READ-ALL SRC-U !
   SRC-U @ 0 <= if s" aot-wid-build: cannot read src/habu/stdin.f" BF-BUILD-RC die then ;

: WS? ( n -- bool ) {: c:n :}      \ whitespace: space, tab, cr, lf
   c 32 = c 9 = or c 13 = or c 10 = or ;

: SRC-LAST ( -- n )                \ index of last non-whitespace byte, or -1
   SRC-U @ 1-
   begin dup 0 >= while
      dup SRC-BUF + c@ WS? 0= if exit then
      1-
   repeat ;

: TAIL-BAD ( -- )
   s" aot-wid-build: src/habu/stdin.f no longer ends with STDIN-DRIVER:RUN" BF-BUILD-RC die ;

\ Length of stdin.f to keep: everything before the terminal driver call.
: RUN-KEEP ( -- n )
   SRC-LAST 1+ {: end:n :}
   s" STDIN-DRIVER:RUN" {: tail:ptr tailu:n :}
   end tailu < if TAIL-BAD then
   end tailu - {: start:n :}
   SRC-BUF start + tailu tail tailu STR= 0= if TAIL-BAD then
   start 0 > if SRC-BUF start 1- + c@ WS? 0= if TAIL-BAD then then
   start ;

: DRV-RESET ( -- )
   0 DRV-U ! ;

: DRV+ ( ptr u8 n -- ) {: a:ptr u:n :}
   DRV-U @ u + DRV-CAP > if s" aot-wid-build: driver buffer overflow" BF-BUILD-RC die then
   a DRV-BUF DRV-U @ + u BYTE-COPY
   DRV-U @ u + DRV-U ! ;

: DRV-NL ( -- )
   10 DRV-BUF DRV-U @ + c!
   DRV-U @ 1+ DRV-U ! ;

\ Optional AOT DATA-span forge (dot habu-guard-aot-data-49de2ee6). The reserve in
\ EM-AOT-RELOC-DATA advances DP by the baked LAOTDATASIZE span read straight from
\ the image; test/aot-data-span-forge.f probes that guard by baking a forged span.
\ When HABU_AOT_SPAN is set to a decimal, that value overwrites the captured span
\ AFTER CAPTURE-REPL and BEFORE ENGINE-EMIT:FORTH, so LAOTDATASIZE carries the forged value
\ (the forge test passes 2*DATA-SIZE, unambiguously past the seed headroom). No env
\ leaves the real capture untouched (the plain protected-WID build path).
: SPAN-FORGE-LINE ( -- )
   s" HABU_AOT_SPAN" GETENV {: v:ptr vu:n :}
   vu 0 > if
      s"    " DRV+  v vu DRV+  s"  AOT-DATA-SIZE !" DRV+ DRV-NL
   then ;

\ Optional corrupt-registry forge. The emitted engine validates this baked count
\ before touching the live protected-WID table, so PROT-WID-MAX+1 must die at
\ cold startup with the named AOT corruption diagnostic.
: PWID-CORRUPT-LINE ( -- )
   s" HABU_AOT_PWID_CORRUPT" GETENV nip 0 > if
      s"    PROT-WID-MAX 1+ AOT-PWID-N !" DRV+ DRV-NL
   then ;

\ Append the fixture driver, injecting two protected word-list ids
\ (300, one slot for WID > 255; and 70000, one slot for WID > 65535) into the
\ capture buffer after CAPTURE-REPL. These two literal ids are the fixture
\ contract: test/aot-wid-suite.f asserts exactly 300 and 70000 are restored, so
\ any drift here turns that suite red (it is self-checking) - keep the two in step.
: INJECT ( -- )
   s" package STDIN-DRIVER" DRV+ DRV-NL
   s" ' CAPTURE-REPL" DRV+ DRV-NL
   s" ;package" DRV+ DRV-NL
   s" execute" DRV+ DRV-NL
   s" package AOT-CAPTURE" DRV+ DRV-NL
   s" 300 0 PWID-PUT   70000 1 PWID-PUT   2 AOT-PWID-N !" DRV+ DRV-NL
   PWID-CORRUPT-LINE
   s" ;package" DRV+ DRV-NL
   SPAN-FORGE-LINE
   s" 0 0= STDIN? !" DRV+ DRV-NL
   s" HB@ 0 ENGINE-EMIT:FORTH" DRV+ DRV-NL
   S\" s\" hb\" STDIN-OUT DRV-EMIT-IMAGE" DRV+ DRV-NL
   s" DRV-EXIT-OK" DRV+ DRV-NL ;

: GEN-DRIVER ( -- )
   DRV-PATH!
   READ-STDIN-SRC
   RUN-KEEP {: keep:n :}
   DRV-RESET
   SRC-BUF keep DRV+                \ stdin.f minus its terminal driver call
   INJECT                           \ ... plus the registry-baking fixture
   DRV-PATH$ DRV-BUF DRV-U @ WRITE-ALL ;

: EMIT-PWID-STDIN ( -- )
   s" stage2-src" DRV-PATH$ BF-EMIT-STDIN-RUN-SOURCE ;

\ Build the maker (hb-pwid-mk) from the stage engine, then run the maker to emit
\ the final variant hb-pwid (mirrors BF-BUILD-STDIN-FROM-STAGE with our driver).
: RUN-MAKER ( -- )
   s" stage2-got" s" hb-pwid-mk" BF-RENAME-TMP
   s" hb-pwid-mk" BF-CHMOD-X-TMP
   s" hb-stdin-got" BF-REMOVE-TMP
   s" hb-pwid-mk" BF-RUN-ENV-TMP BF-RC0
   s" hb-stdin-got" BF-EXPECT
   s" hb-stdin-got" s" hb-pwid" BF-RENAME-TMP
   s" hb-pwid" BF-CHMOD-X-TMP
   s" hb-pwid" BF-CODESIGN-VERIFY-TMP ;

public

: BUILD ( -- )
   BF-STAGE-FIXPOINT               \ stage engine at fixpoint (reused if bin/hb already converged)
   GEN-DRIVER
   EMIT-PWID-STDIN
   BF-CERTIFY-STDIN
   BF-RUN-STAGE                    \ stage compiles the injected stdin source -> maker
   RUN-MAKER
   s" aot-wid-build: hb-pwid ready" type cr ;

;package

AOT-WID-BUILD:BUILD
