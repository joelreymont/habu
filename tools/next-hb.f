\ next-hb.f - builds the next arm64 hb from this tree as a cross-build
\ (LINKER:WITH): hb's startup files in hb's own order (src/habu/habu2.f
\ EMIT-HOST-LOAD-PREFIX), the runtime's files, then the product's registry and
\ installs; writes the program the argument names, with its map beside it.
\ Run from the tree root: bin/hb --load tools/next-hb.f -- <program>
\ Two files it writes under <out>/next-hb/ load inside the build, so a warm
\ build links what they do: reset.f, the token hb's prefix runs after include.f,
\ and end.f, the product's registry and installs.

require src/lib/error.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/build-out.f
require src/core/include.f
require src/habu/boot-files.f
require src/habu/linker.f
require src/habu/link-data.f

package NEXT-HB
private

PTR-VARIABLE ROW-PATH                   \ the file the build loads, for a failure's message
variable ROW-LEN
create DIR PATH-CAP allot  variable DIR-U
create RESET-PATH PATH-CAP allot  variable RESET-U
create END-PATH PATH-CAP allot  variable END-U

: RESET$ ( -- ptr u8 n ) RESET-PATH RESET-U @ ;
: END$ ( -- ptr u8 n ) END-PATH END-U @ ;


: PATHS ( -- )
   BUILD-OUT:BASE$ s" next-hb" DIR FS:JOIN-PATH DIR-U !
   DIR DIR-U @ FS:MAKE-DIRS
   DIR DIR-U @ s" reset.f" RESET-PATH FS:JOIN-PATH RESET-U !
   DIR DIR-U @ s" end.f" END-PATH FS:JOIN-PATH END-U ! ;


\ A row loads when its kind is common or the builder's own target (habu2.f PFX-LOAD?).
: LOAD? ( n -- bool )
   case
      BOOT-FILES:COMMON of true endof
      BOOT-FILES:LINUX of HB-TARGET-LINUX? endof
      BOOT-FILES:MACOS of HB-TARGET-MACOS? endof
      BOOT-FILES:X64 of HB-TARGET-LINUX-X86-64? endof
      false swap
   endcase ;


: INCLUDE-FILE ( ptr u8 n -- )
   {: a:ptr u:n :}
   a ROW-PATH !  u ROW-LEN !
   a u included ;


\ include.f defines both source vectors; hb's prefix resets them right after it
\ (habu2.f PFX-LOAD-CORE-ROW).
: ROW ( n n ptr u8 n -- )
   {: idx:n kind:n a:ptr u:n :}
   kind LOAD? 0= if exit then
   a u INCLUDE-FILE
   a u s" src/core/include.f" STR= if RESET$ INCLUDE-FILE then ;

: PROVIDE ( n n ptr u8 n -- )
   {: idx:n kind:n a:ptr u:n :}
   kind LOAD? if a u provided then ;

: REQUIRE-FILE ( ptr u8 n -- )
   {: a:ptr u:n :}
   a ROW-PATH !  u ROW-LEN !
   a u required ;


\ hb's startup files: habu2.f EMIT-HOST-LOAD-PREFIX's loads and provide rows.
\ The provide rows go to the build's registry, which `provided` writes inside a build.
\ They come before the loads, so the first boot file is the registry's first row, by
\ which hb finds its tree (include.f ENGINE-FIND-FOR), not a file a startup file requires.
: STARTUP ( -- )
   BOOT-FILES:CHECKER ['] PROVIDE BOOT-FILES:EACH
   BOOT-FILES:DECL ['] PROVIDE BOOT-FILES:EACH
   BOOT-FILES:CORE BOOT-FILES:DYNAMIC or BOOT-FILES:SEAL or BOOT-FILES:ARGV or
      BOOT-FILES:INTERNAL or BOOT-FILES:TOPROW or ['] PROVIDE BOOT-FILES:EACH
   BOOT-FILES:CHECKER ['] ROW BOOT-FILES:EACH
   BOOT-FILES:DECL ['] ROW BOOT-FILES:EACH
   BOOT-FILES:CORE BOOT-FILES:DYNAMIC or ['] ROW BOOT-FILES:EACH
   BOOT-FILES:OWNER ['] ROW BOOT-FILES:EACH
   BOOT-FILES:OWNER ['] PROVIDE BOOT-FILES:EACH
   BOOT-FILES:SEAL ['] ROW BOOT-FILES:EACH
   BOOT-FILES:BASELIB BOOT-FILES:RESTLIB or ['] PROVIDE BOOT-FILES:EACH
   BOOT-FILES:BASELIB BOOT-FILES:RESTLIB or ['] ROW BOOT-FILES:EACH
   BOOT-FILES:ARGV ['] ROW BOOT-FILES:EACH
   BOOT-FILES:INTERNAL ['] ROW BOOT-FILES:EACH
   BOOT-FILES:TOPROW ['] ROW BOOT-FILES:EACH ;


: REPL-TERM$ ( -- ptr u8 n )
   HB-TARGET-MACOS? if s" src/os/macos/repl-term.f" exit then
   s" src/os/linux/repl-term.f" ;


\ native-runtime.f's files after the startup ones, without its capture steps.
: RUNTIME-FILES ( -- )
   s" src/habu/engine-origin.f" REQUIRE-FILE
   s" src/compiler/native/compiler.f" REQUIRE-FILE
   s" src/habu/runtime-arm64.f" REQUIRE-FILE
   REPL-TERM$ REQUIRE-FILE
   s" src/habu/repl.f" REQUIRE-FILE
   s" src/habu/code-bytes.f" REQUIRE-FILE
   s" src/habu/debug-watch.f" REQUIRE-FILE
   s" src/habu/stepper.f" REQUIRE-FILE
   s" src/habu/debug.f" REQUIRE-FILE
   s" src/lib/c2-owner.f" REQUIRE-FILE
   s" src/lib/executable-build.f" REQUIRE-FILE
   s" src/habu/link-data.f" REQUIRE-FILE
   s" src/habu/runtime.f" REQUIRE-FILE ;


: END+ ( ptr u8 n -- ) {: a:ptr u:n :} END$ a u FS:APPEND-FILE ;

\ A registry row below the tree root, by its tree-relative name.
: REG-ROW ( ptr u8 n -- )
   {: a:ptr u:n :}
   SOURCE-ROOT:CWD$ {: c:ptr cu:n :}
   u cu 1 + > if a cu c cu STR= a cu + c@ FS:SLASH = and else false then
   0= if s" next-hb: a registry row outside the tree: " type a u type cr
      s" next-hb: build failed" 1 die then
   s\" s\" " END+  a cu + 1 + u cu - 1 - END+  s\" \" REQUIRE-REG:APPEND\n" END+ ;


\ The product's registry is the build's: end.f records each row with the
\ product's own include.f and packs them into its DATA, as a capture's prepare
\ does (native-runtime.f CAPTURE-PREPARE); then the installs. Last the product's
\ IMAGE-LIFECYCLE:PREPARE releases what its files' top levels acquired in this
\ process (FFI's library handles, mappings), which the written program must not carry.
: END-WRITE ( -- )
   END$ s\" REQUIRE-BOOT-OPEN\n" FS:WRITE-ALL
   REQUIRE-N @ REQUIRE-BASE @ ?do i REQUIRE-SLOT i REQUIRE-LEN@ REG-ROW loop
   s\" REQUIRE-BOOT-FREEZE\nREQUIRE-REG:PERSIST\nRUNTIME:INSTALL\nIMAGE-LIFECYCLE:PREPARE\n" END+ ;


: BUILD ( -- )
   STARTUP
   RUNTIME-FILES
   END-WRITE
   END$ INCLUDE-FILE
   s" compiled " type LINKER:COMPILED . cr
   0 SCRIPT-ARGV$ LINKER:WRITE ;

: WITH-BUILD ( -- ) s" engine" ['] BUILD LINKER:WITH ;


: FAILED ( n -- )
   {: rc:n :}
   s" next-hb: " type ROW-PATH @ ROW-LEN @ type s"  threw " type rc .
   rc ERROR:NAME$ type cr
   s" next-hb: build failed" 1 die ;

public

: RUN ( -- )
   SCRIPT-ARGC 1 <> if s" usage: hb --load tools/next-hb.f -- <program>" 64 die then
   HB-TARGET-LINUX-X86-64? if s" next-hb: builds arm64 only" 64 die then
   PATHS
   RESET$ s\" SOURCE-INPUT:RESET\n" FS:WRITE-ALL
   ['] WITH-BUILD catch {: rc:n :}
   rc 0<> if rc FAILED then ;

;package

NEXT-HB:RUN
