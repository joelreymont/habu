\ hb-build-core.f - builds this tree's arm64 hb, hb with a program's files
\ (--repl), or a stripped program, as a cross-build in the running hb
\ (LINKER:WITH) and writes it with its map beside it. Each loads hb's startup
\ files in hb's own order (src/habu/habu2.f EMIT-HOST-LOAD-PREFIX); hb then loads
\ the runtime's files, --repl the program's source, and both their registry and
\ installs; a stripped program loads its source and its entry.
\ The files written under <out>/hb-build/ load inside the build, so a warm build
\ links what they do: reset.f, the token hb's prefix runs after include.f;
\ end-<sha>.f, hb's registry and installs; app-end.f, a stripped program's entry.
\ Builds sharing an out root write each whole (FS:ATOMIC-WRITE-FILE).

require src/lib/error.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/build-out.f
require src/core/include.f
require src/core/sha256.f
require src/habu/boot-files.f
require src/habu/linker.f
require src/habu/link-data.f

package HB-BUILD-CORE
private

PTR-VARIABLE ROW-PATH                   \ the file the build loads, for a failure's message
variable ROW-LEN
PTR-VARIABLE SRC-PATH
variable SRC-LEN
PTR-VARIABLE OUT-PATH
variable OUT-LEN
create DIR PATH-CAP allot  variable DIR-U
create RESET-PATH PATH-CAP allot  variable RESET-U
create END-PATH PATH-CAP allot  variable END-U
create END-NAME 70 allot                \ end-, 64 hex digits, .f
create END-SHA SHA256-CTX-BYTES allot
create END-DIGEST 32 allot
create APP-END-PATH PATH-CAP allot  variable APP-END-U
DYNAMIC-BUFFER TEXT u8                  \ the end file's text until it is written
variable TEXT-U

: RESET$ ( -- ptr u8 n ) RESET-PATH RESET-U @ ;
: END$ ( -- ptr u8 n ) END-PATH END-U @ ;
: APP-END$ ( -- ptr u8 n ) APP-END-PATH APP-END-U @ ;
: SRC$ ( -- ptr u8 n ) SRC-PATH @ SRC-LEN @ ;
: OUT$ ( -- ptr u8 n ) OUT-PATH @ OUT-LEN @ ;


: PATHS ( -- )
   BUILD-OUT:BASE$ s" hb-build" DIR FS:JOIN-PATH DIR-U !
   DIR DIR-U @ FS:MAKE-DIRS
   DIR DIR-U @ s" reset.f" RESET-PATH FS:JOIN-PATH RESET-U !
   DIR DIR-U @ s" app-end.f" APP-END-PATH FS:JOIN-PATH APP-END-U !
   RESET$ s\" SOURCE-INPUT:RESET\n" FS:ATOMIC-WRITE-FILE ;


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


\ hb's files after STARTUP's, and the source verifier
\ tools/check-verify-child.f runs.
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
   s" src/habu/verify-source.f" REQUIRE-FILE
   s" src/habu/runtime.f" REQUIRE-FILE ;


: TEXT+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   TEXT-U @ u + TEXT-RESERVE
   a TEXT-U @ TEXT u BYTE-COPY
   u TEXT-U +! ;

\ A row's bytes inside the end file's s\" literal: `"` and `\` take a backslash.
: ROW+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 ?do
      a i + c@ dup 34 = swap 92 = or if s" \" TEXT+ then
      a i + 1 TEXT+
   loop ;

\ A registry row below the tree root by its tree-relative name. A program's row
\ outside it keeps its canonical path (include.f REQUIRE-STORE); hb's is refused.
: REG-ROW ( ptr u8 n bool -- )
   {: a:ptr u:n engine:bool :}
   SOURCE-ROOT:CWD$ {: c:ptr cu:n :}
   u cu 1 + > if a cu c cu STR= a cu + c@ FS:SLASH = and else false then
   {: below:bool :}
   below 0= engine and if a ROW-PATH !  u ROW-LEN !  E-BUILD-PATH throw then
   s\" s\\\" " TEXT+
   below if a cu + 1 + u cu - 1 - ROW+ else a u ROW+ then
   s\" \" REQUIRE-REG:APPEND\n" TEXT+ ;


\ The end file's name carries its text's SHA-256, so builds sharing an out root
\ whose registries differ never load each other's.
: END-PATH! ( -- )
   END-SHA 0 TEXT TEXT-U @ END-DIGEST SHA256-IN
   s" end-" drop END-NAME 4 BYTE-COPY
   END-DIGEST END-NAME 4 + SHA256>HEX
   s" .f" drop END-NAME 68 + 2 BYTE-COPY
   DIR DIR-U @ END-NAME 70 END-PATH FS:JOIN-PATH END-U ! ;


\ The product's registry is the build's: the end file records each row with the
\ product's own include.f and packs them into its DATA; then the installs, and
\ under --repl the program's MAIN as hb's entry, which hb runs before its REPL
\ or standard input (src/habu/main.f ROUTED). Last the product's
\ IMAGE-LIFECYCLE:PREPARE releases what its files' top levels acquired in this
\ process (FFI's library handles, mappings), which the written program must not
\ carry.
: END-WRITE ( n bool -- )
   {: app:n entry:bool :}
   0 TEXT-U !
   s\" REQUIRE-BOOT-OPEN\n" TEXT+
   REQUIRE-N @ REQUIRE-BASE @ ?do i REQUIRE-SLOT i REQUIRE-LEN@ i app < REG-ROW loop
   s\" REQUIRE-BOOT-FREEZE\nREQUIRE-REG:PERSIST\nRUNTIME:INSTALL\n" TEXT+
   entry if s\" ' MAIN APP-ENTRY:XT!\n" TEXT+ then
   s\" IMAGE-LIFECYCLE:PREPARE\n" TEXT+
   END-PATH!
   END$ 0 TEXT TEXT-U @ FS:ATOMIC-WRITE-FILE
   TEXT-RELEASE ;


: ENGINE-BODY ( -- )
   STARTUP
   RUNTIME-FILES
   REQUIRE-N @ false END-WRITE
   END$ INCLUDE-FILE
   OUT$ LINKER:WRITE ;


: REPL-BODY ( -- )
   STARTUP
   RUNTIME-FILES
   REQUIRE-N @ {: app:n :}
   SRC$ REQUIRE-FILE
   app true END-WRITE
   END$ INCLUDE-FILE
   OUT$ LINKER:WRITE ;


\ A program's entry is its global MAIN, whose effect APP-ENTRY:XT! holds to ( -- ):
\ the checker refuses app-end.f at MAIN when there is none or it takes or leaves
\ cells. The product's IMAGE-LIFECYCLE:PREPARE then runs, as END-WRITE's does.
: APP-END-WRITE ( -- )
   APP-END$ s\" ' MAIN APP-ENTRY:XT!\nIMAGE-LIFECYCLE:PREPARE\n" FS:ATOMIC-WRITE-FILE ;

: APP-BODY ( -- )
   STARTUP
   SRC$ INCLUDE-FILE
   APP-END$ INCLUDE-FILE
   OUT$ LINKER:WRITE ;

public

\ The file the last build was loading, for the message of a build that threw.
: ROW$ ( -- ptr u8 n ) ROW-PATH @ ROW-LEN @ ;

\ Build this tree's hb and write it to the path.
: ENGINE ( ptr u8 n -- )
   {: out:ptr outu:n :}
   out OUT-PATH !  outu OUT-LEN !
   PATHS
   s" engine" ['] ENGINE-BODY LINKER:WITH ;

\ Build hb with the source's files, the source's MAIN its entry, and write it to
\ the second path.
: REPL ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n out:ptr outu:n :}
   src SRC-PATH !  srcu SRC-LEN !  out OUT-PATH !  outu OUT-LEN !
   PATHS
   s" engine" ['] REPL-BODY LINKER:WITH ;

\ Build the source as a stripped program and write it to the second path.
: APP ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n out:ptr outu:n :}
   src SRC-PATH !  srcu SRC-LEN !  out OUT-PATH !  outu OUT-LEN !
   PATHS  APP-END-WRITE
   s" app" ['] APP-BODY LINKER:WITH ;

;package
