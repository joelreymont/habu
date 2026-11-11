\ hb-build-core.f - builds this tree's arm64 hb, hb with a program's files
\ (--repl), or a stripped program, as a cross-build in the running hb
\ (LINKER:WITH) and writes it with its map beside it. A native build first
\ compiles the tree's arm64 kernel writer and writes the kernel object the
\ build then links (KERNEL-BUILD); a Wasm program builds none. Each loads hb's
\ startup files in hb's own order (src/habu/habu2.f EMIT-HOST-LOAD-PREFIX); hb then loads
\ the runtime's files, --repl the program's source, and both their registry and
\ installs; a stripped program loads its source and its entry.
\ The files written under <out>/hb-build/<target>/ load inside the build, so a
\ warm build links what they do: reset.f, the token hb's prefix runs after
\ include.f; end.f, hb's registry and installs, which under --repl is
\ end/<the source's path>/end.f; app-end.f, a stripped program's entry. Builds
\ sharing an out root write each whole, and only when it changes (WRITE-CHANGED).

require src/lib/error.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/build-out.f
require src/core/sha256.f
require src/core/include.f
require src/habu/boot-files.f
require src/compiler/target/model.f
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
create END-DIR PATH-CAP allot           \ a --repl end file's directory (REPL-END!)
create APP-END-PATH PATH-CAP allot  variable APP-END-U
create KERNEL-PATH PATH-CAP allot  variable KERNEL-U
7 64 + 4 + constant KERNEL-NAME-U   \ kernel-<64 hex digits>.hko
create KERNEL-NAME KERNEL-NAME-U allot
TYPED-VARIABLE KERNEL-ENGINE? bool     \ the kernel build writes an engine's kernel
\ the build's target, which every build stores before PATHS
TYPED-VARIABLE TARGET RTARGET:resolved-target
DYNAMIC-BUFFER TEXT u8                  \ the end file's text until it is written
variable TEXT-U
DYNAMIC-BUFFER PRESENT u8               \ a written file's bytes while SAME-FILE? compares them

: RESET$ ( -- ptr u8 n ) RESET-PATH RESET-U @ ;
: END$ ( -- ptr u8 n ) END-PATH END-U @ ;
: APP-END$ ( -- ptr u8 n ) APP-END-PATH APP-END-U @ ;
: SRC$ ( -- ptr u8 n ) SRC-PATH @ SRC-LEN @ ;
: OUT$ ( -- ptr u8 n ) OUT-PATH @ OUT-LEN @ ;


\ True when the file at the path holds exactly the bytes; false when it is missing.
: SAME-FILE? ( ptr u8 n ptr u8 n -- bool )
   {: pa:ptr pu:n a:ptr u:n :}
   pa pu FS:OPEN-SIZED {: file:fd size:n found:bool :}
   found 0= if false exit then
   size u <> if file FD>N close false exit then
   size PRESENT-RESERVE
   file 0 PRESENT size FS:READ-SIZED
   0 PRESENT size a u STR=
   PRESENT-RELEASE ;


\ A file the build loads keeps its inode and change time while its bytes stay
\ the same, so a later build that stamps it sees it unchanged.
: WRITE-CHANGED ( ptr u8 n ptr u8 n -- )
   {: pa:ptr pu:n a:ptr u:n :}
   pa pu a u SAME-FILE? if exit then
   pa pu a u FS:ATOMIC-WRITE-FILE ;


\ The tool's files sit under the target's name, since their text follows from the target.
: PATHS ( -- )
   BUILD-OUT:BASE$ s" hb-build" RESET-PATH FS:JOIN-PATH {: u:n :}
   RESET-PATH u  TARGET @ RTARGET:PROFILE$  DIR FS:JOIN-PATH DIR-U !
   DIR DIR-U @ FS:MAKE-DIRS
   DIR DIR-U @ s" reset.f" RESET-PATH FS:JOIN-PATH RESET-U !
   DIR DIR-U @ s" app-end.f" APP-END-PATH FS:JOIN-PATH APP-END-U !
   RESET$ s\" SOURCE-INPUT:RESET\n" WRITE-CHANGED ;


: TARGET? ( RTARGET:profile-id -- bool )
   TARGET @ RTARGET:PROFILE@ RTARGET-PROFILE--ID:EQ ;

: TARGET-WASM? ( RTARGET:resolved-target -- bool )
   RTARGET:ARCH@ CTARGET-ARCH:WASM CTARGET-ARCH:EQ ;

: WASM? ( -- bool ) TARGET @ TARGET-WASM? ;

64 constant USAGE-RC                    \ sysexits EX_USAGE

\ A row loads when its kind is common or the build's target (habu2.f PFX-LOAD?).
: LOAD? ( n -- bool )
   case
      BOOT-FILES:COMMON of true endof
      BOOT-FILES:LINUX of RTARGET-PROFILE--ID:AARCH64-UNKNOWN-LINUX-GNU TARGET? endof
      BOOT-FILES:MACOS of RTARGET-PROFILE--ID:AARCH64-APPLE-DARWIN TARGET? endof
      BOOT-FILES:X64 of RTARGET-PROFILE--ID:X86-64-UNKNOWN-LINUX-GNU TARGET? endof
      BOOT-FILES:WASM of RTARGET-PROFILE--ID:WASM32-UNKNOWN-UNKNOWN TARGET? endof
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
   RTARGET-PROFILE--ID:AARCH64-APPLE-DARWIN TARGET? if s" src/os/macos/repl-term.f" exit then
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


\ hb's end file is <out>/hb-build/<target>/end.f: its text follows from the tree
\ and the target.
: ENGINE-END! ( -- ) DIR DIR-U @ s" end.f" END-PATH FS:JOIN-PATH END-U ! ;

\ A --repl program's end file is <out>/hb-build/<target>/end/<its source's canonical
\ path>/end.f: its text follows from the tree, the target and that source, so two
\ programs built under one out root never load each other's.
: REPL-END! ( -- )
   SRC$ SOURCE-ROOT:CANONICAL drop {: src:ptr srcu:n :}
   DIR DIR-U @ s" end" END-PATH FS:JOIN-PATH {: endu:n :}
   END-PATH endu  src 1 +  srcu 1 -  END-DIR FS:JOIN-PATH {: diru:n :}
   END-DIR diru FS:MAKE-DIRS
   END-DIR diru s" end.f" END-PATH FS:JOIN-PATH END-U ! ;


\ The product's registry is the build's: the end file records each row with the
\ product's own include.f and packs them into its DATA; then the installs, and
\ under --repl the program's MAIN as hb's entry, which hb runs before its REPL
\ or standard input (src/habu/main.f ROUTED).
: END-WRITE ( n bool -- )
   {: app:n entry:bool :}
   0 TEXT-U !
   s\" REQUIRE-BOOT-OPEN\n" TEXT+
   REQUIRE-N @ REQUIRE-BASE @ ?do i REQUIRE-SLOT i REQUIRE-LEN@ i app < REG-ROW loop
   s\" REQUIRE-BOOT-FREEZE\nREQUIRE-REG:PERSIST\nRUNTIME:INSTALL\n" TEXT+
   entry if s\" ' MAIN APP-ENTRY:XT!\n" TEXT+ then
   END$ 0 TEXT TEXT-U @ WRITE-CHANGED
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
\ cells.
: APP-END-WRITE ( -- )
   APP-END$ s\" ' MAIN APP-ENTRY:XT!\n" WRITE-CHANGED ;

\ A Wasm module's kernel words (WKWORDS) load ahead of the program, so its words
\ and the link reach them.
: APP-BODY ( -- )
   STARTUP
   WASM? if s" src/arch/wasm/kernel-words.f" REQUIRE-FILE then
   SRC$ INCLUDE-FILE
   APP-END$ INCLUDE-FILE
   OUT$ LINKER:WRITE ;

\ The tree's KERNEL-OBJECT:WRITE, which the kernel build compiled; it lays the
\ object's bytes and this tool writes the file (KERNEL-WRITE).
CAST: >WRITER ( n -- [ bool [ ptr u8 n -- ] -- ] )

: WRITER ( -- [ bool [ ptr u8 n -- ] -- ] )
   s" KERNEL-OBJECT" BOOT:NAMESPACE-WL CHECKER-RESOLVE:WL-PROBE {: pkg:ptr :}
   pkg XREF-FOUND? 0= if s" hb-build: the tree has no package KERNEL-OBJECT" 1 die then
   s" WRITE" pkg XREF-PKG-PUBLIC CHECKER-RESOLVE:WL-PROBE {: rec:ptr :}
   rec XREF-FOUND? 0= if s" hb-build: KERNEL-OBJECT has no public WRITE" 1 die then
   rec XREF-START >WRITER ;

\ Writes the object as <DIR>/kernel-<the SHA-256 in its bytes 8-39, in hex>.hko
\ and that path into KERNEL-PATH.
: KERNEL-WRITE ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 40 < if s" hb-build: the kernel object is too short" 1 die then
   s" kernel-" {: k:ptr ku:n :}
   k KERNEL-NAME ku BYTE-COPY
   a 8 +  KERNEL-NAME 7 +  SHA256>HEX
   s" .hko" {: h:ptr hu:n :}
   h  KERNEL-NAME 71 +  hu BYTE-COPY
   DIR DIR-U @ KERNEL-NAME KERNEL-NAME-U KERNEL-PATH FS:JOIN-PATH KERNEL-U !
   KERNEL-PATH KERNEL-U @  a u  FS:ATOMIC-WRITE-FILE ;

: KERNEL-BODY ( -- )
   STARTUP
   s" src/arch/arm64/kernel-object.f" REQUIRE-FILE
   KERNEL-ENGINE? @ ['] KERNEL-WRITE WRITER execute ;

public

\ hb and a REPL program run where hb runs, so only a program builds for Wasm: a
\ command line calls this before it makes anything.
: REFUSE-WASM ( RTARGET:resolved-target -- )
   TARGET-WASM? if
      s" hb-build: a Wasm target builds a program, not an engine or a REPL" USAGE-RC die
   then ;

\ Writes this tree's kernel object, an engine's or a stripped program's, under
\ <out>/hb-build/<target>/, the tree's writer laying its bytes and this tool
\ writing the file, and reads it for the builds that follow. A build's registry
\ starts empty, so the writer's files load from this tree; a kernel build shares
\ a program build's objects.
: KERNEL-BUILD ( bool RTARGET:resolved-target -- )
   {: engine:bool target:RTARGET:resolved-target :}
   engine KERNEL-ENGINE? !  target TARGET !
   PATHS
   NULL-PTR ROW-PATH !  0 ROW-LEN !
   s" kernel" target ['] KERNEL-BODY LINKER:WITH
   KERNEL-PATH KERNEL-U @ LINKER:KERNEL ;

\ The kernel object the last KERNEL-BUILD wrote, for a child process's
\ LINKER:KERNEL.
: KERNEL$ ( -- ptr u8 n ) KERNEL-PATH KERNEL-U @ ;

\ The file the build is loading, for the message of a build that failed; empty
\ until the build opened last loads its first file.
: ROW$ ( -- ptr u8 n ) ROW-PATH @ ROW-LEN @ ;

\ Build this tree's hb for the target and write it to the path (REFUSE-WASM).
: ENGINE ( ptr u8 n RTARGET:resolved-target -- )
   {: out:ptr outu:n target:RTARGET:resolved-target :}
   target REFUSE-WASM
   out OUT-PATH !  outu OUT-LEN !
   true target KERNEL-BUILD  ENGINE-END!
   NULL-PTR ROW-PATH !  0 ROW-LEN !
   s" engine" target ['] ENGINE-BODY LINKER:WITH ;

\ Build hb with the source's files, the source's MAIN its entry, for the target
\ and write it to the second path (REFUSE-WASM).
: REPL ( ptr u8 n ptr u8 n RTARGET:resolved-target -- )
   {: src:ptr srcu:n out:ptr outu:n target:RTARGET:resolved-target :}
   target REFUSE-WASM
   src SRC-PATH !  srcu SRC-LEN !  out OUT-PATH !  outu OUT-LEN !
   true target KERNEL-BUILD  REPL-END!
   NULL-PTR ROW-PATH !  0 ROW-LEN !
   s" engine" target ['] REPL-BODY LINKER:WITH ;

\ Build the source as a stripped program for the target and write it to the
\ second path. A Wasm target links no arm64 kernel object.
: APP ( ptr u8 n ptr u8 n RTARGET:resolved-target -- )
   {: src:ptr srcu:n out:ptr outu:n target:RTARGET:resolved-target :}
   src SRC-PATH !  srcu SRC-LEN !  out OUT-PATH !  outu OUT-LEN !
   target TARGET !
   WASM? if PATHS else false target KERNEL-BUILD then
   APP-END-WRITE
   NULL-PTR ROW-PATH !  0 ROW-LEN !
   s" app" target ['] APP-BODY LINKER:WITH ;

;package
