\ hb-build-core.f - builds this tree's arm64 hb, hb with a program's files
\ (--repl), or a stripped program, as a cross-build in the running hb and writes
\ it with its map beside it. A native build first compiles the tree's arm64
\ kernel writer and writes the kernel object the build then links
\ (KERNEL-BUILD); a Wasm program builds none. Each loads hb's startup files in
\ hb's own order (STARTUP); hb then loads the runtime's files, --repl the
\ program's source, and both their registry and installs; a stripped program
\ loads ffi-abi.f and task.f unless it is Wasm (APP-FILES), then its source and
\ its entry. Before any program source the build seals
\ every package it holds (SEAL-ENGINE). The build loads src/core/source-reset.f
\ and src/habu/app-end.f from the tree; a stripped program's build loads
\ nothing from under the out. Under <out>/hb-build/<target>/ it writes hb's end
\ file, its registry and installs, which it loads like a source, so a warm build
\ links the end file's object. Kernel objects and their inputs files sit in hb's
\ object directory, one directory per target and tree (KERNEL-PATHS), each
\ file's name starting with its kind. An object's name holds its checksum, so
\ an object file is never rewritten with other bytes; each kind's inputs file
\ names the last object of its kind written. A build whose inputs file still
\ holds links that object with no kernel build (ADOPT?).
\ Superseded objects stay until the out root is cleared. Builds sharing an out
\ root write an end file whole, and only when it changes (WRITE-CHANGED), and
\ each kernel file whole.

require src/lib/error.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/build-out.f
require src/lib/le.f
require src/core/include.f
require src/habu/boot-files.f
require src/compiler/target/model.f
require src/habu/linker.f
require src/habu/link-data.f

package HB-BUILD-CORE
private

PTR-VARIABLE ROW-PATH
variable ROW-LEN
PTR-VARIABLE SRC-PATH
variable SRC-LEN
PTR-VARIABLE OUT-PATH
variable OUT-LEN
create DIR PATH-CAP allot  variable DIR-U
create END-PATH PATH-CAP allot  variable END-U
create END-DIR PATH-CAP allot           \ a --repl end file's directory (REPL-END!)
create KNAME PATH-CAP allot  variable KNAME-U    \ a name KERNEL-PATHS joins
create KERNEL-DIR PATH-CAP allot  variable KERNEL-DIR-U
create KERNEL-PATH PATH-CAP allot  variable KERNEL-U
create INPUTS-PATH PATH-CAP allot  variable INPUTS-U
TYPED-VARIABLE KERNEL-ENGINE? bool
\ the build's target, which every build stores before PATHS
TYPED-VARIABLE TARGET RTARGET:resolved-target
DYNAMIC-BUFFER TEXT u8                  \ the end file's text until it is written
variable TEXT-U
DYNAMIC-BUFFER PRESENT u8               \ a written file's bytes while SAME-FILE? compares them
DYNAMIC-BUFFER RECORD u8                \ an inputs file's bytes while KERNEL-WRITE or ADOPT? holds them
variable RECORD-U                       \ the inputs file's size once INPUTS-READ read it
variable INPUTS-FD                      \ INPUTS-READ's open descriptor until FS:READ-SIZED owns it, else -1

: END$ ( -- ptr u8 n ) END-PATH END-U @ ;
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
\ END-PATH holds <out>/hb-build until ENGINE-END! or REPL-END! fills it.
: PATHS ( -- )
   BUILD-OUT:BASE$ s" hb-build" END-PATH FS:JOIN-PATH {: u:n :}
   END-PATH u  TARGET @ RTARGET:PROFILE$  DIR FS:JOIN-PATH DIR-U !
   DIR DIR-U @ FS:MAKE-DIRS ;


: TARGET? ( RTARGET:profile-id -- bool )
   TARGET @ RTARGET:PROFILE@ RTARGET-PROFILE--ID:EQ ;

: TARGET-WASM? ( RTARGET:resolved-target -- bool )
   RTARGET:ARCH@ CTARGET-ARCH:WASM CTARGET-ARCH:EQ ;

: WASM? ( -- bool ) TARGET @ TARGET-WASM? ;

64 constant USAGE-RC                    \ sysexits EX_USAGE

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


: ROW ( n n ptr u8 n -- )
   {: idx:n kind:n a:ptr u:n :}
   kind LOAD? 0= if exit then
   a u INCLUDE-FILE
   a u s" src/core/include.f" STR= if s" src/core/source-reset.f" INCLUDE-FILE then ;

: PROVIDE ( n n ptr u8 n -- )
   {: idx:n kind:n a:ptr u:n :}
   kind LOAD? if a u provided then ;

: REQUIRE-FILE ( ptr u8 n -- )
   {: a:ptr u:n :}
   a ROW-PATH !  u ROW-LEN !
   a u required ;


\ The provide rows go to the build's registry, which `provided` writes inside a
\ build. They come before the loads, so the first boot file is the registry's
\ first row, by which hb finds its tree, not a file a startup file requires.
: STARTUP ( -- )
   BOOT-FILES:CHECKER ['] PROVIDE BOOT-FILES:EACH
   BOOT-FILES:DECL ['] PROVIDE BOOT-FILES:EACH
   BOOT-FILES:CORE BOOT-FILES:DYNAMIC or BOOT-FILES:ARGV or
      BOOT-FILES:INTERNAL or BOOT-FILES:TOPROW or ['] PROVIDE BOOT-FILES:EACH
   BOOT-FILES:CHECKER ['] ROW BOOT-FILES:EACH
   BOOT-FILES:DECL ['] ROW BOOT-FILES:EACH
   BOOT-FILES:CORE BOOT-FILES:DYNAMIC or ['] ROW BOOT-FILES:EACH
   BOOT-FILES:OWNER ['] ROW BOOT-FILES:EACH
   BOOT-FILES:OWNER ['] PROVIDE BOOT-FILES:EACH
   BOOT-FILES:BASELIB BOOT-FILES:RESTLIB or ['] PROVIDE BOOT-FILES:EACH
   BOOT-FILES:BASELIB BOOT-FILES:RESTLIB or ['] ROW BOOT-FILES:EACH
   BOOT-FILES:ARGV ['] ROW BOOT-FILES:EACH
   BOOT-FILES:INTERNAL ['] ROW BOOT-FILES:EACH
   BOOT-FILES:TOPROW ['] ROW BOOT-FILES:EACH ;


: REPL-TERM$ ( -- ptr u8 n )
   RTARGET-PROFILE--ID:AARCH64-APPLE-DARWIN TARGET? if s" src/os/macos/repl-term.f" exit then
   s" src/os/linux/repl-term.f" ;


: RUNTIME-FILES ( -- )
   s" src/habu/engine-origin.f" REQUIRE-FILE
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
   s" src/lib/fs-anchored.f" REQUIRE-FILE
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
\ outside it keeps its canonical path; hb's is refused.
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
\ or standard input.
: END-WRITE ( n bool -- )
   {: app:n entry:bool :}
   0 TEXT-U !
   s\" REQUIRE-BOOT-OPEN\n" TEXT+
   REQUIRE-N @ REQUIRE-BASE @ ?do i REQUIRE-SLOT i REQUIRE-LEN@ i app < REG-ROW loop
   s\" REQUIRE-BOOT-FREEZE\nREQUIRE-REG:PERSIST\nRUNTIME:INSTALL\n" TEXT+
   entry if s\" ' MAIN APP-ENTRY:XT!\n" TEXT+ then
   END$ 0 TEXT TEXT-U @ WRITE-CHANGED
   TEXT-RELEASE ;


\ Seals every package the build holds, so no program source reopens one.
: SEAL-ENGINE ( -- )
   ndict@ 0 ?do
      i XREF-REC XREF-WORDLIST BOOT:NAMESPACE-WL = if i XREF-REC XREF:SEAL then
   loop ;

\ An owner row makes FFI and TASK when the build opens, so their files load before the seal.
\ Neither compiles for Wasm.
: APP-FILES ( -- )
   WASM? if exit then
   s" src/lib/ffi-abi.f" REQUIRE-FILE
   s" src/lib/task.f" REQUIRE-FILE ;


\ hb carries the build command's files, so `hb --load tools/hb-build.f`
\ compiles only that driver.
: ENGINE-BODY ( -- )
   STARTUP
   RUNTIME-FILES
   s" tools/hb-build-lib.f" REQUIRE-FILE
   SEAL-ENGINE
   REQUIRE-N @ false END-WRITE
   END$ INCLUDE-FILE
   OUT$ LINKER:WRITE ;


: REPL-BODY ( -- )
   STARTUP
   RUNTIME-FILES
   SEAL-ENGINE
   REQUIRE-N @ {: app:n :}
   SRC$ REQUIRE-FILE
   app true END-WRITE
   END$ INCLUDE-FILE
   OUT$ LINKER:WRITE ;


\ A Wasm module's kernel words load ahead of the program, so its words and the
\ link reach them.
: APP-BODY ( -- )
   STARTUP
   WASM? if s" src/arch/wasm/kernel-words.f" REQUIRE-FILE then
   APP-FILES
   SEAL-ENGINE
   SRC$ INCLUDE-FILE
   s" src/habu/app-end.f" INCLUDE-FILE
   OUT$ LINKER:WRITE ;

\ The tree's KERNEL-OBJECT:WRITE, which the kernel build compiled.
CAST: >WRITER ( n -- [ bool [ ptr u8 n -- ] -- ] )

: WRITER ( -- [ bool [ ptr u8 n -- ] -- ] )
   s" KERNEL-OBJECT" BOOT:NAMESPACE-WL CHECKER-RESOLVE:WL-PROBE {: pkg:ptr :}
   pkg XREF-FOUND? 0= if s" hb-build: the tree has no package KERNEL-OBJECT" 1 die then
   s" WRITE" pkg XREF-PKG-PUBLIC CHECKER-RESOLVE:WL-PROBE {: rec:ptr :}
   rec XREF-FOUND? 0= if s" hb-build: KERNEL-OBJECT has no public WRITE" 1 die then
   rec XREF-START >WRITER ;

\ An inputs file is the object's checksum (its bytes 8-40), the object's stamp
\ after its write (change time seconds, nanoseconds, size), the length of the
\ log that follows, then the kernel build's LINKER:INPUTS$; numbers are u64s
\ low byte first.
32 constant SUM-U
SUM-U 32 + constant HEAD-U
\ A real kernel build's inputs file is far below INPUTS-CAP; one over it is
\ damaged, and ADOPT? maps no buffer of its size.
16 1024 * 1024 * constant INPUTS-CAP

: KNAME+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   KNAME-U @ u + PATH-CAP > if E-FS-CAPACITY throw then
   a  KNAME KNAME-U @ +  u BYTE-COPY
   u KNAME-U +! ;

: KIND$ ( -- ptr u8 n )
   KERNEL-ENGINE? @ if s" kernel-engine" else s" kernel-app" then ;

\ The kernel directory is BUILD-OUT:OBJ$ of <profile>/kernel/<the tree root's
\ canonical path>; the inputs file in it is kernel-engine.inputs, or
\ kernel-app.inputs for a stripped program's kernel.
: KERNEL-PATHS ( -- )
   0 KNAME-U !  0 KERNEL-U !
   TARGET @ RTARGET:PROFILE$ KNAME+  s" /kernel" KNAME+
   SOURCE-ROOT:CWD$ {: c:ptr cu:n :}
   cu 0 > if c c@ FS:SLASH = else false then 0= if s" /" KNAME+ then
   c cu KNAME+
   KNAME KNAME-U @ BUILD-OUT:OBJ$ {: d:ptr du:n :}
   d KERNEL-DIR du BYTE-COPY  du KERNEL-DIR-U !
   0 KNAME-U !  KIND$ KNAME+  s" .inputs" KNAME+
   KERNEL-DIR du KNAME KNAME-U @ INPUTS-PATH FS:JOIN-PATH INPUTS-U ! ;

: HEX+ ( n -- )
   {: b:n :}
   s" 0123456789abcdef" drop {: d:ptr :}
   d b 4 rshift + 1 KNAME+
   d b 15 and + 1 KNAME+ ;

\ KERNEL-PATH becomes the object whose checksum is the SUM-U bytes:
\ kernel-engine-<them as lower-case hex>.hko in the kernel directory, or
\ kernel-app-<…>.hko.
: OBJECT-PATH! ( ptr u8 -- )
   {: sum:ptr :}
   0 KNAME-U !  KIND$ KNAME+  s" -" KNAME+
   SUM-U 0 ?do sum i + c@ HEX+ loop
   s" .hko" KNAME+
   KERNEL-DIR KERNEL-DIR-U @ KNAME KNAME-U @ KERNEL-PATH FS:JOIN-PATH KERNEL-U ! ;

\ Writes the object whole at the path its checksum names, then the inputs file
\ whole, unless the object's stamp cannot be read. The stamp fails an object
\ changed in place since. Nothing older is deleted.
: KERNEL-WRITE ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 40 < if s" hb-build: the kernel object is too short" 1 die then
   KERNEL-DIR KERNEL-DIR-U @ FS:MAKE-DIRS
   a 8 + OBJECT-PATH!
   KERNEL-PATH KERNEL-U @ a u FS:ATOMIC-WRITE-FILE
   KERNEL-PATH KERNEL-U @ FS:TRY-FILE-META 0= if 2drop 2drop drop exit then
   {: size:n ms:n mns:n cs:n cns:n :}
   LINKER:INPUTS$ {: l:ptr lu:n :}
   HEAD-U lu + RECORD-RESERVE
   a 8 +  0 RECORD  SUM-U BYTE-COPY
   cs  SUM-U RECORD LE:U64!
   cns  SUM-U 8 + RECORD LE:U64!
   size  SUM-U 16 + RECORD LE:U64!
   lu  SUM-U 24 + RECORD LE:U64!
   l  HEAD-U RECORD  lu BYTE-COPY
   INPUTS-PATH INPUTS-U @  0 RECORD  HEAD-U lu +  FS:ATOMIC-WRITE-FILE
   RECORD-RELEASE ;

\ The inputs file whole into RECORD through one descriptor, its size into
\ RECORD-U; false when it is missing, shorter than its head or over INPUTS-CAP.
: INPUTS-READ ( -- bool )
   INPUTS-PATH INPUTS-U @ FS:OPEN-SIZED {: file:fd n:n found:bool :}
   found 0= if false exit then
   file FD>N INPUTS-FD !
   n HEAD-U <  n INPUTS-CAP > or if false exit then
   n RECORD-RESERVE
   -1 INPUTS-FD !
   file 0 RECORD n FS:READ-SIZED
   n RECORD-U !  true ;

: INPUTS-KEEPING ( bool -- bool ) drop INPUTS-READ ;

\ INPUTS-READ, false also when the file cannot be opened or read, or a buffer
\ of its size mapped; any other throw passes.
: INPUTS-READ? ( -- bool )
   false ['] INPUTS-KEEPING catch {: ok code:n :}
   code 0= if ok exit then
   code E-FS-OPEN =  code E-FS-STAT = or  code E-FS-IO = or
   code DYNAMIC-STORAGE:E-MAP = or  if false exit then
   code throw ;

\ Whether the log runs exactly to the file's end, and the object the checksum
\ names has the recorded stamp.
: HEAD-HOLDS? ( -- bool )
   RECORD-U @ HEAD-U -  SUM-U 24 + RECORD LE:U64@  <> if false exit then
   0 RECORD OBJECT-PATH!
   KERNEL-PATH KERNEL-U @ FS:TRY-FILE-META 0= if 2drop 2drop drop false exit then
   {: size:n ms:n mns:n cs:n cns:n :}
   SUM-U RECORD LE:U64@ cs =  SUM-U 8 + RECORD LE:U64@ cns = and
   SUM-U 16 + RECORD LE:U64@ size = and ;

: RECORD-HOLDS? ( -- bool )
   INPUTS-READ? 0= if false exit then
   HEAD-HOLDS? 0= if false exit then
   HEAD-U RECORD  RECORD-U @ HEAD-U -  LINKER:INPUTS-HOLD? ;

: INPUTS-DONE ( -- )
   INPUTS-FD @ 0 >= if INPUTS-FD @ close then
   -1 INPUTS-FD !
   RECORD-RELEASE ;

\ Whether the inputs file reads whole, its object stands as it records, and the
\ inputs it logs hold now; KERNEL-PATH then names that object. The descriptor
\ and RECORD are released on every exit, a throw included.
: ADOPT? ( -- bool )
   -1 INPUTS-FD !
   ['] RECORD-HOLDS? ['] INPUTS-DONE finally ;

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

\ Writes this tree's kernel object, an engine's or a stripped program's, in
\ hb's object directory, the tree's writer laying its bytes and this tool
\ writing the file, unless the last one written there still holds; then reads
\ the object adopted or written, by the path its checksum names, for the
\ builds that follow. A build's registry starts empty, so the writer's files
\ load from this tree; a kernel build shares a program build's objects. The
\ build and its adoption run from the tree's root, whatever the caller's, so
\ the inputs file records that root and holds for every caller.
: KERNEL-BUILD ( bool RTARGET:resolved-target -- )
   {: engine:bool target:RTARGET:resolved-target :}
   engine KERNEL-ENGINE? !  target TARGET !
   PATHS  KERNEL-PATHS
   SOURCE-ROOT:CWD$ [:
      ADOPT? 0= if
         NULL-PTR ROW-PATH !  0 ROW-LEN !
         s" kernel" target ['] KERNEL-BODY LINKER:WITH
      then
   ;] SOURCE-ROOT:WITH
   KERNEL-PATH KERNEL-U @ LINKER:KERNEL ;

\ The path of the kernel object the last KERNEL-BUILD wrote or adopted, which
\ names its checksum, so a later read of it reads the bytes the build linked.
: KERNEL$ ( -- ptr u8 n ) KERNEL-PATH KERNEL-U @ ;

\ The tree's inputs file for the last KERNEL-BUILD's profile and kind; any
\ kernel build of this tree may replace it with a log that holds as well, read
\ from the tree's root.
: KERNEL-INPUTS$ ( -- ptr u8 n ) INPUTS-PATH INPUTS-U @ ;

\ The file the build is loading, for the message of a build that failed; empty
\ until the build opened last loads its first file.
: ROW$ ( -- ptr u8 n ) ROW-PATH @ ROW-LEN @ ;

\ Build this tree's hb for the target and write it to the path.
: ENGINE ( ptr u8 n RTARGET:resolved-target -- )
   {: out:ptr outu:n target:RTARGET:resolved-target :}
   target REFUSE-WASM
   out OUT-PATH !  outu OUT-LEN !
   true target KERNEL-BUILD  ENGINE-END!
   NULL-PTR ROW-PATH !  0 ROW-LEN !
   s" engine" target ['] ENGINE-BODY LINKER:WITH ;

\ Build hb with the source's files, the source's MAIN its entry, for the target
\ and write it to the second path.
: REPL ( ptr u8 n ptr u8 n RTARGET:resolved-target -- )
   {: src:ptr srcu:n out:ptr outu:n target:RTARGET:resolved-target :}
   target REFUSE-WASM
   src SRC-PATH !  srcu SRC-LEN !  out OUT-PATH !  outu OUT-LEN !
   true target KERNEL-BUILD  REPL-END!
   NULL-PTR ROW-PATH !  0 ROW-LEN !
   s" engine" target ['] REPL-BODY LINKER:WITH ;

\ Build the source as a stripped program for the target and write it to
\ the second path.
: APP ( ptr u8 n ptr u8 n RTARGET:resolved-target -- )
   {: src:ptr srcu:n out:ptr outu:n target:RTARGET:resolved-target :}
   src SRC-PATH !  srcu SRC-LEN !  out OUT-PATH !  outu OUT-LEN !
   target TARGET !
   WASM? 0= if false target KERNEL-BUILD then
   NULL-PTR ROW-PATH !  0 ROW-LEN !
   s" app" target ['] APP-BODY LINKER:WITH ;

;package
