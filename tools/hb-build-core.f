\ hb-build-core.f - builds this tree's native hb, hb with a program's files
\ (--repl), or a stripped program, as a cross-build in the running hb and writes
\ it with its map beside it. A native build first compiles the target's
\ kernel writer and writes the kernel object the build then links
\ (KERNEL-BUILD); a Wasm program builds none. Each loads hb's startup files in
\ hb's own order (STARTUP); hb and --repl then load the runtime's files. Every
\ build loads the engine-library list (ENGINE-LIB-FILES) and seals every package
\ it holds (SEAL-ENGINE); then --repl loads the program's source, hb and --repl
\ their registry and installs, and a stripped program its source and its
\ entry. The build loads src/core/source-reset.f and src/habu/app-end.f from
\ the tree; a stripped program's build loads nothing from under the out. Under
\ <out>/hb-build/<target>/ it writes hb's end file, its registry and installs,
\ which it loads like a source, so a warm build links the end file's object.
\ Kernel objects and their inputs files sit in hb's object directory, one
\ directory per target and tree (KERNEL-PATHS), each file's name starting with
\ its kind. An object's name holds its checksum, so an object file is never
\ rewritten with other bytes; each kind's inputs file names the last object of
\ its kind written. A build whose inputs file still holds links that object with
\ no kernel build (ADOPT?). Superseded objects stay until the out root is
\ cleared. Builds sharing an out root write an end file whole, and only when it
\ changes (WRITE-CHANGED), and each kernel file whole.

require src/core/exit-status.f
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
require src/compiler/target.f

package HB-BUILD-CORE
private

\ Where the build is (WHERE$): idle outside a build; opening in LINKER:WITH's
\ open; between in its body outside a top-level file; loading such a file.
ENUM stage idle opening between loading ;ENUM
1 LAYOUT-BUFFER STAGE-BUF stage
PTR-VARIABLE MODE-PATH                  \ the open build's LINKER:WITH mode
variable MODE-LEN
PTR-VARIABLE FILE-PATH                  \ the file a loading build loads
variable FILE-LEN
\ WHERE$'s text; the longest, the kernel build outside its files, is 34 bytes.
create WHERE-BUF 64 allot  variable WHERE-U
PTR-VARIABLE SRC-PATH
variable SRC-LEN
TYPED-VARIABLE OUT-Q [ -- str ]
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

: END$ ( -- str ) END-PATH END-U @ >str ;
: SRC$ ( -- str ) SRC-PATH @ SRC-LEN @ >str ;
: OUT$ ( -- str ) OUT-Q @ execute ;

: STAGE! ( stage -- ) 0 STAGE-BUF ! ;
: STAGE@ ( -- stage ) 0 STAGE-BUF @ ;
: MODE$ ( -- str ) MODE-PATH @ MODE-LEN @ >str ;
: FILE$ ( -- str ) FILE-PATH @ FILE-LEN @ >str ;

: OPENING ( str -- )
   {: s:str :}
   s STR:DATA MODE-PATH !  s STR:LEN MODE-LEN !
   construct stage opening STAGE! ;

: LOADING ( str -- )
   {: s:str :}
   s STR:DATA FILE-PATH !  s STR:LEN FILE-LEN !
   construct stage loading STAGE! ;

: BETWEEN ( -- ) construct stage between STAGE! ;

: WHERE+ ( str -- )
   {: s:str :}
   s STR:DATA  WHERE-BUF WHERE-U @ +  s STR:LEN BYTE-COPY
   s STR:LEN WHERE-U +! ;

\ "the <mode> build" and the tail.
: BUILD$ ( str -- str )
   {: s:str :}
   0 WHERE-U !
   s" the " WHERE+  MODE$ WHERE+  s"  build" WHERE+  s WHERE+
   WHERE-BUF WHERE-U @ >str ;


\ True when the file at the path holds exactly the bytes; false when it is missing.
: SAME-FILE? ( str str -- bool )
   {: ps:str s:str :}
   ps FS:OPEN-SIZED {: file:fd size:n found:bool :}
   found 0= if false exit then
   size s STR:LEN <> if file FD>N close false exit then
   size PRESENT-RESERVE
   file 0 PRESENT size FS:READ-SIZED
   0 PRESENT size >str s STR:=
   PRESENT-RELEASE ;


\ A file the build loads keeps its inode and change time while its bytes stay
\ the same, so a later build that stamps it sees it unchanged.
: WRITE-CHANGED ( str str -- )
   {: ps:str s:str :}
   ps s SAME-FILE? if exit then
   ps s FS:ATOMIC-WRITE-FILE ;


\ The tool's files sit under the target's name, since their text follows from the target.
\ END-PATH holds <out>/hb-build until ENGINE-END! or REPL-END! fills it.
: PATHS ( -- )
   BUILD-OUT:BASE$ s" hb-build" END-PATH FS:JOIN-PATH {: u:n :}
   END-PATH u >str  TARGET @ RTARGET:PROFILE$  DIR FS:JOIN-PATH DIR-U !
   DIR DIR-U @ >str FS:MAKE-DIRS ;


: TARGET? ( RTARGET:profile-id -- bool )
   TARGET @ RTARGET:PROFILE@ RTARGET-PROFILE--ID:EQ ;

: TARGET-WASM? ( RTARGET:resolved-target -- bool )
   RTARGET:ARCH@ CTARGET-ARCH:WASM CTARGET-ARCH:EQ ;

: WASM? ( -- bool ) TARGET @ TARGET-WASM? ;


: LOAD? ( n -- bool )
   case
      BOOT-FILES:COMMON of true endof
      BOOT-FILES:LINUX of RTARGET-PROFILE--ID:AARCH64-UNKNOWN-LINUX-GNU TARGET? endof
      BOOT-FILES:MACOS of RTARGET-PROFILE--ID:AARCH64-APPLE-DARWIN TARGET? endof
      BOOT-FILES:X64 of RTARGET-PROFILE--ID:X86-64-UNKNOWN-LINUX-GNU TARGET? endof
      BOOT-FILES:WASM of RTARGET-PROFILE--ID:WASM32-UNKNOWN-UNKNOWN TARGET? endof
      false swap
   endcase ;


\ A file the build includes is a provided row or the build's own glue: read as
\ no file's, its calls are not checked.
: INCLUDE-FILE ( str -- )
   {: s:str :}
   s LOADING
   0 [: s included ;] REQUIRES:WITH-FILE
   BETWEEN ;


: ROW ( n n str -- )
   {: idx:n kind:n s:str :}
   kind LOAD? 0= if exit then
   s INCLUDE-FILE
   s s" src/core/include.f" STR:= if s" src/core/source-reset.f" INCLUDE-FILE then ;

: PROVIDE ( n n str -- )
   {: idx:n kind:n s:str :}
   kind LOAD? if s provided then ;

: REQUIRE-FILE ( str -- )
   {: s:str :}
   s LOADING
   s required
   BETWEEN ;

\ A program's source is an entry, as `hb --load` reads it: its own file, named
\ from the directory hb runs in.
: ENTRY-FILE ( str -- )
   {: s:str :}
   s LOADING
   s script-required
   BETWEEN ;


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
   BOOT-FILES:OWNER ['] PROVIDE BOOT-FILES:EACH
   BOOT-FILES:OWNER ['] ROW BOOT-FILES:EACH
   BOOT-FILES:BASELIB BOOT-FILES:RESTLIB or ['] PROVIDE BOOT-FILES:EACH
   BOOT-FILES:BASELIB BOOT-FILES:RESTLIB or ['] ROW BOOT-FILES:EACH
   BOOT-FILES:ARGV ['] ROW BOOT-FILES:EACH
   BOOT-FILES:INTERNAL ['] ROW BOOT-FILES:EACH
   BOOT-FILES:TOPROW ['] ROW BOOT-FILES:EACH ;


: RUNTIME-FILES ( -- )
   s" src/habu/engine-origin.f" REQUIRE-FILE
   s" src/habu/code-bytes.f" REQUIRE-FILE
   s" src/habu/debug-watch.f" REQUIRE-FILE
   s" src/habu/stepper.f" REQUIRE-FILE
   s" src/habu/debug.f" REQUIRE-FILE
   s" src/habu/link-data.f" REQUIRE-FILE
   s" src/habu/verify-source.f" REQUIRE-FILE
   s" src/habu/runtime.f" REQUIRE-FILE ;


: TEXT+ ( str -- )
   {: s:str :}
   TEXT-U @ s STR:LEN + TEXT-RESERVE
   s STR:DATA TEXT-U @ TEXT s STR:LEN BYTE-COPY
   s STR:LEN TEXT-U +! ;

\ A row's bytes inside the end file's s\" literal: `"` and `\` take a backslash.
: ROW+ ( str -- )
   {: s:str :}
   s STR:LEN 0 ?do
      s STR:DATA i + c@ dup 34 = swap 92 = or if s" \" TEXT+ then
      s STR:DATA i + 1 >str TEXT+
   loop ;

\ A registry row below the engine's source root, which the build reads hb's
\ files from, by its name under it: the built program matches a require
\ against its rows under its own engine root, a Habu tree. A program's row
\ outside the root keeps its canonical path; an hb's is rejected. A file's row
\ mints its id.
: REG-ROW ( str bool bool -- )
   {: s:str engine:bool file:bool :}
   SOURCE-ROOT:ENGINE$ {: c:str :}
   c STR:LEN {: cu:n :}
   s STR:LEN cu 1 + > if
      s STR:DATA cu >str c STR:= s STR:DATA cu + c@ FS:SLASH = and
   else false then
   {: below:bool :}
   below 0= engine and if s LOADING  E-BUILD-PATH throw then
   s\" s\\\" " TEXT+
   below if
      s STR:DATA cu + 1 + s STR:LEN cu - 1 - >str ROW+
   else s ROW+ then
   file if s\" \" REQUIRE-REG:APPEND-FILE\n" else s\" \" REQUIRE-REG:APPEND\n" then
   TEXT+ ;

: ROW-FILE? ( n -- bool ) REQUIRE-REG:ID 0<> ;

\ The id the end file gives the file of id h: the rank of its row among the
\ rows that mint one; 0 for a file of no row.
: TARGET-ID ( n -- n ) {: h:n :}
   h 0= if 0 exit then
   0 REQUIRE-REG:COUNT REQUIRE-REG:BASE ?do
      i REQUIRE-REG:ID  dup 0<> if swap 1 + swap then
      h = if unloop exit then
   loop drop 0 ;

: NUM+ ( n -- ) ERROR-TEXT:DIGITS$ TEXT+  s"  " TEXT+ ;

\ The build's records [first, end), all of file h, as the target numbers both.
: SPAN+ ( n n n -- ) {: first:n end:n h:n :}
   h TARGET-ID {: id:n :}
   id 0= if exit then
   first LINKER:TARGET-INDEX NUM+  end LINKER:TARGET-INDEX NUM+  id NUM+
   s\" REQUIRES:SPAN\n" TEXT+ ;

\ The run from, of file h, takes record r, else ends before it.
: RUN-STEP ( n n n -- n n ) {: from:n h:n r:n :}
   r REQUIRES:RECORD-FILE {: now:n :}
   now h = if from h exit then
   from r h SPAN+
   r now ;

\ Each run of one file's records in the build.
: SPANS+ ( -- )
   LINKER:FIRST-RECORD ndict@ {: first:n end:n :}
   first  first REQUIRES:RECORD-FILE
   end first 1 + ?do i RUN-STEP loop
   {: from:n h:n :}
   from end h SPAN+ ;


\ hb's end file is <out>/hb-build/<target>/end.f: its text follows from the tree
\ and the target.
: ENGINE-END! ( -- )
   DIR DIR-U @ >str s" end.f" END-PATH FS:JOIN-PATH END-U ! ;

\ A --repl program's end file is <out>/hb-build/<target>/end/<its source's canonical
\ path>/end.f: its text follows from the tree, the target and that source, so two
\ programs built under one out root never load each other's.
: REPL-END! ( -- )
   SRC$ SOURCE-ROOT:CANONICAL drop {: src:str :}
   DIR DIR-U @ >str s" end" END-PATH FS:JOIN-PATH {: endu:n :}
   END-PATH endu >str  src STR:DATA 1 +  src STR:LEN 1 - >str
   END-DIR FS:JOIN-PATH {: diru:n :}
   END-DIR diru >str FS:MAKE-DIRS
   END-DIR diru >str s" end.f" END-PATH FS:JOIN-PATH END-U ! ;


\ The end file states the build's registry rows and each file's run of records,
\ then the installs and, under --repl, MAIN as hb's entry.
: END-WRITE ( n bool -- )
   {: app:n entry:bool :}
   0 TEXT-U !
   s\" REQUIRE-REG:BOOT-OPEN\n" TEXT+
   REQUIRE-REG:COUNT REQUIRE-REG:BASE ?do
      i REQUIRE-REG:SLOT i REQUIRE-REG:LEN >str i app < i ROW-FILE? REG-ROW
   loop
   s\" REQUIRE-REG:BOOT-FREEZE\nREQUIRE-REG:PERSIST\n" TEXT+
   SPANS+
   s\" REQUIRES:PERSIST\nRUNTIME:INSTALL\n" TEXT+
   entry if s\" ' MAIN APP-ENTRY:XT!\n" TEXT+ then
   END$ 0 TEXT TEXT-U @ >str WRITE-CHANGED
   TEXT-RELEASE ;


\ Seals every package the build holds, so no program source reopens one, and
\ makes the C2-MEM entries it holds scoped entries to that source.
: SEAL-ENGINE ( -- )
   ndict@ 0 ?do
      i XREF-REC XREF-WORDLIST BOOT:NAMESPACE-WL = if i XREF-REC XREF:SEAL then
   loop
   CHECK:SCOPE-ENTRIES! ;

\ Loaded before the seal in every build: past it no code reaches a private word
\ of these files' packages. Wasm compiles only those before WASM? exits.
: ENGINE-LIB-FILES ( -- )
   s" src/lib/fmt.f" REQUIRE-FILE
   s" src/lib/byte-buffer.f" REQUIRE-FILE
   s" src/lib/reason.f" REQUIRE-FILE
   s" src/habu/data-floor.f" REQUIRE-FILE
   WASM? if exit then
   s" src/lib/ffi-abi.f" REQUIRE-FILE
   s" src/lib/task.f" REQUIRE-FILE
   s" src/lib/c2-memory.f" REQUIRE-FILE
   s" src/lib/fs-anchored.f" REQUIRE-FILE
   s" src/lib/fork-child.f" REQUIRE-FILE
   s" src/lib/codegen.f" REQUIRE-FILE
   s" src/lib/ffi-callback.f" REQUIRE-FILE
   s" src/lib/genio-core.f" REQUIRE-FILE ;


\ hb carries the build command's files, so `hb --load tools/hb-build.f`
\ compiles only that driver.
: ENGINE-BODY ( -- )
   BETWEEN
   STARTUP
   RUNTIME-FILES
   s" tools/hb-build-lib.f" REQUIRE-FILE
   ENGINE-LIB-FILES
   SEAL-ENGINE
   REQUIRE-REG:COUNT false END-WRITE
   END$ INCLUDE-FILE
   OUT$ LINKER:WRITE ;


\ A program's entry is its global MAIN. While the program loads the checker
\ rejects a MAIN that is not ( -- ) where it is declared; a program that
\ defines none is rejected once it has loaded, before its entry is named.
: ENTRY-OPEN ( -- ) s" MAIN" CHECK:ENTRY! ;

: ENTRY-CLOSE ( -- )
   s" " CHECK:ENTRY!
   s" MAIN" BOOT:GLOBAL-WL CHECKER-RESOLVE:WL-PROBE XREF-FOUND? if exit then
   SRC$ REJECTION:FILE!
   s" E-NO-ENTRY" REJECTION:OPEN
   s" the program defines no `MAIN`" REJECTION:EMIT-STR
   s" MAIN" REJECTION:AT  s" not defined" REJECTION:EMIT-STR
   REJECTION:HELP
   s" define `: MAIN ( -- ) ... ;` in " REJECTION:EMIT-STR  SRC$ REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;


: REPL-BODY ( -- )
   BETWEEN
   STARTUP
   RUNTIME-FILES
   ENGINE-LIB-FILES
   SEAL-ENGINE
   REQUIRE-REG:COUNT {: app:n :}
   ENTRY-OPEN
   SRC$ REQUIRE-FILE
   ENTRY-CLOSE
   app true END-WRITE
   END$ INCLUDE-FILE
   OUT$ LINKER:WRITE ;


\ A Wasm module's kernel words load ahead of the program, so its words and the
\ link reach them.
: APP-BODY ( -- )
   BETWEEN
   STARTUP
   WASM? if s" src/arch/wasm/kernel-words.f" REQUIRE-FILE then
   ENGINE-LIB-FILES
   SEAL-ENGINE
   ENTRY-OPEN
   SRC$ ENTRY-FILE
   ENTRY-CLOSE
   s" src/habu/app-end.f" INCLUDE-FILE
   OUT$ LINKER:WRITE ;

\ The tree's KERNEL-OBJECT:WRITE, which the kernel build compiled.
CAST: >WRITER ( n -- [ bool [ str -- ] -- ] )

: WRITER ( -- [ bool [ str -- ] -- ] )
   s" KERNEL-OBJECT" BOOT:NAMESPACE-WL CHECKER-RESOLVE:WL-PROBE {: pkg:^ :}
   pkg XREF-FOUND? 0= if
      s" hb-build: the tree has no package KERNEL-OBJECT" EXIT-STATUS:INTERNAL die
   then
   s" WRITE" pkg XREF-PKG-PUBLIC CHECKER-RESOLVE:WL-PROBE {: rec:^ :}
   rec XREF-FOUND? 0= if
      s" hb-build: KERNEL-OBJECT has no public WRITE" EXIT-STATUS:INTERNAL die
   then
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

: KNAME+ ( str -- )
   {: s:str :}
   KNAME-U @ s STR:LEN + PATH-CAP > if E-FS-CAPACITY throw then
   s STR:DATA  KNAME KNAME-U @ +  s STR:LEN BYTE-COPY
   s STR:LEN KNAME-U +! ;

: KIND$ ( -- str )
   KERNEL-ENGINE? @ if s" kernel-engine" else s" kernel-app" then ;

\ The kernel directory is BUILD-OUT:OBJ$ of <profile>/kernel/<the tree root's
\ canonical path>; the inputs file in it is kernel-engine.inputs, or
\ kernel-app.inputs for a stripped program's kernel.
: KERNEL-PATHS ( -- )
   0 KNAME-U !  0 KERNEL-U !
   TARGET @ RTARGET:PROFILE$ KNAME+  s" /kernel" KNAME+
   SOURCE-ROOT:ENGINE$ {: c:str :}
   c STR:LEN 0 > if c STR:DATA c@ FS:SLASH = else false then
   0= if s" /" KNAME+ then
   c KNAME+
   KNAME KNAME-U @ >str BUILD-OUT:OBJ$ {: d:str :}
   d STR:DATA KERNEL-DIR d STR:LEN BYTE-COPY  d STR:LEN KERNEL-DIR-U !
   0 KNAME-U !  KIND$ KNAME+  s" .inputs" KNAME+
   KERNEL-DIR d STR:LEN >str KNAME KNAME-U @ >str
   INPUTS-PATH FS:JOIN-PATH INPUTS-U ! ;

: HEX+ ( n -- )
   {: b:n :}
   s" 0123456789abcdef" STR:DATA {: d:^ :}
   d b 4 rshift + 1 >str KNAME+
   d b 15 and + 1 >str KNAME+ ;

\ KERNEL-PATH becomes the object whose checksum is the SUM-U bytes:
\ kernel-engine-<them as lower-case hex>.hko in the kernel directory, or
\ kernel-app-<…>.hko.
: OBJECT-PATH! ( ^u8 -- )
   {: sum:^ :}
   0 KNAME-U !  KIND$ KNAME+  s" -" KNAME+
   SUM-U 0 ?do sum i + c@ HEX+ loop
   s" .hko" KNAME+
   KERNEL-DIR KERNEL-DIR-U @ >str KNAME KNAME-U @ >str
   KERNEL-PATH FS:JOIN-PATH KERNEL-U ! ;

\ Writes the object whole at the path its checksum names, then the inputs file
\ whole, unless the object's stamp cannot be read. The stamp fails an object
\ changed in place since. Nothing older is deleted.
: KERNEL-WRITE ( str -- )
   {: s:str :}
   KERNEL-DIR KERNEL-DIR-U @ >str FS:MAKE-DIRS
   s STR:DATA 8 + OBJECT-PATH!
   KERNEL-PATH KERNEL-U @ >str s FS:ATOMIC-WRITE-FILE
   KERNEL-PATH KERNEL-U @ >str FS:TRY-FILE-META
   0= if 2drop 2drop drop exit then
   {: size:n ms:n mns:n cs:n cns:n :}
   LINKER:INPUTS$ {: l:str :}
   l STR:LEN {: lu:n :}
   HEAD-U lu + RECORD-RESERVE
   s STR:DATA 8 +  0 RECORD  SUM-U BYTE-COPY
   cs  SUM-U RECORD LE:U64!
   cns  SUM-U 8 + RECORD LE:U64!
   size  SUM-U 16 + RECORD LE:U64!
   lu  SUM-U 24 + RECORD LE:U64!
   l STR:DATA  HEAD-U RECORD  lu BYTE-COPY
   INPUTS-PATH INPUTS-U @ >str  0 RECORD  HEAD-U lu + >str
   FS:ATOMIC-WRITE-FILE
   RECORD-RELEASE ;

\ The inputs file whole into RECORD through one descriptor, its size into
\ RECORD-U; false when it is missing, shorter than its head or over INPUTS-CAP.
: INPUTS-READ ( -- bool )
   INPUTS-PATH INPUTS-U @ >str FS:OPEN-SIZED {: file:fd n:n found:bool :}
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
   code E-MEM-MAP = or  if false exit then
   code throw ;

\ Whether the log runs exactly to the file's end, and the object the checksum
\ names has the recorded stamp.
: HEAD-HOLDS? ( -- bool )
   RECORD-U @ HEAD-U -  SUM-U 24 + RECORD LE:U64@  <> if false exit then
   0 RECORD OBJECT-PATH!
   KERNEL-PATH KERNEL-U @ >str FS:TRY-FILE-META
   0= if 2drop 2drop drop false exit then
   {: size:n ms:n mns:n cs:n cns:n :}
   SUM-U RECORD LE:U64@ cs =  SUM-U 8 + RECORD LE:U64@ cns = and
   SUM-U 16 + RECORD LE:U64@ size = and ;

: RECORD-HOLDS? ( -- bool )
   INPUTS-READ? 0= if false exit then
   HEAD-HOLDS? 0= if false exit then
   HEAD-U RECORD  RECORD-U @ HEAD-U - >str  LINKER:INPUTS-HOLD? ;

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
   BETWEEN
   STARTUP
   TARGET @ RTARGET:ARCH@ CTARGET-ARCH:X86-64 CTARGET-ARCH:EQ if
      s" src/arch/x86-64/kernel-object.f"
   else s" src/arch/arm64/kernel-object.f" then REQUIRE-FILE
   KERNEL-ENGINE? @ ['] KERNEL-WRITE WRITER execute ;

\ LINKER:WITH's build of the mode, which is idle once it returns.
: LINK ( str RTARGET:resolved-target [ -- renders nominal roots ] -- )
   {: mode:str target:RTARGET:resolved-target body :}
   mode OPENING
   mode target body LINKER:WITH
   construct stage idle STAGE! ;

\ hb and a REPL program run where hb runs, so only a program builds for Wasm:
\ ENGINE and REPL call this before they make anything.
: REJECT-WASM ( RTARGET:resolved-target -- )
   TARGET-WASM? if
      s" E-BUILD-USAGE" REJECTION:OPEN
      s" a Wasm target builds a program, not an engine or a REPL" REJECTION:EMIT-STR
      REJECTION:HELP
      s" build the engine or REPL for macos-aarch64, linux-aarch64 or linux-x86-64, or drop `--engine` / `--repl` to build a Wasm program"
      REJECTION:EMIT-STR
      EXIT-STATUS:USAGE REJECTION:STOP
   then ;

public

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
   SOURCE-ROOT:ENGINE$ [:
      ADOPT? 0= if
         s" kernel" target ['] KERNEL-BODY LINK
      then
   ;] SOURCE-ROOT:WITH
   KERNEL-PATH KERNEL-U @ >str LINKER:KERNEL ;

\ The path of the kernel object the last KERNEL-BUILD wrote or adopted, which
\ names its checksum, so a later read of it reads the bytes the build linked.
: KERNEL$ ( -- str ) KERNEL-PATH KERNEL-U @ >str ;

\ The tree's inputs file for the last KERNEL-BUILD's profile and kind; any
\ kernel build of this tree may replace it with a log that holds as well, read
\ from the tree's root.
: KERNEL-INPUTS$ ( -- str ) INPUTS-PATH INPUTS-U @ >str ;

\ Where a build that ended was: loading answers its file; opening "the <mode>
\ build's open"; between "the <mode> build outside its files"; idle empty.
: WHERE$ ( -- str )
   STAGE@ MATCH stage
      idle OF WHERE-BUF 0 >str ENDOF
      opening OF s" 's open" BUILD$ ENDOF
      between OF s"  outside its files" BUILD$ ENDOF
      loading OF FILE$ ENDOF
   ;MATCH ;

\ Build this tree's hb for the target and write it to the path the quotation
\ answers when the build writes.
: ENGINE ( [ -- str ] RTARGET:resolved-target -- )
   {: out target:RTARGET:resolved-target :}
   target REJECT-WASM
   out OUT-Q !
   true target KERNEL-BUILD  ENGINE-END!
   s" engine" target ['] ENGINE-BODY LINK ;

\ Build hb with the source's files, the source's MAIN its entry, for the target
\ and write it to the path the quotation answers when the build writes.
: REPL ( str [ -- str ] RTARGET:resolved-target -- )
   {: src:str out target:RTARGET:resolved-target :}
   target REJECT-WASM
   src STR:DATA SRC-PATH !  src STR:LEN SRC-LEN !  out OUT-Q !
   true target KERNEL-BUILD  REPL-END!
   s" engine" target ['] REPL-BODY LINK ;

\ Build the source as a stripped program for the target and write it to
\ the path the quotation answers when the build writes.
: APP ( str [ -- str ] RTARGET:resolved-target -- )
   {: src:str out target:RTARGET:resolved-target :}
   src STR:DATA SRC-PATH !  src STR:LEN SRC-LEN !  out OUT-Q !
   target TARGET !
   WASM? 0= if false target KERNEL-BUILD then
   s" app" target ['] APP-BODY LINK ;

;package
