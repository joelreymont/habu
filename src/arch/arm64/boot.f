\ boot.f - the arm64 start-up at offset 0 of a product's code. START, maps the
\ stacks and DATA and places the DATA image in it: on Linux it moves the
\ carrier's pages to each run the table names, on macOS it stores each run's
\ cells. It adds the load offset to each cell the DATA list names, writes the
\ run-time cells and calls the entry the build stored. ENGINE-START, first
\ maps the code region, copies the record image into its dictionary band and
\ slides the record list, then builds the name index and enters the engine's
\ main. Its caller binds the tail's labels.
HB-TARGET-LINUX-ARM64? HB-TARGET-MACOS? or ?TARGET
require src/core/exit-status.f
require src/lib/le.f
require src/lib/string.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/arch/arm64/mnem.f
require src/habu/layout.f
require src/habu/xref.f

HB-TARGET-LINUX-ARM64? [if]
   require src/os/linux/layout.f
   require src/os/linux/sys.f
[then]
HB-TARGET-MACOS? [if] require src/os/macos/sys.f [then]

require src/os/script-argv.f
require src/habu/rt.f
require src/habu/crash.f

HB-TARGET-LINUX-ARM64? [if]
   require src/os/linux/proc-watch.f
   require src/os/linux/proc-control.f
[then]
HB-TARGET-MACOS? [if]
   require src/os/macos/proc-watch.f
   require src/os/macos/proc-control.f
[then]

require src/habu/habu1.f
require src/habu/prof.f
require src/habu/fdio.f
require src/habu/habu2.f

package A64BOOT

\ x9 = the runtime address of the boot's first word, which is code offset 0.
: TEXT-BASE, ( label -- ) 9 swap A64ICODE:ADR, ;

\ rd = text base + label's offset; x9 holds the text base.
: TEXT-AT, ( n label -- )
   {: rd:n l:label :}
   rd l A64ICODE:LOFF,  rd 9 rd A64ICODE:ADD, ;


HB-TARGET-LINUX-ARM64? [if]
\ Move each run [off][bytes] in [x6, x7) from the carrier, x10 at its pages,
\ to DATA + off. A failed move dies.
: PLACE-RUNS, ( label label label -- )
   {: text0:label start:label end:label :}
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: more:label moved:label done:label :}
   text0 TEXT-BASE,
   6 start TEXT-AT,  7 end TEXT-AT,
   10 LINUX-LAYOUT:CARRIER-VA VA>N A64ICODE:LIT64,
   more A64ICODE:LBL,  6 7 A64ICODE:CMP,  A64ASM:C-CS done A64ICODE:BCOND,
      11 6 0 A64ICODE:LDR,  1 6 8 A64ICODE:LDR,  6 6 16 A64ICODE:ADDI,  11 XREG-RBASE 11 A64ICODE:ADD,
      0 10 0 A64ICODE:ADDI,  2 1 0 A64ICODE:ADDI,  3 SYSCALL:MREMAP-MOVE-FIXED A64ICODE:MOVZ,  4 11 0 A64ICODE:ADDI,
      SYSCALL:NR-MREMAP SYSCALL:SYS,
      0 11 A64ICODE:CMP,  A64ASM:C-EQ moved A64ICODE:BCOND,
         s" hb: moving the image's data into the fixed data region failed; mremap answered:"
         EXIT-STATUS:SYSTEM 0 CRASH:DIE-LINE-VALUE,
      moved A64ICODE:LBL,
      10 10 1 A64ICODE:ADD,  more A64ICODE:B,
   done A64ICODE:LBL, ;
[then]

HB-TARGET-MACOS? [if]
\ Store each run in [x1, x2) at DATA + off, x3 the target and x4 the cells left;
\ DATA outside the runs stays zero.
: PLACE-RUNS, ( label label label -- )
   {: text0:label start:label end:label :}
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: more:label each:label done:label :}
   text0 TEXT-BASE,
   1 start TEXT-AT,  2 end TEXT-AT,
   more A64ICODE:LBL,  1 2 A64ICODE:CMP,  A64ASM:C-CS done A64ICODE:BCOND,
      3 1 0 A64ICODE:LDR,  4 1 8 A64ICODE:LDR,  1 1 16 A64ICODE:ADDI,  3 XREG-RBASE 3 A64ICODE:ADD,
      each A64ICODE:LBL,  4 more A64ICODE:CBZ,
         5 1 0 A64ICODE:LDR,  5 3 0 A64ICODE:STR,
         1 1 8 A64ICODE:ADDI,  3 3 8 A64ICODE:ADDI,  4 4 1 A64ICODE:SUBI,  each A64ICODE:B,
   done A64ICODE:LBL, ;
[then]


\ x21 = text base - the address the code is linked at, added to the cell at
\ base + off for each u64 off in [x22, x23); base is a register SLIDE-AT, keeps.
: SLIDE-AT, ( n label label label n -- )
   {: linked:n text0:label start:label end:label base:n :}
   A64ICODE:LBL A64ICODE:LBL {: more:label done:label :}
   text0 TEXT-BASE,
   10 linked A64ICODE:LIT64,  21 9 10 A64ICODE:SUB,
   22 start TEXT-AT,  23 end TEXT-AT,
   more A64ICODE:LBL,  22 23 A64ICODE:CMP,  A64ASM:C-CS done A64ICODE:BCOND,
      24 22 0 A64ICODE:LDR,  24 base 24 A64ICODE:ADD,
      25 24 0 A64ICODE:LDR,  25 25 21 A64ICODE:ADD,  25 24 0 A64ICODE:STR,
      22 22 8 A64ICODE:ADDI,  more A64ICODE:B,
   done A64ICODE:LBL, ;

: SLIDE, ( n label label label -- ) XREG-RBASE SLIDE-AT, ;


\ Copy the record image [start, end) to x16, the dictionary band's alias, a
\ record at a time, NDICT counting them.
: COPY-RECORDS, ( label label label -- )
   {: text0:label start:label end:label :}
   A64ICODE:LBL A64ICODE:LBL {: more:label done:label :}
   text0 TEXT-BASE,
   1 start TEXT-AT,  2 end TEXT-AT,
   NDICT 0 A64ICODE:MOVZ,
   more A64ICODE:LBL,  1 2 A64ICODE:CMP,  A64ASM:C-CS done A64ICODE:BCOND,
      DREC 0 ?do  3 1 i A64ICODE:LDR,  3 16 i A64ICODE:STR,  8 +loop
      1 1 DREC A64ICODE:ADDI,  16 16 DREC A64ICODE:ADDI,  NDICT NDICT 1 A64ICODE:ADDI,  more A64ICODE:B,
   done A64ICODE:LBL, ;


: RUNTIME-CELLS, ( -- )
   A64ICODE:XDS XREG-RBASE STACK-ABI:BASE-CELL A64ICODE:STR,
   7 STACK-ABI:BOOT-BYTES A64ICODE:LIT64,  7 XREG-RBASE STACK-ABI:CAP-CELL A64ICODE:STR,
   13 XREG-RBASE ARGC-CELL A64ICODE:STR,  14 XREG-RBASE ARGV-CELL A64ICODE:STR,  15 XREG-RBASE ENVP-CELL A64ICODE:STR, ;


\ The process's entry: argc/argv/envp into x13-x15, which RUNTIME-CELLS, keeps,
\ the crash handler's stack and the boot data stack (x19).
: ENTRY, ( label -- )
   {: text0:label :}
   A64ICODE:ASM-LEN 0<> if s" boot: the start-up must sit at code offset 0" EXIT-STATUS:INTERNAL die then
   text0 A64ICODE:LBL,
   ENGINE-EMIT:EM-ENTRY-ARGS
   CRASH:C-SIGNAL-STACK
   STACK-ABI:BOOT-BYTES A64ICODE:XDS STACK-GUARD:EMIT-MAP ;

\ Map DATA (x20), place its runs and slide the listed code addresses (Linux
\ lists none); the text size comes from the header the slid RBASE-CELL finds.
: DATA, ( n label label label label label -- )
   {: linked:n text0:label image:label image-end:label list:label list-end:label :}
   ENGINE-EMIT:EM-MMAP-DATA-REGION  XREG-RBASE 0 0 A64ICODE:ADDI,
   text0 image image-end PLACE-RUNS,
   linked text0 list list-end SLIDE,
   RUNTIME-CELLS,
   ENGINE-EMIT:EM-TEXT-SIZE ;

: STACKS, ( label -- )
   {: text0:label :}
   ENGINE-EMIT:EM-FRAME-STACKS
   text0 TEXT-BASE,  11 CRASH:LCRASHH A64ICODE:LABEL@ TEXT-AT,  CRASH:G-INSTALL-CRASH-X11 ;

\ SIGTRAP repointed from the crash handler at the breakpoint handler; an
\ engine's only (a stripped program has no breakpoints).
: TRAP, ( label -- )
   {: text0:label :}
   text0 TEXT-BASE,  11 ENGINE-EMIT:LTRAPH A64ICODE:LABEL@ TEXT-AT,
   11 CRASH:SIGACT-SIGINFO CRASH:C-SIGACTION-FRAME  5 CRASH:INSTALL-SIGACT  CRASH:C-SIGACTION-FRAME-DONE ;

\ The persistent breakpoints' trampolines at the live code's start, stored as
\ BRK words through the region's RW alias (x17 = alias - base), their address in
\ BPTRAMP-CELL; CP starts after them.
: TRAMPOLINES, ( -- )
   9 CP 0 A64ICODE:ADDI,  9 XREG-RBASE BPTRAMP-CELL A64ICODE:STR,
   10 $D4200000D4200000 A64ICODE:LIT64,  11 CP 17 A64ICODE:ADD,
   BREAKPOINT:BYTES 0 ?do 10 11 i A64ICODE:STR, 8 +loop
   CP CP BREAKPOINT:BYTES A64ICODE:ADDI, ;

: CALL-EXIT, ( n -- )
   {: cell:n :}
   9 XREG-RBASE cell A64ICODE:LDR,  9 A64ICODE:BLR,
   0 0 A64ICODE:MOVZ,  ENGINE-EMIT:LEXITHOOK A64ICODE:LABEL@ A64ICODE:BL,  SYSCALL:NR-EXIT-GROUP SYSCALL:SYS, ;


public

\ A stripped program's start-up: the code's linked address, then the DATA
\ image's and list's start and end labels. It has no records and an empty code
\ region at the text's end, so every xt is below CP and needs no ISB.
: START, ( n label label label label -- )
   {: linked:n image:label image-end:label list:label list-end:label :}
   A64ICODE:LBL {: text0:label :}
   text0 ENTRY,
   NDICT 0 A64ICODE:MOVZ,
   linked text0 image image-end list list-end DATA,
   DBASE XREG-RBASE RBASE-CELL A64ICODE:LDR,  9 XREG-RBASE TEXT-SIZE-CELL A64ICODE:LDR,
   DBASE DBASE 9 A64ICODE:ADD,  CP DBASE 0 A64ICODE:ADDI,
   text0 STACKS,
   APP-ENTRY:XT-CELL CALL-EXIT,  A64ICODE:DATA-AFTER, ;

\ An engine's start-up: START,'s labels, then the record image's and the record
\ list's start and end; x17 keeps alias - base for REGION-ALIAS:DELTA-CELL.
: ENGINE-START, ( n label label label label label label label label -- )
   {: linked:n image:label image-end:label list:label list-end:label
      recs:label recs-end:label rlist:label rlist-end:label :}
   A64ICODE:LBL {: text0:label :}
   text0 ENTRY,
   text0 TEXT-BASE,  XREG-RBASE 9 0 A64ICODE:ADDI,
   ENGINE-EMIT:EM-MMAP-CODE-REGION                      \ x0 = the region's base
   DBASE 0 0 A64ICODE:ADDI,
   CP DBASE 0 A64ICODE:ADDI,  5 DICT-SIZE A64ICODE:LIT64,  CP CP 5 A64ICODE:ADD,
   17 21 0 A64ICODE:ADDI,
   16 DBASE 17 A64ICODE:ADD,  text0 recs recs-end COPY-RECORDS,
   16 DBASE 17 A64ICODE:ADD,  linked text0 rlist rlist-end 16 SLIDE-AT,
   linked text0 image image-end list list-end DATA,
   17 XREG-RBASE REGION-ALIAS:DELTA-CELL A64ICODE:STR,
   9 A64ICODE:SP 0 A64ICODE:ADDI,  9 XREG-RBASE MAIN-STACK-TOP-CELL A64ICODE:STR,
   9 ENGINE-EMIT:CB-SELF,  9 XREG-RBASE MAIN-THREAD-CELL A64ICODE:STR,
   DBASE XREG-RBASE DBASE-CELL A64ICODE:STR,
   TRAMPOLINES,
   EM-DEF-OCC:LINIT A64ICODE:LABEL@ A64ICODE:BL,
   text0 STACKS,  text0 TRAP,
   ENGINE-EMIT:LHIDXBUILD A64ICODE:LABEL@ A64ICODE:BL,                    \ Maps the index, keys [0, NDICT)
   ENGINE-MAIN:XT-CELL CALL-EXIT,  A64ICODE:DATA-AFTER, ;

;package
