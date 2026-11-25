\ boot.f - the arm64 start-up at offset 0 of a product's code. START, maps the
\ stacks and DATA and places the DATA image in it: on Linux it moves the
\ carrier's pages to each run the table names, on macOS it stores each run's
\ cells. It adds the load offset to each cell the DATA list names, writes the
\ run-time cells and calls the entry the build stored. ENGINE-START, first
\ maps the code region, copies the record image into its dictionary band and
\ slides the record list, then builds the name index and enters the engine's
\ main. Its caller binds the tail's labels.
HB-TARGET-LINUX? HB-TARGET-MACOS? or ?TARGET
require src/core/exit-status.f
require src/lib/le.f
require src/lib/string.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/arch/arm64/mnem.f
require src/habu/layout.f
require src/habu/xref.f

HB-TARGET-LINUX? [if]
   require src/os/linux/layout.f
   require src/os/linux/sys.f
[then]
HB-TARGET-MACOS? [if] require src/os/macos/sys.f [then]

require src/os/script-argv.f
require src/habu/rt.f
require src/habu/crash.f

HB-TARGET-LINUX? [if]
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
using A64ASM
using A64ICODE
using CRASH
using ENGINE-EMIT
using SYSCALL

\ x9 = the runtime address of the boot's first word, which is code offset 0.
: TEXT-BASE, ( label -- ) 9 swap ADR, ;

\ rd = text base + label's offset; x9 holds the text base.
: TEXT-AT, ( n label -- )
   {: rd:n l:label :}
   rd l LOFF,  rd 9 rd ADD, ;


HB-TARGET-LINUX? [if]
\ Move each run [off][bytes] in [x6, x7) from the carrier, x10 at its pages,
\ to DATA + off. A failed move dies.
: PLACE-RUNS, ( label label label -- )
   {: text0:label start:label end:label :}
   LBL LBL LBL {: more:label moved:label done:label :}
   text0 TEXT-BASE,
   6 start TEXT-AT,  7 end TEXT-AT,
   10 CARRIER-OFF LINUX-LAYOUT:CODE-OFF - LIT64,  10 9 10 ADD,
   more LBL,  6 7 CMP,  C-CS done BCOND,
      11 6 0 LDR,  1 6 8 LDR,  6 6 16 ADDI,  11 XREG-RBASE 11 ADD,
      0 10 0 ADDI,  2 1 0 ADDI,  3 MREMAP-MOVE-FIXED MOVZ,  4 11 0 ADDI,
      NR-MREMAP SYS,
      0 11 CMP,  C-EQ moved BCOND,
         s" hb: moving the image's data into the fixed data region failed; mremap answered:"
         EXIT-STATUS:SYSTEM 0 DIE-LINE-VALUE,
      moved LBL,
      10 10 1 ADD,  more B,
   done LBL, ;
[then]

HB-TARGET-MACOS? [if]
\ Store each run in [x1, x2) at DATA + off, x3 the target and x4 the cells left;
\ DATA outside the runs stays zero.
: PLACE-RUNS, ( label label label -- )
   {: text0:label start:label end:label :}
   LBL LBL LBL {: more:label each:label done:label :}
   text0 TEXT-BASE,
   1 start TEXT-AT,  2 end TEXT-AT,
   more LBL,  1 2 CMP,  C-CS done BCOND,
      3 1 0 LDR,  4 1 8 LDR,  1 1 16 ADDI,  3 XREG-RBASE 3 ADD,
      each LBL,  4 more CBZ,
         5 1 0 LDR,  5 3 0 STR,
         1 1 8 ADDI,  3 3 8 ADDI,  4 4 1 SUBI,  each B,
   done LBL, ;
[then]


\ x21 = text base - the address the code is linked at, added to the cell at
\ base + off for each u64 off in [x22, x23); base is a register SLIDE-AT, keeps.
: SLIDE-AT, ( n label label label n -- )
   {: linked:n text0:label start:label end:label base:n :}
   LBL LBL {: more:label done:label :}
   text0 TEXT-BASE,
   10 linked LIT64,  21 9 10 SUB,
   22 start TEXT-AT,  23 end TEXT-AT,
   more LBL,  22 23 CMP,  C-CS done BCOND,
      24 22 0 LDR,  24 base 24 ADD,
      25 24 0 LDR,  25 25 21 ADD,  25 24 0 STR,
      22 22 8 ADDI,  more B,
   done LBL, ;

: SLIDE, ( n label label label -- ) XREG-RBASE SLIDE-AT, ;


\ Copy the record image [start, end) to x16, the dictionary band's alias, a
\ record at a time, NDICT counting them.
: COPY-RECORDS, ( label label label -- )
   {: text0:label start:label end:label :}
   LBL LBL {: more:label done:label :}
   text0 TEXT-BASE,
   1 start TEXT-AT,  2 end TEXT-AT,
   NDICT 0 MOVZ,
   more LBL,  1 2 CMP,  C-CS done BCOND,
      DREC 0 ?do  3 1 i LDR,  3 16 i STR,  8 +loop
      1 1 DREC ADDI,  16 16 DREC ADDI,  NDICT NDICT 1 ADDI,  more B,
   done LBL, ;


: RUNTIME-CELLS, ( -- )
   XDS XREG-RBASE STACK-ABI:BASE-CELL STR,
   7 STACK-ABI:BOOT-BYTES LIT64,  7 XREG-RBASE STACK-ABI:CAP-CELL STR,
   13 XREG-RBASE ARGC-CELL STR,  14 XREG-RBASE ARGV-CELL STR,  15 XREG-RBASE ENVP-CELL STR, ;


\ The process's entry: argc/argv/envp into x13-x15, which RUNTIME-CELLS, keeps,
\ the crash handler's stack and the boot data stack (x19).
: ENTRY, ( label -- )
   {: text0:label :}
   ASM-LEN 0<> if s" boot: the start-up must sit at code offset 0" EXIT-STATUS:INTERNAL die then
   text0 LBL,
   EM-ENTRY-ARGS
   C-SIGNAL-STACK
   STACK-ABI:BOOT-BYTES XDS STACK-GUARD:EMIT-MAP ;

\ Map DATA (x20), place its runs and slide the listed code addresses (Linux
\ lists none); the text size comes from the header the slid RBASE-CELL finds.
: DATA, ( n label label label label label -- )
   {: linked:n text0:label image:label image-end:label list:label list-end:label :}
   EM-MMAP-DATA-REGION  XREG-RBASE 0 0 ADDI,
   text0 image image-end PLACE-RUNS,
   linked text0 list list-end SLIDE,
   RUNTIME-CELLS,
   EM-TEXT-SIZE ;

: STACKS, ( label -- )
   {: text0:label :}
   EM-FRAME-STACKS
   text0 TEXT-BASE,  11 LCRASHH LABEL@ TEXT-AT,  G-INSTALL-CRASH-X11 ;

\ SIGTRAP repointed from the crash handler at the breakpoint handler; an
\ engine's only (a stripped program has no breakpoints).
: TRAP, ( label -- )
   {: text0:label :}
   text0 TEXT-BASE,  11 LTRAPH LABEL@ TEXT-AT,
   11 SIGACT-SIGINFO C-SIGACTION-FRAME  5 INSTALL-SIGACT  C-SIGACTION-FRAME-DONE ;

\ The persistent breakpoints' trampolines at the live code's start, stored as
\ BRK words through the region's RW alias (x17 = alias - base), their address in
\ BPTRAMP-CELL; CP starts after them.
: TRAMPOLINES, ( -- )
   9 CP 0 ADDI,  9 XREG-RBASE BPTRAMP-CELL STR,
   10 $D4200000D4200000 LIT64,  11 CP 17 ADD,
   BREAKPOINT:BYTES 0 ?do 10 11 i STR, 8 +loop
   CP CP BREAKPOINT:BYTES ADDI, ;

: CALL-EXIT, ( n -- )
   {: cell:n :}
   9 XREG-RBASE cell LDR,  9 BLR,
   0 0 MOVZ,  LEXITHOOK LABEL@ BL,  NR-EXIT-GROUP SYS, ;


public

\ A stripped program's start-up: the code's linked address, then the DATA
\ image's and list's start and end labels. It has no records and an empty code
\ region at the text's end, so every xt is below CP and needs no ISB.
: START, ( n label label label label -- )
   {: linked:n image:label image-end:label list:label list-end:label :}
   LBL {: text0:label :}
   text0 ENTRY,
   NDICT 0 MOVZ,
   linked text0 image image-end list list-end DATA,
   DBASE XREG-RBASE RBASE-CELL LDR,  9 XREG-RBASE TEXT-SIZE-CELL LDR,
   DBASE DBASE 9 ADD,  CP DBASE 0 ADDI,
   text0 STACKS,
   APP-ENTRY:XT-CELL CALL-EXIT, ;

\ An engine's start-up: START,'s labels, then the record image's and the record
\ list's start and end; x17 keeps alias - base for REGION-ALIAS:DELTA-CELL.
: ENGINE-START, ( n label label label label label label label label -- )
   {: linked:n image:label image-end:label list:label list-end:label
      recs:label recs-end:label rlist:label rlist-end:label :}
   LBL {: text0:label :}
   text0 ENTRY,
   text0 TEXT-BASE,  XREG-RBASE 9 0 ADDI,
   EM-MMAP-CODE-REGION                      \ x0 = the region's base
   DBASE 0 0 ADDI,
   CP DBASE 0 ADDI,  5 DICT-SIZE LIT64,  CP CP 5 ADD,
   17 21 0 ADDI,
   16 DBASE 17 ADD,  text0 recs recs-end COPY-RECORDS,
   16 DBASE 17 ADD,  linked text0 rlist rlist-end 16 SLIDE-AT,
   linked text0 image image-end list list-end DATA,
   17 XREG-RBASE REGION-ALIAS:DELTA-CELL STR,
   9 SP 0 ADDI,  9 XREG-RBASE MAIN-STACK-TOP-CELL STR,
   9 CB-SELF,  9 XREG-RBASE MAIN-THREAD-CELL STR,
   DBASE XREG-RBASE DBASE-CELL STR,
   TRAMPOLINES,
   EM-DEF-OCC:LINIT LABEL@ BL,
   text0 STACKS,  text0 TRAP,
   LHIDXBUILD LABEL@ BL,                    \ Maps the index, keys [0, NDICT)
   ENGINE-MAIN:XT-CELL CALL-EXIT, ;

;using
;using
;using
;using
;using
;package
