\ boot.f - the arm64 start-up at offset 0 of a product's code. START, maps the
\ stacks and DATA, stores the DATA image's runs in it, adds the load offset to
\ each cell the DATA list names, writes the run-time cells and calls the entry
\ the build stored. ENGINE-START, first maps the code region, copies the record
\ image into its dictionary band and slides the record list, then builds the
\ name index and enters the engine's main. Its caller binds the tail's labels.
require src/lib/le.f
require src/lib/string.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/arch/arm64/mnem.f
require src/habu/layout.f
require src/habu/xref.f

HB-TARGET-LINUX? HB-TARGET-MACOS? or 0= [if] s" boot: unknown target" 76 die [then]
HB-TARGET-LINUX? [if] require src/os/linux/sys.f [then]
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

require src/habu/regalloc.f
require src/habu/habu1.f
require src/habu/prof.f
require src/habu/fdio.f
require src/habu/aot-decl.f
require src/habu/aot-ident.f
require src/habu/aot-owned.f
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


\ Store each run in [x1, x2) (link-data.f RUNS,) at DATA + off, x3 the target
\ and x4 the cells left; DATA outside the runs stays zero (EM-MMAP-DATA-REGION).
: COPY-DATA, ( label label label -- )
   {: text0:label start:label end:label :}
   LBL LBL LBL {: more:label each:label done:label :}
   text0 TEXT-BASE,
   1 start TEXT-AT,  2 end TEXT-AT,
   more LBL,  1 2 CMP,  C-CS done BCOND,
      3 1 0 LDR,  4 1 8 LDR,  1 1 16 ADDI,  3 DATA 3 ADD,
      each LBL,  4 more CBZ,
         5 1 0 LDR,  5 3 0 STR,
         1 1 8 ADDI,  3 3 8 ADDI,  4 4 1 SUBI,  each B,
   done LBL, ;


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

: SLIDE, ( n label label label -- ) DATA SLIDE-AT, ;


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
   XDS DATA STACK-ABI:BASE-CELL STR,
   7 STACK-ABI:BOOT-BYTES LIT64,  7 DATA STACK-ABI:CAP-CELL STR,
   13 DATA ARGC-CELL STR,  14 DATA ARGV-CELL STR,  15 DATA ENVP-CELL STR, ;


\ The process's entry: argc/argv/envp into x13-x15, which RUNTIME-CELLS, keeps,
\ the crash handler's stack and the boot data stack (x19).
: ENTRY, ( label -- )
   {: text0:label :}
   ASM-LEN 0<> if s" boot: the start-up must sit at code offset 0" 76 die then
   text0 LBL,
   EM-ENTRY-ARGS                            \ habu2.f
   C-SIGNAL-STACK                           \ crash.f
   STACK-ABI:BOOT-BYTES XDS STACK-GUARD:EMIT-MAP ;   \ rt.f

\ DATA mapped (x20), its image's runs stored and its listed code addresses slid.
\ The text size is read from the header the slid RBASE-CELL finds.
: DATA, ( n label label label label label -- )
   {: linked:n text0:label image:label image-end:label list:label list-end:label :}
   EM-MMAP-DATA-REGION  DATA 0 0 ADDI,      \ habu2.f
   text0 image image-end COPY-DATA,
   linked text0 list list-end SLIDE,
   RUNTIME-CELLS,
   EM-TEXT-SIZE ;                           \ habu2.f

\ The return and loop stacks, then the crash handler.
: STACKS, ( label -- )
   {: text0:label :}
   EM-FRAME-STACKS                          \ habu2.f
   text0 TEXT-BASE,  11 LCRASHH LABEL@ TEXT-AT,  G-INSTALL-CRASH-X11 ;   \ crash.f

\ SIGTRAP repointed from the crash handler at the breakpoint handler; an
\ engine's only (a stripped program has no breakpoints).
: TRAP, ( label -- )
   {: text0:label :}
   text0 TEXT-BASE,  11 LTRAPH LABEL@ TEXT-AT,
   11 SIGACT-SIGINFO C-SIGACTION-FRAME  5 INSTALL-SIGACT  C-SIGACTION-FRAME-DONE ;

\ The persistent breakpoints' trampolines at the live code's start, stored as
\ BRK words through the region's RW alias (x17 = alias - base), their address
\ in BPTRAMP-CELL (layout.f); CP starts after them.
: TRAMPOLINES, ( -- )
   9 CP 0 ADDI,  9 DATA BPTRAMP-CELL STR,
   10 $D4200000D4200000 LIT64,  11 CP 17 ADD,
   BPTRAMP-BYTES 0 ?do 10 11 i STR, 8 +loop
   CP CP BPTRAMP-BYTES ADDI, ;

\ Calls the xt in DATA's cell, then exits 0 through habu2.f EMIT-EXITHOOK.
: CALL-EXIT, ( n -- )
   {: cell:n :}
   9 DATA cell LDR,  9 BLR,
   0 0 MOVZ,  LEXITHOOK LABEL@ BL,  NR-EXIT-GROUP SYS, ;


public

\ A stripped program's start-up: the code's linked address, then the DATA
\ image's and list's start and end labels, bound later.
: START, ( n label label label label -- )
   {: linked:n image:label image-end:label list:label list-end:label :}
   LBL {: text0:label :}
   text0 ENTRY,
   linked text0 image image-end list list-end DATA,
   text0 STACKS,
   APP-ENTRY:XT-CELL CALL-EXIT, ;

\ An engine's start-up: START,'s labels, then the record image's and the
\ record list's start and end. The region's two views come first: habu2.f
\ EM-MMAP-CODE-REGION reads the image headers through x20 = the text base and
\ leaves x21 = alias - base, which x17 keeps for REGION-ALIAS:DELTA-CELL.
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
   17 DATA REGION-ALIAS:DELTA-CELL STR,
   TRAMPOLINES,
   EM-DEF-OCC:LINIT LABEL@ BL,
   text0 STACKS,  text0 TRAP,
   LHIDXBUILD LABEL@ BL,                    \ habu1.f: maps the index, keys [0, NDICT)
   ENGINE-MAIN:XT-CELL CALL-EXIT, ;

;using
;using
;using
;using
;using
;package
