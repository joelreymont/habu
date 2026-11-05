\ boot.f - the arm64 start-up at offset 0 of a stripped product's code. START,
\ maps the stacks and DATA, copies the DATA image in, adds the load offset to
\ each cell the DATA list names, writes the run-time cells and calls the entry
\ the build stored. Its caller binds the four labels in the tail.
require src/arch/arm64/link-kernel.f

package A64BOOT
using A64ASM
using A64ICODE

\ x9 = the runtime address of the boot's first word, which is code offset 0.
: TEXT-BASE, ( label -- ) 9 swap ADR, ;

\ rd = text base + label's offset; x9 holds the text base.
: TEXT-AT, ( n label -- )
   {: rd:n l:label :}
   rd l LOFF,  rd 9 rd ADD, ;


\ x0 = DATA, x1 = the image in the code, x2 = the bytes between its labels,
\ BYTES,' pad included.
: COPY-DATA, ( label label label -- )
   {: text0:label start:label end:label :}
   text0 TEXT-BASE,
   0 DATA 0 ADDI,
   1 start TEXT-AT,
   2 end LOFF,  3 start LOFF,  2 2 3 SUB,
   EM-SNAPSHOT-COPY ;


\ x21 = text base - the address the code is linked at, added to the cell at
\ DATA + off for each u64 off in [x22, x23).
: SLIDE, ( n label label label -- )
   {: linked:n text0:label start:label end:label :}
   LBL LBL {: more:label done:label :}
   text0 TEXT-BASE,
   10 linked LIT64,  21 9 10 SUB,
   22 start TEXT-AT,  23 end TEXT-AT,
   more LBL,  22 23 CMP,  C-CS done BCOND,
      24 22 0 LDR,  24 DATA 24 ADD,
      25 24 0 LDR,  25 25 21 ADD,  25 24 0 STR,
      22 22 8 ADDI,  more B,
   done LBL, ;


: RUNTIME-CELLS, ( -- )
   XDS DATA STACK-ABI:BASE-CELL STR,
   7 STACK-ABI:BOOT-BYTES LIT64,  7 DATA STACK-ABI:CAP-CELL STR,
   13 DATA ARGC-CELL STR,  14 DATA ARGV-CELL STR,  15 DATA ENVP-CELL STR, ;


public

\ The code's linked address, then the DATA image's and list's start and end
\ labels, bound later. x13-x15 keep argc, argv and envp for RUNTIME-CELLS,.
: START, ( n label label label label -- )
   {: linked:n image:label image-end:label list:label list-end:label :}
   LBL {: text0:label :}
   ASM-LEN 0<> if s" boot: the start-up must sit at code offset 0" 76 die then
   text0 LBL,
   EM-ENTRY-ARGS                            \ argc/argv/envp into x13-x15 (habu2.f)
   C-SIGNAL-STACK                           \ the crash handler's stack (crash.f)
   STACK-ABI:BOOT-BYTES XDS STACK-GUARD:EMIT-MAP   \ x19 = the boot data stack (rt.f)
   EM-MMAP-DATA-REGION  DATA 0 0 ADDI,      \ x20 = DATA (habu2.f)
   text0 image image-end COPY-DATA,         \ the DATA image (habu2.f EM-SNAPSHOT-COPY)
   linked text0 list list-end SLIDE,        \ each listed code address, at its load address
   RUNTIME-CELLS,                           \ as habu2.f EM-DATA-INIT writes them
   EM-FRAME-STACKS                          \ return and loop stacks (habu2.f)
   text0 TEXT-BASE,  11 LCRASHH LABEL@ TEXT-AT,  G-INSTALL-CRASH-X11   \ crash.f
   9 DATA APP-ENTRY:XT-CELL LDR,  9 BLR,    \ the entry the build stored, slid
   0 0 MOVZ,  LEXITHOOK LABEL@ BL,  NR-EXIT-GROUP SYS, ;   \ exit 0 through habu2.f EMIT-EXITHOOK

;using
;using
;package
