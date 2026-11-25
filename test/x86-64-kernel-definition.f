\ x86-64-kernel-definition.f - the definition writers of the x86-64 kernel
\ (src/habu/kernel-x64.f DEFINITION,) in the booted harness, cross-built for an
\ x86-64 peer. Each image puts the code region at rest before calling the rows,
\ so a row that writes outside its window faults (the crash handler dumps and
\ dies by SIGSEGV); CLOSED, checks each window after a row writes a record.
require src/core/exit-status.f
require test/x86-64-boot-harness.f

package X64K-DEFINITION
using X64ASM
using X64CODE
using X64RT

5 constant WID                         \ a public wordlist past the fixed ones
6 constant OTHER-WID
9 constant ALIAS-WID
12 constant PARENT-WID
20 constant FIRST-WID                  \ WIDN-CELL before a namespace row takes its wids
-1 constant BOTH-WIDS                  \ namespace-record's flag: a private wid too
0 constant ONE-WID
7 constant SIG-LEN

\ The cell after a record's code cell: a namespace row's private wid, a
\ definition's code length.
X64KERNEL:REC-CODE CELL + constant REC-AUX

\ The flag bits an alias copies, and a source's flags with DKIND:VAL too.
DNAME-IMM DNAME-WIDE or constant COPIED
COPIED DKIND:VAL or constant SOURCE-FLAGS

\ A 17-byte name, longer than DNAME-INL, stored at CP: it fills past the first
\ code slot, so CP moves past the second.
: LONG$ ( -- str ) s" abcdefghijklmnopq" ;
DICT-SIZE X64KERNEL:CODE-SLOT + constant SECOND-SLOT
SECOND-SLOT X64KERNEL:CODE-SLOT + constant PAST-LONG

\ A source name past DNAME-INL, so its record carries DNAME-EXT.
: SOURCE$ ( -- str ) s" a-source-past-inline" ;

: HELLO$ ( -- str ) s" hello" ;
HELLO$ STR:LEN 1+ constant HELLO-LEN        \ "hello" and its space
BODY-TEXT:FIRST-CAP constant FIRST-CAP

\ The text cell a "hello" run starts, in a block whose next two bytes it never
\ wrote, and the cell it ends when it fills the text up to a cell boundary.
: HEAD ( -- n ) s" hello " 0 X64HARNESS:NAME-CELL ;
: TAIL ( -- n ) HEAD 16 lshift ;

: DATA-REG ( -- r64 ) ENGINE-GPR:X64-RBASE >R64 ;
: DBASE-REG ( -- r64 ) ENGINE-GPR:X64-DBASE >R64 ;

\ ---- the body text block, which body-capture maps at run time --------------
: TEXT@, ( r64 -- ) {: r:r64 :}
   r DATA-REG BODY-TEXT:BASE-CELL MEM-OFF ASM-SINK ENC-MOV-RM ;

\ Push the cell off bytes into the text, or into the rows at base + capacity.
: PUSH-TEXT-CELL, ( n -- ) {: off:n :}
   RCX TEXT@,  RAX RCX off MEM-OFF ASM-SINK ENC-MOV-RM  0 G-PUSH ;
: PUSH-ROW-CELL, ( n -- ) {: off:n :}
   RCX TEXT@,  RCX DATA-REG BODY-TEXT:CAP-CELL MEM-OFF ASM-SINK ENC-ADD-RM
   RAX RCX off MEM-OFF ASM-SINK ENC-MOV-RM  0 G-PUSH ;

\ Push the address off bytes into the text, or store it in the DATA cell at
\ cell; pop one and expect it off bytes in.
: PUSH-TEXT-ADDR, ( n -- ) {: off:n :}
   RAX TEXT@,  RAX RAX off MEM-OFF ASM-SINK ENC-LEA  0 G-PUSH ;
: TEXT-ADDR!, ( n n -- ) {: off:n cell:n :}
   RAX TEXT@,  RAX RAX off MEM-OFF ASM-SINK ENC-LEA
   RAX DATA-REG cell MEM-OFF ASM-SINK ENC-MOV-MR ;
: EXPECT-POP-TEXT, ( n -- ) {: want:n :}
   0 G-POP  RAX DATA-REG BODY-TEXT:BASE-CELL MEM-OFF ASM-SINK ENC-SUB-RM
   0 G-PUSH  want X64HARNESS:EXPECT-POP, ;

\ Keep the block's address in scratch cell 0; push 1 once it differs.
: SAVE-BASE, ( -- )
   RAX TEXT@,  RAX DATA-REG X64HARNESS:SCRATCH-OFF MEM-OFF ASM-SINK ENC-MOV-MR ;
: PUSH-MOVED, ( -- )
   X64CODE:LBL {: same:label :}
   RAX ZERO-REG,  RCX TEXT@,
   RCX DATA-REG X64HARNESS:SCRATCH-OFF MEM-OFF ASM-SINK ENC-SUB-RM  C-E same JCC,
   RAX 1 >IMM32 ASM-SINK ENC-MOV-RI32
   same X64CODE:LBL,  0 G-PUSH ;

\ ---- the staging and the checks ----------------------------------------------
: NAMESPACE, ( str n -- ) {: s:str flag:n :}
   s X64HARNESS:PUSH-TEXT,  flag X64HARNESS:PUSH,
   s" namespace-record" X64HARNESS:CALL-ROW, ;

: ALIAS, ( str n n -- ) {: s:str src:n wid:n :}
   s X64HARNESS:PUSH-TEXT,  src X64HARNESS:PUSH,  wid X64HARNESS:PUSH,
   s" alias-record" X64HARNESS:CALL-ROW, ;

: DEF-OPEN, ( str n n -- ) {: s:str wid:n kind:n :}
   s X64HARNESS:PUSH-TEXT,  wid X64HARNESS:PUSH,  kind X64HARNESS:PUSH,
   s" def-open" X64HARNESS:CALL-ROW, ;

: SCOPE, ( n n -- ) {: row:n parent:n :}
   row X64HARNESS:PUSH,  parent X64HARNESS:PUSH,
   s" package-scope!" X64HARNESS:CALL-ROW, ;

: APPEND, ( str -- ) X64HARNESS:PUSH-TEXT,  s" body-capture" X64HARNESS:CALL-ROW, ;

\ The signature row named over SIG-LEN bytes at the text's start, a span whose
\ address the block fixes at run time.
: SIG, ( str -- ) {: row:str :}
   0 PUSH-TEXT-ADDR,  SIG-LEN X64HARNESS:PUSH,
   row X64HARNESS:CALL-ROW, ;

: XREF, ( str n -- ) {: s:str wid:n :}
   s X64HARNESS:PUSH-TEXT,  wid X64HARNESS:PUSH,
   s" xref-search-wl" X64HARNESS:CALL-ROW, ;

: AFTER-SEAL, ( -- ) 1 SEAL-NDICT-CELL X64HARNESS:CELL!, ;
\ The writers reject on a task's or callback's DATA, not on main with tasks live.
: LIVE, ( -- ) 1 TASK-TCB-CELL X64HARNESS:CELL!, ;

\ Move CP n bytes into the region.
: CP-AT, ( n -- ) {: off:n :}
   ENGINE-GPR:X64-CP >R64 DBASE-REG off MEM-OFF ASM-SINK ENC-LEA ;

: PUSH-NDICT, ( -- )
   RAX ENGINE-GPR:X64-NDICT >R64 ASM-SINK ENC-MOV-RR  0 G-PUSH ;

: OR-CELL, ( n -- ) {: off:n :} RAX DATA-REG off MEM-OFF ASM-SINK ENC-OR-RM ;

\ The four package cells or'd together: 0 once they clear.
: PUSH-PKG, ( -- )
   RAX ZERO-REG,
   PKG-PUB-CELL OR-CELL,  PKG-PRI-CELL OR-CELL,
   PKG-PARENT-CELL OR-CELL,  PKG-REC-CELL OR-CELL,
   0 G-PUSH ;

\ The seven signature and clause cells def-open clears, or'd together.
: PUSH-SIGS, ( -- )
   RAX ZERO-REG,
   TSIG-A-CELL OR-CELL,  TSIG-U-CELL OR-CELL,
   TCSIG-A-CELL OR-CELL,  TCSIG-U-CELL OR-CELL,
   DOESB-CELL OR-CELL,  TRUSTED-CELL OR-CELL,  CLAUSE-CELL OR-CELL,
   0 G-PUSH ;

\ The tier-0 definition def-close ends: its seven signature and clause cells
\ set.
: TIER-0-OPEN, ( -- )
   1 NCOMP-DISPATCH:COMP-CELL X64HARNESS:CELL!,
   HELLO$ 0 0 DEF-OPEN,
   1 TSIG-A-CELL X64HARNESS:CELL!,  2 TSIG-U-CELL X64HARNESS:CELL!,
   3 TCSIG-A-CELL X64HARNESS:CELL!,  4 TCSIG-U-CELL X64HARNESS:CELL!,
   5 DOESB-CELL X64HARNESS:CELL!,  6 TRUSTED-CELL X64HARNESS:CELL!,
   7 CLAUSE-CELL X64HARNESS:CELL!, ;

\ One check that a row left record k unwritable: its page stays read-execute,
\ so a probe of its flags faults.
: CLOSED, ( n -- ) {: k:n :}
   k DREC * X64KERNEL:REC-FLAGS + X64HARNESS:PROBE,  X64HARNESS:FAULTED X64HARNESS:EXPECT-POP, ;

\ ---- the writers --------------------------------------------------------------
\ - hb-x64-kernel-namespace-record builds the index, publishes a namespace row
\   with a private wid past a seeded record and checks its index, its two fresh
\   wids, its flags cell and WIDN-CELL; xref-search-wl then finds it under a
\   name that differs in case.
\ - hb-x64-kernel-namespace-private publishes a namespace row whose [8] is 0 and
\   then gives it a private wid.
\ - hb-x64-kernel-alias-record, after the seal and with the wid's neighbour in
\   its bitmap cell protected, aliases a long-named source: the alias carries
\   the source's code and length cells and its IMM, WIDE, MIN-IN and immutable
\   VAL bits, but not its EXT bit, and xref-search-wl finds it.
\ - hb-x64-kernel-package-scope, with a task live past the namespace row, sets
\   the four package cells from that row, then clears them with `-1 0`.
\ - hb-x64-kernel-def-open, after the seal, opens a definition whose name
\   another wid holds: the pending record, its cells, and NDICT unchanged.
\ - hb-x64-kernel-def-open-state checks the state def-open resets: its record's
\   length cell 0, TSIG, TCSIG, DOESB, CLAUSE and TRUSTED clear, COMP-LATCH-CELL 1
\   with COMP-CELL set, and CP, which an inline name leaves.
\ - hb-x64-kernel-def-open-long stores a 17-byte name at CP: CP and the entry
\   move 32 bytes and the pad over poisoned cells is 0.
\ - hb-x64-kernel-body-append maps the block at the first capture: FIRST-CAP,
\   17 * FIRST-CAP bytes, the run at the text's start.
\ - hb-x64-kernel-body-append-fill fills the text exactly to FIRST-CAP in
\   place: the block does not move.
\ - hb-x64-kernel-body-append-grow grows it past FIRST-CAP: the block moves
\   to twice the capacity, its text following.
\ - hb-x64-kernel-body-append-rows, after that growth: TCSIG-A rebased and
\   the three rows at base + capacity, the copied one included.
\ - hb-x64-kernel-trust-sig stores the pending definition's signature span, an
\   address in the block.
\ - hb-x64-kernel-created-sig stores the pending definition's `does>`
\   signature span, an address in the block.
\ - hb-x64-kernel-def-close ends a tier-0 definition whose state cells are set:
\   COMP-LATCH-CELL, the seven signature and clause cells and PEND clear.
\ - hb-x64-kernel-imm-mark sets DNAME-IMM on the newest record, keeping its
\   other bits, and leaves the record before it alone.
\ - hb-x64-kernel-def-abort closes a failed definition's code window and
\   clears its pending signature and body state.
\ - hb-x64-kernel-def-abort-scope preserves a catch frame's HND, return
\   depth and loop top while clearing compile mode.
\ hb-x64-kernel-definition-negative runs the def-open case expecting the wrong
\ pending record.
: NAMESPACE-RECORD-CASE, ( -- )
   s" core" 0 0 X64HARNESS:RECORD,                    \ record 0
   FIRST-WID WIDN-CELL X64HARNESS:CELL!,
   X64KERNEL:HIDX-BUILD,
   s" Pkg" BOTH-WIDS NAMESPACE,
   1 X64HARNESS:EXPECT-POP,
   FIRST-WID 2 + WIDN-CELL X64HARNESS:EXPECT-CELL,
   FIRST-WID 1 X64KERNEL:REC-CODE X64HARNESS:EXPECT-RECORD,
   FIRST-WID 1+ 1 REC-AUX X64HARNESS:EXPECT-RECORD,
   s" Pkg" STR:LEN 1 X64KERNEL:REC-FLAGS X64HARNESS:EXPECT-RECORD,  \ the length alone
   1 CLOSED,
   s" pKG" WL:NAMESPACE XREF,  1 X64HARNESS:EXPECT-ROW, ;

: NAMESPACE-PRIVATE-CASE, ( -- )
   FIRST-WID WIDN-CELL X64HARNESS:CELL!,
   s" ns" ONE-WID NAMESPACE,
   0 X64HARNESS:EXPECT-POP,
   FIRST-WID 0 X64KERNEL:REC-CODE X64HARNESS:EXPECT-RECORD,
   0 0 REC-AUX X64HARNESS:EXPECT-RECORD,
   0 X64HARNESS:PUSH,  s" namespace-private" X64HARNESS:CALL-ROW,
   FIRST-WID 1+ 0 REC-AUX X64HARNESS:EXPECT-RECORD,
   FIRST-WID 2 + WIDN-CELL X64HARNESS:EXPECT-CELL,
   0 CLOSED, ;

: ALIAS-RECORD-CASE, ( -- )
   SOURCE$ 0 SOURCE-FLAGS X64HARNESS:RECORD,          \ record 0, its code cell 1
   REC-AUX X64HARNESS:POKE,                           \ its length cell -1
   AFTER-SEAL,
   s" Al" 0 ALIAS-WID ALIAS,
   1 1 X64KERNEL:REC-CODE X64HARNESS:EXPECT-RECORD,
   -1 1 REC-AUX X64HARNESS:EXPECT-RECORD,
   s" Al" STR:LEN SOURCE-FLAGS or 1 X64KERNEL:REC-FLAGS X64HARNESS:EXPECT-RECORD,
   ALIAS-WID 1 X64KERNEL:REC-WID X64HARNESS:EXPECT-RECORD,
   1 CLOSED,
   s" aL" ALIAS-WID XREF,  1 X64HARNESS:EXPECT-ROW, ;

: PACKAGE-SCOPE-CASE, ( -- )
   FIRST-WID WIDN-CELL X64HARNESS:CELL!,
   s" pk" BOTH-WIDS NAMESPACE,                        \ its index, 0, stays for the row
   LIVE,                                              \ the row stores with a task live
   PARENT-WID X64HARNESS:PUSH,  s" package-scope!" X64HARNESS:CALL-ROW,
   FIRST-WID PKG-PUB-CELL X64HARNESS:EXPECT-CELL,
   FIRST-WID 1+ PKG-PRI-CELL X64HARNESS:EXPECT-CELL,
   PARENT-WID PKG-PARENT-CELL X64HARNESS:EXPECT-CELL,
   PKG-REC-CELL X64HARNESS:PUSH-DATA-CELL,  0 X64HARNESS:EXPECT-POP-REGION,
   -1 0 SCOPE,
   PUSH-PKG,  0 X64HARNESS:EXPECT-POP,
   0 CLOSED, ;

: DEF-OPEN-CASE, ( -- )
   HELLO$ OTHER-WID 0 X64HARNESS:RECORD,              \ record 0: the name in another wid
   AFTER-SEAL,
   HELLO$ WID DKIND:VAL DEF-OPEN,
   PEND-CELL X64HARNESS:PUSH-DATA-CELL,  DREC X64HARNESS:EXPECT-POP-REGION,
   DREC X64HARNESS:PUSH-REGION-CELL,  DICT-SIZE X64HARNESS:EXPECT-POP-REGION,
   HELLO$ STR:LEN DKIND:VAL or 1 X64KERNEL:REC-FLAGS X64HARNESS:EXPECT-RECORD,
   HELLO$ 0 X64HARNESS:NAME-CELL 1 X64KERNEL:REC-NAME X64HARNESS:EXPECT-RECORD,
   WID 1 X64KERNEL:REC-WID X64HARNESS:EXPECT-RECORD,
   PUSH-NDICT,  1 X64HARNESS:EXPECT-POP,
   1 CLOSED, ;

: DEF-OPEN-STATE-CASE, ( -- )
   REC-AUX X64HARNESS:POKE,                           \ record 0's length cell
   1 TSIG-A-CELL X64HARNESS:CELL!,  2 TSIG-U-CELL X64HARNESS:CELL!,
   3 TCSIG-A-CELL X64HARNESS:CELL!,  4 TCSIG-U-CELL X64HARNESS:CELL!,
   5 DOESB-CELL X64HARNESS:CELL!,  6 TRUSTED-CELL X64HARNESS:CELL!,
   7 CLAUSE-CELL X64HARNESS:CELL!,
   1 NCOMP-DISPATCH:COMP-CELL X64HARNESS:CELL!,
   s" w" 0 DKIND:CAST DEF-OPEN,
   0 0 REC-AUX X64HARNESS:EXPECT-RECORD,
   PUSH-SIGS,  0 X64HARNESS:EXPECT-POP,
   1 NCOMP-DISPATCH:COMP-LATCH-CELL X64HARNESS:EXPECT-CELL,
   X64HARNESS:PUSH-CP,  DICT-SIZE X64HARNESS:EXPECT-POP-REGION,
   s" w" STR:LEN DKIND:CAST or 0 X64KERNEL:REC-FLAGS X64HARNESS:EXPECT-RECORD,
   0 CLOSED, ;

: DEF-OPEN-LONG-CASE, ( -- )
   LONG$ 0 0 DEF-OPEN,
   X64HARNESS:PUSH-CP,  PAST-LONG X64HARNESS:EXPECT-POP-REGION,
   X64KERNEL:REC-CODE X64HARNESS:PUSH-REGION-CELL,  PAST-LONG X64HARNESS:EXPECT-POP-REGION,
   LONG$ STR:LEN DNAME-EXT or 0 X64KERNEL:REC-FLAGS X64HARNESS:EXPECT-RECORD,
   X64KERNEL:REC-NAME X64HARNESS:PUSH-REGION-CELL,  DICT-SIZE X64HARNESS:EXPECT-POP-REGION,
   0 CLOSED, ;

\ The block after the first capture maps it; after a second, which fills the
\ text to FIRST-CAP in place; and after a third, which grows it. Each image
\ checks one stage: an image has FIRST-CASE through CASE-END - 1 for its
\ statuses (test/x86-64-peer-harness.f STATUS), eight past CLOSE's two.
: MAPPED, ( -- )
   HELLO$ APPEND, ;
: FILLED, ( -- )
   MAPPED,
   FIRST-CAP HELLO-LEN - BODYLEN-CELL X64HARNESS:CELL!,
   SAVE-BASE,
   HELLO$ APPEND, ;
: GROWN, ( -- )
   FILLED,
   3 TCSIG-A-CELL TEXT-ADDR!,
   HELLO$ APPEND, ;

: BODY-APPEND-CASE, ( -- )
   MAPPED,
   FIRST-CAP BODY-TEXT:CAP-CELL X64HARNESS:EXPECT-CELL,
   FIRST-CAP 17 * BODY-TEXT:SIZE-CELL X64HARNESS:EXPECT-CELL,
   HELLO-LEN BODYLEN-CELL X64HARNESS:EXPECT-CELL,
   0 PUSH-TEXT-CELL,  HEAD X64HARNESS:EXPECT-POP, ;

: BODY-APPEND-FILL-CASE, ( -- )
   FILLED,
   PUSH-MOVED,  0 X64HARNESS:EXPECT-POP,
   FIRST-CAP BODYLEN-CELL X64HARNESS:EXPECT-CELL,
   FIRST-CAP BODY-TEXT:CAP-CELL X64HARNESS:EXPECT-CELL,
   FIRST-CAP CELL - PUSH-TEXT-CELL,  TAIL X64HARNESS:EXPECT-POP, ;

: BODY-APPEND-GROW-CASE, ( -- )
   GROWN,
   PUSH-MOVED,  1 X64HARNESS:EXPECT-POP,
   FIRST-CAP 2 * BODY-TEXT:CAP-CELL X64HARNESS:EXPECT-CELL,
   FIRST-CAP 2 * 17 * BODY-TEXT:SIZE-CELL X64HARNESS:EXPECT-CELL,
   FIRST-CAP HELLO-LEN + BODYLEN-CELL X64HARNESS:EXPECT-CELL,
   0 PUSH-TEXT-CELL,  HEAD X64HARNESS:EXPECT-POP,
   FIRST-CAP CELL - PUSH-TEXT-CELL,  TAIL X64HARNESS:EXPECT-POP,
   FIRST-CAP PUSH-TEXT-CELL,  HEAD X64HARNESS:EXPECT-POP, ;

: BODY-APPEND-ROWS-CASE, ( -- )
   GROWN,
   TCSIG-A-CELL X64HARNESS:PUSH-DATA-CELL,  3 EXPECT-POP-TEXT,
   3 BODY-TEXT:ROWS-CELL X64HARNESS:EXPECT-CELL,
   2 cells PUSH-ROW-CELL,  FIRST-CAP HELLO-LEN - X64HARNESS:EXPECT-POP,   \ row 1, copied
   4 cells PUSH-ROW-CELL,  FIRST-CAP X64HARNESS:EXPECT-POP, ;       \ row 2

\ LONG$ is a capture longer than SIG-LEN.
: TRUST-SIG-CASE, ( -- )
   HELLO$ 0 0 DEF-OPEN,
   LONG$ APPEND,
   s" trust-sig!" SIG,
   TSIG-A-CELL X64HARNESS:PUSH-DATA-CELL,  0 EXPECT-POP-TEXT,
   SIG-LEN TSIG-U-CELL X64HARNESS:EXPECT-CELL,
   0 CLOSED, ;

: CREATED-SIG-CASE, ( -- )
   HELLO$ 0 0 DEF-OPEN,
   LONG$ APPEND,
   s" created-sig!" SIG,
   TCSIG-A-CELL X64HARNESS:PUSH-DATA-CELL,  0 EXPECT-POP-TEXT,
   SIG-LEN TCSIG-U-CELL X64HARNESS:EXPECT-CELL,
   0 CLOSED, ;

: DEF-CLOSE-CASE, ( -- )
   TIER-0-OPEN,
   s" def-close" X64HARNESS:CALL-ROW,
   PUSH-SIGS,  0 X64HARNESS:EXPECT-POP,
   0 NCOMP-DISPATCH:COMP-LATCH-CELL X64HARNESS:EXPECT-CELL,
   0 PEND-CELL X64HARNESS:EXPECT-CELL, ;

\ Record 1 is the newest, a cast whose kind the mark keeps.
: IMM-MARK-CASE, ( -- )
   HELLO$ WID 0 X64HARNESS:RECORD,                    \ record 0
   s" w" WID DKIND:CAST X64HARNESS:RECORD,           \ record 1
   s" imm-mark" X64HARNESS:CALL-ROW,
   s" w" STR:LEN DKIND:CAST or DNAME-IMM or 1 X64KERNEL:REC-FLAGS X64HARNESS:EXPECT-RECORD,
   HELLO$ STR:LEN 0 X64KERNEL:REC-FLAGS X64HARNESS:EXPECT-RECORD,
   1 CLOSED, ;

\ A failed definition clears its compile state while leaving
\ the enclosing catch's handler, return depth and loop top intact.
: DEF-ABORT-CASE, ( -- )
   TIER-0-OPEN,
   14 LOCN-CELL X64HARNESS:CELL!,
   15 BODYLEN-CELL X64HARNESS:CELL!,
   s" def-abort" X64HARNESS:CALL-ROW,
   PUSH-SIGS,  0 X64HARNESS:EXPECT-POP,
   0 CLOSED,
   0 NCOMP-DISPATCH:COMP-LATCH-CELL X64HARNESS:EXPECT-CELL,
   0 PEND-CELL X64HARNESS:EXPECT-CELL,
   0 LOCN-CELL X64HARNESS:EXPECT-CELL,
   0 BODYLEN-CELL X64HARNESS:EXPECT-CELL, ;

: DEF-ABORT-SCOPE-CASE, ( -- )
   TIER-0-OPEN,
   11 HND-CELL X64HARNESS:CELL!,
   12 RSP-CELL X64HARNESS:CELL!,
   13 LOOPSP-CELL X64HARNESS:CELL!,
   16 CMM-CELL X64HARNESS:CELL!,
   s" def-abort" X64HARNESS:CALL-ROW,
   0 CMM-CELL X64HARNESS:EXPECT-CELL,
   11 HND-CELL X64HARNESS:EXPECT-CELL,
   12 RSP-CELL X64HARNESS:EXPECT-CELL,
   13 LOOPSP-CELL X64HARNESS:EXPECT-CELL, ;

\ ---- the rejections -----------------------------------------------------------
\ The armed images exit a rejection before any store, with nothing on fd 2
\ except the live images' E-TASK-COMPILE record. Each call but for its rejection
\ is one the row admits.
: NAMESPACE-LIVE, ( -- ) LIVE,  s" ns" BOTH-WIDS NAMESPACE, ;

: PRIVATE-LIVE, ( -- )
   s" ns" WL:NAMESPACE 0 X64HARNESS:RECORD,      \ a namespace row, [8] 0
   LIVE,
   0 X64HARNESS:PUSH,  s" namespace-private" X64HARNESS:CALL-ROW, ;

: ALIAS-LIVE, ( -- )
   SOURCE$ 0 0 X64HARNESS:RECORD,
   LIVE,
   s" al" 0 WID ALIAS, ;

: DEF-OPEN-LIVE, ( -- ) LIVE,  s" w" 0 0 DEF-OPEN, ;

: CASE-PAIR, ( -- )
   HELLO$ WID 0 X64HARNESS:RECORD,
   s" HELLO" WID 0 DEF-OPEN, ;

: PRIVATE-SET, ( -- )
   s" ns" BOTH-WIDS NAMESPACE,                        \ its index stays for the row
   s" namespace-private" X64HARNESS:CALL-ROW, ;

: ALIAS-INT, ( -- )
   SOURCE$ 0 DNAME-INT X64HARNESS:RECORD,
   s" al" 0 WID ALIAS, ;

\ The long name's two slots end exactly at the ceiling.
: CEILING, ( -- )
   CODE-CEILING PAST-LONG DICT-SIZE - - CP-AT,
   LONG$ 0 0 DEF-OPEN, ;

\ BODYLEN past the capacity, which no growth answers.
: FULL, ( -- )
   HELLO$ APPEND,                                     \ maps FIRST-CAP
   FIRST-CAP 1+ BODYLEN-CELL X64HARNESS:CELL!,
   HELLO$ APPEND, ;

: ALIAS-PROT, ( -- )
   SOURCE$ 0 0 X64HARNESS:RECORD,
   AFTER-SEAL,
   s" al" 0 OWNER-API-PRI-WID ALIAS, ;

\ A pending definition def-open opened with no compiler.
: NO-COMP-CLOSE, ( -- )
   0 NCOMP-DISPATCH:COMP-CELL X64HARNESS:CELL!,
   HELLO$ 0 0 DEF-OPEN,
   s" def-close" X64HARNESS:CALL-ROW, ;

: DEF-OPEN-PROT, ( -- )
   AFTER-SEAL,
   s" w" OWNER-API-PUB-WID 0 DEF-OPEN, ;

: CLOSE ( str -- )
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   X64HARNESS:BOOT-CLOSE, ;

: IMAGE ( [ -- ] bool str -- ) {: negative:bool path:str :}
   negative X64HARNESS:BOOT-OPEN,
   execute
   path CLOSE ;

: ARMED ( [ -- ] n str -- ) {: status:n path:str :}
   false X64HARNESS:BOOT-OPEN,
   status X64HARNESS:EXPECT-EXIT
   execute
   path CLOSE ;

: WRITERS ( -- )
   [: NAMESPACE-RECORD-CASE, ;] false s" hb-x64-kernel-namespace-record" TMP-PATH IMAGE
   [: NAMESPACE-PRIVATE-CASE, ;] false s" hb-x64-kernel-namespace-private" TMP-PATH IMAGE
   [: ALIAS-RECORD-CASE, ;] false s" hb-x64-kernel-alias-record" TMP-PATH IMAGE
   [: PACKAGE-SCOPE-CASE, ;] false s" hb-x64-kernel-package-scope" TMP-PATH IMAGE
   [: DEF-OPEN-CASE, ;] false s" hb-x64-kernel-def-open" TMP-PATH IMAGE
   [: DEF-OPEN-STATE-CASE, ;] false s" hb-x64-kernel-def-open-state" TMP-PATH IMAGE
   [: DEF-OPEN-LONG-CASE, ;] false s" hb-x64-kernel-def-open-long" TMP-PATH IMAGE
   [: BODY-APPEND-CASE, ;] false s" hb-x64-kernel-body-append" TMP-PATH IMAGE
   [: BODY-APPEND-FILL-CASE, ;] false s" hb-x64-kernel-body-append-fill" TMP-PATH IMAGE
   [: BODY-APPEND-GROW-CASE, ;] false s" hb-x64-kernel-body-append-grow" TMP-PATH IMAGE
   [: BODY-APPEND-ROWS-CASE, ;] false s" hb-x64-kernel-body-append-rows" TMP-PATH IMAGE
   [: TRUST-SIG-CASE, ;] false s" hb-x64-kernel-trust-sig" TMP-PATH IMAGE
   [: CREATED-SIG-CASE, ;] false s" hb-x64-kernel-created-sig" TMP-PATH IMAGE
   [: DEF-CLOSE-CASE, ;] false s" hb-x64-kernel-def-close" TMP-PATH IMAGE
   [: IMM-MARK-CASE, ;] false s" hb-x64-kernel-imm-mark" TMP-PATH IMAGE
   [: DEF-ABORT-CASE, ;] false s" hb-x64-kernel-def-abort" TMP-PATH IMAGE
   [: DEF-ABORT-SCOPE-CASE, ;] false s" hb-x64-kernel-def-abort-scope" TMP-PATH IMAGE
   [: DEF-OPEN-CASE, ;] true s" hb-x64-kernel-definition-negative" TMP-PATH IMAGE ;

: REJECTIONS ( -- )
   [: NAMESPACE-LIVE, ;] EXIT-STATUS:TASK-COMPILE
   s" hb-x64-kernel-namespace-record-live-armed" TMP-PATH ARMED
   [: PRIVATE-LIVE, ;] EXIT-STATUS:TASK-COMPILE
   s" hb-x64-kernel-namespace-private-live-armed" TMP-PATH ARMED
   [: ALIAS-LIVE, ;] EXIT-STATUS:TASK-COMPILE
   s" hb-x64-kernel-alias-record-live-armed" TMP-PATH ARMED
   [: DEF-OPEN-LIVE, ;] EXIT-STATUS:TASK-COMPILE
   s" hb-x64-kernel-def-open-live-armed" TMP-PATH ARMED
   [: CASE-PAIR, ;] EXIT-STATUS:SEAL-VIOLATION
   s" hb-x64-kernel-def-open-case-armed" TMP-PATH ARMED
   [: PRIVATE-SET, ;] EXIT-STATUS:SEAL-VIOLATION
   s" hb-x64-kernel-namespace-private-set-armed" TMP-PATH ARMED
   [: ALIAS-INT, ;] EXIT-STATUS:SEAL-VIOLATION
   s" hb-x64-kernel-alias-record-int-armed" TMP-PATH ARMED
   [: -1 5 SCOPE, ;] EXIT-STATUS:SEAL-VIOLATION
   s" hb-x64-kernel-package-scope-clear-armed" TMP-PATH ARMED
   [: CEILING, ;] EXIT-STATUS:SEAL-VIOLATION
   s" hb-x64-kernel-def-open-ceiling-armed" TMP-PATH ARMED
   [: FULL, ;] EXIT-STATUS:SEAL-VIOLATION
   s" hb-x64-kernel-body-append-full-armed" TMP-PATH ARMED
   [: s" trust-sig!" SIG, ;] EXIT-STATUS:SEAL-VIOLATION
   s" hb-x64-kernel-trust-sig-armed" TMP-PATH ARMED
   [: s" created-sig!" SIG, ;] EXIT-STATUS:SEAL-VIOLATION
   s" hb-x64-kernel-created-sig-armed" TMP-PATH ARMED
   [: s" def-close" X64HARNESS:CALL-ROW, ;] EXIT-STATUS:SEAL-VIOLATION
   s" hb-x64-kernel-def-close-armed" TMP-PATH ARMED
   [: NO-COMP-CLOSE, ;] EXIT-STATUS:SEAL-VIOLATION
   s" hb-x64-kernel-def-close-tier-armed" TMP-PATH ARMED
   [: ALIAS-PROT, ;] EXIT-STATUS:SEAL-WORDLIST
   s" hb-x64-kernel-alias-record-prot-armed" TMP-PATH ARMED
   [: DEF-OPEN-PROT, ;] EXIT-STATUS:SEAL-WORDLIST
   s" hb-x64-kernel-def-open-prot-armed" TMP-PATH ARMED ;

public

: RUN ( -- )
   T-RESET
   X64HARNESS:INIT
   WRITERS
   REJECTIONS
   X64HARNESS:DISPOSE
   T-REPORT ;

;using
;using
;using
;package

X64K-DEFINITION:RUN
