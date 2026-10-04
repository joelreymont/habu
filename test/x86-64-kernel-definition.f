\ x86-64-kernel-definition.f - the definition writers of the x86-64 kernel
\ (src/habu/kernel-x64.f DEFINITION,) in the booted harness, cross-built for an
\ x86-64 peer. Each image seeds its records and cells, puts the code region at
\ rest and then calls the rows, so a row that writes the region outside its
\ window faults into the crash handler's dump (134). After a row writes a
\ record, one check finds every band cell clear and the record's page
\ read-execute. Every image ends by checking the data stack is empty and the
\ machine stack balanced.
\
\ Each of these exits 0:
\ - hb-x64-kernel-namespace-record builds the index, publishes a namespace row
\   with a private wid past a seeded record and checks its index, its two fresh
\   wids, its flags cell and WIDN-CELL; xref-search-wl then finds it under a
\   name that differs in case.
\ - hb-x64-kernel-namespace-private publishes a namespace row whose [8] is 0 and
\   then gives it a private wid.
\ - hb-x64-kernel-alias-record, after the seal and with the wid's neighbour in
\   its bitmap cell protected, aliases a long-named source: the alias carries
\   the source's code and length cells and exactly its IMM, WIDE and MIN-IN
\   bits, neither its VAL nor its EXT bit, and xref-search-wl finds it.
\ - hb-x64-kernel-package-scope, with a task live past the namespace row, sets
\   the four package cells from that row, then clears them with `-1 0`.
\ - hb-x64-kernel-def-open, after the seal, opens a definition whose name
\   another wid holds: the pending record, its cells, and NDICT unchanged.
\ - hb-x64-kernel-def-open-state checks the state def-open resets: its record's
\   length cell 0, TSIG, TCSIG, DOESB and TRUSTED clear, DEF-TIER-CELL taking
\   TIER-CELL and the provenance window open at CP, which an inline name leaves.
\ - hb-x64-kernel-def-open-long stores a 17-byte name at CP: CP, the entry and
\   the open window move 32 bytes, the pad over poisoned cells is 0 and
\   code-origin answers 1 for the name's slots.
\ - hb-x64-kernel-body-append fills BODYBUF exactly to BODYBUF-CAP.
\ - hb-x64-kernel-body-append-pass2 appends in pass 2, which stores nothing.
\ - hb-x64-kernel-trust-sig stores the pending definition's signature span.
\ - hb-x64-kernel-created-sig stores the pending definition's `does>`
\   signature span.
\ - hb-x64-kernel-def-close ends a tier-1 definition whose state cells are set
\   and whose CP moved: the provenance window closed with code-origin 1 over
\   the span, and DEF-TIER, the six signature and clause cells and PEND clear.
\ - hb-x64-kernel-def-abort closes a failed definition's window with unknown
\   provenance and clears its pending signature and body state.
\ - hb-x64-kernel-def-abort-scope preserves a catch frame's HND, return and
\   loop depths while clearing compile mode.
\ hb-x64-kernel-definition-negative runs the def-open case expecting the wrong
\ pending record and exits 21.
\
\ The armed images exit a refusal before any store, with nothing on fd 2:
\ - 79, a task live, one image for each row that guards:
\   -namespace-record-live-armed, -namespace-private-live-armed,
\   -alias-record-live-armed, -def-open-live-armed.
\ - 83, ENGINE-ERROR:SEAL-VIOLATION: def-open of a live name that differs only
\   in case (-def-open-case-armed), namespace-private on a row whose [8] is set
\   (-namespace-private-set-armed), an alias of a DNAME-INT source
\   (-alias-record-int-armed), package-scope! `-1 5`
\   (-package-scope-clear-armed), a 17-byte def-open with CP 32 bytes below the
\   code ceiling (-def-open-ceiling-armed), body-append one byte past
\   BODYBUF-CAP (-body-append-full-armed), and trust-sig!, created-sig! and
\   def-close with nothing pending (-trust-sig-armed, -created-sig-armed,
\   -def-close-armed), and def-close on a tier 0 definition
\   (-def-close-tier-armed).
\ - 84, ENGINE-ERROR:SEAL-PACKAGE, after the seal: alias-record into a wid
\   whose bit is set (-alias-record-prot-armed) and def-open into
\   OWNER-API-PUB-WID, always protected (-def-open-prot-armed).
\ The host writes each image; running them is the peer's.
require test/x86-64-boot-harness.f

package X64K-DEFINITION
using X64ASM
using X64CODE
using X64RT

5 constant WID                         \ a public wordlist past the fixed ones
6 constant OTHER-WID
8 constant GUARDED-WID                 \ protected: ALIAS-WID's bitmap cell
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
DNAME-IMM DNAME-WIDE or DNAME-MIN-IN-MASK or constant COPIED
COPIED DKIND:VAL or constant SOURCE-FLAGS

\ A 17-byte name, one past DNAME-INL, stored at CP: its last byte opens the
\ second code slot and CP moves past that one.
: LONG$ ( -- ptr u8 n ) s" abcdefghijklmnopq" ;
DICT-SIZE X64KERNEL:CODE-SLOT + constant SECOND-SLOT
SECOND-SLOT X64KERNEL:CODE-SLOT + constant PAST-LONG

\ A source name past DNAME-INL, so its record carries DNAME-EXT.
: SOURCE$ ( -- ptr u8 n ) s" a-source-past-inline" ;

: HELLO$ ( -- ptr u8 n ) s" hello" ;
BODYBUF-OFF BODYBUF-CAP + constant BUF-END

\ BODYBUF's last cell once "hello" and its space fill the buffer: two poisoned
\ bytes, then the six.
: TAIL ( -- n ) s" hello " 0 X64HARNESS:NAME-CELL 16 lshift $FFFF or ;

: DATA-REG ( -- r64 ) ENGINE-GPR:X64-RBASE >R64 ;
: DBASE-REG ( -- r64 ) ENGINE-GPR:X64-DBASE >R64 ;

\ ---- the staging and the checks ----------------------------------------------
: NAMESPACE, ( ptr u8 n n -- ) {: a:ptr u:n flag:n :}
   a u X64HARNESS:PUSH-TEXT,  flag X64HARNESS:PUSH,
   s" namespace-record" X64HARNESS:CALL-ROW, ;

: ALIAS, ( ptr u8 n n n -- ) {: a:ptr u:n src:n wid:n :}
   a u X64HARNESS:PUSH-TEXT,  src X64HARNESS:PUSH,  wid X64HARNESS:PUSH,
   s" alias-record" X64HARNESS:CALL-ROW, ;

: DEF-OPEN, ( ptr u8 n n n -- ) {: a:ptr u:n wid:n kind:n :}
   a u X64HARNESS:PUSH-TEXT,  wid X64HARNESS:PUSH,  kind X64HARNESS:PUSH,
   s" def-open" X64HARNESS:CALL-ROW, ;

: SCOPE, ( n n -- ) {: row:n parent:n :}
   row X64HARNESS:PUSH,  parent X64HARNESS:PUSH,
   s" package-scope!" X64HARNESS:CALL-ROW, ;

: APPEND, ( ptr u8 n -- ) X64HARNESS:PUSH-TEXT,  s" body-append" X64HARNESS:CALL-ROW, ;

\ The signature row named over SIG-LEN bytes of BODYBUF, a span whose address
\ DATA fixes.
: SIG, ( ptr u8 n -- ) {: row:ptr rowu:n :}
   BODYBUF-OFF X64HARNESS:PUSH-DATA,  SIG-LEN X64HARNESS:PUSH,
   row rowu X64HARNESS:CALL-ROW, ;

: XREF, ( ptr u8 n n -- ) {: a:ptr u:n wid:n :}
   a u X64HARNESS:PUSH-TEXT,  wid X64HARNESS:PUSH,
   s" xref-search-wl" X64HARNESS:CALL-ROW, ;

\ code-origin over the region offsets [lo, hi).
: ORIGIN, ( n n -- ) {: lo:n hi:n :}
   lo X64HARNESS:PUSH-REGION,  hi X64HARNESS:PUSH-REGION,
   s" code-origin" X64HARNESS:CALL-ROW, ;

: PROTECT, ( n -- ) X64HARNESS:PUSH,  s" prot-wid-add" X64HARNESS:CALL-ROW, ;
: AFTER-SEAL, ( -- ) 1 SEAL-NDICT-CELL X64HARNESS:CELL!, ;
: LIVE, ( -- ) 1 TASKS-LIVE-CELL X64HARNESS:CELL!, ;

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

\ The six signature and clause cells def-open clears, or'd together.
: PUSH-SIGS, ( -- )
   RAX ZERO-REG,
   TSIG-A-CELL OR-CELL,  TSIG-U-CELL OR-CELL,
   TCSIG-A-CELL OR-CELL,  TCSIG-U-CELL OR-CELL,
   DOESB-CELL OR-CELL,  TRUSTED-CELL OR-CELL,
   0 G-PUSH ;

\ The long name's second slot less its last byte: 0 when that byte opens the
\ slot and the fifteen bytes past it are zero.
\ The tier-1 definition def-close ends: its six signature and clause cells
\ set and CP 32 bytes past the open window.
: TIER-1-OPEN, ( -- )
   1 NCOMP-DISPATCH:TIER-CELL X64HARNESS:CELL!,
   X64HARNESS:REST,
   HELLO$ 0 0 DEF-OPEN,
   1 TSIG-A-CELL X64HARNESS:CELL!,  2 TSIG-U-CELL X64HARNESS:CELL!,
   3 TCSIG-A-CELL X64HARNESS:CELL!,  4 TCSIG-U-CELL X64HARNESS:CELL!,
   5 DOESB-CELL X64HARNESS:CELL!,  6 TRUSTED-CELL X64HARNESS:CELL!,
   DICT-SIZE 32 + CP-AT, ;

: PUSH-PAD, ( -- )
   RAX DBASE-REG SECOND-SLOT MEM-OFF ASM-SINK ENC-MOV-RM
   RCX LONG$ DNAME-INL X64HARNESS:NAME-CELL >IMM32 ASM-SINK ENC-MOV-RI32
   RAX RCX ASM-SINK ENC-XOR-RR
   RAX DBASE-REG SECOND-SLOT CELL + MEM-OFF ASM-SINK ENC-OR-RM
   0 G-PUSH ;

\ One check that a row closed its window: every band cell clear and record
\ k's page read-execute, so a probe of it faults. The band cells or'd with the
\ probe's answer less FAULTED are 0 when both hold.
: CLOSED, ( n -- ) {: k:n :}
   X64HARNESS:PUSH-BANDS,
   k DREC * X64KERNEL:REC-FLAGS + X64HARNESS:PROBE,
   0 G-POP  1 G-POP
   RAX X64HARNESS:FAULTED negate >IMM8 ASM-SINK ENC-ADD-RI8
   RAX RCX ASM-SINK ENC-OR-RR
   0 G-PUSH
   0 X64HARNESS:EXPECT-POP, ;

\ ---- the rows that exit 0 -----------------------------------------------------
: NAMESPACE-RECORD-CASE, ( -- )
   s" core" 0 0 X64HARNESS:RECORD,                    \ record 0
   FIRST-WID WIDN-CELL X64HARNESS:CELL!,
   X64KERNEL:HIDX-BUILD,
   X64HARNESS:REST,
   s" Pkg" BOTH-WIDS NAMESPACE,
   1 X64HARNESS:EXPECT-POP,
   FIRST-WID 2 + WIDN-CELL X64HARNESS:EXPECT-CELL,
   FIRST-WID 1 X64KERNEL:REC-CODE X64HARNESS:EXPECT-RECORD,
   FIRST-WID 1+ 1 REC-AUX X64HARNESS:EXPECT-RECORD,
   s" Pkg" nip 1 X64KERNEL:REC-FLAGS X64HARNESS:EXPECT-RECORD,  \ the length alone
   1 CLOSED,
   s" pKG" DICT-WL:NAMESPACE XREF,  1 X64HARNESS:EXPECT-ROW, ;

: NAMESPACE-PRIVATE-CASE, ( -- )
   FIRST-WID WIDN-CELL X64HARNESS:CELL!,
   X64HARNESS:REST,
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
   GUARDED-WID PROTECT,
   X64HARNESS:REST,
   s" Al" 0 ALIAS-WID ALIAS,
   1 1 X64KERNEL:REC-CODE X64HARNESS:EXPECT-RECORD,
   -1 1 REC-AUX X64HARNESS:EXPECT-RECORD,
   s" Al" nip COPIED or 1 X64KERNEL:REC-FLAGS X64HARNESS:EXPECT-RECORD,
   ALIAS-WID 1 X64KERNEL:REC-WID X64HARNESS:EXPECT-RECORD,
   1 CLOSED,
   s" aL" ALIAS-WID XREF,  1 X64HARNESS:EXPECT-ROW, ;

: PACKAGE-SCOPE-CASE, ( -- )
   FIRST-WID WIDN-CELL X64HARNESS:CELL!,
   X64HARNESS:REST,
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
   X64HARNESS:REST,
   HELLO$ WID DKIND:VAL DEF-OPEN,
   PEND-CELL X64HARNESS:PUSH-DATA-CELL,  DREC X64HARNESS:EXPECT-POP-REGION,
   DREC X64HARNESS:PUSH-REGION-CELL,  DICT-SIZE X64HARNESS:EXPECT-POP-REGION,
   HELLO$ nip DKIND:VAL or 1 X64KERNEL:REC-FLAGS X64HARNESS:EXPECT-RECORD,
   HELLO$ 0 X64HARNESS:NAME-CELL 1 X64KERNEL:REC-NAME X64HARNESS:EXPECT-RECORD,
   WID 1 X64KERNEL:REC-WID X64HARNESS:EXPECT-RECORD,
   PUSH-NDICT,  1 X64HARNESS:EXPECT-POP,
   1 CLOSED, ;

: DEF-OPEN-STATE-CASE, ( -- )
   REC-AUX X64HARNESS:POKE,                           \ record 0's length cell
   1 TSIG-A-CELL X64HARNESS:CELL!,  2 TSIG-U-CELL X64HARNESS:CELL!,
   3 TCSIG-A-CELL X64HARNESS:CELL!,  4 TCSIG-U-CELL X64HARNESS:CELL!,
   5 DOESB-CELL X64HARNESS:CELL!,  6 TRUSTED-CELL X64HARNESS:CELL!,
   1 NCOMP-DISPATCH:TIER-CELL X64HARNESS:CELL!,
   X64HARNESS:REST,
   s" w" 0 DKIND:CAST DEF-OPEN,
   0 0 REC-AUX X64HARNESS:EXPECT-RECORD,
   PUSH-SIGS,  0 X64HARNESS:EXPECT-POP,
   1 NCOMP-DISPATCH:DEF-TIER-CELL X64HARNESS:EXPECT-CELL,
   TIER-PROV:OPEN-CELL X64HARNESS:PUSH-DATA-CELL,  DICT-SIZE X64HARNESS:EXPECT-POP-REGION,
   X64HARNESS:PUSH-CP,  DICT-SIZE X64HARNESS:EXPECT-POP-REGION,
   s" w" nip DKIND:CAST or 0 X64KERNEL:REC-FLAGS X64HARNESS:EXPECT-RECORD,
   0 CLOSED, ;

: DEF-OPEN-LONG-CASE, ( -- )
   SECOND-SLOT X64HARNESS:POKE,  SECOND-SLOT CELL + X64HARNESS:POKE,
   X64HARNESS:REST,
   LONG$ 0 0 DEF-OPEN,
   X64HARNESS:PUSH-CP,  PAST-LONG X64HARNESS:EXPECT-POP-REGION,
   X64KERNEL:REC-CODE X64HARNESS:PUSH-REGION-CELL,  PAST-LONG X64HARNESS:EXPECT-POP-REGION,
   TIER-PROV:OPEN-CELL X64HARNESS:PUSH-DATA-CELL,  PAST-LONG X64HARNESS:EXPECT-POP-REGION,
   LONG$ nip DNAME-EXT or 0 X64KERNEL:REC-FLAGS X64HARNESS:EXPECT-RECORD,
   X64KERNEL:REC-NAME X64HARNESS:PUSH-REGION-CELL,  DICT-SIZE X64HARNESS:EXPECT-POP-REGION,
   PUSH-PAD,  0 X64HARNESS:EXPECT-POP,
   DICT-SIZE PAST-LONG ORIGIN,  1 X64HARNESS:EXPECT-POP,
   0 CLOSED, ;

: BODY-APPEND-CASE, ( -- )
   -1 BUF-END CELL 2 * - X64HARNESS:CELL!,  -1 BUF-END CELL - X64HARNESS:CELL!,
   BODYBUF-CAP HELLO$ nip 1+ - BODYLEN-CELL X64HARNESS:CELL!,
   X64HARNESS:REST,
   HELLO$ APPEND,
   BODYBUF-CAP BODYLEN-CELL X64HARNESS:EXPECT-CELL,
   TAIL BUF-END CELL - X64HARNESS:EXPECT-CELL,
   -1 BUF-END CELL 2 * - X64HARNESS:EXPECT-CELL, ;

: BODY-APPEND-PASS2-CASE, ( -- )
   -1 BODYBUF-OFF CELL + X64HARNESS:CELL!,
   CELL BODYLEN-CELL X64HARNESS:CELL!,
   1 P2-CELL X64HARNESS:CELL!,
   X64HARNESS:REST,
   HELLO$ APPEND,
   CELL BODYLEN-CELL X64HARNESS:EXPECT-CELL,
   -1 BODYBUF-OFF CELL + X64HARNESS:EXPECT-CELL, ;

: TRUST-SIG-CASE, ( -- )
   X64HARNESS:REST,
   HELLO$ 0 0 DEF-OPEN,
   s" trust-sig!" SIG,
   TSIG-A-CELL X64HARNESS:PUSH-DATA-CELL,  BODYBUF-OFF X64HARNESS:EXPECT-POP-DATA,
   SIG-LEN TSIG-U-CELL X64HARNESS:EXPECT-CELL,
   0 CLOSED, ;

: CREATED-SIG-CASE, ( -- )
   X64HARNESS:REST,
   HELLO$ 0 0 DEF-OPEN,
   s" created-sig!" SIG,
   TCSIG-A-CELL X64HARNESS:PUSH-DATA-CELL,  BODYBUF-OFF X64HARNESS:EXPECT-POP-DATA,
   SIG-LEN TCSIG-U-CELL X64HARNESS:EXPECT-CELL,
   0 CLOSED, ;

: DEF-CLOSE-CASE, ( -- )
   TIER-1-OPEN,
   s" def-close" X64HARNESS:CALL-ROW,
   DICT-SIZE DICT-SIZE 32 + ORIGIN,  1 X64HARNESS:EXPECT-POP,
   PUSH-SIGS,  0 X64HARNESS:EXPECT-POP,
   0 TIER-PROV:OPEN-CELL X64HARNESS:EXPECT-CELL,
   0 NCOMP-DISPATCH:DEF-TIER-CELL X64HARNESS:EXPECT-CELL,
   0 PEND-CELL X64HARNESS:EXPECT-CELL, ;

\ A failed definition closes its code window and compile state while leaving
\ the enclosing catch's handler, return depth and loop depth intact.
: DEF-ABORT-CASE, ( -- )
   TIER-1-OPEN,
   14 LOCN-CELL X64HARNESS:CELL!,
   15 BODYLEN-CELL X64HARNESS:CELL!,
   RDI ENGINE-GPR:X64-CP >R64 X64KERNEL:CODE-SLOT MEM-OFF ASM-SINK ENC-LEA
   X64KERNEL:WINDOW-OPEN,
   s" def-abort" X64HARNESS:CALL-ROW,
   DICT-SIZE DICT-SIZE 32 + ORIGIN,  -1 X64HARNESS:EXPECT-POP,
   PUSH-SIGS,  0 X64HARNESS:EXPECT-POP,
   0 CLOSED,
   0 TIER-PROV:OPEN-CELL X64HARNESS:EXPECT-CELL,
   0 NCOMP-DISPATCH:DEF-TIER-CELL X64HARNESS:EXPECT-CELL,
   0 PEND-CELL X64HARNESS:EXPECT-CELL,
   0 LOCN-CELL X64HARNESS:EXPECT-CELL,
   0 BODYLEN-CELL X64HARNESS:EXPECT-CELL, ;

: DEF-ABORT-SCOPE-CASE, ( -- )
   TIER-1-OPEN,
   11 HND-CELL X64HARNESS:CELL!,
   12 RSP-CELL X64HARNESS:CELL!,
   13 LOOPSP-CELL X64HARNESS:CELL!,
   16 CMM-CELL X64HARNESS:CELL!,
   s" def-abort" X64HARNESS:CALL-ROW,
   0 CMM-CELL X64HARNESS:EXPECT-CELL,
   11 HND-CELL X64HARNESS:EXPECT-CELL,
   12 RSP-CELL X64HARNESS:EXPECT-CELL,
   13 LOOPSP-CELL X64HARNESS:EXPECT-CELL, ;

\ ---- the refusals -------------------------------------------------------------
\ Each call but for its refusal is one the row admits.
: NAMESPACE-LIVE, ( -- ) LIVE,  X64HARNESS:REST,  s" ns" BOTH-WIDS NAMESPACE, ;

: PRIVATE-LIVE, ( -- )
   s" ns" DICT-WL:NAMESPACE 0 X64HARNESS:RECORD,      \ a namespace row, [8] 0
   LIVE,  X64HARNESS:REST,
   0 X64HARNESS:PUSH,  s" namespace-private" X64HARNESS:CALL-ROW, ;

: ALIAS-LIVE, ( -- )
   SOURCE$ 0 0 X64HARNESS:RECORD,
   LIVE,  X64HARNESS:REST,
   s" al" 0 WID ALIAS, ;

: DEF-OPEN-LIVE, ( -- ) LIVE,  X64HARNESS:REST,  s" w" 0 0 DEF-OPEN, ;

: CASE-PAIR, ( -- )
   HELLO$ WID 0 X64HARNESS:RECORD,
   X64HARNESS:REST,
   s" HELLO" WID 0 DEF-OPEN, ;

: PRIVATE-SET, ( -- )
   X64HARNESS:REST,
   s" ns" BOTH-WIDS NAMESPACE,                        \ its index stays for the row
   s" namespace-private" X64HARNESS:CALL-ROW, ;

: ALIAS-INT, ( -- )
   SOURCE$ 0 DNAME-INT X64HARNESS:RECORD,
   X64HARNESS:REST,
   s" al" 0 WID ALIAS, ;

\ The long name's two slots end exactly at the ceiling.
: CEILING, ( -- )
   X64KERNEL:CODE-CEILING PAST-LONG DICT-SIZE - - CP-AT,
   X64HARNESS:REST,
   LONG$ 0 0 DEF-OPEN, ;

\ "hello!" and its space need one byte more than the room left.
: FULL, ( -- )
   BODYBUF-CAP HELLO$ nip 1+ - BODYLEN-CELL X64HARNESS:CELL!,
   X64HARNESS:REST,
   s" hello!" APPEND, ;

: ALIAS-PROT, ( -- )
   SOURCE$ 0 0 X64HARNESS:RECORD,
   AFTER-SEAL,  ALIAS-WID PROTECT,
   X64HARNESS:REST,
   s" al" 0 ALIAS-WID ALIAS, ;

\ A pending definition def-open opened at tier 0.
: TIER-0-CLOSE, ( -- )
   0 NCOMP-DISPATCH:TIER-CELL X64HARNESS:CELL!,
   X64HARNESS:REST,
   HELLO$ 0 0 DEF-OPEN,
   s" def-close" X64HARNESS:CALL-ROW, ;

: DEF-OPEN-PROT, ( -- )
   AFTER-SEAL,
   X64HARNESS:REST,
   s" w" OWNER-API-PUB-WID 0 DEF-OPEN, ;

\ One image: the boot, the case the quotation emits, the depth and balance
\ checks and the exit.
: IMAGE ( [ -- ] bool ptr u8 n -- ) {: negative:bool path:ptr pathu:n :}
   negative X64HARNESS:BOOT-OPEN,
   execute
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   path pathu X64HARNESS:BOOT-CLOSE, ;

: WRITERS ( -- )
   [: NAMESPACE-RECORD-CASE, ;] false s" hb-x64-kernel-namespace-record" TMP-PATH IMAGE
   [: NAMESPACE-PRIVATE-CASE, ;] false s" hb-x64-kernel-namespace-private" TMP-PATH IMAGE
   [: ALIAS-RECORD-CASE, ;] false s" hb-x64-kernel-alias-record" TMP-PATH IMAGE
   [: PACKAGE-SCOPE-CASE, ;] false s" hb-x64-kernel-package-scope" TMP-PATH IMAGE
   [: DEF-OPEN-CASE, ;] false s" hb-x64-kernel-def-open" TMP-PATH IMAGE
   [: DEF-OPEN-STATE-CASE, ;] false s" hb-x64-kernel-def-open-state" TMP-PATH IMAGE
   [: DEF-OPEN-LONG-CASE, ;] false s" hb-x64-kernel-def-open-long" TMP-PATH IMAGE
   [: BODY-APPEND-CASE, ;] false s" hb-x64-kernel-body-append" TMP-PATH IMAGE
   [: BODY-APPEND-PASS2-CASE, ;] false s" hb-x64-kernel-body-append-pass2" TMP-PATH IMAGE
   [: TRUST-SIG-CASE, ;] false s" hb-x64-kernel-trust-sig" TMP-PATH IMAGE
   [: CREATED-SIG-CASE, ;] false s" hb-x64-kernel-created-sig" TMP-PATH IMAGE
   [: DEF-CLOSE-CASE, ;] false s" hb-x64-kernel-def-close" TMP-PATH IMAGE
   [: DEF-ABORT-CASE, ;] false s" hb-x64-kernel-def-abort" TMP-PATH IMAGE
   [: DEF-ABORT-SCOPE-CASE, ;] false s" hb-x64-kernel-def-abort-scope" TMP-PATH IMAGE
   [: DEF-OPEN-CASE, ;] true s" hb-x64-kernel-definition-negative" TMP-PATH IMAGE ;

: REFUSALS ( -- )
   [: NAMESPACE-LIVE, ;] false s" hb-x64-kernel-namespace-record-live-armed" TMP-PATH IMAGE
   [: PRIVATE-LIVE, ;] false s" hb-x64-kernel-namespace-private-live-armed" TMP-PATH IMAGE
   [: ALIAS-LIVE, ;] false s" hb-x64-kernel-alias-record-live-armed" TMP-PATH IMAGE
   [: DEF-OPEN-LIVE, ;] false s" hb-x64-kernel-def-open-live-armed" TMP-PATH IMAGE
   [: CASE-PAIR, ;] false s" hb-x64-kernel-def-open-case-armed" TMP-PATH IMAGE
   [: PRIVATE-SET, ;] false s" hb-x64-kernel-namespace-private-set-armed" TMP-PATH IMAGE
   [: ALIAS-INT, ;] false s" hb-x64-kernel-alias-record-int-armed" TMP-PATH IMAGE
   [: X64HARNESS:REST,  -1 5 SCOPE, ;] false
   s" hb-x64-kernel-package-scope-clear-armed" TMP-PATH IMAGE
   [: CEILING, ;] false s" hb-x64-kernel-def-open-ceiling-armed" TMP-PATH IMAGE
   [: FULL, ;] false s" hb-x64-kernel-body-append-full-armed" TMP-PATH IMAGE
   [: X64HARNESS:REST,  s" trust-sig!" SIG, ;] false s" hb-x64-kernel-trust-sig-armed" TMP-PATH IMAGE
   [: X64HARNESS:REST,  s" created-sig!" SIG, ;] false
   s" hb-x64-kernel-created-sig-armed" TMP-PATH IMAGE
   [: X64HARNESS:REST,  s" def-close" X64HARNESS:CALL-ROW, ;] false
   s" hb-x64-kernel-def-close-armed" TMP-PATH IMAGE
   [: TIER-0-CLOSE, ;] false s" hb-x64-kernel-def-close-tier-armed" TMP-PATH IMAGE
   [: ALIAS-PROT, ;] false s" hb-x64-kernel-alias-record-prot-armed" TMP-PATH IMAGE
   [: DEF-OPEN-PROT, ;] false s" hb-x64-kernel-def-open-prot-armed" TMP-PATH IMAGE ;

public

: RUN ( -- )
   T-RESET
   X64HARNESS:INIT
   WRITERS
   REFUSALS
   X64HARNESS:DISPOSE
   T-REPORT ;

;using
;using
;using
;package

X64K-DEFINITION:RUN
