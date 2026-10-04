\ Emit a stripped x86-64 application from AOT-LINK's reachable closure.
\ Loaded after aot-lib.f only for the x86-64 target.
require lib/le.f
require lib/byte-buffer.f
require src/habu/boot-x64.f
require src/habu/patch-x64.f
require src/habu/image-x64.f

package AOT-LINK

private

: X64-TEXT-VA ( -- n ) VMBASE X64LAYOUT:CODE-OFF + ;
: X64-SINK ( -- ptr u8 ) X64CODE:ASM-SINK ;

\ The DATA load segment is zero-backed. Each row below carries one contiguous
\ nonzero run of the application window; zero-filled holes consume no file bytes.
create X64-WORD 8 allot
variable X64-I  variable X64-J  variable X64-RUNS

: X64-BYTE? ( n -- bool ) BLOB-SRC@ + c@ 0<> ;
: X64-IN-RUN? ( -- bool )
   X64-I @ BLOB-LEN @ >= if false exit then
   X64-I @ X64-BYTE? ;

: X64-COUNT-RUNS ( -- n )
   0 X64-RUNS !  0 X64-I !
   begin X64-I @ BLOB-LEN @ < while
      X64-I @ X64-BYTE? if
         1 X64-RUNS +!
         begin X64-IN-RUN? while 1 X64-I +! repeat
      else
         1 X64-I +!
      then
   repeat
   X64-RUNS @ ;

: X64-U32, ( n -- )
   X64-WORD LE:U32!
   X64-WORD 4 BUF:N>BLEN X64-SINK BUF:APPEND-SPAN ;

: X64-RUNS, ( -- )
   X64-COUNT-RUNS X64-U32,
   0 X64-I !
   begin X64-I @ BLOB-LEN @ < while
      X64-I @ X64-BYTE? if
         X64-I @ X64-J !
         begin X64-IN-RUN? while 1 X64-I +! repeat
         BLOB-SRC @ DATA-VA VA>N - X64-J @ + X64-U32,
         X64-I @ X64-J @ - X64-U32,
         BLOB-SRC@ X64-J @ + X64-I @ X64-J @ - BUF:N>BLEN
            X64-SINK BUF:APPEND-SPAN
      else
         1 X64-I +!
      then
   repeat ;

: X64-XT-ROWS, ( -- )
   XTC-N @ X64-U32,
   XTC-N @ 0 ?do
      i XTC-OFF@ X64-U32,
      i XT-CELL-TARGET X64-U32,
   loop ;

\ Place each reachable body in text without records or interpreter state.
: X64-PACK ( label -- ) {: root:label :}
   NCLO @ PLAN-TABLES
   MEMBER-ORDER
   NCLO @ 0 ?do
      i 0= if root X64CODE:LBL, then
      X64CODE:ASM-LEN i NEWOFF !
      i CLO-BYTES i BLEN !
      i CLO-AT i CLO-BYTES BUF:N>BLEN X64-SINK BUF:APPEND-SPAN
   loop ;

: X64-IMM, ( r64 n -- ) {: r:r64 v:n :}
   r v X64ASM:>IMM64 X64-SINK X64ASM:ENC-MOV-RI64 ;

: X64-CELL! ( r64 n -- ) {: r:r64 off:n :}
   r X64ASM:RBP off X64ASM:MEM-OFF X64-SINK X64ASM:ENC-MOV-MR ;

\ Publish the same owned cells as the ARM entry. The linked boot has already
\ filled argc, argv, envp, stack and signal cells in zero-backed DATA.
: X64-OWNED, ( label -- ) {: root:label :}
   AOT-OWNED:N 0 ?do
      i OWNED-PUBLISHED? if
         i AOT-OWNED:ENTRY-XT? if
            X64ASM:RAX root X64CODE:MOVABS,
         else i AOT-OWNED:TEXT-BASE? if
            X64ASM:RAX X64-TEXT-VA X64-IMM,
         else
            X64ASM:RAX DATA-VA VA>N X64-IMM,
         then then
         X64ASM:RAX i AOT-OWNED:AT DATA-VA VA>N - X64-CELL!
      then
   loop
   X64ASM:RAX BLOB-SRC @ SPAN-CELLS AOT-WINDOW:CELL-BYTES * + X64-IMM,
   X64ASM:RAX DP-CELL X64-CELL! ;

: X64-DECODE, ( label -- ) {: table:label :}
   X64ASM:RSI table X64CODE:MOVABS,
   X64ASM:R9 DATA-VA VA>N X64-IMM,
   X64ASM:R8 X64ASM:R64>N X64ASM:>R32
      X64ASM:RSI X64ASM:MEM-AT X64-SINK X64ASM:ENC-MOV32-RM
   X64ASM:RSI 4 X64ASM:>IMM8 X64-SINK X64ASM:ENC-ADD-RI8
   X64CODE:LBL X64CODE:LBL {: top:label done:label :}
   X64ASM:R8 X64ASM:R8 X64-SINK X64ASM:ENC-TEST-RR
   X64ASM:C-E done X64CODE:JCC,
   top X64CODE:LBL,
   X64ASM:RDI X64ASM:R64>N X64ASM:>R32
      X64ASM:RSI X64ASM:MEM-AT X64-SINK X64ASM:ENC-MOV32-RM
   X64ASM:RDI X64ASM:R9 X64-SINK X64ASM:ENC-ADD-RR
   X64ASM:RCX X64ASM:R64>N X64ASM:>R32
      X64ASM:RSI 4 X64ASM:MEM-OFF X64-SINK X64ASM:ENC-MOV32-RM
   X64ASM:RSI 8 X64ASM:>IMM8 X64-SINK X64ASM:ENC-ADD-RI8
   $F3 X64-SINK BUF:APPEND-BYTE  $A4 X64-SINK BUF:APPEND-BYTE
   X64ASM:R8 X64-SINK X64ASM:ENC-DEC
   X64ASM:C-NE top X64CODE:JCC,
   done X64CODE:LBL, ;

: X64-RESTORE-XT, ( -- )
   X64ASM:R8 X64ASM:R64>N X64ASM:>R32
      X64ASM:RSI X64ASM:MEM-AT X64-SINK X64ASM:ENC-MOV32-RM
   X64ASM:RSI 4 X64ASM:>IMM8 X64-SINK X64ASM:ENC-ADD-RI8
   X64ASM:R9 DATA-VA VA>N X64-IMM,
   X64ASM:R10 X64-TEXT-VA X64-IMM,
   X64CODE:LBL X64CODE:LBL {: top:label done:label :}
   X64ASM:R8 X64ASM:R8 X64-SINK X64ASM:ENC-TEST-RR
   X64ASM:C-E done X64CODE:JCC,
   top X64CODE:LBL,
   X64ASM:RDI X64ASM:R64>N X64ASM:>R32
      X64ASM:RSI X64ASM:MEM-AT X64-SINK X64ASM:ENC-MOV32-RM
   X64ASM:RDI X64ASM:R9 X64-SINK X64ASM:ENC-ADD-RR
   X64ASM:RAX X64ASM:R64>N X64ASM:>R32
      X64ASM:RSI 4 X64ASM:MEM-OFF X64-SINK X64ASM:ENC-MOV32-RM
   X64ASM:RAX X64ASM:R10 X64-SINK X64ASM:ENC-ADD-RR
   X64ASM:RAX X64ASM:RDI X64ASM:MEM-AT X64-SINK X64ASM:ENC-MOV-MR
   X64ASM:RSI 8 X64ASM:>IMM8 X64-SINK X64ASM:ENC-ADD-RI8
   X64ASM:R8 X64-SINK X64ASM:ENC-DEC
   X64ASM:C-NE top X64CODE:JCC,
   done X64CODE:LBL, ;

: X64-SEED, ( -- )
   SEED-N @ STACK-ABI:BOOT-BYTES CELL / > if
      s" aot: initial data stack exceeds allocation" 74 die then
   SEED-N @ 0 ?do
      X64ASM:RAX SEED-CELLS i cells + @ X64-IMM,
      X64ASM:RAX X64ASM:R12 X64ASM:MEM-AT X64-SINK X64ASM:ENC-MOV-MR
      X64ASM:R12 8 X64ASM:>IMM8 X64-SINK X64ASM:ENC-ADD-RI8
   loop ;

: X64-EXIT, ( -- )
   X64CODE:LBL {: done:label :}
   X64ASM:RAX X64ASM:RBP EXIT-HOOK-CELL X64ASM:MEM-OFF
      X64-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:RAX X64-SINK X64ASM:ENC-TEST-RR
   X64ASM:C-E done X64CODE:JCC,
   X64ASM:R9 ZERO-REG,
   X64ASM:R9 EXIT-HOOK-CELL X64-CELL!
   X64ASM:RAX X64-SINK X64ASM:ENC-CALL-REG
   done X64CODE:LBL,
   X64ASM:RDI ZERO-REG,
   NR-EXIT-GROUP SYS, ;

: X64-START, ( label label -- ) {: root:label table:label :}
   0 ELF-REGION-VA DICT-SIZE + X64BOOT:LINKED-START,
   table X64-DECODE,
   X64-RESTORE-XT,
   root X64-OWNED,
   X64-SEED,
   X64ASM:RAX root X64CODE:MOVABS,
   X64ASM:RAX X64-SINK X64ASM:ENC-CALL-REG
   X64-EXIT, ;

variable X64-MEMBER

: X64-SIGNED32 ( n -- n ) {: v:n :}
   v $80000000 and 0<> if v $100000000 - exit then
   v ;

: X64-SIGNED8 ( n -- n ) {: v:n :}
   v $80 and 0<> if v $100 - exit then
   v ;

: X64-DEST ( ptr u8 -- ptr u8 ) {: p:ptr :}
   X64CODE:CODE X64-MEMBER @ NEWOFF @ +
   p X64-MEMBER @ CLO-AT - + ;

: X64-DEST-VA ( ptr u8 -- n ) {: p:ptr :}
   X64-TEXT-VA X64-MEMBER @ NEWOFF @ +
   p X64-MEMBER @ CLO-AT - + ;

: X64-NOP-CALL ( ptr u8 -- ) {: p:ptr :}
   $0F p c!  $1F p 1+ c!  $44 p 2 + c!  0 p 3 + c!  0 p 4 + c! ;

: X64-CALL-SITE ( ptr u8 -- ) {: p:ptr :}
   p LE:U32@ X64-SIGNED32 p 4 + + {: target:ptr :}
   p X64-DEST 1- {: dst:ptr :}
   target DECLARATION-TARGET? if dst X64-NOP-CALL exit then
   X64-MEMBER @ target MAP-TARGET!
   dst p X64-DEST-VA 1- X64-TEXT-VA TNEW @ + X64PATCH:REL32! ;

: X64-ADDR-SITE ( ptr u8 -- ) {: p:ptr :}
   p 2 - X64-MEMBER @ CLO-AT X64-MEMBER @ CLO-BYTES + SITE-LITERAL
      {: size:n v:n :}
   size ADDRESS-CARRIER:MOVABS-BYTES <> if
      s" aot: malformed recorded x86 address site" 74 die then
   v DATA-ADDRESS? if
      X64-MEMBER @ CLO-REC@ p 2 - v DATA-TARGET
   else
      X64-MEMBER @ v CODE-PTR MAP-TARGET!
      X64-TEXT-VA TNEW @ +
   then
   p X64-DEST 2 - swap X64PATCH:MOVABS! ;

: X64-TEXT-REL ( ptr u8 n n -- ) {: p:ptr width:n disp:n :}
   p width + disp + {: target:ptr :}
   \ A recorded E8 call to the declaration-only registrar has no work in a
   \ stripped image. Its store stays; the omitted helper's call becomes a NOP.
   width 4 = if
      p 1- c@ $E8 = if
         target DECLARATION-TARGET? if
            p X64-DEST 1- X64-NOP-CALL exit then
      then
   then
   X64-MEMBER @ target MAP-TARGET!
   X64-TEXT-VA TNEW @ + p X64-DEST-VA width + - {: rel:n :}
   width 1 = if
      rel -128 < rel 127 > or if
         s" aot: copied x86 rel8 target out of reach" 74 die then
      rel p X64-DEST c! exit
   then
   rel -2147483648 < rel 2147483647 > or if
      s" aot: copied x86 rel32 target out of reach" 74 die then
   rel p X64-DEST LE:U32! ;

: X64-TEXT-ABS ( ptr u8 -- ) {: p:ptr :}
   p LE:U64@ {: v:n :}
   v DATA-ADDRESS? if
      X64-MEMBER @ CLO-REC@ p 2 - v DATA-TARGET
   else
      X64-MEMBER @ v CODE-PTR MAP-TARGET!
      X64-TEXT-VA TNEW @ +
   then
   p X64-DEST 2 - swap X64PATCH:MOVABS! ;

: X64-SITE ( ptr u8 n -- ) {: p:ptr kind:n :}
   kind SNAP-RELOC:SITE-CALL = if p X64-CALL-SITE exit then
   kind SNAP-RELOC:SITE-ADDR = if p X64-ADDR-SITE exit then
   kind 3 = if p 1 p c@ X64-SIGNED8 X64-TEXT-REL exit then
   kind 4 = if p 4 p LE:U32@ X64-SIGNED32 X64-TEXT-REL exit then
   kind 5 = if p X64-TEXT-ABS exit then
   s" aot: unknown recorded x86 site kind" 74 die ;

: X64-PATCH-SITES ( -- )
   NCLO @ 0 ?do
      i X64-MEMBER !
      i CLO-AT {: p:ptr :}
      p p i CLO-BYTES + [: X64-SITE ;] EACH-MEMBER-SITE
   loop ;

: X64-WRITE ( -- )
   AOT-OUT PATH0 1537 493 open {: fd:n :}
   fd 0 < if s" aot: cannot open stripped image" 74 die then
   X64CODE:CODE X64CODE:ASM-LEN
   X64CODE:CODE 0
   X64CODE:CODE 0
   fd X64IMAGE:WRITE-FD
   fd close-rc 0 <> if s" aot: stripped image close failed" 74 die then ;

: X64-WRITE-OBJ ( -- )
   AOT-OBJ PATH0 1537 493 open {: fd:n :}
   fd 0 < if s" aot: cannot open object output" 74 die then
   fd X64CODE:CODE X64CODE:ASM-LEN FDIO:WALL
   fd close-rc 0 <> if s" aot: object close failed" 74 die then ;

: LINK-X64 ( -- )
   X64CODE:ASM-SINK X64CODE:CODE-CAP-BYTES BUF:N>BLEN BUF:INIT
   X64CODE:ASM-RESET
   X64CODE:LBL X64CODE:LBL {: root:label table:label :}
   root table X64-START,
   root X64-PACK
   X64-PATCH-SITES
   table X64CODE:LBL,
   X64-RUNS,
   X64-XT-ROWS,
   X64-TEXT-VA X64CODE:ASM-LINK
   X64-WRITE-OBJ
   X64-WRITE
   X64CODE:ASM-SINK BUF:DISPOSE ;

: INSTALL-X64 ( -- ) [: LINK-X64 ;] is LINK-TARGET ;
INSTALL-X64

;package
