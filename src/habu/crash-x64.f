split-str
\ crash-x64.f - the x86-64 fault records and die lines, package X64CRASH: the
\ twin of src/habu/crash.f's emitters over package FAULT-RECORD's texts. A site emits
\ FAULT-OPEN,, any notes, then FAULT-STOP, or FAULT-KEEP,. Each part's text is
\ laid after a jump past it, and the run writes the one r10, the flag
\ FAULT-OPEN, reads, picks. The record grows in a FAULT-FRAME on rsp from r9,
\ its start, at the cursor r8, and is written to fd 2 when it fills and at the
\ record's end. It uses no region and no VM stack and writes rax, rcx, rdx,
\ rsi, rdi and r8..r11, no other register: rbx, rbp and r12..r15 survive. Its
\ pushes go below the frame, so a register a part takes is pushed first and may
\ be any register that holds the value when the part is emitted.
require src/core/exit-status.f
require src/habu/layout.f
require src/habu/wl.f
require src/habu/code-span.f
require src/habu/fault-record.f
require src/arch/x86-64/asm.f
require src/arch/x86-64/icode.f
require src/os/linux-x86-64/sys.f
require src/os/linux/layout.f

package X64CRASH
using X64ASM
using X64CODE
using FAULT-RECORD
using LINUX-LAYOUT   \ the guard

2 constant STDERR

\ The routines' labels, made by EMIT-FAULT-RECORD in the stream it emits them
\ into.
variable PUT-CELL
variable OWNER-CELL
variable DEC-CELL
: PUT-LBL ( -- label ) PUT-CELL @ >LABEL ;
: OWNER-LBL ( -- label ) OWNER-CELL @ >LABEL ;
: DEC-LBL ( -- label ) DEC-CELL @ >LABEL ;

: CUR-REG ( -- r64 ) R8 ;
: FRAME-REG ( -- r64 ) R9 ;
: JSON-REG ( -- r64 ) R10 ;

: IMM, ( r64 n -- ) >IMM64 ASM-SINK ENC-MOV-RI64 ;
\ mov r32, imm32, which zero-extends: a small non-negative number.
: IMM32, ( r64 n -- ) {: r:r64 v:n :}
   r R64>N >R32 v >IMM32 ASM-SINK ENC-MOV32-RI32 ;
: MOV, ( r64 r64 -- ) ASM-SINK ENC-MOV-RR ;
: LOAD, ( r64 mem -- ) ASM-SINK ENC-MOV-RM ;
: TEST, ( r64 -- ) dup ASM-SINK ENC-TEST-RR ;
: CMP8, ( r64 n -- ) >IMM8 ASM-SINK ENC-CMP-RI8 ;
: ADD8, ( r64 n -- ) >IMM8 ASM-SINK ENC-ADD-RI8 ;
: PUSH, ( r64 -- ) ASM-SINK ENC-PUSH ;
: POP, ( r64 -- ) ASM-SINK ENC-POP ;

\ [r8] = the register's low byte, r8 one past it.
: PUT-BYTE, ( r64 -- ) {: r:r64 :}
   r R64>N >R8 CUR-REG MEM-AT ASM-SINK ENC-MOV8-MR
   CUR-REG ASM-SINK ENC-INC ;
\ [r8] = the byte n, through r11.
: PUT-CHAR, ( n -- )  R11 swap IMM32,  R11 PUT-BYTE, ;

\ Writes [r9, r8) to fd 2 and empties the frame. rax, rcx, rdx, rsi and rdi
\ are saved across the write; clobbers r11.
: FLUSH, ( -- )
   RAX PUSH,  RCX PUSH,  RDX PUSH,  RSI PUSH,  RDI PUSH,
   RDI STDERR IMM32,  RSI FRAME-REG MOV,
   RDX CUR-REG MOV,  RDX FRAME-REG ASM-SINK ENC-SUB-RR
   NR-WRITE SYS,
   CUR-REG FRAME-REG MOV,
   RDI POP,  RSI POP,  RDX POP,  RCX POP,  RAX POP, ;

\ FLUSH, when fewer than n bytes of the frame are free. Clobbers r11.
: FLUSH-BELOW, ( n -- ) {: room:n :}
   LBL {: fits:label :}
   R11 CUR-REG MOV,  R11 FRAME-REG ASM-SINK ENC-SUB-RR
   R11 FAULT-FRAME room - >IMM32 ASM-SINK ENC-CMP-RI32  C-BE fits JCC,
   FLUSH,
   fits LBL, ;

\ Writes the prose or the JSON text as r10 says, then empties both.
: PIECE, ( -- )
   LBL LBL LBL LBL {: prose:label json:label code:label put:label :}
   FAULT-PROSE$ {: pa:^ pu:n :}
   FAULT-JSON$ {: ja:^ ju:n :}
   code JMP,
   prose LBL,  pa pu BYTES,
   json LBL,  ja ju BYTES,
   code LBL,
   RSI prose MOVABS,  RDX pu IMM32,
   JSON-REG TEST,  C-E put JCC,
   RSI json MOVABS,  RDX ju IMM32,
   put LBL,  RDI ZERO-REG,  PUT-LBL CALL,
   FAULT-EMPTY ;

\ rax's 16 hex digits at the cursor, most significant first. Clobbers rax,
\ rcx, rdx and r11.
: HEX, ( -- )
   LBL LBL {: next:label digit:label :}
   16 FLUSH-BELOW,
   RCX CUR-REG 16 MEM-OFF ASM-SINK ENC-LEA
   next LBL,
      RDX RAX MOV,  RDX $F >IMM8 ASM-SINK ENC-AND-RI8  RDX $30 ADD8,
      RDX $39 CMP8,  C-BE digit JCC,  RDX $27 ADD8,
      digit LBL,
      RCX ASM-SINK ENC-DEC
      RDX R64>N >R8 RCX MEM-AT ASM-SINK ENC-MOV8-MR
      RAX 4 >IMM8 ASM-SINK ENC-SHR-RI8
      RCX CUR-REG ASM-SINK ENC-CMP-RR  C-NE next JCC,
   CUR-REG 16 ADD8, ;

\ PUT ( rsi = text, rdx = its length, rdi = nonzero to escape it as a JSON
\ string's content ): the text at the cursor. Clobbers rax, rcx, rsi, rdx and
\ r11; rdi survives.
: EMIT-FAULT-PUT ( -- )
   LBL LBL LBL LBL LBL {: next:label plain:label pair:label low:label done:label :}
   PUT-LBL LBL,
   next LBL,
      RDX TEST,  C-E done JCC,
      6 FLUSH-BELOW,
      RAX RSI MEM-AT ASM-SINK ENC-MOVZX-8-RM
      RSI ASM-SINK ENC-INC  RDX ASM-SINK ENC-DEC
      RDI TEST,  C-E plain JCC,
      RCX RAX MOV,                              \ rcx = the byte after the backslash
      RAX $22 CMP8,  C-E pair JCC,
      RAX $5C CMP8,  C-E pair JCC,
      RCX $6E IMM32,  RAX $0A CMP8,  C-E pair JCC,
      RCX $72 IMM32,  RAX $0D CMP8,  C-E pair JCC,
      RCX $74 IMM32,  RAX $09 CMP8,  C-E pair JCC,
      RAX $20 CMP8,  C-AE plain JCC,
      $5C PUT-CHAR,  $75 PUT-CHAR,  $30 PUT-CHAR,  $30 PUT-CHAR,
      RCX RAX MOV,  RCX 4 >IMM8 ASM-SINK ENC-SHR-RI8  RCX $30 ADD8,  RCX PUT-BYTE,
      RCX RAX MOV,  RCX $F >IMM8 ASM-SINK ENC-AND-RI8  RCX $30 ADD8,
      RCX $39 CMP8,  C-BE low JCC,  RCX 7 ADD8,
      low LBL,  RCX PUT-BYTE,  next JMP,
   pair LBL,  $5C PUT-CHAR,  RCX PUT-BYTE,  next JMP,
   plain LBL,  RAX PUT-BYTE,  next JMP,
   done LBL,  RET, ;

\ OWNER ( rdi = an address, rsi = the dictionary's base or 0, rdx = its record
\ count -- rsi = the name of the word whose code holds the address, 0 for none,
\ rdx = its length ): a scan of the records, as many as DICT-CAP. Package rows
\ hold wordlist ids where a word's span is. Clobbers rax, rcx and r11; rdi
\ survives.
: EMIT-FAULT-OWNER ( -- )
   LBL LBL LBL LBL LBL {: scan:label code:label next:label hit:label none:label :}
   LBL {: inline:label :}
   OWNER-LBL LBL,
   RSI TEST,  C-E none JCC,
   RAX DICT-CAP IMM32,  RDX RAX ASM-SINK ENC-CMP-RR  C-A RDX RAX ASM-SINK ENC-CMOVCC
   RCX RSI MOV,
   scan LBL,
      RDX TEST,  C-E none JCC,
      RAX RCX DICT-WORDLIST-SLOT CELL * MEM-OFF LOAD,
      RAX TEST,  C-NS code JCC,
      RAX WL:RETIRED CMP8,  C-NE next JCC,
   code LBL,
      RAX RCX MEM-AT LOAD,  RDI RAX ASM-SINK ENC-CMP-RR  C-B next JCC,
      R11 RCX CELL MEM-OFF LOAD,                \ the raw span (code-span.f)
      R11 CODE-SPAN:MASK >IMM32 ASM-SINK ENC-AND-RI32
      RAX R11 ASM-SINK ENC-ADD-RR  RDI RAX ASM-SINK ENC-CMP-RR  C-B hit JCC,
   next LBL,
      RCX DREC ADD8,  RDX ASM-SINK ENC-DEC  scan JMP,
   hit LBL,
      RAX RCX 16 MEM-OFF LOAD,
      RSI RCX 24 MEM-OFF ASM-SINK ENC-LEA
      R11 DNAME-EXT IMM,  RAX R11 ASM-SINK ENC-TEST-RR  C-E inline JCC,
      RSI RCX 24 MEM-OFF LOAD,
      inline LBL,
      R11 DNAME-LEN-MASK IMM,  RAX R11 ASM-SINK ENC-AND-RR  RDX RAX MOV,
      RET,
   none LBL,  RSI ZERO-REG,  RET, ;

\ DEC ( rax = a signed number ): its decimal digits at the cursor, a minus
\ first when it is negative. Clobbers rax, rcx, rdx, rsi, rdi and r11.
: EMIT-FAULT-DEC ( -- )
   LBL LBL LBL {: pos:label count:label digit:label :}
   DEC-LBL LBL,
   21 FLUSH-BELOW,
   RAX TEST,  C-NS pos JCC,
   $2D PUT-CHAR,  RAX ASM-SINK ENC-NEG           \ unsigned from here
   pos LBL,
   RSI RAX MOV,  RCX 10 IMM32,  RDI ZERO-REG,
   count LBL,
      RDI ASM-SINK ENC-INC  RDX ZERO-REG,  RCX ASM-SINK ENC-DIV
      RAX TEST,  C-NE count JCC,
   CUR-REG RDI ASM-SINK ENC-ADD-RR  RDI CUR-REG MOV,  RAX RSI MOV,
   digit LBL,
      RDX ZERO-REG,  RCX ASM-SINK ENC-DIV  RDX $30 ADD8,
      RDI ASM-SINK ENC-DEC  RDX R64>N >R8 RDI MEM-AT ASM-SINK ENC-MOV8-MR
      RAX TEST,  C-NE digit JCC,
   RET, ;

\ The die line's text room in the frame: the rest holds " 0x", 16 hex digits,
\ " lr 0x", 16 hex digits and the newline.
FAULT-FRAME 42 - constant DIE-LINE-ROOM

variable DIE-CELL
: DIE-LBL ( -- label ) DIE-CELL @ >LABEL ;

\ DIE ( rdi = the exit status, rsi = the text, 1 to DIE-LINE-ROOM bytes, rdx =
\ its length, rax = a value, rcx = nonzero to print it ): entered by a jump, so
\ the qword at rsp is the lr it prints. Writes `<text>[ <rax>] lr <lr>`, each
\ `0x` and 16 hex digits, and a newline to fd 2 in one write, then exits.
: EMIT-DIE-LINE ( -- )
   LBL {: bare:label :}
   DIE-LBL LBL,
   R10 RDI MOV,
   RSP FAULT-FRAME >IMM32 ASM-SINK ENC-SUB-RI32
   FRAME-REG RSP MOV,  CUR-REG RSP MOV,
   RAX PUSH,  RCX PUSH,
   RDI ZERO-REG,  PUT-LBL CALL,
   RCX POP,  RAX POP,
   RCX TEST,  C-E bare JCC,
   $20 PUT-CHAR,  $30 PUT-CHAR,  $78 PUT-CHAR,  HEX,
   bare LBL,
   $20 PUT-CHAR,  $6C PUT-CHAR,  $72 PUT-CHAR,  $20 PUT-CHAR,
   $30 PUT-CHAR,  $78 PUT-CHAR,
   RAX FRAME-REG FAULT-FRAME MEM-OFF LOAD,  HEX,
   $A PUT-CHAR,
   FLUSH,
   RDI R10 MOV,  NR-EXIT-GROUP SYS, ;

: BAD-DIE-TEXT ( str -- )
   {: a:^ u:n :}
   s" E-FAULT-SITE" REJECTION:OPEN
   s" the die line's text is empty, or is longer than the room "
   REJECTION:EMIT-STR  s" its frame holds" REJECTION:EMIT-STR
   s" text" REJECTION:NOTE  a u REJECTION:EMIT-STR
   s" length" REJECTION:NOTE  u REJECTION:EMIT-U
   s" room" REJECTION:NOTE  DIE-LINE-ROOM REJECTION:EMIT-U
   REJECTION:HELP  s" give the line a text of 1 to " REJECTION:EMIT-STR
   DIE-LINE-ROOM REJECTION:EMIT-U  s"  bytes" REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

: DIE-LINE-SITE, ( str n n -- )
   LBL {: a:^ u:n status:n flag:n msg:label :}
   u 0= u DIE-LINE-ROOM > or IF a u BAD-DIE-TEXT THEN
   RDI status IMM32,  RSI msg MOVABS,  RDX u IMM32,  RCX flag IMM32,
   DIE-LBL JMP,
   msg LBL,  a u BYTES, ;

public
\ A jump to DIE with the text and the exit status, the text laid after it. The
\ caller places its own label before it, and rsp where its qword is the lr.
: DIE-LINE, ( str n -- )  0 DIE-LINE-SITE, ;

\ DIE-LINE, that also prints the register's value; the register is copied
\ first, so it may be any of rax, rcx, rdx, rsi and rdi.
: DIE-LINE-VALUE, ( str n r64 -- )
   {: a:^ u:n status:n reg:r64 :}
   reg R64>N RAX R64>N <> IF RAX reg MOV, THEN
   a u status 1 DIE-LINE-SITE, ;

public
\ Opens a record with its code and message: rsp drops FAULT-FRAME, r9 = its
\ start and r8 the cursor, r10 = the JSON flag.
: FAULT-OPEN, ( str str -- FAULT-RECORD:record )
   FAULT-OPEN
   RSP FAULT-FRAME >IMM32 ASM-SINK ENC-SUB-RI32
   FRAME-REG RSP MOV,  CUR-REG RSP MOV,
   JSON-REG LINUX-LAYOUT:DATA-VA VA>N REJECTION-JSON-CELL + IMM,
   JSON-REG JSON-REG MEM-AT LOAD,
   PIECE, ;

\ Note `in`: the word whose code holds the address in rdi, read from the rdx
\ records at rsi; rsi = 0 when the site cannot trust the dictionary. With no
\ such word, note `at` and the address.
: FAULT-IN, ( FAULT-RECORD:record -- FAULT-RECORD:record )
   LBL LBL {: at:label done:label :}
   FAULT-IN {: was:n :}
   OWNER-LBL CALL,
   RSI TEST,  C-E at JCC,
   RSI PUSH,  RDX PUSH,
   PIECE,
   RDX POP,  RSI POP,  RDI JSON-REG MOV,  PUT-LBL CALL,
   done JMP,
   at LBL,
   RDI PUSH,
   was FAULT-AT  PIECE,
   RAX POP,  HEX,
   done LBL, ;

\ A note with the register's value in decimal.
: FAULT-NUMBER, ( FAULT-RECORD:record str r64 -- FAULT-RECORD:record )
   {: k:^ ku:n r:r64 :}
   r PUSH,
   k ku NOTE+  PIECE,
   RAX POP,  DEC-LBL CALL, ;

\ A note with the bytes at the pointer register, the length register's count
\ of them.
: FAULT-TEXT, ( FAULT-RECORD:record str r64 r64 -- FAULT-RECORD:record )
   {: k:^ ku:n p:r64 l:r64 :}
   p PUSH,  l PUSH,
   k ku NOTE+  PIECE,
   RDX POP,  RSI POP,  RDI JSON-REG MOV,  PUT-LBL CALL, ;

\ A note with the address in the register, `0x` and 16 hex digits.
: FAULT-ADDRESS, ( FAULT-RECORD:record str r64 -- FAULT-RECORD:record )
   {: k:^ ku:n r:r64 :}
   r PUSH,
   k ku FAULT-ADDRESS  PIECE,
   RAX POP,  HEX, ;

\ Ends the record with its help and exits with the status.
: FAULT-STOP, ( FAULT-RECORD:record str n -- )
   {: h:^ hu:n status:n :}
   h hu FAULT-STOP
   PIECE,  FLUSH,
   RDI status IMM32,  NR-EXIT-GROUP SYS, ;

\ Ends the record with its help, none for an empty text, and returns, rsp as
\ FAULT-OPEN, found it.
: FAULT-KEEP, ( FAULT-RECORD:record str -- )
   FAULT-KEEP
   PIECE,  FLUSH,
   RSP FAULT-FRAME >IMM32 ASM-SINK ENC-ADD-RI32 ;

\ Makes the routines' labels and emits the routines, the die line's included,
\ where no control falls in; a site emitted after it in the stream reaches them.
: EMIT-FAULT-RECORD ( -- )
   LBL PUT-CELL !  LBL OWNER-CELL !  LBL DEC-CELL !  LBL DIE-CELL !
   EMIT-FAULT-PUT  EMIT-FAULT-OWNER  EMIT-FAULT-DEC  EMIT-DIE-LINE ;

;using   \ LINUX-LAYOUT
;using
;using
;using
;package
