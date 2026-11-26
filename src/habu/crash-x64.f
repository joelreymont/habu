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
require src/habu/fault-record.f
require src/arch/x86-64/asm.f
require src/arch/x86-64/icode.f
require src/os/linux-x86-64/sys.f
require src/os/linux/layout.f

package X64CRASH

2 constant STDERR

\ The routines' labels, made by EMIT-FAULT-RECORD in the stream it emits them
\ into.
variable PUT-CELL
variable OWNER-CELL
variable DEC-CELL
: PUT-LBL ( -- label ) PUT-CELL @ >LABEL ;
: OWNER-LBL ( -- label ) OWNER-CELL @ >LABEL ;
: DEC-LBL ( -- label ) DEC-CELL @ >LABEL ;

: CUR-REG ( -- X64ASM:r64 ) X64ASM:R8 ;
: FRAME-REG ( -- X64ASM:r64 ) X64ASM:R9 ;
: JSON-REG ( -- X64ASM:r64 ) X64ASM:R10 ;

: IMM, ( X64ASM:r64 n -- ) X64ASM:>IMM64 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI64 ;
\ mov r32, imm32, which zero-extends: a small non-negative number.
: IMM32, ( X64ASM:r64 n -- ) {: r:X64ASM:r64 v:n :}
   r X64ASM:R64>N X64ASM:>R32 v X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV32-RI32 ;
: MOV, ( X64ASM:r64 X64ASM:r64 -- ) X64CODE:ASM-SINK X64ASM:ENC-MOV-RR ;
: LOAD, ( X64ASM:r64 X64ASM:mem -- ) X64CODE:ASM-SINK X64ASM:ENC-MOV-RM ;
: TEST, ( X64ASM:r64 -- ) dup X64CODE:ASM-SINK X64ASM:ENC-TEST-RR ;
: CMP8, ( X64ASM:r64 n -- ) X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8 ;
: ADD8, ( X64ASM:r64 n -- ) X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8 ;
: PUSH, ( X64ASM:r64 -- ) X64CODE:ASM-SINK X64ASM:ENC-PUSH ;
: POP, ( X64ASM:r64 -- ) X64CODE:ASM-SINK X64ASM:ENC-POP ;

\ [r8] = the register's low byte, r8 one past it.
: PUT-BYTE, ( X64ASM:r64 -- ) {: r:X64ASM:r64 :}
   r X64ASM:R64>N X64ASM:>R8 CUR-REG X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV8-MR
   CUR-REG X64CODE:ASM-SINK X64ASM:ENC-INC ;
\ [r8] = the byte n, through r11.
: PUT-CHAR, ( n -- )  X64ASM:R11 swap IMM32,  X64ASM:R11 PUT-BYTE, ;

\ Writes [r9, r8) to fd 2 and empties the frame. rax, rcx, rdx, rsi and rdi
\ are saved across the write; clobbers r11.
: FLUSH, ( -- )
   X64ASM:RAX PUSH,  X64ASM:RCX PUSH,  X64ASM:RDX PUSH,  X64ASM:RSI PUSH,  X64ASM:RDI PUSH,
   X64ASM:RDI STDERR IMM32,  X64ASM:RSI FRAME-REG MOV,
   X64ASM:RDX CUR-REG MOV,  X64ASM:RDX FRAME-REG X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   NR-WRITE SYS,
   CUR-REG FRAME-REG MOV,
   X64ASM:RDI POP,  X64ASM:RSI POP,  X64ASM:RDX POP,  X64ASM:RCX POP,  X64ASM:RAX POP, ;

\ FLUSH, when fewer than n bytes of the frame are free. Clobbers r11.
: FLUSH-BELOW, ( n -- ) {: room:n :}
   X64CODE:LBL {: fits:label :}
   X64ASM:R11 CUR-REG MOV,  X64ASM:R11 FRAME-REG X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   X64ASM:R11 FAULT-RECORD:FAULT-FRAME room - X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI32  X64ASM:C-BE fits X64CODE:JCC,
   FLUSH,
   fits X64CODE:LBL, ;

\ Writes the prose or the JSON text as r10 says, then empties both.
: PIECE, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL {: prose:label json:label code:label put:label :}
   FAULT-RECORD:FAULT-PROSE$ {: pa:^ pu:n :}
   FAULT-RECORD:FAULT-JSON$ {: ja:^ ju:n :}
   code X64CODE:JMP,
   prose X64CODE:LBL,  pa pu X64CODE:BYTES,
   json X64CODE:LBL,  ja ju X64CODE:BYTES,
   code X64CODE:LBL,
   X64ASM:RSI prose X64CODE:MOVABS,  X64ASM:RDX pu IMM32,
   JSON-REG TEST,  X64ASM:C-E put X64CODE:JCC,
   X64ASM:RSI json X64CODE:MOVABS,  X64ASM:RDX ju IMM32,
   put X64CODE:LBL,  X64ASM:RDI ZERO-REG,  PUT-LBL X64CODE:CALL,
   FAULT-RECORD:FAULT-EMPTY ;

\ rax's 16 hex digits at the cursor, most significant first. Clobbers rax,
\ rcx, rdx and r11.
: HEX, ( -- )
   X64CODE:LBL X64CODE:LBL {: next:label digit:label :}
   16 FLUSH-BELOW,
   X64ASM:RCX CUR-REG 16 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   next X64CODE:LBL,
      X64ASM:RDX X64ASM:RAX MOV,  X64ASM:RDX $F X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-AND-RI8  X64ASM:RDX $30 ADD8,
      X64ASM:RDX $39 CMP8,  X64ASM:C-BE digit X64CODE:JCC,  X64ASM:RDX $27 ADD8,
      digit X64CODE:LBL,
      X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-DEC
      X64ASM:RDX X64ASM:R64>N X64ASM:>R8 X64ASM:RCX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV8-MR
      X64ASM:RAX 4 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHR-RI8
      X64ASM:RCX CUR-REG X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-NE next X64CODE:JCC,
   CUR-REG 16 ADD8, ;

\ PUT ( rsi = text, rdx = its length, rdi = nonzero to escape it as a JSON
\ string's content ): the text at the cursor. Clobbers rax, rcx, rsi, rdx and
\ r11; rdi survives.
: EMIT-FAULT-PUT ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL {: next:label plain:label pair:label low:label done:label :}
   PUT-LBL X64CODE:LBL,
   next X64CODE:LBL,
      X64ASM:RDX TEST,  X64ASM:C-E done X64CODE:JCC,
      6 FLUSH-BELOW,
      X64ASM:RAX X64ASM:RSI X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RM
      X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-INC  X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-DEC
      X64ASM:RDI TEST,  X64ASM:C-E plain X64CODE:JCC,
      X64ASM:RCX X64ASM:RAX MOV,                              \ rcx = the byte after the backslash
      X64ASM:RAX $22 CMP8,  X64ASM:C-E pair X64CODE:JCC,
      X64ASM:RAX $5C CMP8,  X64ASM:C-E pair X64CODE:JCC,
      X64ASM:RCX $6E IMM32,  X64ASM:RAX $0A CMP8,  X64ASM:C-E pair X64CODE:JCC,
      X64ASM:RCX $72 IMM32,  X64ASM:RAX $0D CMP8,  X64ASM:C-E pair X64CODE:JCC,
      X64ASM:RCX $74 IMM32,  X64ASM:RAX $09 CMP8,  X64ASM:C-E pair X64CODE:JCC,
      X64ASM:RAX $20 CMP8,  X64ASM:C-AE plain X64CODE:JCC,
      $5C PUT-CHAR,  $75 PUT-CHAR,  $30 PUT-CHAR,  $30 PUT-CHAR,
      X64ASM:RCX X64ASM:RAX MOV,  X64ASM:RCX 4 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHR-RI8  X64ASM:RCX $30 ADD8,  X64ASM:RCX PUT-BYTE,
      X64ASM:RCX X64ASM:RAX MOV,  X64ASM:RCX $F X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-AND-RI8  X64ASM:RCX $30 ADD8,
      X64ASM:RCX $39 CMP8,  X64ASM:C-BE low X64CODE:JCC,  X64ASM:RCX 7 ADD8,
      low X64CODE:LBL,  X64ASM:RCX PUT-BYTE,  next X64CODE:JMP,
   pair X64CODE:LBL,  $5C PUT-CHAR,  X64ASM:RCX PUT-BYTE,  next X64CODE:JMP,
   plain X64CODE:LBL,  X64ASM:RAX PUT-BYTE,  next X64CODE:JMP,
   done X64CODE:LBL,  X64CODE:RET, ;

\ OWNER ( rdi = an address, rsi = the dictionary's base or 0, rdx = its record
\ count -- rsi = the name of the word whose code holds the address, 0 for none,
\ rdx = its length ): a scan of the records, as many as DICT-CAP. Package rows
\ hold wordlist ids where a word's span is. Clobbers rax, rcx and r11; rdi
\ survives.
: EMIT-FAULT-OWNER ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL {: scan:label code:label next:label hit:label none:label :}
   X64CODE:LBL {: inline:label :}
   OWNER-LBL X64CODE:LBL,
   X64ASM:RSI TEST,  X64ASM:C-E none X64CODE:JCC,
   X64ASM:RAX DICT-CAP IMM32,  X64ASM:RDX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-A X64ASM:RDX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMOVCC
   X64ASM:RCX X64ASM:RSI MOV,
   scan X64CODE:LBL,
      X64ASM:RDX TEST,  X64ASM:C-E none X64CODE:JCC,
      X64ASM:RAX X64ASM:RCX DICT-WORDLIST-SLOT CELL * X64ASM:MEM-OFF LOAD,
      X64ASM:RAX TEST,  X64ASM:C-NS code X64CODE:JCC,
      X64ASM:RAX WL:RETIRED CMP8,  X64ASM:C-NE next X64CODE:JCC,
   code X64CODE:LBL,
      X64ASM:RAX X64ASM:RCX X64ASM:MEM-AT LOAD,  X64ASM:RDI X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-B next X64CODE:JCC,
      X64ASM:R11 X64ASM:RCX CELL X64ASM:MEM-OFF LOAD,                \ the code's byte count
      X64ASM:RAX X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-ADD-RR  X64ASM:RDI X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-B hit X64CODE:JCC,
   next X64CODE:LBL,
      X64ASM:RCX DREC ADD8,  X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-DEC  scan X64CODE:JMP,
   hit X64CODE:LBL,
      X64ASM:RAX X64ASM:RCX 16 X64ASM:MEM-OFF LOAD,
      X64ASM:RSI X64ASM:RCX 24 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
      X64ASM:R11 DNAME-EXT IMM,  X64ASM:RAX X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E inline X64CODE:JCC,
      X64ASM:RSI X64ASM:RCX 24 X64ASM:MEM-OFF LOAD,
      inline X64CODE:LBL,
      X64ASM:R11 DNAME-LEN-MASK IMM,  X64ASM:RAX X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-AND-RR  X64ASM:RDX X64ASM:RAX MOV,
      X64CODE:RET,
   none X64CODE:LBL,  X64ASM:RSI ZERO-REG,  X64CODE:RET, ;

\ DEC ( rax = a signed number ): its decimal digits at the cursor, a minus
\ first when it is negative. Clobbers rax, rcx, rdx, rsi, rdi and r11.
: EMIT-FAULT-DEC ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: pos:label count:label digit:label :}
   DEC-LBL X64CODE:LBL,
   21 FLUSH-BELOW,
   X64ASM:RAX TEST,  X64ASM:C-NS pos X64CODE:JCC,
   $2D PUT-CHAR,  X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-NEG           \ unsigned from here
   pos X64CODE:LBL,
   X64ASM:RSI X64ASM:RAX MOV,  X64ASM:RCX 10 IMM32,  X64ASM:RDI ZERO-REG,
   count X64CODE:LBL,
      X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-INC  X64ASM:RDX ZERO-REG,  X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-DIV
      X64ASM:RAX TEST,  X64ASM:C-NE count X64CODE:JCC,
   CUR-REG X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-ADD-RR  X64ASM:RDI CUR-REG MOV,  X64ASM:RAX X64ASM:RSI MOV,
   digit X64CODE:LBL,
      X64ASM:RDX ZERO-REG,  X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-DIV  X64ASM:RDX $30 ADD8,
      X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-DEC  X64ASM:RDX X64ASM:R64>N X64ASM:>R8 X64ASM:RDI X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV8-MR
      X64ASM:RAX TEST,  X64ASM:C-NE digit X64CODE:JCC,
   X64CODE:RET, ;

\ The die line's text room in the frame: the rest holds " 0x", 16 hex digits,
\ " lr 0x", 16 hex digits and the newline.
FAULT-RECORD:FAULT-FRAME 42 - constant DIE-LINE-ROOM

variable DIE-CELL
: DIE-LBL ( -- label ) DIE-CELL @ >LABEL ;

\ DIE ( rdi = the exit status, rsi = the text, 1 to DIE-LINE-ROOM bytes, rdx =
\ its length, rax = a value, rcx = nonzero to print it ): entered by a jump, so
\ the qword at rsp is the lr it prints. Writes `<text>[ <rax>] lr <lr>`, each
\ `0x` and 16 hex digits, and a newline to fd 2 in one write, then exits.
: EMIT-DIE-LINE ( -- )
   X64CODE:LBL {: bare:label :}
   DIE-LBL X64CODE:LBL,
   X64ASM:R10 X64ASM:RDI MOV,
   X64ASM:RSP FAULT-RECORD:FAULT-FRAME X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI32
   FRAME-REG X64ASM:RSP MOV,  CUR-REG X64ASM:RSP MOV,
   X64ASM:RAX PUSH,  X64ASM:RCX PUSH,
   X64ASM:RDI ZERO-REG,  PUT-LBL X64CODE:CALL,
   X64ASM:RCX POP,  X64ASM:RAX POP,
   X64ASM:RCX TEST,  X64ASM:C-E bare X64CODE:JCC,
   $20 PUT-CHAR,  $30 PUT-CHAR,  $78 PUT-CHAR,  HEX,
   bare X64CODE:LBL,
   $20 PUT-CHAR,  $6C PUT-CHAR,  $72 PUT-CHAR,  $20 PUT-CHAR,
   $30 PUT-CHAR,  $78 PUT-CHAR,
   X64ASM:RAX FRAME-REG FAULT-RECORD:FAULT-FRAME X64ASM:MEM-OFF LOAD,  HEX,
   $A PUT-CHAR,
   FLUSH,
   X64ASM:RDI X64ASM:R10 MOV,  NR-EXIT-GROUP SYS, ;

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
   X64CODE:LBL {: a:^ u:n status:n flag:n msg:label :}
   u 0= u DIE-LINE-ROOM > or IF a u BAD-DIE-TEXT THEN
   X64ASM:RDI status IMM32,  X64ASM:RSI msg X64CODE:MOVABS,  X64ASM:RDX u IMM32,  X64ASM:RCX flag IMM32,
   DIE-LBL X64CODE:JMP,
   msg X64CODE:LBL,  a u X64CODE:BYTES, ;

public
\ A jump to DIE with the text and the exit status, the text laid after it. The
\ caller places its own label before it, and rsp where its qword is the lr.
: DIE-LINE, ( str n -- )  0 DIE-LINE-SITE, ;

\ DIE-LINE, that also prints the register's value; the register is copied
\ first, so it may be any of rax, rcx, rdx, rsi and rdi.
: DIE-LINE-VALUE, ( str n X64ASM:r64 -- )
   {: a:^ u:n status:n reg:X64ASM:r64 :}
   reg X64ASM:R64>N X64ASM:RAX X64ASM:R64>N <> IF X64ASM:RAX reg MOV, THEN
   a u status 1 DIE-LINE-SITE, ;

public
\ Opens a record with its code and message: rsp drops FAULT-FRAME, r9 = its
\ start and r8 the cursor, r10 = the JSON flag.
: FAULT-OPEN, ( str str -- FAULT-RECORD:record )
   FAULT-RECORD:FAULT-OPEN
   X64ASM:RSP FAULT-RECORD:FAULT-FRAME X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI32
   FRAME-REG X64ASM:RSP MOV,  CUR-REG X64ASM:RSP MOV,
   JSON-REG LINUX-LAYOUT:DATA-VA VA>N REJECTION-JSON-CELL + IMM,
   JSON-REG JSON-REG X64ASM:MEM-AT LOAD,
   PIECE, ;

\ Note `in`: the word whose code holds the address in rdi, read from the rdx
\ records at rsi; rsi = 0 when the site cannot trust the dictionary. With no
\ such word, note `at` and the address.
: FAULT-IN, ( FAULT-RECORD:record -- FAULT-RECORD:record )
   X64CODE:LBL X64CODE:LBL {: at:label done:label :}
   FAULT-RECORD:FAULT-IN {: was:n :}
   OWNER-LBL X64CODE:CALL,
   X64ASM:RSI TEST,  X64ASM:C-E at X64CODE:JCC,
   X64ASM:RSI PUSH,  X64ASM:RDX PUSH,
   PIECE,
   X64ASM:RDX POP,  X64ASM:RSI POP,  X64ASM:RDI JSON-REG MOV,  PUT-LBL X64CODE:CALL,
   done X64CODE:JMP,
   at X64CODE:LBL,
   X64ASM:RDI PUSH,
   was FAULT-RECORD:FAULT-AT  PIECE,
   X64ASM:RAX POP,  HEX,
   done X64CODE:LBL, ;

\ A note with the register's value in decimal.
: FAULT-NUMBER, ( FAULT-RECORD:record str X64ASM:r64 -- FAULT-RECORD:record )
   {: k:^ ku:n r:X64ASM:r64 :}
   r PUSH,
   k ku FAULT-RECORD:NOTE+  PIECE,
   X64ASM:RAX POP,  DEC-LBL X64CODE:CALL, ;

\ A note with the bytes at the pointer register, the length register's count
\ of them.
: FAULT-TEXT, ( FAULT-RECORD:record str X64ASM:r64 X64ASM:r64 -- FAULT-RECORD:record )
   {: k:^ ku:n p:X64ASM:r64 l:X64ASM:r64 :}
   p PUSH,  l PUSH,
   k ku FAULT-RECORD:NOTE+  PIECE,
   X64ASM:RDX POP,  X64ASM:RSI POP,  X64ASM:RDI JSON-REG MOV,  PUT-LBL X64CODE:CALL, ;

\ A note with the address in the register, `0x` and 16 hex digits.
: FAULT-ADDRESS, ( FAULT-RECORD:record str X64ASM:r64 -- FAULT-RECORD:record )
   {: k:^ ku:n r:X64ASM:r64 :}
   r PUSH,
   k ku FAULT-RECORD:FAULT-ADDRESS  PIECE,
   X64ASM:RAX POP,  HEX, ;

\ Ends the record with its help and exits with the status.
: FAULT-STOP, ( FAULT-RECORD:record str n -- )
   {: h:^ hu:n status:n :}
   h hu FAULT-RECORD:FAULT-STOP
   PIECE,  FLUSH,
   X64ASM:RDI status IMM32,  NR-EXIT-GROUP SYS, ;

\ Ends the record with its help, none for an empty text, and returns, rsp as
\ FAULT-OPEN, found it.
: FAULT-KEEP, ( FAULT-RECORD:record str -- )
   FAULT-RECORD:FAULT-KEEP
   PIECE,  FLUSH,
   X64ASM:RSP FAULT-RECORD:FAULT-FRAME X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI32 ;

\ Makes the routines' labels and emits the routines, the die line's included,
\ where no control falls in; a site emitted after it in the stream reaches them.
: EMIT-FAULT-RECORD ( -- )
   X64CODE:LBL PUT-CELL !  X64CODE:LBL OWNER-CELL !  X64CODE:LBL DEC-CELL !  X64CODE:LBL DIE-CELL !
   EMIT-FAULT-PUT  EMIT-FAULT-OWNER  EMIT-FAULT-DEC  EMIT-DIE-LINE ;

;package
