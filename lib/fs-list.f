\ fs-list.f - the entry names of one directory.
\
\ EACH hands every name except . and .. to a quotation in directory order.
\ NAMES writes the names into a caller buffer, newline-separated and in byte
\ order, so two listings of the same directory compare equal. Only the raw
\ open-rd, getdirentries64 and close primitives are involved; the record
\ decoding is shared with the walker in lib/fs.f.
\
\ Load after lib/errors.f and lib/fs.f.
require lib/errors.f
require lib/fs.f

package FS-LIST
public

E-FS-LIST-OPEN constant E-OPEN
E-FS-LIST-READ constant E-READ
E-FS-LIST-ENTRY constant E-ENTRY
E-FS-LIST-CAPACITY constant E-CAPACITY

private

10 constant LF
4096 constant BLOCK-CAP
create BLOCK BLOCK-CAP allot
create DIR-BASE 1 cells allot
variable FD
variable FILLED
variable OFFSET
variable OUT
variable OUT-LEN
variable OUT-CAP
variable SCAN
-1 FD !


: OUT-BASE ( -- ptr u8 ) OUT @ {: start:ptr :} start ;


: CLOSE ( -- )
   FD @ 0 >= if FD @ close then -1 FD ! ;


: FAIL ( n -- ) CLOSE throw ;


\ A listing abandoned by a throw from its quotation leaves FD open until the next OPEN.
: OPEN ( ptr u8 n -- )
   CLOSE
   FS-PATHZ open-rd {: handle:n :}
   handle 0 < if E-OPEN throw then
   handle FD ! ;


: READ-BLOCK ( -- bool )
   FD @ BLOCK BLOCK-CAP DIR-BASE getdirentries64 {: got:n :}
   got 0 < if E-READ FAIL then
   got FILLED ! 0 OFFSET ! got 0 > ;


\ The name of the record at OFFSET, which then moves past the record.
: RECORD ( -- ptr u8 n )
   BLOCK OFFSET @ + {: ent:ptr :}
   ent FS-DIRENT-RECLEN {: rec:n :}
   rec 0 <= OFFSET @ rec + FILLED @ > or if E-ENTRY FAIL then
   ent FS-DIRENT-NAME-END rec > if E-ENTRY FAIL then
   ent FS-DIRENT-NAME
   rec OFFSET +! ;


\ TRUE when a sorts before b in byte order.
: BEFORE? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n b:ptr v:n :}
   u v min {: shared:n :}
   shared 0 ?do
      a i + c@ b i + c@ <> if a i + c@ b i + c@ < unloop exit then
   loop
   u v < ;


: LINE-END ( n -- n )
   begin dup OUT-LEN @ < if dup OUT-BASE swap + c@ LF <> else FALSE then while 1+ repeat ;


\ The offset in OUT where name belongs among the names written so far.
: INSERT-AT ( ptr u8 n -- n ) {: name:ptr size:n :}
   0 SCAN !
   begin SCAN @ OUT-LEN @ < while
      SCAN @ LINE-END {: stop:n :}
      name size OUT-BASE SCAN @ + stop SCAN @ - BEFORE? if SCAN @ exit then
      stop 1+ SCAN !
   repeat
   SCAN @ OUT-LEN @ min ;


: INSERT ( ptr u8 n -- ) {: name:ptr size:n :}
   size 0 ?do name i + c@ LF = if E-ENTRY FAIL then loop
   OUT-LEN @ 0 > if 1 else 0 then {: separator:n :}
   OUT-LEN @ size + separator + OUT-CAP @ > if E-CAPACITY FAIL then
   name size INSERT-AT {: at:n :}
   at OUT-LEN @ = if
      separator 0 <> if LF OUT-BASE OUT-LEN @ + c! 1 OUT-LEN +! then
      name OUT-BASE OUT-LEN @ + size BYTE-COPY
      size OUT-LEN +!
   else
      OUT-LEN @ at - {: tail:n :}
      tail 0 ?do
         OUT-BASE OUT-LEN @ 1- i - + c@ OUT-BASE OUT-LEN @ 1- i - size + 1+ + c!
      loop
      name OUT-BASE at + size BYTE-COPY
      LF OUT-BASE at + size + c!
      size 1+ OUT-LEN +!
   then ;

public

\ Hands each name except . and .. to the quotation, in directory order.
: EACH ( ptr u8 n [ ptr u8 n -- ] -- ) {: path:ptr size:n q :}
   path size OPEN
   begin READ-BLOCK while
      begin OFFSET @ FILLED @ < while
         RECORD 2dup FS-SKIP-SELF-ENTRY? if 2drop else q execute then
      repeat
   repeat
   CLOSE ;


\ Writes the names except . and .. into the caller's buffer, newline-separated
\ and in byte order, and returns the length; names holding a newline are refused.
: NAMES ( ptr u8 n ptr u8 n -- n ) {: path:ptr size:n dst:ptr cap:n :}
   cap 0 < if E-CAPACITY throw then
   dst OUT ! cap OUT-CAP ! 0 OUT-LEN !
   path size [: INSERT ;] EACH
   OUT-LEN @ ;

;package
