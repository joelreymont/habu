\ object-link.f - checked object symbol validation.
\
\ Load after lib/object.f.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/content-key.f
require lib/object.f

package OBJLINK

32 constant MAX-SYMS
$4000 constant SYM-CAP

create SYM-BUF SYM-CAP allot
create EXP-OFFS MAX-SYMS cells allot
create EXP-US MAX-SYMS cells allot
create IMP-OFFS MAX-SYMS cells allot
create IMP-US MAX-SYMS cells allot

variable SYM-U
variable EXP-N
variable IMP-N

: TRUE ( -- bool )
   0 0= ;

: FALSE ( -- bool )
   TRUE 0= ;

: CLEAR ( -- )
   0 SYM-U !
   0 EXP-N !
   0 IMP-N ! ;

: CHECK-IDX ( n n -- ) {: idx:n cap:n :}
   idx 0 < if E-OBJ-FIELD throw then
   idx cap >= if E-OBJ-FIELD throw then ;

: EXP-OFF-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-SYMS CHECK-IDX
   EXP-OFFS idx cells + ;

: EXP-U-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-SYMS CHECK-IDX
   EXP-US idx cells + ;

: IMP-OFF-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-SYMS CHECK-IDX
   IMP-OFFS idx cells + ;

: IMP-U-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-SYMS CHECK-IDX
   IMP-US idx cells + ;

: SYM-ROOM ( n -- ) {: u:n :}
   u 0 <= if E-OBJ-FIELD throw then
   SYM-U @ u + SYM-CAP > if E-OBJ-CAPACITY throw then ;

: SYM+ ( ptr u8 n -- n ) {: a:ptr u:n :}
   u SYM-ROOM
   SYM-U @ {: off:n :}
   a SYM-BUF off + u BYTE-COPY
   off u + SYM-U !
   off ;

: EXPORT-ROOM ( -- )
   EXP-N @ MAX-SYMS >= if E-OBJ-CAPACITY throw then ;

: IMPORT-ROOM ( -- )
   IMP-N @ MAX-SYMS >= if E-OBJ-CAPACITY throw then ;

: EXP$ ( n -- ptr u8 n ) {: idx:n :}
   idx EXP-N @ CHECK-IDX
   SYM-BUF idx EXP-OFF-PTR @ + idx EXP-U-PTR @ ;

: IMP$ ( n -- ptr u8 n ) {: idx:n :}
   idx IMP-N @ CHECK-IDX
   SYM-BUF idx IMP-OFF-PTR @ + idx IMP-U-PTR @ ;

: EXP-MATCH? ( ptr u8 n n -- bool ) {: a:ptr u:n idx:n :}
   idx EXP$ a u STR= ;

: EXP-FIND? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup EXP-N @ < while
      dup a u rot EXP-MATCH? if drop TRUE exit then
      1+
   repeat drop FALSE ;

: EXP+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a u EXP-FIND? if E-OBJ-SCHEMA throw then
   EXPORT-ROOM
   a u SYM+ {: off:n :}
   off EXP-N @ EXP-OFF-PTR !
   u EXP-N @ EXP-U-PTR !
   EXP-N @ 1+ EXP-N ! ;

: IMP+ ( ptr u8 n -- ) {: a:ptr u:n :}
   IMPORT-ROOM
   a u SYM+ {: off:n :}
   off IMP-N @ IMP-OFF-PTR !
   u IMP-N @ IMP-U-PTR !
   IMP-N @ 1+ IMP-N ! ;

: ADD-ROW ( n -- ) {: row:n :}
   row OBJ:ROW-TAG$ s" export" STR= if
      row 0 OBJ:ROW-FIELD$ EXP+
      exit
   then
   row OBJ:ROW-TAG$ s" import" STR= if
      row 0 OBJ:ROW-FIELD$ IMP+
      exit
   then ;

: IMP-RESOLVED? ( n -- bool ) {: idx:n :}
   idx IMP$ EXP-FIND? ;

public

: RESET ( -- )
   CLEAR ;

: EXPORT-COUNT ( -- n )
   EXP-N @ ;

: IMPORT-COUNT ( -- n )
   IMP-N @ ;

: EXPORT$ ( n -- ptr u8 n )
   EXP$ ;

: IMPORT$ ( n -- ptr u8 n )
   IMP$ ;

: EXPORT-FIND? ( ptr u8 n -- bool )
   EXP-FIND? ;

: EXPORT+ ( ptr u8 n -- )
   EXP+ ;

: IMPORT+ ( ptr u8 n -- )
   IMP+ ;

: ADD ( -- )
   0 begin dup OBJ:ROW-COUNT < while
      dup ADD-ROW
      1+
   repeat drop ;

: CHECK ( -- )
   0 begin dup IMP-N @ < while
      dup IMP-RESOLVED? 0= if E-OBJ-SCHEMA throw then
      1+
   repeat drop ;

end-package
