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
64 constant MAX-RELOCS
32 constant MAX-OBJS
$4000 constant SYM-CAP

create SYM-BUF SYM-CAP allot
create EXP-OFFS MAX-SYMS cells allot
create EXP-US MAX-SYMS cells allot
create IMP-OFFS MAX-SYMS cells allot
create IMP-US MAX-SYMS cells allot
create DEF-OFFS MAX-SYMS cells allot
create DEF-US MAX-SYMS cells allot
create DEF-ADDRS MAX-SYMS cells allot
create REL-KIND-OFFS MAX-RELOCS cells allot
create REL-KIND-US MAX-RELOCS cells allot
create REL-SYM-OFFS MAX-RELOCS cells allot
create REL-SYM-US MAX-RELOCS cells allot
create REL-PATCHES MAX-RELOCS cells allot
create REL-TARGETS MAX-RELOCS cells allot
create OBJ-TEXT-BASES MAX-OBJS cells allot
create OBJ-DATA-BASES MAX-OBJS cells allot
create OBJ-TEXT-US MAX-OBJS cells allot
create OBJ-DATA-US MAX-OBJS cells allot

variable SYM-U
variable EXP-N
variable IMP-N
variable DEF-N
variable REL-N
variable OBJ-N
variable TEXT-U
variable DATA-U
variable CUR-TEXT
variable CUR-DATA

: TRUE ( -- bool )
   0 0= ;

: FALSE ( -- bool )
   TRUE 0= ;

: CLEAR ( -- )
   0 SYM-U !
   0 EXP-N !
   0 IMP-N !
   0 DEF-N !
   0 REL-N !
   0 OBJ-N !
   0 TEXT-U !
   0 DATA-U ! ;

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

: DEF-OFF-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-SYMS CHECK-IDX
   DEF-OFFS idx cells + ;

: DEF-U-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-SYMS CHECK-IDX
   DEF-US idx cells + ;

: DEF-ADDR-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-SYMS CHECK-IDX
   DEF-ADDRS idx cells + ;

: REL-KIND-OFF-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-RELOCS CHECK-IDX
   REL-KIND-OFFS idx cells + ;

: REL-KIND-U-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-RELOCS CHECK-IDX
   REL-KIND-US idx cells + ;

: REL-SYM-OFF-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-RELOCS CHECK-IDX
   REL-SYM-OFFS idx cells + ;

: REL-SYM-U-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-RELOCS CHECK-IDX
   REL-SYM-US idx cells + ;

: REL-PATCH-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-RELOCS CHECK-IDX
   REL-PATCHES idx cells + ;

: REL-TARGET-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-RELOCS CHECK-IDX
   REL-TARGETS idx cells + ;

: OBJ-TEXT-BASE-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-OBJS CHECK-IDX
   OBJ-TEXT-BASES idx cells + ;

: OBJ-DATA-BASE-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-OBJS CHECK-IDX
   OBJ-DATA-BASES idx cells + ;

: OBJ-TEXT-U-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-OBJS CHECK-IDX
   OBJ-TEXT-US idx cells + ;

: OBJ-DATA-U-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-OBJS CHECK-IDX
   OBJ-DATA-US idx cells + ;

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

: DEF-ROOM ( -- )
   DEF-N @ MAX-SYMS >= if E-OBJ-CAPACITY throw then ;

: REL-ROOM ( -- )
   REL-N @ MAX-RELOCS >= if E-OBJ-CAPACITY throw then ;

: OBJECT-ROOM ( -- )
   OBJ-N @ MAX-OBJS >= if E-OBJ-CAPACITY throw then ;

: EXP$ ( n -- ptr u8 n ) {: idx:n :}
   idx EXP-N @ CHECK-IDX
   SYM-BUF idx EXP-OFF-PTR @ + idx EXP-U-PTR @ ;

: IMP$ ( n -- ptr u8 n ) {: idx:n :}
   idx IMP-N @ CHECK-IDX
   SYM-BUF idx IMP-OFF-PTR @ + idx IMP-U-PTR @ ;

: DEF-SYM$ ( n -- ptr u8 n ) {: idx:n :}
   idx DEF-N @ CHECK-IDX
   SYM-BUF idx DEF-OFF-PTR @ + idx DEF-U-PTR @ ;

: REL-KIND$ ( n -- ptr u8 n ) {: idx:n :}
   idx REL-N @ CHECK-IDX
   SYM-BUF idx REL-KIND-OFF-PTR @ + idx REL-KIND-U-PTR @ ;

: REL-SYM$ ( n -- ptr u8 n ) {: idx:n :}
   idx REL-N @ CHECK-IDX
   SYM-BUF idx REL-SYM-OFF-PTR @ + idx REL-SYM-U-PTR @ ;

: EXP-MATCH? ( ptr u8 n n -- bool ) {: a:ptr u:n idx:n :}
   idx EXP$ a u STR= ;

: DEF-MATCH? ( ptr u8 n n -- bool ) {: a:ptr u:n idx:n :}
   idx DEF-SYM$ a u STR= ;

: EXP-FIND? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup EXP-N @ < while
      dup a u rot EXP-MATCH? if drop TRUE exit then
      1+
   repeat drop FALSE ;

: DEF-IDX ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 begin dup DEF-N @ < while
      dup a u rot DEF-MATCH? if exit then
      1+
   repeat drop -1 ;

: DEF-HAS? ( ptr u8 n -- bool )
   DEF-IDX 0 >= ;

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

: DEF+ ( ptr u8 n n -- ) {: a:ptr u:n addr:n :}
   a u DEF-HAS? if E-OBJ-SCHEMA throw then
   DEF-ROOM
   a u SYM+ {: off:n :}
   off DEF-N @ DEF-OFF-PTR !
   u DEF-N @ DEF-U-PTR !
   addr DEF-N @ DEF-ADDR-PTR !
   DEF-N @ 1+ DEF-N ! ;

: REL+ ( ptr u8 n ptr u8 n n -- )
   {: kind:ptr kindu:n sym:ptr symu:n patch:n :}
   REL-ROOM
   kind kindu SYM+ {: ko:n :}
   sym symu SYM+ {: so:n :}
   ko REL-N @ REL-KIND-OFF-PTR !
   kindu REL-N @ REL-KIND-U-PTR !
   so REL-N @ REL-SYM-OFF-PTR !
   symu REL-N @ REL-SYM-U-PTR !
   patch REL-N @ REL-PATCH-PTR !
   -1 REL-N @ REL-TARGET-PTR !
   REL-N @ 1+ REL-N ! ;

: TEXT-OFF-CHECK ( n -- ) {: off:n :}
   off 0 < if E-OBJ-SCHEMA throw then
   off CUR-TEXT @ >= if E-OBJ-SCHEMA throw then ;

: ROW-OFF ( n n -- n ) {: row:n field:n :}
   row field OBJ:ROW-FIELD$ STR>NUMBER? if exit then
   drop E-OBJ-FIELD throw ;

: ADD-DEF ( n -- ) {: row:n :}
   row 1 ROW-OFF {: off:n :}
   off TEXT-OFF-CHECK
   row 0 OBJ:ROW-FIELD$ TEXT-U @ off + DEF+ ;

: ADD-RELOC ( n -- ) {: row:n :}
   row 1 ROW-OFF {: off:n :}
   off TEXT-OFF-CHECK
   row 0 OBJ:ROW-FIELD$
   row 2 OBJ:ROW-FIELD$
   TEXT-U @ off + REL+ ;

: ADD-ROW ( n -- ) {: row:n :}
   row OBJ:ROW-TAG$ s" export" STR= if
      row 0 OBJ:ROW-FIELD$ EXP+
      exit
   then
   row OBJ:ROW-TAG$ s" import" STR= if
      row 0 OBJ:ROW-FIELD$ IMP+
      exit
   then
   row OBJ:ROW-TAG$ s" def" STR= if
      row ADD-DEF
      exit
   then
   row OBJ:ROW-TAG$ s" reloc" STR= if
      row ADD-RELOC
      exit
   then ;

: IMP-RESOLVED? ( n -- bool ) {: idx:n :}
   idx IMP$ EXP-FIND? ;

: ROW-TAG= ( n ptr u8 n -- bool ) {: row:n tag:ptr tagu:n :}
   row OBJ:ROW-TAG$ tag tagu STR= ;

: ROW-HEX-U ( n -- n ) {: row:n :}
   row 0 OBJ:ROW-FIELD$ nip {: hexu:n :}
   hexu 1 and 0 <> if E-OBJ-FIELD throw then
   hexu 2 / ;

: SIZE-ROW ( n -- ) {: row:n :}
   row s" text" ROW-TAG= if
      CUR-TEXT @ row ROW-HEX-U + CUR-TEXT !
      exit
   then
   row s" data" ROW-TAG= if
      CUR-DATA @ row ROW-HEX-U + CUR-DATA !
      exit
   then ;

: SCAN-SIZES ( -- )
   0 CUR-TEXT !
   0 CUR-DATA !
   0 begin dup OBJ:ROW-COUNT < while
      dup SIZE-ROW
      1+
   repeat drop ;

: RECORD-OBJECT ( -- )
   OBJECT-ROOM
   TEXT-U @ OBJ-N @ OBJ-TEXT-BASE-PTR !
   DATA-U @ OBJ-N @ OBJ-DATA-BASE-PTR !
   CUR-TEXT @ OBJ-N @ OBJ-TEXT-U-PTR !
   CUR-DATA @ OBJ-N @ OBJ-DATA-U-PTR ! ;

: RESOLVE-RELOC ( n -- ) {: idx:n :}
   idx REL-SYM$ DEF-IDX {: def:n :}
   def 0 < if E-OBJ-SCHEMA throw then
   def DEF-ADDR-PTR @ idx REL-TARGET-PTR ! ;

: CHECK-IMPORTS ( -- )
   0 begin dup IMP-N @ < while
      dup IMP-RESOLVED? 0= if E-OBJ-SCHEMA throw then
      1+
   repeat drop ;

: CHECK-RELOCS ( -- )
   0 begin dup REL-N @ < while
      dup RESOLVE-RELOC
      1+
   repeat drop ;

public

: RESET ( -- )
   CLEAR ;

: EXPORT-COUNT ( -- n )
   EXP-N @ ;

: IMPORT-COUNT ( -- n )
   IMP-N @ ;

: DEF-COUNT ( -- n )
   DEF-N @ ;

: RELOC-COUNT ( -- n )
   REL-N @ ;

: OBJECT-COUNT ( -- n )
   OBJ-N @ ;

: TEXT-SIZE ( -- n )
   TEXT-U @ ;

: DATA-SIZE ( -- n )
   DATA-U @ ;

: OBJECT-TEXT-BASE ( n -- n ) {: idx:n :}
   idx OBJ-N @ CHECK-IDX
   idx OBJ-TEXT-BASE-PTR @ ;

: OBJECT-DATA-BASE ( n -- n ) {: idx:n :}
   idx OBJ-N @ CHECK-IDX
   idx OBJ-DATA-BASE-PTR @ ;

: OBJECT-TEXT-SIZE ( n -- n ) {: idx:n :}
   idx OBJ-N @ CHECK-IDX
   idx OBJ-TEXT-U-PTR @ ;

: OBJECT-DATA-SIZE ( n -- n ) {: idx:n :}
   idx OBJ-N @ CHECK-IDX
   idx OBJ-DATA-U-PTR @ ;

: EXPORT$ ( n -- ptr u8 n )
   EXP$ ;

: IMPORT$ ( n -- ptr u8 n )
   IMP$ ;

: DEF$ ( n -- ptr u8 n )
   DEF-SYM$ ;

: RELOC-KIND$ ( n -- ptr u8 n )
   REL-KIND$ ;

: RELOC-SYM$ ( n -- ptr u8 n )
   REL-SYM$ ;

: DEF-ADDR ( n -- n ) {: idx:n :}
   idx DEF-N @ CHECK-IDX
   idx DEF-ADDR-PTR @ ;

: RELOC-PATCH ( n -- n ) {: idx:n :}
   idx REL-N @ CHECK-IDX
   idx REL-PATCH-PTR @ ;

: RELOC-TARGET ( n -- n ) {: idx:n :}
   idx REL-N @ CHECK-IDX
   idx REL-TARGET-PTR @ dup 0 < if E-OBJ-SCHEMA throw then ;

: EXPORT-FIND? ( ptr u8 n -- bool )
   EXP-FIND? ;

: DEF-FIND? ( ptr u8 n -- bool )
   DEF-HAS? ;

: EXPORT+ ( ptr u8 n -- )
   EXP+ ;

: IMPORT+ ( ptr u8 n -- )
   IMP+ ;

: ADD ( -- )
   SCAN-SIZES
   0 begin dup OBJ:ROW-COUNT < while
      dup ADD-ROW
      1+
   repeat drop
   RECORD-OBJECT
   TEXT-U @ CUR-TEXT @ + TEXT-U !
   DATA-U @ CUR-DATA @ + DATA-U !
   OBJ-N @ 1+ OBJ-N ! ;

: CHECK ( -- )
   CHECK-IMPORTS
   CHECK-RELOCS ;

end-package
