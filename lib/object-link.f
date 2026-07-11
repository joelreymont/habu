\ object-link.f - checked object symbol validation.
\
\ Load after lib/object.f.

require lib/errors.f
require lib/string.f
require lib/adt/option.f                 \ option<n> STR>NUMBER? consumer (switchover wave A)
require lib/memory.f
require lib/fs.f
require lib/content-key.f
require lib/object.f

package OBJLINK

32 constant MAX-SYMS
64 constant MAX-RELOCS
32 constant MAX-OBJS
$4000 constant SYM-CAP
$10000 constant MERGE-CAP
8 constant ABS64-U
65 constant HEX-UP-A
71 constant HEX-UP-G
97 constant HEX-LOW-A
103 constant HEX-LOW-G

create SYM-BUF SYM-CAP allot
create TEXT-BUF MERGE-CAP allot
create DATA-BUF MERGE-CAP allot
create PKG-OFFS MAX-SYMS cells allot
create PKG-US MAX-SYMS cells allot
create PKG-VIS-OFFS MAX-SYMS cells allot
create PKG-VIS-US MAX-SYMS cells allot
create REQ-OFFS MAX-SYMS cells allot
create REQ-US MAX-SYMS cells allot
create TYPE-OFFS MAX-SYMS cells allot
create TYPE-US MAX-SYMS cells allot
create TYPE-KIND-OFFS MAX-SYMS cells allot
create TYPE-KIND-US MAX-SYMS cells allot
create NORET-OFFS MAX-SYMS cells allot
create NORET-US MAX-SYMS cells allot
create EXP-OFFS MAX-SYMS cells allot
create EXP-US MAX-SYMS cells allot
create EXP-EFF-OFFS MAX-SYMS cells allot
create EXP-EFF-US MAX-SYMS cells allot
create IMP-OFFS MAX-SYMS cells allot
create IMP-US MAX-SYMS cells allot
create IMP-EFF-OFFS MAX-SYMS cells allot
create IMP-EFF-US MAX-SYMS cells allot
create DEF-OFFS MAX-SYMS cells allot
create DEF-US MAX-SYMS cells allot
create DEF-EFF-OFFS MAX-SYMS cells allot
create DEF-EFF-US MAX-SYMS cells allot
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
variable PKG-N
variable REQ-N
variable TYPE-N
variable NORET-N
variable EXP-N
variable IMP-N
variable DEF-N
variable REL-N
variable OBJ-N
variable TEXT-U
variable DATA-U
variable CUR-TEXT
variable CUR-DATA
variable APP-TEXT
variable APP-DATA

: TRUE ( -- bool )
   0 0= ;

: FALSE ( -- bool )
   TRUE 0= ;

: CLEAR ( -- )
   0 SYM-U !
   0 PKG-N !
   0 REQ-N !
   0 TYPE-N !
   0 NORET-N !
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

: PKG-OFF-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-SYMS CHECK-IDX
   PKG-OFFS idx cells + ;

: PKG-U-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-SYMS CHECK-IDX
   PKG-US idx cells + ;

: PKG-VIS-OFF-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-SYMS CHECK-IDX
   PKG-VIS-OFFS idx cells + ;

: PKG-VIS-U-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-SYMS CHECK-IDX
   PKG-VIS-US idx cells + ;

: REQ-OFF-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-SYMS CHECK-IDX
   REQ-OFFS idx cells + ;

: REQ-U-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-SYMS CHECK-IDX
   REQ-US idx cells + ;

: TYPE-OFF-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-SYMS CHECK-IDX
   TYPE-OFFS idx cells + ;

: TYPE-U-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-SYMS CHECK-IDX
   TYPE-US idx cells + ;

: TYPE-KIND-OFF-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-SYMS CHECK-IDX
   TYPE-KIND-OFFS idx cells + ;

: TYPE-KIND-U-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-SYMS CHECK-IDX
   TYPE-KIND-US idx cells + ;

: NORET-OFF-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-SYMS CHECK-IDX
   NORET-OFFS idx cells + ;

: NORET-U-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-SYMS CHECK-IDX
   NORET-US idx cells + ;

: EXP-OFF-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-SYMS CHECK-IDX
   EXP-OFFS idx cells + ;

: EXP-U-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-SYMS CHECK-IDX
   EXP-US idx cells + ;

: EXP-EFF-OFF-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-SYMS CHECK-IDX
   EXP-EFF-OFFS idx cells + ;

: EXP-EFF-U-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-SYMS CHECK-IDX
   EXP-EFF-US idx cells + ;

: IMP-OFF-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-SYMS CHECK-IDX
   IMP-OFFS idx cells + ;

: IMP-U-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-SYMS CHECK-IDX
   IMP-US idx cells + ;

: IMP-EFF-OFF-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-SYMS CHECK-IDX
   IMP-EFF-OFFS idx cells + ;

: IMP-EFF-U-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-SYMS CHECK-IDX
   IMP-EFF-US idx cells + ;

: DEF-OFF-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-SYMS CHECK-IDX
   DEF-OFFS idx cells + ;

: DEF-U-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-SYMS CHECK-IDX
   DEF-US idx cells + ;

: DEF-EFF-OFF-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-SYMS CHECK-IDX
   DEF-EFF-OFFS idx cells + ;

: DEF-EFF-U-PTR ( n -- ptr n ) {: idx:n :}
   idx MAX-SYMS CHECK-IDX
   DEF-EFF-US idx cells + ;

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

: MERGE-ROOM ( n n -- ) {: have:n add:n :}
   add 0 < if E-OBJ-CAPACITY throw then
   have add + MERGE-CAP > if E-OBJ-CAPACITY throw then ;

: EXPORT-ROOM ( -- )
   EXP-N @ MAX-SYMS >= if E-OBJ-CAPACITY throw then ;

: PACKAGE-ROOM ( -- )
   PKG-N @ MAX-SYMS >= if E-OBJ-CAPACITY throw then ;

: REQUIRE-ROOM ( -- )
   REQ-N @ MAX-SYMS >= if E-OBJ-CAPACITY throw then ;

: TYPE-ROOM ( -- )
   TYPE-N @ MAX-SYMS >= if E-OBJ-CAPACITY throw then ;

: NORET-ROOM ( -- )
   NORET-N @ MAX-SYMS >= if E-OBJ-CAPACITY throw then ;

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

: PKG$ ( n -- ptr u8 n ) {: idx:n :}
   idx PKG-N @ CHECK-IDX
   SYM-BUF idx PKG-OFF-PTR @ + idx PKG-U-PTR @ ;

: PKG-VIS$ ( n -- ptr u8 n ) {: idx:n :}
   idx PKG-N @ CHECK-IDX
   SYM-BUF idx PKG-VIS-OFF-PTR @ + idx PKG-VIS-U-PTR @ ;

: REQ$ ( n -- ptr u8 n ) {: idx:n :}
   idx REQ-N @ CHECK-IDX
   SYM-BUF idx REQ-OFF-PTR @ + idx REQ-U-PTR @ ;

: TYPE-SYM$ ( n -- ptr u8 n ) {: idx:n :}
   idx TYPE-N @ CHECK-IDX
   SYM-BUF idx TYPE-OFF-PTR @ + idx TYPE-U-PTR @ ;

: TYPE-KIND-SYM$ ( n -- ptr u8 n ) {: idx:n :}
   idx TYPE-N @ CHECK-IDX
   SYM-BUF idx TYPE-KIND-OFF-PTR @ + idx TYPE-KIND-U-PTR @ ;

: NORET-SYM$ ( n -- ptr u8 n ) {: idx:n :}
   idx NORET-N @ CHECK-IDX
   SYM-BUF idx NORET-OFF-PTR @ + idx NORET-U-PTR @ ;

: IMP$ ( n -- ptr u8 n ) {: idx:n :}
   idx IMP-N @ CHECK-IDX
   SYM-BUF idx IMP-OFF-PTR @ + idx IMP-U-PTR @ ;

: DEF-SYM$ ( n -- ptr u8 n ) {: idx:n :}
   idx DEF-N @ CHECK-IDX
   SYM-BUF idx DEF-OFF-PTR @ + idx DEF-U-PTR @ ;

: EXP-EFF$ ( n -- ptr u8 n ) {: idx:n :}
   idx EXP-N @ CHECK-IDX
   SYM-BUF idx EXP-EFF-OFF-PTR @ + idx EXP-EFF-U-PTR @ ;

: IMP-EFF$ ( n -- ptr u8 n ) {: idx:n :}
   idx IMP-N @ CHECK-IDX
   SYM-BUF idx IMP-EFF-OFF-PTR @ + idx IMP-EFF-U-PTR @ ;

: DEF-EFF$ ( n -- ptr u8 n ) {: idx:n :}
   idx DEF-N @ CHECK-IDX
   SYM-BUF idx DEF-EFF-OFF-PTR @ + idx DEF-EFF-U-PTR @ ;

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

: EXP-IDX ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 begin dup EXP-N @ < while
      dup a u rot EXP-MATCH? if exit then
      1+
   repeat drop -1 ;

: EXP-FIND? ( ptr u8 n -- bool )
   EXP-IDX 0 >= ;

: DEF-IDX ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 begin dup DEF-N @ < while
      dup a u rot DEF-MATCH? if exit then
      1+
   repeat drop -1 ;

: DEF-HAS? ( ptr u8 n -- bool )
   DEF-IDX 0 >= ;

: PKG+ ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n vis:ptr visu:n :}
   PACKAGE-ROOM
   a u SYM+ {: off:n :}
   vis visu SYM+ {: voff:n :}
   off PKG-N @ PKG-OFF-PTR !
   u PKG-N @ PKG-U-PTR !
   voff PKG-N @ PKG-VIS-OFF-PTR !
   visu PKG-N @ PKG-VIS-U-PTR !
   PKG-N @ 1+ PKG-N ! ;

: REQ+ ( ptr u8 n -- ) {: a:ptr u:n :}
   REQUIRE-ROOM
   a u SYM+ {: off:n :}
   off REQ-N @ REQ-OFF-PTR !
   u REQ-N @ REQ-U-PTR !
   REQ-N @ 1+ REQ-N ! ;

: TYPE+ ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n kind:ptr kindu:n :}
   TYPE-ROOM
   a u SYM+ {: off:n :}
   kind kindu SYM+ {: koff:n :}
   off TYPE-N @ TYPE-OFF-PTR !
   u TYPE-N @ TYPE-U-PTR !
   koff TYPE-N @ TYPE-KIND-OFF-PTR !
   kindu TYPE-N @ TYPE-KIND-U-PTR !
   TYPE-N @ 1+ TYPE-N ! ;

: NORET+ ( ptr u8 n -- ) {: a:ptr u:n :}
   NORET-ROOM
   a u SYM+ {: off:n :}
   off NORET-N @ NORET-OFF-PTR !
   u NORET-N @ NORET-U-PTR !
   NORET-N @ 1+ NORET-N ! ;

: EXP+ ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n eff:ptr effu:n :}
   a u EXP-FIND? if E-OBJ-SCHEMA throw then
   EXPORT-ROOM
   a u SYM+ {: off:n :}
   eff effu SYM+ {: eoff:n :}
   off EXP-N @ EXP-OFF-PTR !
   u EXP-N @ EXP-U-PTR !
   eoff EXP-N @ EXP-EFF-OFF-PTR !
   effu EXP-N @ EXP-EFF-U-PTR !
   EXP-N @ 1+ EXP-N ! ;

: IMP+ ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n eff:ptr effu:n :}
   IMPORT-ROOM
   a u SYM+ {: off:n :}
   eff effu SYM+ {: eoff:n :}
   off IMP-N @ IMP-OFF-PTR !
   u IMP-N @ IMP-U-PTR !
   eoff IMP-N @ IMP-EFF-OFF-PTR !
   effu IMP-N @ IMP-EFF-U-PTR !
   IMP-N @ 1+ IMP-N ! ;

: DEF+ ( ptr u8 n ptr u8 n n -- )
   {: a:ptr u:n eff:ptr effu:n addr:n :}
   a u DEF-HAS? if E-OBJ-SCHEMA throw then
   DEF-ROOM
   a u SYM+ {: off:n :}
   eff effu SYM+ {: eoff:n :}
   off DEF-N @ DEF-OFF-PTR !
   u DEF-N @ DEF-U-PTR !
   eoff DEF-N @ DEF-EFF-OFF-PTR !
   effu DEF-N @ DEF-EFF-U-PTR !
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

: HEX-NIB ( n -- n ) {: c:n :}
   c STR-ZERO >= c STR-ZERO 10 + < and if c STR-ZERO - exit then
   c HEX-LOW-A >= c HEX-LOW-G < and if c 87 - exit then
   c HEX-UP-A >= c HEX-UP-G < and if c 55 - exit then
   E-OBJ-FIELD throw ;

: HEX-BYTE@ ( ptr u8 -- n ) {: a:ptr :}
   a c@ HEX-NIB 4 lshift
   a 1 + c@ HEX-NIB or ;

: HEX-BYTE-I ( ptr u8 n -- n ) {: a:ptr idx:n :}
   a idx 2 * + HEX-BYTE@ ;

: HEX-BYTE! ( ptr u8 n ptr u8 n -- )
   {: a:ptr idx:n dst:ptr off:n :}
   a idx HEX-BYTE-I dst off + idx + c! ;

: HEX>BUF ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n dst:ptr off:n :}
   u 1 and 0 <> if E-OBJ-FIELD throw then
   u 2 / {: bytes:n :}
   0 begin dup bytes < while
      dup a swap dst off HEX-BYTE!
      1+
   repeat drop ;

: TEXT-OFF-CHECK ( n -- ) {: off:n :}
   off 0 < if E-OBJ-SCHEMA throw then
   off CUR-TEXT @ >= if E-OBJ-SCHEMA throw then ;

: ROW-OFF ( n n -- n ) {: row:n field:n :}
   row field OBJ:ROW-FIELD$ STR>NUMBER? MATCH option
     none OF E-OBJ-FIELD throw ENDOF
     some OF ENDOF
   ;MATCH ;

: ADD-DEF ( n -- ) {: row:n :}
   row 1 ROW-OFF {: off:n :}
   off TEXT-OFF-CHECK
   row 0 OBJ:ROW-FIELD$
   row 2 OBJ:ROW-FIELD$
   TEXT-U @ off + DEF+ ;

: ADD-RELOC ( n -- ) {: row:n :}
   row 1 ROW-OFF {: off:n :}
   off TEXT-OFF-CHECK
   row 0 OBJ:ROW-FIELD$
   row 2 OBJ:ROW-FIELD$
   TEXT-U @ off + REL+ ;

: APPEND-TEXT ( n -- ) {: row:n :}
   row 0 OBJ:ROW-FIELD$ {: a:ptr u:n :}
   u 2 / {: bytes:n :}
   a u TEXT-BUF TEXT-U @ APP-TEXT @ + HEX>BUF
   APP-TEXT @ bytes + APP-TEXT ! ;

: APPEND-DATA ( n -- ) {: row:n :}
   row 0 OBJ:ROW-FIELD$ {: a:ptr u:n :}
   u 2 / {: bytes:n :}
   a u DATA-BUF DATA-U @ APP-DATA @ + HEX>BUF
   APP-DATA @ bytes + APP-DATA ! ;

: APPEND-ROW ( n -- ) {: row:n :}
   row OBJ:ROW-TAG$ s" text" STR= if row APPEND-TEXT exit then
   row OBJ:ROW-TAG$ s" data" STR= if row APPEND-DATA exit then ;

: ADD-ROW ( n -- ) {: row:n :}
   row OBJ:ROW-TAG$ s" package" STR= if
      row 0 OBJ:ROW-FIELD$ row 1 OBJ:ROW-FIELD$ PKG+
      exit
   then
   row OBJ:ROW-TAG$ s" require" STR= if
      row 0 OBJ:ROW-FIELD$ REQ+
      exit
   then
   row OBJ:ROW-TAG$ s" type" STR= if
      row 0 OBJ:ROW-FIELD$ row 1 OBJ:ROW-FIELD$ TYPE+
      exit
   then
   row OBJ:ROW-TAG$ s" noret" STR= if
      row 0 OBJ:ROW-FIELD$ NORET+
      exit
   then
   row OBJ:ROW-TAG$ s" export" STR= if
      row 0 OBJ:ROW-FIELD$ row 1 OBJ:ROW-FIELD$ EXP+
      exit
   then
   row OBJ:ROW-TAG$ s" import" STR= if
      row 0 OBJ:ROW-FIELD$ row 1 OBJ:ROW-FIELD$ IMP+
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

: SECTION-ROOM ( -- )
   TEXT-U @ CUR-TEXT @ MERGE-ROOM
   DATA-U @ CUR-DATA @ MERGE-ROOM ;

: APPEND-SECTIONS ( -- )
   0 APP-TEXT !
   0 APP-DATA !
   0 begin dup OBJ:ROW-COUNT < while
      dup APPEND-ROW
      1+
   repeat drop
   APP-TEXT @ CUR-TEXT @ <> if E-OBJ-SCHEMA throw then
   APP-DATA @ CUR-DATA @ <> if E-OBJ-SCHEMA throw then ;

: IMP-RESOLVED? ( n -- bool ) {: idx:n :}
   idx IMP$ EXP-IDX {: exp:n :}
   exp 0 < if FALSE exit then
   idx IMP-EFF$ exp EXP-EFF$ STR= ;

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

: TEXT-RANGE ( n n -- ) {: off:n u:n :}
   off 0 < if E-OBJ-SCHEMA throw then
   u 0 < if E-OBJ-SCHEMA throw then
   off u + TEXT-U @ > if E-OBJ-SCHEMA throw then ;

: TEXT-U8! ( n n -- ) {: val:n off:n :}
   val STR-BYTE-MAX and TEXT-BUF off + c! ;

: U64-LE! ( n n -- ) {: val:n off:n :}
   val off TEXT-U8!
   val 8 rshift off 1 + TEXT-U8!
   val 16 rshift off 2 + TEXT-U8!
   val 24 rshift off 3 + TEXT-U8!
   val 32 rshift off 4 + TEXT-U8!
   val 40 rshift off 5 + TEXT-U8!
   val 48 rshift off 6 + TEXT-U8!
   val 56 rshift off 7 + TEXT-U8! ;

: APPLY-RELOC ( n -- ) {: idx:n :}
   idx REL-KIND$ s" abs64" STR= 0= if E-OBJ-SCHEMA throw then
   idx REL-PATCH-PTR @ {: patch:n :}
   idx REL-TARGET-PTR @ {: target:n :}
   target 0 < if E-OBJ-SCHEMA throw then
   patch ABS64-U TEXT-RANGE
   target patch U64-LE! ;

: APPLY-RELOCS ( -- )
   0 begin dup REL-N @ < while
      dup APPLY-RELOC
      1+
   repeat drop ;

public

: RESET ( -- )
   CLEAR ;

: EXPORT-COUNT ( -- n )
   EXP-N @ ;

: PACKAGE-COUNT ( -- n )
   PKG-N @ ;

: REQUIRE-COUNT ( -- n )
   REQ-N @ ;

: TYPE-COUNT ( -- n )
   TYPE-N @ ;

: NORET-COUNT ( -- n )
   NORET-N @ ;

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

: TEXT$ ( -- ptr u8 n )
   TEXT-BUF TEXT-U @ ;

: DATA$ ( -- ptr u8 n )
   DATA-BUF DATA-U @ ;

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

: PACKAGE$ ( n -- ptr u8 n )
   PKG$ ;

: PACKAGE-VIS$ ( n -- ptr u8 n )
   PKG-VIS$ ;

: REQUIRE$ ( n -- ptr u8 n )
   REQ$ ;

: TYPE$ ( n -- ptr u8 n )
   TYPE-SYM$ ;

: TYPE-KIND$ ( n -- ptr u8 n )
   TYPE-KIND-SYM$ ;

: NORET$ ( n -- ptr u8 n )
   NORET-SYM$ ;

: IMPORT$ ( n -- ptr u8 n )
   IMP$ ;

: DEF$ ( n -- ptr u8 n )
   DEF-SYM$ ;

: EXPORT-EFFECT$ ( n -- ptr u8 n )
   EXP-EFF$ ;

: IMPORT-EFFECT$ ( n -- ptr u8 n )
   IMP-EFF$ ;

: DEF-EFFECT$ ( n -- ptr u8 n )
   DEF-EFF$ ;

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

: EXPORT+ ( ptr u8 n ptr u8 n -- )
   EXP+ ;

: IMPORT+ ( ptr u8 n ptr u8 n -- )
   IMP+ ;

: ADD ( -- )
   SCAN-SIZES
   SECTION-ROOM
   0 begin dup OBJ:ROW-COUNT < while
      dup ADD-ROW
      1+
   repeat drop
   RECORD-OBJECT
   APPEND-SECTIONS
   TEXT-U @ CUR-TEXT @ + TEXT-U !
   DATA-U @ CUR-DATA @ + DATA-U !
   OBJ-N @ 1+ OBJ-N ! ;

: CHECK ( -- )
   CHECK-IMPORTS
   CHECK-RELOCS ;

: APPLY ( -- )
   CHECK
   APPLY-RELOCS ;

end-package
