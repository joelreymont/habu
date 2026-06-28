\ trust-lint-core.f - keep TRUST sites pinned to TRUSTED.md.
\ Load after tools/date.f, lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f,
\ tools/lint/text.f, tools/lint/token.f, and tools/lint/lib.f.

90 constant TL-MAX-AUDIT-AGE
512 constant TL-MAX
32 constant TL-NUM-CAP
16 constant TL-CELL-MAX
7 constant TL-S-TABLES
10 constant TL-M-TABLES
TL-S-TABLES TL-MAX * constant TL-S-CELLS
TL-M-TABLES TL-MAX * constant TL-M-CELLS

10 constant TL-LF
45 constant TL-DASH
48 constant TL-ZERO
58 constant TL-COLON
92 constant TL-BSLASH
96 constant TL-BTICK
124 constant TL-PIPE

create TL-PATH-BUF FS-PATH-CAP allot
create TL-SITE-BUF FS-PATH-CAP allot
create TL-NUM-BUF TL-NUM-CAP allot
create TL-ONE 1 allot

create TL-CO TL-CELL-MAX cells allot
create TL-CL TL-CELL-MAX cells allot

variable TL-S-TAB-A
variable TL-M-TAB-A
variable TL-END
variable TL-S#
variable TL-M#
variable TL-BAD
variable TL-NUM-L
variable TL-SITE-U
variable TL-LINE
variable TL-I
variable TL-J
variable TL-K
variable TL-C#
variable TL-START
variable TL-AI
variable TL-BI
variable TL-TODAY-DAYS
variable TL-OUT-FD
variable TL-REPORT-SUCCESS
variable TL-STR-A
variable TL-STR-LIMIT
variable TL-FILE-A
variable TL-FILE-LIMIT
variable TL-CUR-PATH-A
variable TL-CUR-PATH-U
variable TL-CUR-LINE
variable TL-ROOT-A
variable TL-ROOT-U
variable TL-LA
variable TL-LU
variable TL-LX
variable TL-LS
variable TL-LE
variable TL-NA
variable TL-NU
variable TL-NB
variable TL-NV

: TL-S-TAB-A-FIELD ( -- ptr ptr a ) TL-S-TAB-A 0 ptr-field ;
: TL-M-TAB-A-FIELD ( -- ptr ptr a ) TL-M-TAB-A 0 ptr-field ;
: TL-CUR-PATH-A-FIELD ( -- ptr ptr u8 ) TL-CUR-PATH-A 0 ptr-field ;
: TL-ROOT-A-FIELD ( -- ptr ptr u8 ) TL-ROOT-A 0 ptr-field ;
: TL-STR-A-FIELD ( -- ptr ptr u8 ) TL-STR-A 0 ptr-field ;
: TL-FILE-A-FIELD ( -- ptr ptr u8 ) TL-FILE-A 0 ptr-field ;
: TL-LA-FIELD ( -- ptr ptr u8 ) TL-LA 0 ptr-field ;
: TL-NA-FIELD ( -- ptr ptr u8 ) TL-NA 0 ptr-field ;
: TL-NB-FIELD ( -- ptr ptr u8 ) TL-NB 0 ptr-field ;

: TL-S-TAB-A@ ( -- ptr a ) TL-S-TAB-A-FIELD @ ;
: TL-M-TAB-A@ ( -- ptr a ) TL-M-TAB-A-FIELD @ ;
: TL-CUR-PATH-A@ ( -- ptr u8 ) TL-CUR-PATH-A-FIELD @ ;
: TL-ROOT-A@ ( -- ptr u8 ) TL-ROOT-A-FIELD @ ;
: TL-STR-A@ ( -- ptr u8 ) TL-STR-A-FIELD @ ;
: TL-FILE-A@ ( -- ptr u8 ) TL-FILE-A-FIELD @ ;
: TL-LA@ ( -- ptr u8 ) TL-LA-FIELD @ ;
: TL-NA@ ( -- ptr u8 ) TL-NA-FIELD @ ;
: TL-NB@ ( -- ptr u8 ) TL-NB-FIELD @ ;

: TL-S-TAB-A! ( ptr a -- ) TL-S-TAB-A-FIELD ! ;
: TL-M-TAB-A! ( ptr a -- ) TL-M-TAB-A-FIELD ! ;
: TL-CUR-PATH-A! ( ptr u8 -- ) TL-CUR-PATH-A-FIELD ! ;
: TL-ROOT-A! ( ptr u8 -- ) TL-ROOT-A-FIELD ! ;
: TL-STR-A! ( ptr u8 -- ) TL-STR-A-FIELD ! ;
: TL-FILE-A! ( ptr u8 -- ) TL-FILE-A-FIELD ! ;
: TL-LA! ( ptr u8 -- ) TL-LA-FIELD ! ;
: TL-NA! ( ptr u8 -- ) TL-NA-FIELD ! ;
: TL-NB! ( ptr u8 -- ) TL-NB-FIELD ! ;

: TL-ALLOC-TABLES ( -- )
   TL-S-TAB-A @ 0= if TL-S-CELLS >COUNT MEM-ALLOC-CELLS TL-S-TAB-A! then
   TL-M-TAB-A @ 0= if TL-M-CELLS >COUNT MEM-ALLOC-CELLS TL-M-TAB-A! then ;

: TL-TABLE-OFF ( n -- n )
   TL-MAX * cells ;

: TL-S-TABLE ( n -- ptr a )
   TL-S-TAB-A@ swap TL-TABLE-OFF + ;

: TL-M-TABLE ( n -- ptr a )
   TL-M-TAB-A@ swap TL-TABLE-OFF + ;

: TL-S-NO ( -- ptr a ) 0 TL-S-TABLE ;
: TL-S-NL ( -- ptr a ) 1 TL-S-TABLE ;
: TL-S-EO ( -- ptr a ) 2 TL-S-TABLE ;
: TL-S-EL ( -- ptr a ) 3 TL-S-TABLE ;
: TL-S-PO ( -- ptr a ) 4 TL-S-TABLE ;
: TL-S-PL ( -- ptr a ) 5 TL-S-TABLE ;
: TL-S-LINE ( -- ptr a ) 6 TL-S-TABLE ;

: TL-M-NO ( -- ptr a ) 0 TL-M-TABLE ;
: TL-M-NL ( -- ptr a ) 1 TL-M-TABLE ;
: TL-M-EO ( -- ptr a ) 2 TL-M-TABLE ;
: TL-M-EL ( -- ptr a ) 3 TL-M-TABLE ;
: TL-M-TO ( -- ptr a ) 4 TL-M-TABLE ;
: TL-M-TL ( -- ptr a ) 5 TL-M-TABLE ;
: TL-M-SO ( -- ptr a ) 6 TL-M-TABLE ;
: TL-M-SL ( -- ptr a ) 7 TL-M-TABLE ;
: TL-M-AO ( -- ptr a ) 8 TL-M-TABLE ;
: TL-M-AL ( -- ptr a ) 9 TL-M-TABLE ;

: TL-C! ( n -- ) TL-ONE c! ;

: TL-WRITE ( n ptr u8 n -- ) {: fd a:ptr u :}
   u 0= if exit then
   fd a u write u <> if E-FS-IO throw then ;

: TL-OUT ( ptr u8 n -- )
   TL-OUT-FD @ -rot TL-WRITE ;

: TL-OUT-FD! ( fd -- )
   TL-OUT-FD ! ;

: TL-REPORT-SUCCESS! ( bool -- )
   TL-REPORT-SUCCESS ! ;

: TL-C ( n -- ) TL-C! TL-ONE 1 TL-OUT ;
: TL-NL ( -- ) TL-LF TL-C ;

: TL-TRUE ( -- bool )
   0 0= ;

: TL-FALSE ( -- bool )
   0 1 = ;

: TL-NOT ( bool -- bool )
   IF TL-FALSE ELSE TL-TRUE THEN ;

: TL-U. ( n -- )
   0 TL-NUM-L !
   dup 0= IF drop TL-ZERO TL-C exit THEN
   begin dup 0 > while
      dup 10 mod TL-ZERO + TL-NUM-BUF TL-NUM-L @ + c!
      10 /
      TL-NUM-L @ 1+ TL-NUM-L !
   repeat drop
   begin TL-NUM-L @ 0 > while
      TL-NUM-L @ 1- TL-NUM-L !
      TL-NUM-BUF TL-NUM-L @ + c@ TL-C
   repeat ;

: TL-FAIL ( ptr u8 n -- ) 76 die ;

: TL-REQUIRE-BUFFERS ( -- )
   TL-STR-LIMIT @ 0 <= if s" trust-lint: string buffer not configured" TL-FAIL then
   TL-FILE-LIMIT @ 0 <= if s" trust-lint: file buffer not configured" TL-FAIL then ;

: TRUST-LINT-BUFFERS! ( ptr u8 n ptr u8 n -- ) {: str:ptr stru file:ptr fileu :}
   stru 0 <= if s" trust-lint: invalid string buffer" TL-FAIL then
   fileu 0 <= if s" trust-lint: invalid file buffer" TL-FAIL then
   str TL-STR-A!
   stru TL-STR-LIMIT !
   file TL-FILE-A!
   fileu TL-FILE-LIMIT ! ;

: TL-ROOT! ( ptr u8 n -- ) {: a:ptr u :}
   a TL-ROOT-A!
   u TL-ROOT-U ! ;

: TL-ROOT-SELF? ( -- bool )
   TL-ROOT-U @ 0= IF TL-TRUE exit THEN
   TL-ROOT-A@ TL-ROOT-U @ s" ." LINT-STR= ;

: TL-ROOTED$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   TL-ROOT-SELF? IF a u exit THEN
   TL-ROOT-U @ u + 1 + FS-PATH-CAP > IF s" trust-lint: root path too long" TL-FAIL THEN
   TL-ROOT-A@ TL-PATH-BUF TL-ROOT-U @ BYTE-COPY
   TL-ROOT-A@ TL-ROOT-U @ 1- + c@ FS-SLASH = IF
      a TL-PATH-BUF TL-ROOT-U @ + u BYTE-COPY
      TL-PATH-BUF TL-ROOT-U @ u +
      exit
   THEN
   FS-SLASH TL-PATH-BUF TL-ROOT-U @ + c!
   a TL-PATH-BUF TL-ROOT-U @ 1 + + u BYTE-COPY
   TL-PATH-BUF TL-ROOT-U @ 1 + u + ;

: TL-REL$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   TL-ROOT-SELF? IF a u exit THEN
   u TL-ROOT-U @ <= IF a u exit THEN
   a TL-ROOT-U @ TL-ROOT-A@ TL-ROOT-U @ LINT-STR= 0= IF a u exit THEN
   a TL-ROOT-U @ + c@ FS-SLASH <> IF a u exit THEN
   a TL-ROOT-U @ 1 + +  u TL-ROOT-U @ 1 + - ;

: TL-STORE$ ( ptr u8 n -- n n ) {: a:ptr u :}
   TL-END @ u + TL-STR-LIMIT @ > IF s" trust-lint: string store overflow" TL-FAIL THEN
   a TL-STR-A@ TL-END @ + u LINT-BMOVE
   TL-END @ u
   TL-END @ u + TL-END ! ;

: TL-O$ ( n n -- ptr u8 n )
   swap TL-STR-A@ + swap ;

: TL-A@ ( ptr a n -- n )
   cells + @ ;

: TL-A! ( n ptr a n -- )
   cells + ! ;

: TL-S-NAME$ ( n -- ptr u8 n ) {: k :} TL-S-NO k TL-A@ TL-S-NL k TL-A@ TL-O$ ;
: TL-S-EFF$  ( n -- ptr u8 n ) {: k :} TL-S-EO k TL-A@ TL-S-EL k TL-A@ TL-O$ ;
: TL-S-PATH$ ( n -- ptr u8 n ) {: k :} TL-S-PO k TL-A@ TL-S-PL k TL-A@ TL-O$ ;
: TL-M-NAME$ ( n -- ptr u8 n ) {: k :} TL-M-NO k TL-A@ TL-M-NL k TL-A@ TL-O$ ;
: TL-M-EFF$  ( n -- ptr u8 n ) {: k :} TL-M-EO k TL-A@ TL-M-EL k TL-A@ TL-O$ ;
: TL-M-TEST$ ( n -- ptr u8 n ) {: k :} TL-M-TO k TL-A@ TL-M-TL k TL-A@ TL-O$ ;
: TL-M-SITE$ ( n -- ptr u8 n ) {: k :} TL-M-SO k TL-A@ TL-M-SL k TL-A@ TL-O$ ;
: TL-M-AUDIT$ ( n -- ptr u8 n ) {: k :} TL-M-AO k TL-A@ TL-M-AL k TL-A@ TL-O$ ;

: TL-FIND-SITE ( ptr u8 n -- n ) {: a:ptr u :}
   0 begin dup TL-S# @ < while
      dup TL-S-NAME$ a u LINT-STR= IF exit THEN
      1+
   repeat drop -1 ;

: TL-FIND-MAN ( ptr u8 n -- n ) {: a:ptr u :}
   0 begin dup TL-M# @ < while
      dup TL-M-NAME$ a u LINT-STR= IF exit THEN
      1+
   repeat drop -1 ;

: TL-FIND-SITE-IN-PATH ( ptr u8 n ptr u8 n -- n ) {: name:ptr nameu path:ptr pathu :}
   0 begin dup TL-S# @ < while
      dup TL-S-NAME$ name nameu LINT-STR= if
         dup TL-S-PATH$ path pathu LINT-STR= if exit then
      then
      1+
   repeat drop -1 ;

: TL-FIND-MAN-SITE ( ptr u8 n ptr u8 n -- n ) {: name:ptr nameu site:ptr siteu :}
   0 begin dup TL-M# @ < while
      dup TL-M-NAME$ name nameu LINT-STR= if
         dup TL-M-SITE$ site siteu LINT-STR= if exit then
      then
      1+
   repeat drop -1 ;

: TL-PRINT-SITE ( n -- )
   dup TL-S-PATH$ TL-OUT
   TL-COLON TL-C
   TL-S-LINE swap TL-A@ TL-U. ;

: TL-BAD+ ( -- ) TL-BAD @ 1+ TL-BAD ! ;

: TL-SITE+ ( ptr u8 n -- ) {: a:ptr u :}
   TL-SITE-U @ u + FS-PATH-CAP > IF s" trust-lint: site path too long" TL-FAIL THEN
   a TL-SITE-BUF TL-SITE-U @ + u LINT-BMOVE
   TL-SITE-U @ u + TL-SITE-U ! ;

: TL-SITE-C ( n -- ) {: c :}
   TL-SITE-U @ 1 + FS-PATH-CAP > IF s" trust-lint: site path too long" TL-FAIL THEN
   c TL-SITE-BUF TL-SITE-U @ + c!
   TL-SITE-U @ 1 + TL-SITE-U ! ;

: TL-SITE-U+ ( n -- ) {: n :}
   n 0 < IF s" trust-lint: negative line" TL-FAIL THEN
   n 10 >= IF n 10 / RECURSE THEN
   n 10 mod TL-ZERO + TL-SITE-C ;

: TL-SCAN-SITE$ ( n -- ptr u8 n ) {: sk :}
   0 TL-SITE-U !
   sk TL-S-PATH$ TL-SITE+
   TL-COLON TL-SITE-C
   TL-S-LINE sk TL-A@ TL-SITE-U+
   TL-SITE-BUF TL-SITE-U @ ;

: TL-FIND-SITE-FOR-MAN ( n -- n ) {: mk :}
   0 begin dup TL-S# @ < while
      dup TL-S-NAME$ mk TL-M-NAME$ LINT-STR= if
         dup TL-SCAN-SITE$ mk TL-M-SITE$ LINT-STR= if exit then
      then
      1+
   repeat drop -1 ;

: TL-SITE-DRIFT ( n n -- ) {: sk mk :}
   s" SITE-DRIFT " TL-OUT sk TL-PRINT-SITE
   s" : `" TL-OUT sk TL-S-NAME$ TL-OUT
   s" ` manifest site `" TL-OUT mk TL-M-SITE$ TL-OUT
   s" ` does not match scanned site `" TL-OUT sk TL-SCAN-SITE$ TL-OUT
   s" `" TL-OUT TL-NL
   TL-BAD+ ;

: TL-DUP-SITE ( n -- )
   s" DUPLICATE-TRUST " TL-OUT
   TL-CUR-PATH-A@ TL-CUR-PATH-U @ TL-OUT TL-COLON TL-C TL-CUR-LINE @ TL-U.
   s" : `" TL-OUT P1A@ P1U @ TL-OUT
   s" ` already trusted at " TL-OUT
   TL-PRINT-SITE TL-NL
   TL-BAD+ ;

: TL-ADD-SITE ( -- )
   P1A@ P1U @ TL-CUR-PATH-A@ TL-CUR-PATH-U @ TL-FIND-SITE-IN-PATH dup 0 >= IF TL-DUP-SITE ELSE drop THEN
   TL-S# @ TL-MAX >= IF s" trust-lint: too many TRUST sites" TL-FAIL THEN
   P1A@ P1U @ TL-STORE$ TL-S-NL TL-S# @ TL-A! TL-S-NO TL-S# @ TL-A!
   P2A@ P2U @ TL-STORE$ TL-S-EL TL-S# @ TL-A! TL-S-EO TL-S# @ TL-A!
   TL-CUR-PATH-A@ TL-CUR-PATH-U @ TL-STORE$ TL-S-PL TL-S# @ TL-A! TL-S-PO TL-S# @ TL-A!
   TL-CUR-LINE @ TL-S-LINE TL-S# @ TL-A!
   TL-S# @ 1+ TL-S# ! ;

: TL-DUP-ROW ( n ptr u8 n -- ) {: k a:ptr u :}
   s" DUPLICATE-ROW TRUSTED.md: `" TL-OUT
   a u TL-OUT
   s" ` appears more than once" TL-OUT TL-NL
   TL-BAD+ ;

: TL-CODE-LEN ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   0 begin dup u < while
      a over + c@ TL-BSLASH = IF a swap exit THEN
      1+
   repeat drop a u ;

: TL-LINE-LEN ( ptr u8 n -- ptr u8 n )
   dup 0 > IF
      2dup + 1- c@ 13 = IF 1- THEN
   THEN ;

: TL-SCAN-SRC-LINE ( ptr u8 n -- )
   TL-CODE-LEN TRUST-SITE? IF
      TL-ADD-SITE
   THEN ;

: TL-DO-SRC-LINE ( n -- )
   TL-LE !
   TL-LINE @ 1+ TL-LINE !
   TL-LINE @ TL-CUR-LINE !
   TL-LA@ TL-LS @ +  TL-LE @ TL-LS @ -  TL-LINE-LEN
   TL-SCAN-SRC-LINE
   TL-LE @ 1+ TL-LS ! ;

: TL-FOR-SRC-LINES ( ptr u8 n -- )
   TL-LU !  TL-LA!
   0 TL-LINE !  0 TL-LX !  0 TL-LS !
   begin TL-LX @ TL-LU @ < while
      TL-LA@ TL-LX @ + c@ TL-LF = IF TL-LX @ TL-DO-SRC-LINE THEN
      TL-LX @ 1+ TL-LX !
   repeat
   TL-LS @ TL-LU @ < IF TL-LU @ TL-DO-SRC-LINE THEN ;

: TL-SCAN-SRC-FILE ( ptr u8 n -- ) {: a:ptr u :}
   a u s" .f" HAS-EXT? 0= IF exit THEN
   a u TL-REL$ TL-CUR-PATH-U ! TL-CUR-PATH-A!
   a u TL-FILE-A@ TL-FILE-LIMIT @ READ-FILE TL-FOR-SRC-LINES ;

: TL-CELL! ( ptr u8 n -- ) {: a:ptr u :}
   TL-C# @ TL-CELL-MAX >= IF exit THEN
   a TL-CO TL-C# @ cells + !
   u TL-CL TL-C# @ cells + !
   TL-C# @ 1+ TL-C# ! ;

: TL-CELL$ ( n -- ptr u8 n )
   dup cells TL-CO + @  swap cells TL-CL + @ ;

: TL-SPLIT-PIPE ( ptr u8 n -- ) {: a:ptr u :}
   0 TL-C# !
   0 TL-I !
   begin TL-I @ u < while
      a TL-I @ + c@ TL-PIPE = IF
         TL-I @ 1+ TL-START !
         TL-I @ 1+ TL-I !
         begin TL-I @ u <  a TL-I @ + c@ TL-PIPE <> and while
            TL-I @ 1+ TL-I !
         repeat
         a TL-START @ +  TL-I @ TL-START @ -  LINT-TRIM TL-CELL!
      ELSE
         TL-I @ 1+ TL-I !
      THEN
   repeat ;

: TL-UNBACKTICK ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   u 2 >= IF
      a c@ TL-BTICK =  a u 1- + c@ TL-BTICK = and IF a 1+ u 2 - exit THEN
   THEN
   a u ;

: TL-SEPARATOR? ( ptr u8 n -- bool ) {: a:ptr u :}
   u 0= IF TL-FALSE exit THEN
   0 begin dup u < while
      a over + c@ dup TL-DASH = over TL-COLON = or swap 32 = or TL-NOT IF drop TL-FALSE exit THEN
      1+
   repeat drop TL-TRUE ;

: TL-MAN-NAME-CELL$ ( -- ptr u8 n )
   0 TL-CELL$ TL-UNBACKTICK LINT-TRIM ;

: TL-MAN-EFF-CELL$ ( -- ptr u8 n )
   1 TL-CELL$ TL-UNBACKTICK LINT-TRIM ;

: TL-MAN-TEST-CELL$ ( -- ptr u8 n )
   3 TL-CELL$ LINT-TRIM ;

: TL-MAN-SITE-CELL$ ( -- ptr u8 n )
   4 TL-CELL$ LINT-TRIM ;

: TL-MAN-AUDIT-CELL$ ( -- ptr u8 n )
   5 TL-CELL$ LINT-TRIM ;

: TL-MAN-HEADER? ( -- bool )
   TL-MAN-NAME-CELL$ s" Word" LINT-STR= ;

: TL-MAN-SEPARATOR? ( -- bool )
   TL-MAN-NAME-CELL$ TL-SEPARATOR? ;

: TL-CHECK-DUP-MAN ( -- )
   TL-MAN-NAME-CELL$ TL-MAN-SITE-CELL$ TL-FIND-MAN-SITE dup 0 >= IF
      TL-MAN-NAME-CELL$ TL-DUP-ROW
   ELSE
      drop
   THEN ;

: TL-STORE-MAN-NAME ( -- )
   TL-MAN-NAME-CELL$ TL-STORE$ TL-M-NL TL-M# @ TL-A! TL-M-NO TL-M# @ TL-A! ;

: TL-STORE-MAN-EFF ( -- )
   TL-MAN-EFF-CELL$ TL-STORE$ TL-M-EL TL-M# @ TL-A! TL-M-EO TL-M# @ TL-A! ;

: TL-STORE-MAN-TEST ( -- )
   TL-MAN-TEST-CELL$ TL-STORE$ TL-M-TL TL-M# @ TL-A! TL-M-TO TL-M# @ TL-A! ;

: TL-STORE-MAN-SITE ( -- )
   TL-MAN-SITE-CELL$ TL-STORE$ TL-M-SL TL-M# @ TL-A! TL-M-SO TL-M# @ TL-A! ;

: TL-STORE-MAN-AUDIT ( -- )
   TL-MAN-AUDIT-CELL$ TL-STORE$ TL-M-AL TL-M# @ TL-A! TL-M-AO TL-M# @ TL-A! ;

: TL-ADD-MAN ( -- )
   TL-CHECK-DUP-MAN
   TL-M# @ TL-MAX >= IF s" trust-lint: too many manifest rows" TL-FAIL THEN
   TL-STORE-MAN-NAME
   TL-STORE-MAN-EFF
   TL-STORE-MAN-TEST
   TL-STORE-MAN-SITE
   TL-STORE-MAN-AUDIT
   TL-M# @ 1+ TL-M# ! ;

: TL-SCAN-MAN-LINE ( ptr u8 n -- ) {: a:ptr u :}
   u 0= IF exit THEN
   a c@ TL-PIPE <> IF exit THEN
   a u TL-SPLIT-PIPE
   TL-C# @ 6 < IF exit THEN
   TL-MAN-HEADER? IF exit THEN
   TL-MAN-SEPARATOR? IF exit THEN
   TL-ADD-MAN ;

: TL-DO-MAN-LINE ( n -- )
   TL-LE !
   TL-LINE @ 1+ TL-LINE !
   TL-LINE @ TL-CUR-LINE !
   TL-LA@ TL-LS @ +  TL-LE @ TL-LS @ -  TL-LINE-LEN
   TL-SCAN-MAN-LINE
   TL-LE @ 1+ TL-LS ! ;

: TL-FOR-MAN-LINES ( ptr u8 n -- )
   TL-LU !  TL-LA!
   0 TL-LINE !  0 TL-LX !  0 TL-LS !
   begin TL-LX @ TL-LU @ < while
      TL-LA@ TL-LX @ + c@ TL-LF = IF TL-LX @ TL-DO-MAN-LINE THEN
      TL-LX @ 1+ TL-LX !
   repeat
   TL-LS @ TL-LU @ < IF TL-LU @ TL-DO-MAN-LINE THEN ;

: TL-SCAN-MANIFEST ( -- )
   s" TRUSTED.md" TL-ROOTED$ 2dup EXISTS? 0= IF
      2drop
      s" trust-lint: TRUSTED.md missing - the trust manifest is required" TL-OUT TL-NL
      1 throw
   THEN
   TL-FILE-A@ TL-FILE-LIMIT @ READ-FILE TL-FOR-MAN-LINES ;

: TL-A-END? ( -- bool ) TL-AI @ TL-NU @ >= ;
: TL-B-END? ( -- bool ) TL-BI @ TL-NV @ >= ;
: TL-A-C@ ( -- n ) TL-NA@ TL-AI @ + c@ ;
: TL-B-C@ ( -- n ) TL-NB@ TL-BI @ + c@ ;
: TL-A-WS? ( -- bool ) TL-A-END? IF TL-FALSE ELSE TL-A-C@ LINT-WS? THEN ;
: TL-B-WS? ( -- bool ) TL-B-END? IF TL-FALSE ELSE TL-B-C@ LINT-WS? THEN ;

: TL-N-SKIP ( -- )
   begin TL-A-WS? while TL-AI @ 1+ TL-AI ! repeat
   begin TL-B-WS? while TL-BI @ 1+ TL-BI ! repeat ;

: TL-N-TOKEN= ( -- bool )
   begin
      TL-A-END? IF TL-B-END? IF TL-TRUE exit THEN TL-B-WS? exit THEN
      TL-B-END? IF TL-A-WS? exit THEN
      TL-A-WS? IF TL-B-WS? exit THEN
      TL-B-WS? IF TL-FALSE exit THEN
      TL-A-C@ LINT-FOLD TL-B-C@ LINT-FOLD <> IF TL-FALSE exit THEN
      TL-AI @ 1+ TL-AI !  TL-BI @ 1+ TL-BI !
   again ;

: TL-NORM= ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   a TL-NA!  u TL-NU !  b TL-NB!  v TL-NV !
   0 TL-AI !  0 TL-BI !
   begin
      TL-N-SKIP
      TL-A-END? TL-B-END? and IF TL-TRUE exit THEN
      TL-A-END? TL-B-END? or IF TL-FALSE exit THEN
      TL-N-TOKEN= TL-NOT IF TL-FALSE exit THEN
   again ;

: TL-TODAY ( -- n )
   TL-TODAY-DAYS @ ;

: TL-UNMANIFESTED-SITE ( n -- ) {: sk :}
   s" UNMANIFESTED " TL-OUT sk TL-PRINT-SITE
   s" : `" TL-OUT sk TL-S-NAME$ TL-OUT
   s" ` is TRUSTed but has no TRUSTED.md row" TL-OUT TL-NL
   TL-BAD+ ;

: TL-FIND-MANIFEST-SITE ( n -- bool ) {: sk :}
   sk TL-S-NAME$ sk TL-SCAN-SITE$ TL-FIND-MAN-SITE dup 0 < IF
      drop
      sk TL-S-NAME$ TL-FIND-MAN dup 0 < IF
         drop
         sk TL-UNMANIFESTED-SITE
      ELSE
         sk swap TL-SITE-DRIFT
      THEN
      TL-FALSE exit
   THEN
   TL-K !
   TL-TRUE ;

: TL-CHECK-EFFECT-DRIFT ( n -- ) {: sk :}
   sk TL-S-EFF$ TL-K @ TL-M-EFF$ TL-NORM= TL-NOT IF
      s" EFFECT-DRIFT " TL-OUT sk TL-PRINT-SITE
      s" : `" TL-OUT sk TL-S-NAME$ TL-OUT
      s" ` code effect `" TL-OUT sk TL-S-EFF$ TL-OUT
      s" ` != TRUSTED.md `" TL-OUT TL-K @ TL-M-EFF$ TL-OUT s" `" TL-OUT TL-NL
      TL-BAD+
   THEN ;

: TL-CHECK-SITE-DRIFT ( n -- ) {: sk :}
   sk TL-SCAN-SITE$ TL-K @ TL-M-SITE$ LINT-STR= 0= IF
      sk TL-K @ TL-SITE-DRIFT
   THEN ;

: TL-CHECK-TEST-CELL ( n -- ) {: sk :}
   TL-K @ TL-M-TEST$ LINT-TRIM nip 0= IF
      s" UNTESTED " TL-OUT sk TL-PRINT-SITE
      s" : `" TL-OUT sk TL-S-NAME$ TL-OUT
      s" ` has an empty Tests cell in TRUSTED.md" TL-OUT TL-NL
      TL-BAD+
   THEN ;

: TL-BAD-AUDIT-DATE ( n -- ) {: sk :}
   s" BAD-AUDIT-DATE TRUSTED.md: `" TL-OUT sk TL-S-NAME$ TL-OUT
   s" ` has invalid Last audited `" TL-OUT TL-K @ TL-M-AUDIT$ TL-OUT s" `" TL-OUT TL-NL
   TL-BAD+ ;

: TL-FUTURE-AUDIT-DATE ( n -- ) {: sk :}
   s" FUTURE-AUDIT TRUSTED.md: `" TL-OUT sk TL-S-NAME$ TL-OUT
   s" ` Last audited `" TL-OUT TL-K @ TL-M-AUDIT$ TL-OUT
   s" ` is in the future" TL-OUT TL-NL
   TL-BAD+ ;

: TL-STALE-AUDIT-DATE ( n n -- ) {: sk age :}
   s" STALE-AUDIT TRUSTED.md: `" TL-OUT sk TL-S-NAME$ TL-OUT
   s" ` Last audited `" TL-OUT TL-K @ TL-M-AUDIT$ TL-OUT
   s" ` is " TL-OUT age TL-U. s"  day(s) old" TL-OUT TL-NL
   TL-BAD+ ;

: TL-CHECK-AUDIT-DATE ( n -- ) {: sk :}
   TL-K @ TL-M-AUDIT$ PARSE-YMD 0= IF
      drop
      sk TL-BAD-AUDIT-DATE exit
   THEN
   TL-TODAY swap -
   dup 0 < IF
      drop
      sk TL-FUTURE-AUDIT-DATE exit
   THEN
   dup TL-MAX-AUDIT-AGE > IF
      sk swap TL-STALE-AUDIT-DATE
   ELSE
      drop
   THEN ;

: TL-CHECK-SITE ( n -- ) {: sk :}
   sk TL-FIND-MANIFEST-SITE 0= IF exit THEN
   sk TL-CHECK-EFFECT-DRIFT
   sk TL-CHECK-SITE-DRIFT
   sk TL-CHECK-TEST-CELL
   sk TL-CHECK-AUDIT-DATE ;

: TL-SCANNED-SITE? ( ptr u8 n -- bool )
   2dup s" src/" LINT-STARTS-WITH? IF 2drop TL-TRUE exit THEN
   s" lib/" LINT-STARTS-WITH? ;

: TL-CHECK-STALE-ROW ( n -- ) {: mk :}
   mk TL-M-SITE$ TL-SCANNED-SITE? TL-NOT IF exit THEN
   mk TL-FIND-SITE-FOR-MAN 0 < IF
      s" STALE-ROW TRUSTED.md " TL-OUT mk TL-M-SITE$ TL-OUT
      s" : `" TL-OUT mk TL-M-NAME$ TL-OUT
      s" ` has a row but no TRUST site in src/ or lib/ scanned roots" TL-OUT TL-NL
      TL-BAD+
   THEN ;

: TL-SCAN-OPTIONAL-ROOT ( ptr u8 n -- ) {: a:ptr u :}
   a u TL-ROOTED$ 2dup EXISTS? IF [: TL-SCAN-SRC-FILE ;] WALK-FILES ELSE 2drop THEN ;

: TL-RESET ( -- )
   TL-ALLOC-TABLES
   TL-REQUIRE-BUFFERS
   0 TL-END !  0 TL-S# !  0 TL-M# !  0 TL-BAD !
;

: TL-CHECK-SITES ( -- )
   0 begin dup TL-S# @ < while dup TL-CHECK-SITE 1+ repeat drop ;

: TL-CHECK-STALE-ROWS ( -- )
   0 begin dup TL-M# @ < while dup TL-CHECK-STALE-ROW 1+ repeat drop ;

: TL-REPORT ( -- )
   TL-BAD @ 0= TL-REPORT-SUCCESS @ 0= and if exit then
   s" trust-lint: " TL-OUT TL-S# @ TL-U. s"  TRUST site(s), " TL-OUT
   TL-M# @ TL-U. s"  manifest row(s), " TL-OUT TL-BAD @ TL-U.
   s"  finding(s)" TL-OUT TL-NL
   TL-BAD @ 0 > IF 1 throw THEN ;

: TRUST-LINT-RESET ( -- )
   TL-RESET ;

: TRUST-LINT-ROOT! ( ptr u8 n -- )
   TL-ROOT! ;

: TRUST-LINT-TODAY! ( n -- )
   TL-TODAY-DAYS ! ;

: TRUST-LINT-TODAY-NOW ( -- )
   epoch-seconds DATE-SECONDS-DAY / TRUST-LINT-TODAY! ;

: TRUST-LINT ( -- )
   TRUST-LINT-RESET
   s" src" TL-SCAN-OPTIONAL-ROOT
   s" lib" TL-SCAN-OPTIONAL-ROOT
   TL-SCAN-MANIFEST
   TL-CHECK-SITES
   TL-CHECK-STALE-ROWS
   TL-REPORT ;

: TRUST-LINT-SOURCE+ ( ptr u8 n -- )
   TL-SCAN-SRC-FILE ;

: TRUST-LINT-SOURCES-FINISH ( -- )
   TL-SCAN-MANIFEST
   TL-CHECK-SITES
   TL-REPORT ;

: TRUST-LINT-SOURCE-FILE ( ptr u8 n -- )
   TRUST-LINT-RESET
   TRUST-LINT-SOURCE+
   TRUST-LINT-SOURCES-FINISH ;

1 >FD TL-OUT-FD!
TL-TRUE TL-REPORT-SUCCESS!
