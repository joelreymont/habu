\ trust-lint.f - keep TRUST sites pinned to TRUSTED.md.
\ Load after tools/date.f, tools/lint/text.f, tools/lint/token.f, tools/lint/lib.f, tools/fs.f, and tools/argv.f.
\ Run: bin/hb --load tools/date.f tools/lint/text.f tools/lint/token.f tools/lint/lib.f tools/fs.f tools/argv.f tools/trust-lint.f -- [ROOT] [TODAY]
\ Or:  bin/hb --load ... tools/trust-lint.f -- source-only SOURCE [ROOT] [TODAY]

0 set-check

90 constant TL-MAX-AUDIT-AGE
256 constant TL-MAX
$10000 constant TL-STR-CAP
$20000 constant TL-FILE-CAP
32 constant TL-NUM-CAP
16 constant TL-CELL-MAX

10 constant TL-LF
45 constant TL-DASH
48 constant TL-ZERO
58 constant TL-COLON
92 constant TL-BSLASH
96 constant TL-BTICK
124 constant TL-PIPE

create TL-PATH-BUF FS-PATH-CAP allot
create TL-SITE-BUF FS-PATH-CAP allot
create TL-STR-BUF TL-STR-CAP allot
create TL-FILE-BUF TL-FILE-CAP allot
create TL-NUM-BUF TL-NUM-CAP allot
create TL-ONE 1 allot

create TL-S-NO TL-MAX cells allot
create TL-S-NL TL-MAX cells allot
create TL-S-EO TL-MAX cells allot
create TL-S-EL TL-MAX cells allot
create TL-S-PO TL-MAX cells allot
create TL-S-PL TL-MAX cells allot
create TL-S-LINE TL-MAX cells allot

create TL-M-NO TL-MAX cells allot
create TL-M-NL TL-MAX cells allot
create TL-M-EO TL-MAX cells allot
create TL-M-EL TL-MAX cells allot
create TL-M-TO TL-MAX cells allot
create TL-M-TL TL-MAX cells allot
create TL-M-SO TL-MAX cells allot
create TL-M-SL TL-MAX cells allot
create TL-M-AO TL-MAX cells allot
create TL-M-AL TL-MAX cells allot

create TL-CO TL-CELL-MAX cells allot
create TL-CL TL-CELL-MAX cells allot

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
variable TL-CUR-PATH-A
variable TL-CUR-PATH-U
variable TL-CUR-LINE
variable TL-ROOT-A
variable TL-ROOT-U
variable TL-SOURCE-ONLY
variable TL-SOURCE-A
variable TL-SOURCE-U
variable TL-LA
variable TL-LU
variable TL-LX
variable TL-LS
variable TL-LE
variable TL-NA
variable TL-NU
variable TL-NB
variable TL-NV

: TL-CHECK-HOOK ( -- )
   CHECK! ;
' TL-CHECK-HOOK set-check

: TL-C! ( n -- ) TL-ONE c! ;
: TL-OUT ( ptr u8 n -- ) dup 0= IF 2drop exit THEN 1 -rot write drop ;
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

: TL-ROOT! ( ptr u8 n -- ) {: a:ptr u :}
   a TL-ROOT-A !
   u TL-ROOT-U ! ;

: TL-ROOT-SELF? ( -- bool )
   TL-ROOT-U @ 0= IF TL-TRUE exit THEN
   TL-ROOT-A @ TL-ROOT-U @ s" ." STR= ;

: TL-ROOTED$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   TL-ROOT-SELF? IF a u exit THEN
   TL-ROOT-U @ u + 1 + FS-PATH-CAP > IF s" trust-lint: root path too long" TL-FAIL THEN
   TL-ROOT-A @ TL-PATH-BUF TL-ROOT-U @ COPY-BYTES
   TL-ROOT-A @ TL-ROOT-U @ 1- + c@ FS-SLASH = IF
      a TL-PATH-BUF TL-ROOT-U @ + u COPY-BYTES
      TL-PATH-BUF TL-ROOT-U @ u +
      exit
   THEN
   FS-SLASH TL-PATH-BUF TL-ROOT-U @ + c!
   a TL-PATH-BUF TL-ROOT-U @ 1 + + u COPY-BYTES
   TL-PATH-BUF TL-ROOT-U @ 1 + u + ;

: TL-REL$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   TL-ROOT-SELF? IF a u exit THEN
   u TL-ROOT-U @ <= IF a u exit THEN
   a TL-ROOT-U @ TL-ROOT-A @ TL-ROOT-U @ STR= 0= IF a u exit THEN
   a TL-ROOT-U @ + c@ FS-SLASH <> IF a u exit THEN
   a TL-ROOT-U @ 1 + +  u TL-ROOT-U @ 1 + - ;

: TL-STORE$ ( ptr u8 n -- n n ) {: a:ptr u :}
   TL-END @ u + TL-STR-CAP > IF s" trust-lint: string store overflow" TL-FAIL THEN
   a TL-STR-BUF TL-END @ + u BMOVE
   TL-END @ u
   TL-END @ u + TL-END ! ;

: TL-O$ ( n n -- ptr u8 n )
   swap TL-STR-BUF + swap ;

: TL-A@ ( ptr n n -- n )
   cells + @ ;

: TL-A! ( n ptr n n -- )
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
      dup TL-S-NAME$ a u STR= IF exit THEN
      1+
   repeat drop -1 ;

: TL-FIND-MAN ( ptr u8 n -- n ) {: a:ptr u :}
   0 begin dup TL-M# @ < while
      dup TL-M-NAME$ a u STR= IF exit THEN
      1+
   repeat drop -1 ;

: TL-PRINT-SITE ( n -- )
   dup TL-S-PATH$ TL-OUT
   TL-COLON TL-C
   TL-S-LINE swap TL-A@ TL-U. ;

: TL-BAD+ ( -- ) TL-BAD @ 1+ TL-BAD ! ;

: TL-SITE+ ( ptr u8 n -- ) {: a:ptr u :}
   TL-SITE-U @ u + FS-PATH-CAP > IF s" trust-lint: site path too long" TL-FAIL THEN
   a TL-SITE-BUF TL-SITE-U @ + u BMOVE
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

: TL-SITE-DRIFT ( n n -- ) {: sk mk :}
   s" SITE-DRIFT " TL-OUT sk TL-PRINT-SITE
   s" : `" TL-OUT sk TL-S-NAME$ TL-OUT
   s" ` manifest site `" TL-OUT mk TL-M-SITE$ TL-OUT
   s" ` does not match scanned site `" TL-OUT sk TL-SCAN-SITE$ TL-OUT
   s" `" TL-OUT TL-NL
   TL-BAD+ ;

: TL-DUP-SITE ( n -- )
   s" DUPLICATE-TRUST " TL-OUT
   TL-CUR-PATH-A @ TL-CUR-PATH-U @ TL-OUT TL-COLON TL-C TL-CUR-LINE @ TL-U.
   s" : `" TL-OUT P1A @ P1U @ TL-OUT
   s" ` already trusted at " TL-OUT
   TL-PRINT-SITE TL-NL
   TL-BAD+ ;

: TL-ADD-SITE ( -- )
   P1A @ P1U @ TL-FIND-SITE dup 0 >= IF TL-DUP-SITE ELSE drop THEN
   TL-S# @ TL-MAX >= IF s" trust-lint: too many TRUST sites" TL-FAIL THEN
   P1A @ P1U @ TL-STORE$ TL-S-NL TL-S# @ TL-A! TL-S-NO TL-S# @ TL-A!
   P2A @ P2U @ TL-STORE$ TL-S-EL TL-S# @ TL-A! TL-S-EO TL-S# @ TL-A!
   TL-CUR-PATH-A @ TL-CUR-PATH-U @ TL-STORE$ TL-S-PL TL-S# @ TL-A! TL-S-PO TL-S# @ TL-A!
   TL-CUR-LINE @ TL-S-LINE TL-S# @ TL-A!
   TL-S# @ 1+ TL-S# ! ;

: TL-DUP-ROW ( n ptr u8 n -- ) {: k a:ptr u :}
   s" DUPLICATE-ROW TRUSTED.md: `" TL-OUT
   a u TL-OUT
   s" ` appears more than once" TL-OUT TL-NL
   TL-BAD+ ;

: TL-ADD-MAN ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- ) {: name:ptr nu eff:ptr eu tests:ptr tu site:ptr su audit:ptr au :}
   name nu TL-FIND-MAN dup 0 >= IF name nu TL-DUP-ROW ELSE drop THEN
   TL-M# @ TL-MAX >= IF s" trust-lint: too many manifest rows" TL-FAIL THEN
   name nu TL-STORE$ TL-M-NL TL-M# @ TL-A! TL-M-NO TL-M# @ TL-A!
   eff eu TL-STORE$ TL-M-EL TL-M# @ TL-A! TL-M-EO TL-M# @ TL-A!
   tests tu TL-STORE$ TL-M-TL TL-M# @ TL-A! TL-M-TO TL-M# @ TL-A!
   site su TL-STORE$ TL-M-SL TL-M# @ TL-A! TL-M-SO TL-M# @ TL-A!
   audit au TL-STORE$ TL-M-AL TL-M# @ TL-A! TL-M-AO TL-M# @ TL-A!
   TL-M# @ 1+ TL-M# ! ;

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
   TL-LA @ TL-LS @ +  TL-LE @ TL-LS @ -  TL-LINE-LEN
   TL-SCAN-SRC-LINE
   TL-LE @ 1+ TL-LS ! ;

: TL-FOR-SRC-LINES ( ptr u8 n -- )
   TL-LU !  TL-LA !
   0 TL-LINE !  0 TL-LX !  0 TL-LS !
   begin TL-LX @ TL-LU @ < while
      TL-LA @ TL-LX @ + c@ TL-LF = IF TL-LX @ TL-DO-SRC-LINE THEN
      TL-LX @ 1+ TL-LX !
   repeat
   TL-LS @ TL-LU @ < IF TL-LU @ TL-DO-SRC-LINE THEN ;

: TL-SCAN-SRC-FILE ( ptr u8 n -- ) {: a:ptr u :}
   a u s" .f" HAS-EXT? 0= IF exit THEN
   a u TL-REL$ TL-CUR-PATH-U ! TL-CUR-PATH-A !
   a u TL-FILE-BUF TL-FILE-CAP READ-FILE TL-FOR-SRC-LINES ;

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
         a TL-START @ +  TL-I @ TL-START @ -  TRIM TL-CELL!
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

: TL-SCAN-MAN-LINE ( ptr u8 n -- ) {: a:ptr u :}
   u 0= IF exit THEN
   a c@ TL-PIPE <> IF exit THEN
   a u TL-SPLIT-PIPE
   TL-C# @ 6 < IF exit THEN
   0 TL-CELL$ TL-UNBACKTICK TRIM
   2dup s" Word" STR= IF 2drop exit THEN
   2dup TL-SEPARATOR? IF 2drop exit THEN
   2dup 1 TL-CELL$ TL-UNBACKTICK TRIM
   3 TL-CELL$ TRIM
   4 TL-CELL$ TRIM
   5 TL-CELL$ TRIM
   TL-ADD-MAN ;

: TL-DO-MAN-LINE ( n -- )
   TL-LE !
   TL-LINE @ 1+ TL-LINE !
   TL-LINE @ TL-CUR-LINE !
   TL-LA @ TL-LS @ +  TL-LE @ TL-LS @ -  TL-LINE-LEN
   TL-SCAN-MAN-LINE
   TL-LE @ 1+ TL-LS ! ;

: TL-FOR-MAN-LINES ( ptr u8 n -- )
   TL-LU !  TL-LA !
   0 TL-LINE !  0 TL-LX !  0 TL-LS !
   begin TL-LX @ TL-LU @ < while
      TL-LA @ TL-LX @ + c@ TL-LF = IF TL-LX @ TL-DO-MAN-LINE THEN
      TL-LX @ 1+ TL-LX !
   repeat
   TL-LS @ TL-LU @ < IF TL-LU @ TL-DO-MAN-LINE THEN ;

: TL-SCAN-MANIFEST ( -- )
   s" TRUSTED.md" TL-ROOTED$ 2dup EXISTS? 0= IF
      2drop
      s" trust-lint: TRUSTED.md missing - the trust manifest is required" TL-OUT TL-NL
      1 throw
   THEN
   TL-FILE-BUF TL-FILE-CAP READ-FILE TL-FOR-MAN-LINES ;

: TL-A-END? ( -- bool ) TL-AI @ TL-NU @ >= ;
: TL-B-END? ( -- bool ) TL-BI @ TL-NV @ >= ;
: TL-A-C@ ( -- n ) TL-NA @ TL-AI @ + c@ ;
: TL-B-C@ ( -- n ) TL-NB @ TL-BI @ + c@ ;
: TL-A-WS? ( -- bool ) TL-A-END? IF TL-FALSE ELSE TL-A-C@ WS? THEN ;
: TL-B-WS? ( -- bool ) TL-B-END? IF TL-FALSE ELSE TL-B-C@ WS? THEN ;

: TL-N-SKIP ( -- )
   begin TL-A-WS? while TL-AI @ 1+ TL-AI ! repeat
   begin TL-B-WS? while TL-BI @ 1+ TL-BI ! repeat ;

: TL-N-TOKEN= ( -- bool )
   begin
      TL-A-END? IF TL-B-END? IF TL-TRUE exit THEN TL-B-WS? exit THEN
      TL-B-END? IF TL-A-WS? exit THEN
      TL-A-WS? IF TL-B-WS? exit THEN
      TL-B-WS? IF TL-FALSE exit THEN
      TL-A-C@ FOLD TL-B-C@ FOLD <> IF TL-FALSE exit THEN
      TL-AI @ 1+ TL-AI !  TL-BI @ 1+ TL-BI !
   again ;

: TL-NORM= ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   a TL-NA !  u TL-NU !  b TL-NB !  v TL-NV !
   0 TL-AI !  0 TL-BI !
   begin
      TL-N-SKIP
      TL-A-END? TL-B-END? and IF TL-TRUE exit THEN
      TL-A-END? TL-B-END? or IF TL-FALSE exit THEN
      TL-N-TOKEN= TL-NOT IF TL-FALSE exit THEN
   again ;

: TL-BAD-TODAY ( ptr u8 n -- )
   s" BAD-TODAY today argument invalid `" TL-OUT
   TL-OUT
   s" `" TL-OUT TL-NL
   1 throw ;

: TL-TODAY ( -- n )
   TL-TODAY-DAYS @ ;

: TL-CHECK-SITE ( n -- ) {: sk :}
   sk TL-S-NAME$ TL-FIND-MAN dup 0 < IF
      drop
      s" UNMANIFESTED " TL-OUT sk TL-PRINT-SITE
      s" : `" TL-OUT sk TL-S-NAME$ TL-OUT
      s" ` is TRUSTed but has no TRUSTED.md row" TL-OUT TL-NL
      TL-BAD+ exit
   THEN
   TL-K !
   sk TL-S-EFF$ TL-K @ TL-M-EFF$ TL-NORM= TL-NOT IF
      s" EFFECT-DRIFT " TL-OUT sk TL-PRINT-SITE
      s" : `" TL-OUT sk TL-S-NAME$ TL-OUT
      s" ` code effect `" TL-OUT sk TL-S-EFF$ TL-OUT
      s" ` != TRUSTED.md `" TL-OUT TL-K @ TL-M-EFF$ TL-OUT s" `" TL-OUT TL-NL
      TL-BAD+
   THEN
   sk TL-SCAN-SITE$ TL-K @ TL-M-SITE$ STR= 0= IF
      sk TL-K @ TL-SITE-DRIFT
   THEN
   TL-K @ TL-M-TEST$ TRIM nip 0= IF
      s" UNTESTED " TL-OUT sk TL-PRINT-SITE
      s" : `" TL-OUT sk TL-S-NAME$ TL-OUT
      s" ` has an empty Tests cell in TRUSTED.md" TL-OUT TL-NL
      TL-BAD+
   THEN
   TL-K @ TL-M-AUDIT$ PARSE-YMD 0= IF
      drop
      s" BAD-AUDIT-DATE TRUSTED.md: `" TL-OUT sk TL-S-NAME$ TL-OUT
      s" ` has invalid Last audited `" TL-OUT TL-K @ TL-M-AUDIT$ TL-OUT s" `" TL-OUT TL-NL
      TL-BAD+ exit
   THEN
   TL-TODAY swap -
   dup 0 < IF
      drop
      s" FUTURE-AUDIT TRUSTED.md: `" TL-OUT sk TL-S-NAME$ TL-OUT
      s" ` Last audited `" TL-OUT TL-K @ TL-M-AUDIT$ TL-OUT
      s" ` is in the future" TL-OUT TL-NL
      TL-BAD+ exit
   THEN
   dup TL-MAX-AUDIT-AGE > IF
      s" STALE-AUDIT TRUSTED.md: `" TL-OUT sk TL-S-NAME$ TL-OUT
      s" ` Last audited `" TL-OUT TL-K @ TL-M-AUDIT$ TL-OUT
      s" ` is " TL-OUT TL-U. s"  day(s) old" TL-OUT TL-NL
      TL-BAD+
   ELSE
      drop
   THEN ;

: TL-SCANNED-SITE? ( ptr u8 n -- bool )
   2dup s" src/" STARTS-WITH? IF 2drop TL-TRUE exit THEN
   s" lib/" STARTS-WITH? ;

: TL-CHECK-STALE-ROW ( n -- ) {: mk :}
   mk TL-M-SITE$ TL-SCANNED-SITE? TL-NOT IF exit THEN
   mk TL-M-NAME$ TL-FIND-SITE 0 < IF
      s" STALE-ROW TRUSTED.md " TL-OUT mk TL-M-SITE$ TL-OUT
      s" : `" TL-OUT mk TL-M-NAME$ TL-OUT
      s" ` has a row but no TRUST site in src/ or lib/ scanned roots" TL-OUT TL-NL
      TL-BAD+
   THEN ;

: TL-SCAN-OPTIONAL-ROOT ( ptr u8 n -- ) {: a:ptr u :}
   a u TL-ROOTED$ 2dup EXISTS? IF ['] TL-SCAN-SRC-FILE WALK-FILES ELSE 2drop THEN ;

: TL-RESET ( -- )
   0 TL-END !  0 TL-S# !  0 TL-M# !  0 TL-BAD !
;

: TL-CHECK-SITES ( -- )
   0 begin dup TL-S# @ < while dup TL-CHECK-SITE 1+ repeat drop ;

: TL-CHECK-STALE-ROWS ( -- )
   0 begin dup TL-M# @ < while dup TL-CHECK-STALE-ROW 1+ repeat drop ;

: TL-REPORT ( -- )
   s" trust-lint: " TL-OUT TL-S# @ TL-U. s"  TRUST site(s), " TL-OUT
   TL-M# @ TL-U. s"  manifest row(s), " TL-OUT TL-BAD @ TL-U.
   s"  finding(s)" TL-OUT TL-NL
   TL-BAD @ 0 > IF 1 throw THEN ;

: TRUST-LINT ( -- )
   TL-RESET
   s" src" TL-SCAN-OPTIONAL-ROOT
   s" lib" TL-SCAN-OPTIONAL-ROOT
   TL-SCAN-MANIFEST
   TL-CHECK-SITES
   TL-CHECK-STALE-ROWS
   TL-REPORT ;

: TRUST-LINT-SOURCE ( -- )
   TL-RESET
   TL-SOURCE-A @ TL-SOURCE-U @ TL-SCAN-SRC-FILE
   TL-SCAN-MANIFEST
   TL-CHECK-SITES
   TL-REPORT ;

: TL-CONFIG-TODAY ( n -- ) {: idx :}
   idx ARGV-POS$ 2dup PARSE-YMD 0= IF drop TL-BAD-TODAY THEN
   TL-TODAY-DAYS ! 2drop ;

: TL-CONFIG-SOURCE ( -- )
   ARGV-POS# 2 < IF s" wrong number of positional arguments" ARGV-FAIL THEN
   ARGV-POS# 4 > IF s" wrong number of positional arguments" ARGV-FAIL THEN
   1 ARGV-POS$ TL-SOURCE-U ! TL-SOURCE-A !
   ARGV-POS# 2 > IF 2 ARGV-POS$ TL-ROOT! ELSE s" ." TL-ROOT! THEN
   ARGV-POS# 3 > IF 3 TL-CONFIG-TODAY ELSE epoch-seconds DATE-SECONDS-DAY / TL-TODAY-DAYS ! THEN ;

: TL-CONFIG-ROOT ( -- )
   0 2 ARGV-EXPECT-POS
   ARGV-POS# 0 > IF 0 ARGV-POS$ TL-ROOT! ELSE s" ." TL-ROOT! THEN
   ARGV-POS# 1 > IF 1 TL-CONFIG-TODAY ELSE epoch-seconds DATE-SECONDS-DAY / TL-TODAY-DAYS ! THEN ;

: TL-CONFIG ( -- )
   s" tools/trust-lint.f [ROOT] [TODAY] | source-only SOURCE [ROOT] [TODAY]" ARGV-USAGE!
   ARGV-PARSE
   0 TL-SOURCE-ONLY !
   ARGV-POS# 0 > IF
      0 ARGV-POS$ s" source-only" STR= IF
         -1 TL-SOURCE-ONLY !
         TL-CONFIG-SOURCE
         exit
      THEN
   THEN
   TL-CONFIG-ROOT ;

: TL-MAIN ( -- )
   TL-CONFIG
   TL-SOURCE-ONLY @ IF TRUST-LINT-SOURCE ELSE TRUST-LINT THEN ;

TL-MAIN
