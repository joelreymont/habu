\ ref-solutions.f — certified habu answer key for the array-algorithm benchmark.
\ Proves every array task is FEASIBLE in habu and that the io-vector
\ ground truth in the harness=array rows of tasks.tsv is correct. Verify two ways:
\   bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f
\      lib/process.f lib/process-argv.f lib/source.f tools/argv.f
\      tools/check.f -- bench/llm/ref-solutions.f -> rc 0 (all defs certify)
\   bin/hb < bench/llm/ref-solutions.f         -> prints REF-OK (all io-vectors pass)
variable BI  variable BV
variable REF-SPLIT-A
variable REF-SPLIT-U
variable REF-SPLIT-NEXT
variable REF-SPLIT-OK
8 constant REF-MAP-CAP
create REF-MAP REF-MAP-CAP MAP-CELLS cells allot
variable REF-MAP-V
variable REF-MAP-OK
variable REF-MAP-EACH-COUNT
variable REF-MAP-EACH-SUM
variable REF-MAP-EACH-LEN-SUM
64 constant REF-RX-CAP
create REF-RX REF-RX-CAP allot
variable REF-RX-LEN
variable REF-RX-OFF
variable REF-RX-U
variable REF-RX-OK
create REF-FS-OUT FS-PATH-CAP allot

: ARR-SUM    ( ptr a n -- i64 ) {: arr:ptr len :} 0 len 0 ?do i cells arr + @ + loop ;
: ARR-MAX    ( ptr a n -- i64 ) {: arr:ptr len :} arr @ len 1 ?do i cells arr + @ max loop ;
: ARR-MIN    ( ptr a n -- i64 ) {: arr:ptr len :} arr @ len 1 ?do i cells arr + @ min loop ;
: ARGMAX     ( ptr a n -- i64 ) {: arr:ptr len :} arr @ BV ! 0 BI ! len 1 ?do i cells arr + @ BV @ > if i cells arr + @ BV ! i BI ! then loop BI @ ;
: COUNT-EVEN ( ptr a n -- i64 ) {: arr:ptr len :} 0 len 0 ?do i cells arr + @ 2 mod 0= if 1+ then loop ;
: REVERSE    ( ptr a n -- ) {: arr:ptr len :} len 2 / 0 ?do i cells arr + @ len 1 - i - cells arr + @ i cells arr + ! len 1 - i - cells arr + ! loop ;
: PREFIXSUM  ( ptr a n -- ) {: arr:ptr len :} len 1 ?do i 1 - cells arr + @ i cells arr + @ + i cells arr + ! loop ;
: SQ-EACH    ( ptr a n -- ) {: arr:ptr len :} len 0 ?do i cells arr + @ dup * i cells arr + ! loop ;
: NEGATE-EACH ( ptr a n -- ) {: arr:ptr len :} len 0 ?do i cells arr + @ negate i cells arr + ! loop ;
: RUNMAX     ( ptr a n -- ) {: arr:ptr len :} len 1 ?do i 1 - cells arr + @ i cells arr + @ max i cells arr + ! loop ;
: FIND-FIRST-NEG ( ptr a n -- i64 ) {: arr:ptr len :} -1 BI ! len 0 ?do BI @ -1 = if i cells arr + @ 0 < if i BI ! then then loop BI @ ;
: ABS-EACH ( ptr a n -- ) {: arr:ptr len :} len 0 ?do i cells arr + @ abs i cells arr + ! loop ;
: ADD-INDEX ( ptr a n -- ) {: arr:ptr len :} len 0 ?do i cells arr + @ i + i cells arr + ! loop ;
: PREFIX-PROD ( ptr a n -- ) {: arr:ptr len :} len 1 ?do i 1 - cells arr + @ i cells arr + @ * i cells arr + ! loop ;
: REVERSE-INNER ( ptr a n -- ) {: arr:ptr len :} len 2 <= if exit then len 2 - 2 / 0 ?do 1 i + cells arr + @ len 2 - i - cells arr + @ 1 i + cells arr + ! len 2 - i - cells arr + ! loop ;

32 constant REF-DATE-BUF-LEN
create REF-DATE-BUF REF-DATE-BUF-LEN allot

: REF-BYTES= ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   u v <> if 0 0= 0= exit then
   0 begin dup u < while
      dup a + c@ over b + c@ <> if drop 0 0= 0= exit then
      1+
   repeat drop 0 0= ;

: DATE-PARSE-OK? ( -- bool )
   s" 2026-06-16" PARSE-YMD swap 20620 = and ;

: DATE-FORMAT-OK? ( -- bool )
   20620 REF-DATE-BUF REF-DATE-BUF-LEN FORMAT-YMD
   s" 2026-06-16" REF-BYTES= ;

: EPOCH-UTC-OK? ( -- bool )
   90061 REF-DATE-BUF REF-DATE-BUF-LEN FORMAT-EPOCH-UTC
   s" 1970-01-02T01:01:01Z" REF-BYTES= ;

: MONO-ELAPSED? ( -- bool )
   TIME-MONO-NS 0 100000 0 do i + loop drop TIME-MONO-NS swap - 0 >= ;

: INVALID-DATE? ( -- bool )
   s" 2026-02-29" PARSE-YMD 0= swap drop ;

: STR-TRIM-OK? ( -- bool )
   s"   alpha  " TRIM s" alpha" STR= ;

: REF-SPLIT-CAPTURE ( ptr u8 n n n -- )
   SPLIT-NEXT
   REF-SPLIT-OK !
   REF-SPLIT-NEXT !
   REF-SPLIT-U !
   REF-SPLIT-A ! ;

: STR-SPLIT-OK? ( -- bool )
   s" a,b" 44 0 REF-SPLIT-CAPTURE
   REF-SPLIT-OK @ 0= if STR-FALSE exit then
   REF-SPLIT-A @ REF-SPLIT-U @ s" a" STR= 0= if STR-FALSE exit then
   REF-SPLIT-NEXT @ 2 <> if STR-FALSE exit then
   s" a,b" 44 REF-SPLIT-NEXT @ REF-SPLIT-CAPTURE
   REF-SPLIT-OK @ 0= if STR-FALSE exit then
   REF-SPLIT-A @ REF-SPLIT-U @ s" b" STR= 0= if STR-FALSE exit then
   REF-SPLIT-NEXT @ 4 <> if STR-FALSE exit then
   STR-TRUE ;

: STR-BUILDER-OK? ( -- bool )
   SB-RESET
   s" alpha" SB-APPEND
   45 SB-APPEND-C
   s" beta" SB-APPEND
   SB$ s" alpha-beta" STR= ;

: STR-PARSE-I64-OK? ( -- bool )
   s" -9223372036854775808" STR>NUMBER? 0= if drop STR-FALSE exit then
   STR-MIN-I64 <> if STR-FALSE exit then
   s" 9223372036854775808" STR>NUMBER? if drop STR-FALSE else drop STR-TRUE then ;

: STR-PREFIX-SUFFIX-OK? ( -- bool )
   s" habu-forth" s" habu" STARTS-WITH? 0= if STR-FALSE exit then
   s" habu-forth" s" forth" ENDS-WITH? ;

: STR-SEARCH-OK? ( -- bool )
   s" alpha-beta" s" beta" FIND-SUB 6 <> if STR-FALSE exit then
   s" alpha-beta" s" ph" CONTAINS? 0= if STR-FALSE exit then
   s" alpha-beta" s" gamma" CONTAINS? if STR-FALSE else STR-TRUE then ;

: REF-MAP-GET! ( ptr a n ptr u8 n -- )
   MAP-GET
   REF-MAP-OK !
   REF-MAP-V ! ;

: REF-MAP-EXPECT ( ptr u8 n n -- bool ) {: key:ptr len want :}
   REF-MAP REF-MAP-CAP key len REF-MAP-GET!
   REF-MAP-OK @ 0= if STR-FALSE exit then
   REF-MAP-V @ want = ;

: REF-MAP-EACH-RESET ( -- )
   0 REF-MAP-EACH-COUNT !
   0 REF-MAP-EACH-SUM !
   0 REF-MAP-EACH-LEN-SUM ! ;

: REF-MAP-EACH-RECORD ( ptr u8 n n -- ) {: key:ptr len value :}
   REF-MAP-EACH-COUNT @ 1 + REF-MAP-EACH-COUNT !
   REF-MAP-EACH-SUM @ value + REF-MAP-EACH-SUM !
   REF-MAP-EACH-LEN-SUM @ len + REF-MAP-EACH-LEN-SUM ! ;

: REF-MAP-INCR ( ptr u8 n -- ) {: key:ptr len :}
   REF-MAP REF-MAP-CAP key len MAP-GET if
      1 +
   else
      drop 1
   then
   REF-MAP REF-MAP-CAP key len MAP-SET ;

: MAP-COUNT-OK? ( -- bool )
   REF-MAP REF-MAP-CAP MAP-INIT
   10 REF-MAP REF-MAP-CAP s" alpha" MAP-SET
   20 REF-MAP REF-MAP-CAP s" beta" MAP-SET
   30 REF-MAP REF-MAP-CAP s" gamma" MAP-SET
   REF-MAP MAP-COUNT@ 3 <> if STR-FALSE exit then
   s" beta" 20 REF-MAP-EXPECT ;

: MAP-MISS-OK? ( -- bool )
   REF-MAP REF-MAP-CAP MAP-INIT
   REF-MAP REF-MAP-CAP s" missing" REF-MAP-GET!
   REF-MAP-OK @ if STR-FALSE exit then
   REF-MAP-V @ 0 <> if STR-FALSE exit then
   REF-MAP REF-MAP-CAP s" missing" MAP-HAS? 0= ;

: MAP-UPDATE-OK? ( -- bool )
   REF-MAP REF-MAP-CAP MAP-INIT
   1 REF-MAP REF-MAP-CAP s" alpha" MAP-SET
   7 REF-MAP REF-MAP-CAP s" alpha" MAP-SET
   REF-MAP MAP-COUNT@ 1 <> if STR-FALSE exit then
   s" alpha" 7 REF-MAP-EXPECT ;

: MAP-COLLISION-OK? ( -- bool )
   REF-MAP REF-MAP-CAP MAP-INIT
   11 REF-MAP REF-MAP-CAP s" a" MAP-SET
   22 REF-MAP REF-MAP-CAP s" i" MAP-SET
   33 REF-MAP REF-MAP-CAP s" q" MAP-SET
   REF-MAP MAP-COUNT@ 3 <> if STR-FALSE exit then
   s" a" 11 REF-MAP-EXPECT 0= if STR-FALSE exit then
   s" i" 22 REF-MAP-EXPECT 0= if STR-FALSE exit then
   s" q" 33 REF-MAP-EXPECT ;

: MAP-EACH-OK? ( -- bool )
   REF-MAP REF-MAP-CAP MAP-INIT
   10 REF-MAP REF-MAP-CAP s" a" MAP-SET
   20 REF-MAP REF-MAP-CAP s" b" MAP-SET
   30 REF-MAP REF-MAP-CAP s" c" MAP-SET
   REF-MAP-EACH-RESET
   REF-MAP REF-MAP-CAP [: REF-MAP-EACH-RECORD ;] MAP-EACH
   REF-MAP-EACH-COUNT @ 3 <> if STR-FALSE exit then
   REF-MAP-EACH-SUM @ 60 <> if STR-FALSE exit then
   REF-MAP-EACH-LEN-SUM @ 3 = ;

: MAP-GROUP-OK? ( -- bool )
   REF-MAP REF-MAP-CAP MAP-INIT
   s" red" REF-MAP-INCR
   s" blue" REF-MAP-INCR
   s" red" REF-MAP-INCR
   REF-MAP MAP-COUNT@ 2 <> if STR-FALSE exit then
   s" red" 2 REF-MAP-EXPECT 0= if STR-FALSE exit then
   s" blue" 1 REF-MAP-EXPECT ;

: REF-RX-COMPILE! ( ptr u8 n -- )
   REF-RX REF-RX-CAP RX-COMPILE REF-RX-LEN ! ;

: REF-RX-FIND! ( ptr u8 n -- )
   REF-RX REF-RX-LEN @ RX-FIND
   REF-RX-OK !
   REF-RX-U !
   REF-RX-OFF ! ;

: RX-MATCH-OK? ( -- bool )
   s" ^[a-z]+-[0-9]+$" REF-RX-COMPILE!
   s" habu-2026" REF-RX REF-RX-LEN @ RX-MATCH? 0= if STR-FALSE exit then
   s" habu-xx" REF-RX REF-RX-LEN @ RX-MATCH? if STR-FALSE else STR-TRUE then ;

: RX-FIND-OK? ( -- bool )
   s" a.+c" REF-RX-COMPILE!
   s" zzaXczz" REF-RX-FIND!
   REF-RX-OK @ 0= if STR-FALSE exit then
   REF-RX-OFF @ 2 <> if STR-FALSE exit then
   REF-RX-U @ 3 = ;

: RX-COUNT-OK? ( -- bool )
   s" [0-9]" REF-RX-COMPILE!
   s" a1b23" REF-RX REF-RX-LEN @ RX-COUNT 3 = ;

: REF-FS-JOIN$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: pa:ptr pu na:ptr nu :}
   pa pu na nu REF-FS-OUT JOIN-PATH
   REF-FS-OUT swap ;

: FS-PATH-KINDS-OK? ( -- bool )
   s" AGENTS.md" EXISTS? 0= if STR-FALSE exit then
   s" AGENTS.md" FILE? 0= if STR-FALSE exit then
   s" AGENTS.md" DIR? if STR-FALSE exit then
   s" src" DIR? 0= if STR-FALSE exit then
   s" src" FILE? if STR-FALSE else STR-TRUE then ;

: FS-BASENAME-OK? ( -- bool )
   s" src/core/checker.f" BASENAME s" checker.f" STR= 0= if STR-FALSE exit then
   s" src/" BASENAME s" " STR= 0= if STR-FALSE exit then
   s" file.f" BASENAME s" file.f" STR= ;

: FS-JOIN-OK? ( -- bool )
   s" src" s" core" REF-FS-JOIN$ s" src/core" STR= 0= if STR-FALSE exit then
   s" src/" s" core" REF-FS-JOIN$ s" src/core" STR= 0= if STR-FALSE exit then
   s" /" s" tmp" REF-FS-JOIN$ s" /tmp" STR= ;

variable AP  variable #BAD  0 #BAD !
: G= ( n n -- ) <> if 1 #BAD +! then ;

\ array->scalar checks
here 3 , 1 , 4 , 1 , 5 , AP !  AP @ 5 ARR-SUM 14 G=
here 1 , 2 , 3 , AP !          AP @ 3 ARR-SUM 6 G=
here -2 , -3 , AP !            AP @ 2 ARR-SUM -5 G=
here 3 , 1 , 4 , 1 , 5 , AP !  AP @ 5 ARR-MAX 5 G=
here -5 , -2 , -9 , AP !       AP @ 3 ARR-MAX -2 G=
here 3 , 1 , 4 , 1 , 5 , AP !  AP @ 5 ARR-MIN 1 G=
here -5 , -2 , -9 , AP !       AP @ 3 ARR-MIN -9 G=
here 3 , 1 , 4 , 1 , 5 , AP !  AP @ 5 ARGMAX 4 G=
here 1 , 5 , 5 , 2 , AP !      AP @ 4 ARGMAX 1 G=
here 9 , 1 , 1 , AP !          AP @ 3 ARGMAX 0 G=
here 3 , 1 , 4 , 1 , 5 , AP !  AP @ 5 COUNT-EVEN 1 G=
here 2 , 4 , 6 , AP !          AP @ 3 COUNT-EVEN 3 G=
here 0 , 2 , 0 , AP !          AP @ 3 COUNT-EVEN 3 G=

\ array->array checks (in place; read back each cell)
here 1 , 2 , 3 , AP !  AP @ 3 REVERSE  AP @ 0 cells + @ 3 G= AP @ 1 cells + @ 2 G= AP @ 2 cells + @ 1 G=
here 1 , 2 , AP !      AP @ 2 REVERSE  AP @ 0 cells + @ 2 G= AP @ 1 cells + @ 1 G=
here 3 , 1 , 4 , 1 , 5 , AP !  AP @ 5 PREFIXSUM  AP @ 0 cells + @ 3 G= AP @ 1 cells + @ 4 G= AP @ 2 cells + @ 8 G= AP @ 3 cells + @ 9 G= AP @ 4 cells + @ 14 G=
here 2 , -1 , 3 , AP !  AP @ 3 PREFIXSUM  AP @ 0 cells + @ 2 G= AP @ 1 cells + @ 1 G= AP @ 2 cells + @ 4 G=
here -2 , 3 , AP !  AP @ 2 SQ-EACH  AP @ 0 cells + @ 4 G= AP @ 1 cells + @ 9 G=
here -2 , 0 , 7 , AP !  AP @ 3 NEGATE-EACH  AP @ 0 cells + @ 2 G= AP @ 1 cells + @ 0 G= AP @ 2 cells + @ -7 G=
here 2 , 7 , 1 , AP !  AP @ 3 RUNMAX  AP @ 0 cells + @ 2 G= AP @ 1 cells + @ 7 G= AP @ 2 cells + @ 7 G=
here 5 , 4 , 3 , AP !  AP @ 3 RUNMAX  AP @ 0 cells + @ 5 G= AP @ 1 cells + @ 5 G= AP @ 2 cells + @ 5 G=
here 3 , -1 , 4 , -2 , AP !  AP @ 4 FIND-FIRST-NEG 1 G=
here 1 , 2 , 3 , AP !        AP @ 3 FIND-FIRST-NEG -1 G=
here -5 , AP !               AP @ 1 FIND-FIRST-NEG 0 G=
here 0 , -1 , AP !           AP @ 2 FIND-FIRST-NEG 1 G=
here 3 , -1 , -4 , 0 , AP !  AP @ 4 ABS-EACH  AP @ 0 cells + @ 3 G= AP @ 1 cells + @ 1 G= AP @ 2 cells + @ 4 G= AP @ 3 cells + @ 0 G=
here -2 , 3 , AP !           AP @ 2 ABS-EACH  AP @ 0 cells + @ 2 G= AP @ 1 cells + @ 3 G=
here 3 , 1 , 4 , AP !        AP @ 3 ADD-INDEX  AP @ 0 cells + @ 3 G= AP @ 1 cells + @ 2 G= AP @ 2 cells + @ 6 G=
here 0 , 0 , 0 , AP !        AP @ 3 ADD-INDEX  AP @ 0 cells + @ 0 G= AP @ 1 cells + @ 1 G= AP @ 2 cells + @ 2 G=
here 2 , 3 , 4 , AP !        AP @ 3 PREFIX-PROD  AP @ 0 cells + @ 2 G= AP @ 1 cells + @ 6 G= AP @ 2 cells + @ 24 G=
here -1 , 2 , -3 , AP !      AP @ 3 PREFIX-PROD  AP @ 0 cells + @ -1 G= AP @ 1 cells + @ -2 G= AP @ 2 cells + @ 6 G=
here 1 , 2 , 3 , 4 , 5 , AP !  AP @ 5 REVERSE-INNER  AP @ 0 cells + @ 1 G= AP @ 1 cells + @ 4 G= AP @ 2 cells + @ 3 G= AP @ 3 cells + @ 2 G= AP @ 4 cells + @ 5 G=
here 1 , 2 , 3 , 4 , AP !    AP @ 4 REVERSE-INNER  AP @ 0 cells + @ 1 G= AP @ 1 cells + @ 3 G= AP @ 2 cells + @ 2 G= AP @ 3 cells + @ 4 G=
here 1 , 2 , AP !            AP @ 2 REVERSE-INNER  AP @ 0 cells + @ 1 G= AP @ 1 cells + @ 2 G=

\ stdlib date/time checks
DATE-PARSE-OK? -1 G=
DATE-FORMAT-OK? -1 G=
EPOCH-UTC-OK? -1 G=
MONO-ELAPSED? -1 G=
INVALID-DATE? -1 G=
STR-TRIM-OK? -1 G=
STR-SPLIT-OK? -1 G=
STR-BUILDER-OK? -1 G=
STR-PARSE-I64-OK? -1 G=
STR-PREFIX-SUFFIX-OK? -1 G=
STR-SEARCH-OK? -1 G=
MAP-COUNT-OK? -1 G=
MAP-MISS-OK? -1 G=
MAP-UPDATE-OK? -1 G=
MAP-COLLISION-OK? -1 G=
MAP-EACH-OK? -1 G=
MAP-GROUP-OK? -1 G=
RX-MATCH-OK? -1 G=
RX-FIND-OK? -1 G=
RX-COUNT-OK? -1 G=
FS-PATH-KINDS-OK? -1 G=
FS-BASENAME-OK? -1 G=
FS-JOIN-OK? -1 G=

: REP #BAD @ 0= if ." REF-OK" else ." REF-FAIL bad=" #BAD @ . then cr ; REP
