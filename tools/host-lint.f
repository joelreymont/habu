\ host-lint.f - reject retired host-script workflow hooks.
\ Load after tools/lint/lib.f and tools/fs.f.

0 set-check

$20000 constant HOST-CAP

create HOST-BUF HOST-CAP allot
create HOST-NUM 32 allot
create HOST-PAT-A 8 allot
create HOST-PAT-B 4 allot

variable HOST-BAD
variable HOST-LEN
variable HOST-NUM-L
variable HOST-PATH-A
variable HOST-PATH-U

: HOST-NL ( -- )
   10 emit ;

: HOST-U. ( u -- )
   0 HOST-NUM-L !
   dup 0= IF drop 48 emit exit THEN
   begin dup 0 > while
      dup 10 mod 48 + HOST-NUM HOST-NUM-L @ + c!
      10 /
      HOST-NUM-L @ 1+ HOST-NUM-L !
   repeat drop
   begin HOST-NUM-L @ 0 > while
      HOST-NUM-L @ 1- HOST-NUM-L !
      HOST-NUM HOST-NUM-L @ + c@ emit
   repeat ;

: HOST-PATTERNS ( -- )
   112 HOST-PAT-A c!
   121 HOST-PAT-A 1 + c!
   116 HOST-PAT-A 2 + c!
   104 HOST-PAT-A 3 + c!
   111 HOST-PAT-A 4 + c!
   110 HOST-PAT-A 5 + c!
   46 HOST-PAT-B c!
   112 HOST-PAT-B 1 + c!
   121 HOST-PAT-B 2 + c! ;

: HOST-FIND-CI {: a u b v :} ( -- idx|-1 )
   v 0= IF 0 exit THEN
   u v < IF -1 exit THEN
   0 begin dup u v - <= while
      dup a + v b v STR=CI IF exit THEN
      1+
   repeat drop -1 ;

: HOST-LINE# {: a u idx :} ( -- n )
   1 0 begin dup idx < while
      dup a + c@ 10 = IF swap 1+ swap THEN
      1+
   repeat drop ;

: HOST-TEXT? {: a u :} ( -- f )
   a u s" .f" HAS-EXT? IF -1 exit THEN
   a u s" .fs" HAS-EXT? IF -1 exit THEN
   a u s" .sh" HAS-EXT? IF -1 exit THEN
   a u s" .md" HAS-EXT? IF -1 exit THEN
   a u s" .tsv" HAS-EXT? IF -1 exit THEN
   a u s" .txt" HAS-EXT? IF -1 exit THEN
   a u s" .json" HAS-EXT? IF -1 exit THEN
   a u s" .jsonl" HAS-EXT? ;

: HOST-REPORT-PATH ( a u -- )
   s" HOST-LINT " type
   type
   s" : path contains retired host-script token" type HOST-NL
   HOST-BAD @ 1+ HOST-BAD ! ;

: HOST-REPORT-CONTENT {: a u line :} ( -- )
   s" HOST-LINT " type
   a u type 58 emit line HOST-U.
   s" : content contains retired host-script token" type HOST-NL
   HOST-BAD @ 1+ HOST-BAD ! ;

: HOST-PATH-BAD? ( a u -- f )
   2dup HOST-PAT-A 6 HOST-FIND-CI 0 >= IF 2drop -1 exit THEN
   HOST-PAT-B 3 HOST-FIND-CI 0 >= ;

: HOST-CHECK-A ( a u -- )
   HOST-PAT-A 6 HOST-FIND-CI
   dup 0 >= IF
      HOST-BUF HOST-LEN @ rot HOST-LINE#
      HOST-PATH-A @ HOST-PATH-U @ rot HOST-REPORT-CONTENT
   ELSE
      drop
   THEN ;

: HOST-CHECK-B ( a u -- )
   HOST-PAT-B 3 HOST-FIND-CI
   dup 0 >= IF
      HOST-BUF HOST-LEN @ rot HOST-LINE#
      HOST-PATH-A @ HOST-PATH-U @ rot HOST-REPORT-CONTENT
   ELSE
      drop
   THEN ;

: HOST-SCAN-CONTENT ( -- )
   HOST-BUF HOST-LEN @ HOST-CHECK-A
   HOST-BUF HOST-LEN @ HOST-CHECK-B ;

: HOST-SCAN-FILE {: a u :} ( -- )
   a u HOST-PATH-BAD? IF a u HOST-REPORT-PATH exit THEN
   a u HOST-TEXT? 0= IF exit THEN
   a HOST-PATH-A !  u HOST-PATH-U !
   a u HOST-BUF HOST-CAP READ-FILE nip HOST-LEN !
   HOST-SCAN-CONTENT ;

: HOST-LINT ( -- )
   HOST-PATTERNS
   0 HOST-BAD !
   s" ." ['] HOST-SCAN-FILE WALK-FILES
   s" host-lint: " type HOST-BAD @ HOST-U. s"  finding(s)" type HOST-NL
   HOST-BAD @ 0 > IF 1 throw THEN ;

HOST-LINT
