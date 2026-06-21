\ fs-mutate.f - checked filesystem mutation helpers.
\
\ Load after lib/errors.f, lib/string.f, and lib/fs.f.

$FFF constant FS-MUT-MODE-PERM
73 constant FS-MUT-MODE-EXEC
493 constant FS-MUT-MODE-DIR
8192 constant FS-MUT-COPY-CAP
64 constant FS-MUT-CLEANUP-MAX
0 constant FS-MUT-CLEANUP-FILE
1 constant FS-MUT-CLEANUP-DIR
2 constant FS-MUT-CLEANUP-TREE
$2D constant FS-MUT-DASH
$2E constant FS-MUT-DOT
$2F constant FS-MUT-SLASH

create FS-MUT-PATHZ2-BUF FS-PATHZ-CAP allot
create FS-MUT-COPY-BUF FS-MUT-COPY-CAP allot
create FS-MUT-ATOMIC-PATH FS-PATH-CAP allot
create FS-MUT-TMP-PATH FS-PATH-CAP allot
create FS-MUT-CLEANUP-PATHS FS-MUT-CLEANUP-MAX FS-PATH-CAP * allot
create FS-MUT-CLEANUP-US FS-MUT-CLEANUP-MAX cells allot
create FS-MUT-CLEANUP-KINDS FS-MUT-CLEANUP-MAX cells allot

variable FS-MUT-CLEANUP-N

create FS-MUT-ATOMIC-SUFFIX
   FS-MUT-DOT c, 116 c, 109 c, 112 c,

: FS-MUT-PATHZ2 ( ptr u8 n -- ptr u8 )
   FS-MUT-PATHZ2-BUF FS-PATHZ-INTO ;

: FS-MUT-CHECK-SUFFIX-CAP ( n n -- ) {: u su :}
   u 0 < if E-FS-PATH throw then
   su 0 < if E-FS-PATH throw then
   u su + FS-PATH-CAP > if E-FS-CAPACITY throw then ;

: FS-MUT-SUFFIX-PATH ( ptr u8 n ptr u8 n ptr u8 -- n ) {: a:ptr u s:ptr su dst:ptr :}
   u su FS-MUT-CHECK-SUFFIX-CAP
   a dst u BYTE-COPY
   s dst u + su BYTE-COPY
   u su + ;

: FS-MUT-CLEANUP-SLOT ( n -- ptr u8 ) {: idx :}
   idx 0 < if E-FS-CAPACITY throw then
   idx FS-MUT-CLEANUP-MAX >= if E-FS-CAPACITY throw then
   idx FS-PATH-CAP * FS-MUT-CLEANUP-PATHS + ;

: FS-MUT-CLEANUP-U-PTR ( n -- ptr n ) {: idx :}
   idx 0 < if E-FS-CAPACITY throw then
   idx FS-MUT-CLEANUP-MAX >= if E-FS-CAPACITY throw then
   idx cells FS-MUT-CLEANUP-US + ;

: FS-MUT-CLEANUP-KIND-PTR ( n -- ptr n ) {: idx :}
   idx 0 < if E-FS-CAPACITY throw then
   idx FS-MUT-CLEANUP-MAX >= if E-FS-CAPACITY throw then
   idx cells FS-MUT-CLEANUP-KINDS + ;

: REMOVE-FILE ( ptr u8 n -- ) {: a:ptr u :}
   a u FS-PATHZ unlink 0 < if E-FS-IO throw then ;

: RENAME-FILE ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu dst:ptr dstu :}
   src srcu FS-PATHZ dst dstu FS-MUT-PATHZ2 rename 0 < if E-FS-IO throw then ;

: CHMOD-X ( ptr u8 n -- ) {: a:ptr u :}
   a u STAT-MODE FS-MUT-MODE-PERM and FS-MUT-MODE-EXEC or {: mode :}
   a u FS-PATHZ mode chmod 0 < if E-FS-IO throw then ;

: MAKE-SYMLINK ( ptr u8 n ptr u8 n -- ) {: target:ptr targetu link:ptr linku :}
   target targetu FS-PATHZ link linku FS-MUT-PATHZ2 symlink 0 < if E-FS-IO throw then ;

: MKDIR-MODE ( ptr u8 n n -- ) {: a:ptr u mode :}
   a u FS-PATHZ mode mkdir 0 < if E-FS-IO throw then ;

: MAKE-DIR ( ptr u8 n -- )
   FS-MUT-MODE-DIR MKDIR-MODE ;

: REMOVE-DIR ( ptr u8 n -- ) {: a:ptr u :}
   a u FS-PATHZ rmdir 0 < if E-FS-IO throw then ;

: FS-MUT-REMOVE-FILE-WALK ( ptr u8 n -- ) {: a:ptr u :}
   a u FS-PATHZ unlink 0 < if E-FS-IO FS-THROW-WALK then ;

: FS-MUT-REMOVE-DIR-WALK ( ptr u8 n -- ) {: a:ptr u :}
   a u FS-PATHZ rmdir 0 < if E-FS-IO FS-THROW-WALK then ;

: FS-MUT-REMOVE-TREE-PATH ( ptr u8 n -- ) {: a:ptr u :}
   a u SYMLINK? if a u FS-MUT-REMOVE-FILE-WALK exit then
   a u EXISTS? 0= if exit then
   a u FILE? if a u FS-MUT-REMOVE-FILE-WALK exit then
   a u DIR? 0= if E-FS-STAT FS-THROW-WALK then
   FS-DEPTH @ 1 + FS-MAX-DEPTH >= if E-FS-DEPTH FS-THROW-WALK then
   a u FS-OPEN-DIR FS-FD!
   0 FS-BASE@ !
   begin FS-READ-DIR while
      0 FS-OFF!
      begin FS-OFF@ FS-N@ < while
         FS-CUR-DIR FS-OFF@ + FS-ENT !
         FS-ENT @ FS-DIRENT-RECLEN FS-REC!
         FS-CHECK-RECORD
         FS-ENT @ FS-DIRENT-NAME 2dup FS-DOT-ENTRY? 0= if
            2dup FS-DOTDOT-ENTRY? 0= if
               FS-NAME-U ! FS-NAME-A !
               a u FS-NAME-U @ FS-CHECK-WALK-JOIN-CAP
               a u FS-NAME-A @ FS-NAME-U @ FS-NEXT-PATH JOIN-PATH FS-CHILD-U !
               FS-DEPTH @ 1 + FS-DEPTH !
               FS-CUR-PATH FS-CHILD-U @ RECURSE
               FS-DEPTH @ 1 - FS-DEPTH !
            else
               2drop
            then
         else
            2drop
         then
         FS-OFF@ FS-REC@ + FS-OFF!
      repeat
   repeat
   FS-FD@ close
   -1 FS-FD!
   a u FS-MUT-REMOVE-DIR-WALK ;

: REMOVE-TREE ( ptr u8 n -- ) {: a:ptr u :}
   u 0 <= if E-FS-PATH throw then
   FS-FDS-RESET
   0 FS-DEPTH !
   a u FS-WALK-ROOT!
   FS-CUR-PATH u FS-MUT-REMOVE-TREE-PATH ;

: FS-MUT-MKDIR-ONE ( ptr u8 n -- ) {: a:ptr u :}
   a u FS-PATHZ FS-MUT-MODE-DIR mkdir {: rc :}
   rc 0 < if
      a u DIR? 0= if E-FS-IO throw then
   then ;

: MAKE-DIRS ( ptr u8 n -- ) {: a:ptr u :}
   u 0 <= if E-FS-PATH throw then
   1 begin dup u < while
      a over + c@ FS-MUT-SLASH = if
         dup 0 > if a over FS-MUT-MKDIR-ONE then
      then
      1+
   repeat drop
   a u FS-MUT-MKDIR-ONE ;

: COPY-FILE ( ptr u8 n ptr u8 n n -- ) {: src:ptr srcu dst:ptr dstu cap :}
   cap 0 < if E-FS-CAPACITY throw then
   cap FS-MUT-COPY-CAP > if E-FS-CAPACITY throw then
   src srcu FS-MUT-COPY-BUF cap READ-ALL {: n :}
   dst dstu FS-MUT-COPY-BUF n WRITE-ALL ;

: ATOMIC-WRITE-FILE ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu src:ptr srcu :}
   path pathu FS-MUT-ATOMIC-SUFFIX 4 FS-MUT-ATOMIC-PATH FS-MUT-SUFFIX-PATH {: tempu :}
   FS-MUT-ATOMIC-PATH tempu src srcu WRITE-ALL
   FS-MUT-ATOMIC-PATH tempu path pathu RENAME-FILE ;

: FS-MUT-SB-U ( n -- ) {: n :}
   n 0 < if E-FS-PATH throw then
   n 10 >= if n 10 / RECURSE then
   n 10 mod 48 + SB-APPEND-C ;

: FS-MUT-BUILD-TEMP ( ptr u8 n ptr u8 n -- ptr u8 n ) {: base:ptr baseu prefix:ptr prefixu :}
   SB-RESET
   base baseu SB-APPEND
   FS-MUT-SLASH SB-APPEND-C
   prefix prefixu SB-APPEND
   FS-MUT-DASH SB-APPEND-C
   mono-ns FS-MUT-SB-U
   SB$ {: a:ptr u :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a FS-MUT-TMP-PATH u BYTE-COPY
   FS-MUT-TMP-PATH u ;

: MAKE-TEMP-DIR ( ptr u8 n ptr u8 n -- ptr u8 n )
   FS-MUT-BUILD-TEMP 2dup MAKE-DIR ;

: TMPDIR-MKDIR ( ptr u8 n -- ptr u8 n ) {: prefix:ptr prefixu :}
   s" TMPDIR" GETENV dup 0= if 2drop s" /tmp" then
   prefix prefixu MAKE-TEMP-DIR ;

: CLEANUP-RESET ( -- )
   0 FS-MUT-CLEANUP-N ! ;

: FS-MUT-CLEANUP+ ( ptr u8 n n -- ) {: a:ptr u kind :}
   FS-MUT-CLEANUP-N @ FS-MUT-CLEANUP-MAX >= if E-FS-CAPACITY throw then
   u 0 < if E-FS-PATH throw then
   u FS-PATH-CAP > if E-FS-PATH throw then
   kind FS-MUT-CLEANUP-FILE <>
   kind FS-MUT-CLEANUP-DIR <> and
   kind FS-MUT-CLEANUP-TREE <> and if E-FS-IO throw then
   a FS-MUT-CLEANUP-N @ FS-MUT-CLEANUP-SLOT u BYTE-COPY
   u FS-MUT-CLEANUP-N @ FS-MUT-CLEANUP-U-PTR !
   kind FS-MUT-CLEANUP-N @ FS-MUT-CLEANUP-KIND-PTR !
   FS-MUT-CLEANUP-N @ 1 + FS-MUT-CLEANUP-N ! ;

: CLEANUP+ ( ptr u8 n -- )
   FS-MUT-CLEANUP-FILE FS-MUT-CLEANUP+ ;

: CLEANUP-DIR+ ( ptr u8 n -- )
   FS-MUT-CLEANUP-DIR FS-MUT-CLEANUP+ ;

: CLEANUP-TREE+ ( ptr u8 n -- )
   FS-MUT-CLEANUP-TREE FS-MUT-CLEANUP+ ;

: FS-MUT-CLEANUP-REMOVE ( n -- ) {: idx :}
   idx FS-MUT-CLEANUP-SLOT idx FS-MUT-CLEANUP-U-PTR @ {: a:ptr u :}
   idx FS-MUT-CLEANUP-KIND-PTR @ {: kind :}
   a u SYMLINK? if a u REMOVE-FILE exit then
   a u EXISTS? 0= if exit then
   kind FS-MUT-CLEANUP-TREE = if
      a u REMOVE-TREE
   else
   kind FS-MUT-CLEANUP-DIR = if
      a u REMOVE-DIR
   else
      a u REMOVE-FILE
   then then ;

: CLEANUP-RUN ( -- )
   FS-MUT-CLEANUP-N @ begin dup 0 > while
      1 - dup FS-MUT-CLEANUP-REMOVE
   repeat drop
   CLEANUP-RESET ;
