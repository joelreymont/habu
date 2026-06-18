\ fs-mutate.f - checked filesystem mutation helpers.
\
\ Load after lib/errors.f, lib/string.f, and lib/fs.f.

$FFF constant FS-MUT-MODE-PERM
73 constant FS-MUT-MODE-EXEC

create FS-MUT-PATHZ2-BUF FS-PATHZ-CAP allot

: FS-MUT-PATHZ2 ( ptr u8 n -- ptr u8 )
   FS-MUT-PATHZ2-BUF FS-PATHZ-INTO ;

: REMOVE-FILE ( ptr u8 n -- ) {: a:ptr u :}
   a u FS-PATHZ unlink 0 < if E-FS-IO throw then ;

: RENAME-FILE ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu dst:ptr dstu :}
   src srcu FS-PATHZ dst dstu FS-MUT-PATHZ2 rename 0 < if E-FS-IO throw then ;

: CHMOD-X ( ptr u8 n -- ) {: a:ptr u :}
   a u STAT-MODE FS-MUT-MODE-PERM and FS-MUT-MODE-EXEC or {: mode :}
   a u FS-PATHZ mode chmod 0 < if E-FS-IO throw then ;
