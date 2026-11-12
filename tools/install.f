\ install.f - installs the tree in the working directory into
\ $HOME/.local/lib/habu. Run from the root of a tree with its own bin/hb and
\ bin/hb.map: bin/hb --load tools/install.f
\ The destination ends holding the tree's bin/hb, bin/hb.map, src/, tools/ and
\ test/, without their symlinks, and its own .out. A file equal byte for byte,
\ and an hb equal apart from its signature, is left untouched, so its change
\ time keeps the objects client builds made from it. Every other file is written
\ beside the old one and renamed into place: a running hb keeps its old file,
\ and an hb overwritten in place keeps the inode whose code signature the kernel
\ cached, so the kernel kills it at launch (`echo bye | hb` dies by SIGKILL,
\ rc 137). hb goes last, so an engine lands only over the sources that match it.
\ The destination is locked for the whole run; a failed install is finished by
\ running it again. The tool signs nothing (the build signs) and writes nothing
\ outside the destination.

require src/lib/error.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/fs-lock.f
require src/lib/codesign.f
require src/core/include.f

package INSTALL

66 constant NOINPUT-RC
78 constant CONFIG-RC

create DEST PATH-CAP allot          variable DEST-U
create TO PATH-CAP allot            variable TO-U
create FROM PATH-CAP allot          variable FROM-U
create MSG 64 allot
DYNAMIC-BUFFER BYTES u8
\ Paths the prune removes once its walk ends, each followed by a zero byte: a
\ walk that unlinks entries of the directory it is reading can miss others.
DYNAMIC-BUFFER DOOMED u8            variable DOOMED-U

: DEST$ ( -- ptr u8 n ) DEST DEST-U @ ;
: TO$ ( -- ptr u8 n ) TO TO-U @ ;
: FROM$ ( -- ptr u8 n ) FROM FROM-U @ ;


\ A tree-relative name as a destination path and as a working-directory path.
: TO! ( ptr u8 n -- ) {: r:ptr ru:n :}
   DEST$ r ru TO FS:JOIN-PATH TO-U ! ;

: FROM! ( ptr u8 n -- ) {: r:ptr ru:n :}
   SOURCE-ROOT:CWD$ r ru FROM FS:JOIN-PATH FROM-U ! ;


\ The part of path p below a root of ru bytes.
: BELOW ( ptr u8 n n -- ptr u8 n ) {: p:ptr u:n ru:n :}
   p ru 1 + +  u ru 1 + - ;


: DEST! ( -- )
   s" HOME" GETENV {: h:ptr hu:n :}
   hu 0= if s" install: HOME is not set" CONFIG-RC die then
   h hu s" .local/lib/habu" DEST FS:JOIN-PATH DEST-U ! ;


: MISSING ( ptr u8 n -- ) {: a:ptr u:n :}
   s" install: the working directory has no " {: p:ptr pu:n :}
   p MSG pu BYTE-COPY
   a MSG pu + u BYTE-COPY
   MSG pu u + NOINPUT-RC die ;

: PLAIN-FILE? ( ptr u8 n -- bool )
   2dup FS:SYMLINK? if 2drop false exit then
   FS:FILE? ;

: PLAIN-DIR? ( ptr u8 n -- bool )
   2dup FS:SYMLINK? if 2drop false exit then
   FS:DIR? ;

: NEED-FILE ( ptr u8 n -- )
   2dup FROM! FROM$ FS:FILE? if 2drop exit then
   MISSING ;

: NEED-DIR ( ptr u8 n -- )
   2dup FROM! FROM$ PLAIN-DIR? if 2drop exit then
   MISSING ;


\ Writes nothing, so a tree that fails it leaves the destination as it was.
: CHECK ( -- )
   s" bin/hb" NEED-FILE
   s" bin/hb.map" NEED-FILE
   s" src" NEED-DIR
   s" tools" NEED-DIR
   s" test" NEED-DIR ;


: DOOM ( ptr u8 n -- ) {: a:ptr u:n :}
   DOOMED-U @ {: at:n :}
   at u + 1 + DOOMED-RESERVE
   a at DOOMED u BYTE-COPY
   0 at u + DOOMED c!
   at u + 1 + DOOMED-U ! ;

: DOOM-NAMED ( ptr u8 n ptr u8 n -- ) {: p:ptr u:n a:ptr au:n :}
   p u a au TO FS:JOIN-PATH TO-U !
   TO$ FS:TRY-LSTAT if TO$ DOOM then ;


\ The walk never shows an entry FS's SKIP-DIR? names, so the copy never
\ carries one and the prune looks each up in every kept directory. The
\ top-level .out stays.
: DOOM-HIDDEN ( ptr u8 n -- ) {: p:ptr u:n :}
   p u s" .jj" DOOM-NAMED
   p u s" .jj-ws" DOOM-NAMED
   p u s" .git" DOOM-NAMED
   p u s" .dots" DOOM-NAMED
   u DEST-U @ = if exit then
   p u s" .out" DOOM-NAMED ;


: TOP? ( ptr u8 n -- bool ) {: r:ptr ru:n :}
   r ru s" src" STR=  r ru s" tools" STR= or
   r ru s" test" STR= or  r ru s" bin" STR= or ;

: IN-BIN? ( ptr u8 n -- bool ) {: r:ptr ru:n :}
   ru 4 > if r 4 s" bin/" STR= else false then ;

: NESTED? ( ptr u8 n -- bool )
   FS:SLASH INDEX-OF MATCH option
      none OF false ENDOF
      some OF drop true ENDOF
   ;MATCH ;

: BIN-KEPT? ( ptr u8 n -- bool ) {: r:ptr ru:n :}
   r ru s" bin/hb" STR=  r ru s" bin/hb.map" STR= or ;


\ Whether destination entry p, at tree-relative r and not a symlink, is one
\ the tree carries.
: KEPT? ( ptr u8 n ptr u8 n -- bool ) {: r:ptr ru:n p:ptr u:n :}
   r ru NESTED? 0= if r ru TOP? p u FS:DIR? and exit then
   r ru IN-BIN? if r ru BIN-KEPT? p u FS:FILE? and exit then
   r ru FROM!
   p u FS:DIR? if FROM$ PLAIN-DIR? exit then
   p u FS:FILE? if FROM$ PLAIN-FILE? exit then
   false ;


\ The prune walk's filter: it dooms every entry the tree does not carry, and
\ descends only into the directories it keeps.
: PRUNE? ( ptr u8 n -- bool ) {: p:ptr u:n :}
   u DEST-U @ = if p u DOOM-HIDDEN true exit then
   p u FS:SYMLINK? if p u DOOM false exit then
   p u DEST-U @ BELOW p u KEPT? if
      p u FS:DIR? 0= if false exit then
      p u DOOM-HIDDEN true exit
   then
   p u DOOM false ;

: REMOVE ( ptr u8 n -- )
   2dup PLAIN-DIR? if FS:REMOVE-TREE else FS:REMOVE-FILE then ;

: DOOMED-LEN ( n -- n ) {: at:n :}
   at begin dup DOOMED c@ 0<> while 1+ repeat at - ;

: REMOVE-ONE ( n -- n ) {: at:n :}
   at DOOMED at DOOMED-LEN {: a:ptr u:n :}
   a u REMOVE
   at u + 1 + ;

: PRUNE ( -- )
   0 DOOMED-U !
   DEST$ [: PRUNE? ;] [: 2drop ;] FS:WALK-FILES-IF
   0 begin dup DOOMED-U @ < while REMOVE-ONE repeat drop
   DOOMED-RELEASE ;


\ Both files, A's bytes then B's. The byte reserved past them keeps index AU
\ inside the reserve when B is empty.
: READ-BOTH ( ptr u8 n ptr u8 n -- ptr u8 n ptr u8 n ) {: a:ptr au:n b:ptr bu:n :}
   a au FS:FILE-SIZE {: as:n :}
   b bu FS:FILE-SIZE {: bs:n :}
   as bs + 1 + BYTES-RESERVE
   a au 0 BYTES as FS:READ-ALL as <> if E-FS-IO throw then
   b bu as BYTES bs FS:READ-ALL bs <> if E-FS-IO throw then
   0 BYTES as  as BYTES bs ;

: SAME-FILE? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr au:n b:ptr bu:n :}
   b bu FS:FILE? 0= if false exit then
   a au FS:FILE-SIZE b bu FS:FILE-SIZE <> if false exit then
   a au b bu READ-BOTH STR= ;

: SAME-HB? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr au:n b:ptr bu:n :}
   b bu FS:FILE? 0= if false exit then
   a au b bu READ-BOTH CODESIGN:SAME-IMAGE? ;


\ REPLACE-STAGED fills: the source path rides above the sibling's.
: FILL-COPY ( ptr u8 n ptr u8 n -- ptr u8 n ) {: s:ptr su:n t:ptr tu:n :}
   s su t tu FS:COPY-FILE-STREAM
   s su ;

: FILL-HB ( ptr u8 n ptr u8 n -- ptr u8 n ) {: s:ptr su:n t:ptr tu:n :}
   s su t tu FS:COPY-FILE-STREAM
   t tu FS:CHMOD-X
   s su ;


\ The copy walk's filter: symlinks are not carried, and a directory exists in
\ the destination before its files are written.
: CARRY? ( ptr u8 n -- bool ) {: p:ptr u:n :}
   p u FS:SYMLINK? if false exit then
   p u FS:DIR? if p u SOURCE-ROOT:CWD$ nip BELOW TO! TO$ FS:MAKE-DIRS then
   true ;

: CARRY ( ptr u8 n -- ) {: p:ptr u:n :}
   p u SOURCE-ROOT:CWD$ nip BELOW TO!
   p u TO$ SAME-FILE? if exit then
   p u TO$ [: FILL-COPY ;] FS:REPLACE-STAGED 2drop ;

: CARRY-TREE ( ptr u8 n -- )
   FROM! FROM$ [: CARRY? ;] [: CARRY ;] FS:WALK-FILES-IF ;

: CARRY-MAP ( -- )
   s" bin/hb.map" FROM!  s" bin/hb.map" TO!
   FROM$ TO$ SAME-FILE? if exit then
   FROM$ TO$ [: FILL-COPY ;] FS:REPLACE-STAGED 2drop ;

: CARRY-HB ( -- )
   s" bin/hb" FROM!  s" bin/hb" TO!
   FROM$ TO$ SAME-HB? if exit then
   FROM$ TO$ [: FILL-HB ;] FS:REPLACE-STAGED 2drop ;


: COPY ( -- )
   s" src" CARRY-TREE
   s" tools" CARRY-TREE
   s" test" CARRY-TREE
   s" bin" TO! TO$ FS:MAKE-DIRS
   CARRY-MAP
   CARRY-HB
   BYTES-RELEASE ;


: MAIN ( -- )
   DEST!
   CHECK
   DEST$ FS:MAKE-DIRS
   DEST$ FS-LOCK:OPEN-DIR {: held:fd :}
   held FS-LOCK:WAIT
   PRUNE
   COPY
   held FD>N close
   s" install: installed into " type DEST$ type cr ;

MAIN
;package
