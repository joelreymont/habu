\ install.f - installs the tree in the working directory as the current Habu.
\ Run from the root of a tree with its own bin/hb and bin/hb.map:
\ bin/hb --load tools/install.f
\ It copies the tree's bin/hb, bin/hb.map, src/, tools/ and test/, without their
\ symlinks, into a new $HOME/.local/lib/habu-trees/tree-<seed>-<attempt>, then
\ renames a fresh link, habu.next, over $HOME/.local/lib/habu to name it. An hb
\ whose working directory is no Habu tree reads the tree its executable's real
\ path is in, so one started before the rename reads its old tree whole and one
\ started after reads the new one. macOS resolves that path when the root is
\ first needed, not at exec, so there an hb started as the link moves can pair
\ the old engine with the new tree. Before it builds, an install removes every
\ entry of habu-trees that is neither the link's target nor run by a process,
\ and a habu.next a dead install left. An hb whose tree is removed under it
\ (one that holds it only as working directory, one the scan cannot see, or one
\ still inside exec while two installs pass) fails `include: cannot open`.
\ Installs hold a lock on habu-trees; a failed one is finished by running it
\ again. The tool signs nothing (the build signs).

require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/fs-lock.f
require src/lib/fs-list.f
require src/lib/fs-identity.f
require src/lib/process-tree.f
require src/core/include.f

package INSTALL

66 constant NOINPUT-RC
78 constant CONFIG-RC

create LIB PATH-CAP allot           variable LIB-U
create LIB-TREES PATH-CAP allot     variable LIB-TREES-U
create LINK PATH-CAP allot          variable LINK-U
create NEXT PATH-CAP allot          variable NEXT-U
create TREE PATH-CAP allot          variable TREE-U
create ENTRY PATH-CAP allot         variable ENTRY-U
create ENTRY-HB PATH-CAP allot      variable ENTRY-HB-U
create TO PATH-CAP allot            variable TO-U
create FROM PATH-CAP allot          variable FROM-U
create MSG 64 allot

: LIB$ ( -- ptr u8 n ) LIB LIB-U @ ;
: LIB-TREES$ ( -- ptr u8 n ) LIB-TREES LIB-TREES-U @ ;
: LINK$ ( -- ptr u8 n ) LINK LINK-U @ ;
: NEXT$ ( -- ptr u8 n ) NEXT NEXT-U @ ;
: TREE$ ( -- ptr u8 n ) TREE TREE-U @ ;
: ENTRY$ ( -- ptr u8 n ) ENTRY ENTRY-U @ ;
: ENTRY-HB$ ( -- ptr u8 n ) ENTRY-HB ENTRY-HB-U @ ;
: TO$ ( -- ptr u8 n ) TO TO-U @ ;
: FROM$ ( -- ptr u8 n ) FROM FROM-U @ ;


\ A tree-relative name as a path in the new tree and as a working-directory
\ path.
: TO! ( ptr u8 n -- ) {: r:ptr ru:n :}
   TREE$ r ru TO FS:JOIN-PATH TO-U ! ;

: FROM! ( ptr u8 n -- ) {: r:ptr ru:n :}
   SOURCE-ROOT:CWD$ r ru FROM FS:JOIN-PATH FROM-U ! ;


\ The part of path p below a root of ru bytes.
: BELOW ( ptr u8 n n -- ptr u8 n ) {: p:ptr u:n ru:n :}
   p ru 1 + +  u ru 1 + - ;


: PATHS! ( -- )
   s" HOME" GETENV {: h:ptr hu:n :}
   hu 0= if s" install: HOME is not set" CONFIG-RC die then
   h hu s" .local/lib" LIB FS:JOIN-PATH LIB-U !
   LIB$ s" habu-trees" LIB-TREES FS:JOIN-PATH LIB-TREES-U !
   LIB$ s" habu" LINK FS:JOIN-PATH LINK-U !
   LIB$ s" habu.next" NEXT FS:JOIN-PATH NEXT-U ! ;


: MISSING ( ptr u8 n -- ) {: a:ptr u:n :}
   s" install: the working directory has no " {: p:ptr pu:n :}
   p MSG pu BYTE-COPY
   a MSG pu + u BYTE-COPY
   MSG pu u + NOINPUT-RC die ;

: PLAIN-DIR? ( ptr u8 n -- bool )
   2dup FS:SYMLINK? if 2drop false exit then
   FS:DIR? ;

: NEED-FILE ( ptr u8 n -- )
   2dup FROM! FROM$ FS:FILE? if 2drop exit then
   MISSING ;

: NEED-DIR ( ptr u8 n -- )
   2dup FROM! FROM$ PLAIN-DIR? if 2drop exit then
   MISSING ;


\ Writes nothing, so a tree that fails it leaves the installation as it was.
: CHECK ( -- )
   s" bin/hb" NEED-FILE
   s" bin/hb.map" NEED-FILE
   s" src" NEED-DIR
   s" tools" NEED-DIR
   s" test" NEED-DIR
   LINK$ FS:TRY-LSTAT if LINK$ FS:SYMLINK? 0= if
      s" install: ~/.local/lib/habu is not a link; make ~/.local/lib/habu-trees, move habu to habu-trees/tree-0 and link habu to habu-trees/tree-0"
      CONFIG-RC die
   then then ;


\ Whether a process runs the hb of the tree at path p.
: RUN? ( ptr u8 n -- bool )
   s" bin/hb" ENTRY-HB FS:JOIN-PATH ENTRY-HB-U !
   ENTRY-HB$ FS:FILE? if ENTRY-HB$ PROC-TREE:RUNS? else false then ;

: SWEEP-ONE ( ptr u8 n -- ) {: a:ptr u:n :}
   LIB-TREES$ a u ENTRY FS:JOIN-PATH ENTRY-U !
   ENTRY$ LINK$ FS:SAMEFILE if exit then
   ENTRY$ RUN? if exit then
   ENTRY$ FS:REMOVE-TREE ;

: SWEEP ( -- )
   NEXT$ FS:TRY-LSTAT if NEXT$ FS:REMOVE-FILE then
   LIB-TREES$ [: SWEEP-ONE ;] FS-LIST:EACH ;


\ The copy walk's filter: symlinks are not carried, and a directory exists in
\ the new tree before its files are written.
: CARRY? ( ptr u8 n -- bool ) {: p:ptr u:n :}
   p u FS:SYMLINK? if false exit then
   p u FS:DIR? if p u SOURCE-ROOT:CWD$ nip BELOW TO! TO$ FS:MAKE-DIRS then
   true ;

: CARRY ( ptr u8 n -- ) {: p:ptr u:n :}
   p u SOURCE-ROOT:CWD$ nip BELOW TO!
   p u TO$ FS:COPY-FILE-STREAM ;

: CARRY-TREE ( ptr u8 n -- )
   FROM! FROM$ [: CARRY? ;] [: CARRY ;] FS:WALK-FILES-IF ;


\ The answer FS:MKTEMP-DIR-IN gives is transient, so TREE copies it.
: BUILD ( -- )
   LIB-TREES$ s" tree" FS:MKTEMP-DIR-IN {: a:ptr u:n :}
   a TREE u BYTE-COPY u TREE-U !
   TREE$ 493 FS:CHMOD-MODE              \ 0755
   s" src" CARRY-TREE  s" tools" CARRY-TREE  s" test" CARRY-TREE
   s" bin" TO! TO$ FS:MAKE-DIRS
   s" bin/hb.map" FROM! FROM$ CARRY
   s" bin/hb" FROM! FROM$ CARRY  TO$ FS:CHMOD-X ;

: PUBLISH ( -- )
   TREE$ LIB-U @ BELOW NEXT$ FS:MAKE-SYMLINK
   NEXT$ LINK$ FS:RENAME-FILE ;


: MAIN ( -- )
   PATHS!
   CHECK
   LIB-TREES$ FS:MAKE-DIRS
   LIB-TREES$ FS-LOCK:OPEN-DIR {: held:fd :}
   held FS-LOCK:WAIT
   SWEEP
   BUILD
   PUBLISH
   held FD>N close
   s" install: installed into " type TREE$ type cr ;

MAIN
;package
