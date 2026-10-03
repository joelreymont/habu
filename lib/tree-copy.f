\ tree-copy.f - private copies of checkout files, for tools and tests that build
\ or boot a tree without touching the checkout. A checkout file is named
\ relative to the working directory, the checkout, or by its absolute path under
\ it, as a source closure lists it; its copy keeps the checkout-relative path
\ under the root it is copied into.
\ Storage: process-wide (the root and destination cells, the lib/fs.f walk
\ stack and the lib/fs-mutate.f copy buffer), so a copy is single-task.

require lib/errors.f
require lib/fs.f
require lib/fs-mutate.f
require src/core/include.f

package TREE-COPY
private

PTR-VARIABLE INTO-P                    variable INTO-U
create DEST FS-PATH-CAP allot          variable DEST-U

: INTO$ ( -- ptr u8 n ) INTO-P @ INTO-U @ ;
: DEST$ ( -- ptr u8 n ) DEST DEST-U @ ;

: INTO! ( ptr u8 n -- ) INTO-U ! INTO-P ! ;

\ Only a checkout file has a checkout-relative path: a name that resolves
\ outside the working directory is refused, never joined under the root.
: CHECKOUT-REL ( ptr u8 n -- ptr u8 n )
   SOURCE-ROOT:CANONICAL drop {: a:ptr u:n :}
   a u SOURCE-ROOT:CWD$ SOURCE-ROOT:BELOW? 0= if E-FS-PATH throw then
   a u SOURCE-ROOT:CWD$ SOURCE-ROOT:RELATIVE ;

\ A walk callback has no context, so the root is read from INTO$.
: MEMBER ( ptr u8 n -- )
   {: a:ptr u:n :}
   INTO$ a u CHECKOUT-REL DEST JOIN-PATH DEST-U !
   DEST$ SOURCE-ROOT:DIRNAME MAKE-DIRS
   a u DEST$ COPY-FILE-STREAM ;

public

\ The checkout file path, copied to its checkout path under root.
: FILE ( ptr u8 n ptr u8 n -- ) INTO! MEMBER ;

\ Every file under the checkout directory path, copied the same way.
: TREE ( ptr u8 n ptr u8 n -- ) INTO! [: MEMBER ;] WALK-FILES ;

\ src/, lib/ and tools/: the sources tools/native-build.f builds from.
: BUILD-SOURCES ( ptr u8 n -- )
   {: root:ptr rootu:n :}
   s" src" root rootu TREE
   s" lib" root rootu TREE
   s" tools" root rootu TREE ;

;package
