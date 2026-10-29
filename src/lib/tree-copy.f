\ tree-copy.f - private copies of checkout files for tools and tests that build
\ or boot a tree without touching the checkout. A name is relative to the working
\ directory or the checkout, or absolute under it; its copy keeps the
\ checkout-relative path under the root. A name outside the checkout is E-FS-PATH.
\ STORAGE CLASS. PROCESS-WIDE (root and destination cells, the fs.f walk stack,
\ the fs-mutate.f copy buffer): a copy is single-task.

require src/lib/errors.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/core/include.f

package TREE-COPY
private

PTR-VARIABLE INTO-P                    variable INTO-U
create DEST PATH-CAP allot          variable DEST-U

: INTO$ ( -- ptr u8 n ) INTO-P @ INTO-U @ ;
: DEST$ ( -- ptr u8 n ) DEST DEST-U @ ;

: INTO! ( ptr u8 n -- ) INTO-U ! INTO-P ! ;

: CHECKOUT-REL ( ptr u8 n -- ptr u8 n )
   SOURCE-ROOT:CANONICAL drop {: a:ptr u:n :}
   a u SOURCE-ROOT:CWD$ SOURCE-ROOT:BELOW? 0= if E-FS-PATH throw then
   a u SOURCE-ROOT:CWD$ SOURCE-ROOT:RELATIVE ;

\ A walk callback has no context, so the root is read from INTO$.
: MEMBER ( ptr u8 n -- )
   {: a:ptr u:n :}
   INTO$ a u CHECKOUT-REL DEST FS:JOIN-PATH DEST-U !
   DEST$ SOURCE-ROOT:DIRNAME FS:MAKE-DIRS
   a u DEST$ FS:COPY-FILE-STREAM ;

public

: FILE ( ptr u8 n ptr u8 n -- ) INTO! MEMBER ;

: TREE ( ptr u8 n ptr u8 n -- ) INTO! [: MEMBER ;] FS:WALK-FILES ;

\ src/ and tools/: the sources tools/native-build.f builds from.
: BUILD-SOURCES ( ptr u8 n -- )
   {: root:ptr rootu:n :}
   s" src" root rootu TREE
   s" tools" root rootu TREE ;

;package
