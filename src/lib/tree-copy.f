\ tree-copy.f - private copies of checkout files for tools and tests that build
\ or boot a tree without touching the checkout. A name is relative to the working
\ directory or the checkout, or absolute under it; its copy keeps the
\ checkout-relative path under the root. A name outside the checkout is E-FS-PATH.
\ STORAGE CLASS. PROCESS-WIDE (root and destination cells, the fs.f walk stack,
\ the fs-mutate.f copy buffer): a copy is single-task.

require src/lib/error.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/core/include.f

package TREE-COPY
private

PTR-VARIABLE INTO-P                    variable INTO-U
create DEST PATH-CAP allot          variable DEST-U

: INTO$ ( -- str ) INTO-P @ INTO-U @ >str ;
: DEST$ ( -- str ) DEST DEST-U @ >str ;

: INTO! ( str -- ) dup STR:LEN INTO-U ! STR:DATA INTO-P ! ;

: CHECKOUT-REL ( str -- str )
   SOURCE-ROOT:CANONICAL drop {: s:str :}
   s SOURCE-ROOT:CWD$ SOURCE-ROOT:BELOW? 0= if E-FS-PATH throw then
   s SOURCE-ROOT:CWD$ SOURCE-ROOT:RELATIVE ;

\ A walk callback has no context, so the root is read from INTO$.
: MEMBER ( str -- )
   {: s:str :}
   INTO$ s CHECKOUT-REL DEST FS:JOIN-PATH DEST-U !
   DEST$ SOURCE-ROOT:DIRNAME FS:MAKE-DIRS
   s DEST$ FS:COPY-FILE-STREAM ;

public

: FILE ( str str -- ) INTO! MEMBER ;

: TREE ( str str -- ) INTO! [: MEMBER ;] FS:WALK-FILES ;

\ src/ and tools/: the sources tools/hb-build.f builds from.
: BUILD-SOURCES ( str -- )
   {: root:str :}
   s" src" root TREE
   s" tools" root TREE ;

;package
