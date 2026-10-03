\ tree-copy-lib.f - private copies of checkout files, for tests that build or
\ edit a tree without touching the checkout. Paths are relative to the working
\ directory, the checkout, and each copy keeps its path under the root it is
\ copied into. Used by test/host-checker-row-e2e.f,
\ test/native-builder-image-lib.f, test/native-builder-image-e2e.f and
\ test/compiler/native-hookless-reject.f.

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

\ A walk callback has no context, so the root is read from INTO$.
: MEMBER ( ptr u8 n -- ) {: a:ptr u:n :}
   INTO$ a u DEST JOIN-PATH DEST-U !
   DEST$ SOURCE-ROOT:DIRNAME MAKE-DIRS
   a u DEST$ COPY-FILE-STREAM ;

public

\ The checkout file path, copied to the same path under root.
: FILE ( ptr u8 n ptr u8 n -- ) INTO! MEMBER ;

\ src/, lib/ and tools/: the sources tools/native-build.f builds from.
: BUILD-SOURCES ( ptr u8 n -- )
   INTO!
   s" src" [: MEMBER ;] WALK-FILES
   s" lib" [: MEMBER ;] WALK-FILES
   s" tools" [: MEMBER ;] WALK-FILES ;

;package
