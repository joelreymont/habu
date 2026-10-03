\ native-unit-stale.f - a unit imports only into the tree that exported it:
\ a native build in another root refuses the unit at NBR and publishes no engine.
\ The tree is three links (lib, tools, src) back to this checkout, so every
\ byte the build reads is the exporter's and only the root differs.
\ The import stops at NBR with E-NUNIT-FILE, the unit's key against the build's:
\ the key folds the canonical path of every file loaded up to NBR as well as its
\ bytes. That an edit of the unit's own bytes changes the key is
\ test/native-unit-key-e2e.f's UNIT-EDIT case; that a key mismatch refuses the
\ unit file is test/native-unit-file.f's.
\ test/native-unit-lib.f has the fixture and the other row; run alone:
\ bin/hb --load test/native-unit-stale.f

require lib/test.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require test/native-unit-lib.f

package NATIVE-UNIT-TEST

create TREE FS-PATH-CAP allot          variable TREE-U
create LINK FS-PATH-CAP allot
create TARGET FS-PATH-CAP allot

: TREE$ ( -- ptr u8 n ) TREE TREE-U @ ;

: IN-TREE ( ptr u8 n -- ptr u8 n ) {: rel:ptr relu:n :}
   TREE$ rel relu LINK JOIN-PATH LINK swap ;

: LINK-BACK ( ptr u8 n -- ) {: rel:ptr relu:n :}
   SOURCE-ROOT:CWD$ rel relu TARGET JOIN-PATH {: targetu:n :}
   TARGET targetu rel relu IN-TREE MAKE-SYMLINK ;

: OTHER-ROOT ( -- )
   s" tree" AT {: a:ptr u:n :}
   a TREE u BYTE-COPY u TREE-U !
   TREE$ MAKE-DIRS
   s" lib" LINK-BACK
   s" tools" LINK-BACK
   s" src" LINK-BACK ;

: ROOT-REFUSED ( -- )
   s" a unit imports only into the tree that exported it: another root refuses it at NBR and publishes nothing" T-LABEL
   OTHER-ROOT
   s" hb-stale" TREE$ IMPORT
   RC @ 0<> TTRUE
   \ -8599 is E-NUNIT-FILE (lib/errors.f): the unit refused at NBR, not any
   \ other failure of the build.
   OUT$ s" native-build: uncaught throw code -8599" CONTAINS? TTRUE
   s" hb-stale" AT EXISTS? TFALSE ;

public

: STALE-MAIN ( -- )
   T-RESET
   s" native-unit-stale" SETUP
   ROOT-REFUSED
   T-REPORT
   s" native unit tree: " type ROOT$ type cr ;

;package

NATIVE-UNIT-TEST:STALE-MAIN
