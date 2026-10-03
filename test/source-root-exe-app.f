\ source-root-exe-app.f - an application outside the engine's tree.
\
\ test/source-root-exe-test.f copies this file to <tmp>/app/app.f, writes the
\ app's own lib/errors.f beside it and runs it from an empty directory with a
\ copy of the engine at <tmp>/tree/bin/hb, whose lib and src are the tree's:
\
\    <tmp>/tree/bin/hb --load <tmp>/app/app.f -- <working directory> <tmp>/tree
\
\ Every require here is relative. Neither the owner (<tmp>/app) nor the working
\ directory holds lib/test.f or lib/fs.f, so they resolve at the engine root,
\ and so do their own requires of the files the engine bakes. The app's own
\ lib/errors.f resolves at the owner and stays the app's.
require lib/test.f
require lib/fs.f
require lib/errors.f

using SOURCE-ROOT
package SOURCE-ROOT-EXE-APP

create PATH-BUF INCLUDE-PATH-CAP allot
variable PATH-U

: INVOKED$ ( -- ptr u8 n ) 0 SCRIPT-ARGV$ ;
: ROOT$ ( -- ptr u8 n ) 1 SCRIPT-ARGV$ ;

\ JOIN answers in resolver scratch that the next query overwrites.
: ENGINE-COPY$ ( -- ptr u8 n )
   ROOT$ s" lib/errors.f" JOIN {: a:ptr u:n :}
   a PATH-BUF u BYTE-COPY u PATH-U !
   PATH-BUF PATH-U @ ;

: RUN ( -- )
   T-RESET
   s" the working directory is where the engine was started" T-LABEL
   CWD$ INVOKED$ T$=
   s" the engine root is the executable's tree" T-LABEL
   ENGINE$ ROOT$ T$=
   s" the app's own lib/errors.f loaded at its owner" T-LABEL
   SOURCE-ROOT-EXE-APP-ERRORS:MARK 1 T=
   s" the app's own lib/errors.f is not engine-provided" T-LABEL
   s" lib/errors.f" ENGINE-PROVIDES? TFALSE
   s" a baked file found at the engine root is engine-provided" T-LABEL
   s" lib/string.f" ENGINE-PROVIDES? TTRUE
   s" the engine's copy named by its absolute path is engine-provided" T-LABEL
   ENGINE-COPY$ ENGINE-PROVIDES? TTRUE
   \ A second copy of lib/errors.f would exit 78 on its first definition.
   ENGINE-COPY$ required
   s" a library the engine does not bake is not engine-provided" T-LABEL
   s" lib/fs.f" ENGINE-PROVIDES? TFALSE
   s" a discovery base does not hide the frozen rows" T-LABEL
   REQUIRE-SNAPSHOT
   s" lib/string.f" ENGINE-PROVIDES? TTRUE
   REQUIRE-RESTORE
   T-REPORT ;

RUN
;package
;using
