\ source-root-exe-app.f - an application outside the engine's tree.
\ test/source-root-exe-test.f copies this file to <tmp>/app/app.f with the app's
\ own src/lib/error.f beside it, and runs it from an empty directory with an
\ engine copy at <tmp>/tree/bin/hb:
\    <tmp>/tree/bin/hb --load <tmp>/app/app.f -- <working directory> <tmp>/tree

\ Neither the owner nor the working directory holds the test framework's files
\ or the fixture's inner.f, so these relative requires resolve at the engine
\ root; the app's error.f resolves at the owner.
require src/lib/test/assert.f
require test/fixtures/source-root-exe/inner.f
require src/lib/error.f

package SOURCE-ROOT-EXE-APP

create PATH-BUF PATH-CAP allot
variable PATH-U

: INVOKED$ ( -- str ) 0 SCRIPT-ARGV$ ;
: ROOT$ ( -- str ) 1 SCRIPT-ARGV$ ;

\ JOIN answers in resolver scratch that the next query overwrites.
: ENGINE-COPY$ ( -- str )
   ROOT$ s" src/lib/error.f" SOURCE-ROOT:JOIN {: s:str :}
   s STR:DATA PATH-BUF s STR:LEN BYTE-COPY s STR:LEN PATH-U !
   PATH-BUF PATH-U @ >str ;

: RUN ( -- )
   T-RESET
   s" the working directory is where the engine was started" T-LABEL
   SOURCE-ROOT:CWD$ INVOKED$ T$=
   s" the engine root is the executable's tree" T-LABEL
   SOURCE-ROOT:ENGINE$ ROOT$ T$=
   s" the app's own src/lib/error.f loaded at its owner" T-LABEL
   SOURCE-ROOT-EXE-APP-ERRORS:MARK 1 T=
   s" the app's own src/lib/error.f is not engine-provided" T-LABEL
   s" src/lib/error.f" REQUIRE-REG:ENGINE-PROVIDES? TFALSE
   s" a baked file found at the engine root is engine-provided" T-LABEL
   s" src/lib/string.f" REQUIRE-REG:ENGINE-PROVIDES? TTRUE
   s" the engine's copy named by its absolute path is engine-provided" T-LABEL
   ENGINE-COPY$ REQUIRE-REG:ENGINE-PROVIDES? TTRUE
   \ A second copy of src/lib/error.f would exit 70 on its first definition.
   ENGINE-COPY$ required
   s" a library the engine does not bake is not engine-provided" T-LABEL
   s" test/fixtures/source-root-exe/inner.f" REQUIRE-REG:ENGINE-PROVIDES? TFALSE
   s" a discovery base does not hide the frozen rows" T-LABEL
   REQUIRE-REG:SNAPSHOT
   s" src/lib/string.f" REQUIRE-REG:ENGINE-PROVIDES? TTRUE
   REQUIRE-REG:RESTORE
   T-REPORT ;

RUN
;package
