\ require-check.f - a file calls only the words of files it requires
\ directly: a call of a word only another file's require loaded is rejected,
\ and a file that requires it loads; a boot prefix word and the words of a
\ file it includes need no require. A call in an included file is its
\ includer's, which the rejection names, and so is a provided file's an
\ include reads: its calls are judged and its words are callable only from
\ files that require its includer. The include case loads word.f
\ through sibling.f, which good.f then finds known. A word of a file hb
\ carries beyond the boot prefix is judged the same way. A program whose MAIN
\ is in a file its source requires builds, plain and under --repl, since the
\ build's own glue that names MAIN is no file's. After an edit a warm build
\ into the cold build's out root rejects as a cold build does: a require
\ dropped from an included file or from its includer, an include made a
\ require, a call before the file's own require of a file a sibling loaded,
\ granted by an include's dropped require, a call only a text's require
\ would grant, and a call in a file a text included, once the file's own text
\ includes it. A build keeps the kernel of a builder a text ran. tools/check.f
\ --verify-only gives each file the load's verdict, at the same place: a call
\ in a body, a top-level token, a tick, an EXPORT operand, one after a body,
\ an `is` target, a TRUSTED: body or its clause, a word hb carries that another
\ file called first and a top-level definer: learned, a storage definer of a library, or FUNCTION:,
\ whether or not a `using` shadows it. It checks the rest after a definer it
\ rejects. A text a word evaluates and a body a definer renders are no file's:
\ their calls are not judged and a require the text makes grants the file that
\ evaluates it nothing, while the words they define are that file's. The
\ file's own tokens are judged after such a text, one that throws too, and in
\ such a body. A scan
\ that follows no requires, of a buffer or the child's --index, is judged as
\ no file's. A `names:` row's target and a name in a literal are no call. A
\ part's top-level call stays fresh warm while its file is required, and
\ rejects warm once it is not. A file loads with the require its rejection
\ suggests, of an engine file whose name its own root holds and of a file whose
\ path holds blanks.
\ hb-build.f and check.f read their argv, so they are spawned, one process per
\ file.

require src/core/exit-status.f
require src/lib/string.f
require src/lib/string-option.f
require src/lib/adt/option.f
require src/lib/test/assert.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-command.f
require src/lib/engine-candidate.f
require src/habu/verify-source.f

package RQC-TEST

private

: BAD-RC ( -- n )
   [: s\" s\" test/fixtures/require-check/bad.f\" required" evaluate-closed ;]
   catch ;

: INCLUDE-RC ( -- n )
   [: s\" s\" test/fixtures/require-check/part-main.f\" required"
      evaluate-closed ;]
   catch ;

: CARRIED-RC ( -- n )
   [: s\" s\" test/fixtures/require-check/carried-bad.f\" required"
      evaluate-closed ;]
   catch ;

: TOP-RC ( -- n )
   [: s\" s\" test/fixtures/require-check/top-bad.f\" required"
      evaluate-closed ;]
   catch ;

: TICK-RC ( -- n )
   [: s\" s\" test/fixtures/require-check/tick-bad.f\" required"
      evaluate-closed ;]
   catch ;

: EXPORT-RC ( -- n )
   [: s\" s\" test/fixtures/require-check/export-bad.f\" required"
      evaluate-closed ;]
   catch ;

: IS-RC ( -- n )
   [: s\" s\" test/fixtures/require-check/is-bad.f\" required"
      evaluate-closed ;]
   catch ;

: TRUSTED-RC ( -- n )
   [: s\" s\" test/fixtures/require-check/trusted-bad.f\" required"
      evaluate-closed ;]
   catch ;

: TRUSTED-HEAD-RC ( -- n )
   [: s\" s\" test/fixtures/require-check/trusted-head.f\" required"
      evaluate-closed ;]
   catch ;

: TRUSTED-CLAUSE-RC ( -- n )
   [: s\" s\" test/fixtures/require-check/trusted-clause.f\" required"
      evaluate-closed ;]
   catch ;

: EXPORT-AFTER-RC ( -- n )
   [: s\" s\" test/fixtures/require-check/export-after.f\" required"
      evaluate-closed ;]
   catch ;

: EXISTS-AGAIN-RC ( -- n )
   [: s\" s\" test/fixtures/require-check/exists-again.f\" required"
      evaluate-closed ;]
   catch ;

: PROV-MAIN-RC ( -- n )
   [: s\" s\" test/fixtures/require-check/prov-main.f\" required"
      evaluate-closed ;]
   catch ;

: PROV-BAD-RC ( -- n )
   [: s\" s\" test/fixtures/require-check/prov-bad.f\" required"
      evaluate-closed ;]
   catch ;

: DEFINER-RC ( -- n )
   [: s\" s\" test/fixtures/require-check/definer-bad.f\" required"
      evaluate-closed ;]
   catch ;

: USER-RC ( -- n )
   [: s\" s\" test/fixtures/require-check/user-bad.f\" required"
      evaluate-closed ;]
   catch ;

: FUNCTION-RC ( -- n )
   [: s\" s\" test/fixtures/require-check/function-bad.f\" required"
      evaluate-closed ;]
   catch ;

: SHADOW-RC ( -- n )
   [: s\" s\" test/fixtures/require-check/shadow-bad.f\" required"
      evaluate-closed ;]
   catch ;

: PLAIN-RC ( -- n )
   [: s\" s\" test/fixtures/require-check/shadow-plain.f\" required"
      evaluate-closed ;]
   catch ;

: TEXT-RC ( -- n )
   [: s\" s\" test/fixtures/require-check/text-bad.f\" required"
      evaluate-closed ;]
   catch ;

: TEXT-MADE-RC ( -- n )
   [: s\" s\" test/fixtures/require-check/text-made-bad.f\" required"
      evaluate-closed ;]
   catch ;

: STRETCH-RC ( -- n )
   [: s\" s\" test/fixtures/require-check/stretch-bad.f\" required"
      evaluate-closed ;]
   catch ;

: BUILT-RC ( -- n )
   [: s\" s\" test/fixtures/require-check/built-bad.f\" required"
      evaluate-closed ;]
   catch ;

: CLAUSE-RC ( -- n )
   [: s\" s\" test/fixtures/require-check/shadow-clause.f\" required"
      evaluate-closed ;]
   catch ;

: GLOBAL-RC ( -- n )
   [: s\" s\" test/fixtures/require-check/shadow-global.f\" required"
      evaluate-closed ;]
   catch ;

: REST-RC ( -- n )
   [: s\" s\" test/fixtures/require-check/definer-rest.f\" required"
      evaluate-closed ;]
   catch ;

: BUFFER-RC ( -- n )
   [: s\" require src/lib/task.f\nTASK:+USER RQC-BUF n\n"
      VERIFY:SOURCE-BUF-IN-SCOPE ;]
   catch ;

: BUFFER-BODY-RC ( -- n )
   [: s" : RQC-BUF-LEN ( -- bool ) 5 BUF:N>BLEN drop true ;"
      VERIFY:SOURCE-BUF-IN-SCOPE ;]
   catch ;

: NAMES-ROW-RC ( -- n )
   [: s\" s\" test/fixtures/require-check/names-row.f\" required"
      evaluate-closed ;]
   catch ;

: NAMES-LIT-RC ( -- n )
   [: s\" s\" test/fixtures/require-check/names-lit.f\" required"
      evaluate-closed ;]
   catch ;

: HAS ( str -- )
   {: s:str :}
   REJECTION:BUFFER$ s STR:CONTAINS? TTRUE ;

\ A load's rc is the rejection's, which names head and place.
: REJECTED-AT ( n str str -- )
   {: rc:n h:str p:str :}
   rc EXIT-STATUS:REJECTED T=  h HAS  p HAS ;

: BAD-HEAD ( -- str )
   s" error[E-UNREQUIRED]: `RQC-WORD:SEVEN` is defined in `test/fixtures/require-check/word.f`, which `test/fixtures/require-check/bad.f` does not require" ;

: TOP-HEAD ( -- str )
   s" error[E-UNREQUIRED]: `RQC-WORD:SEVEN` is defined in `test/fixtures/require-check/word.f`, which `test/fixtures/require-check/top-bad.f` does not require" ;

: TICK-HEAD ( -- str )
   s" error[E-UNREQUIRED]: `RQC-WORD:SEVEN` is defined in `test/fixtures/require-check/word.f`, which `test/fixtures/require-check/tick-bad.f` does not require" ;

: EXPORT-HEAD ( -- str )
   s" error[E-UNREQUIRED]: `RQC-WORD:SEVEN` is defined in `test/fixtures/require-check/word.f`, which `test/fixtures/require-check/export-bad.f` does not require" ;

: IS-HEAD ( -- str )
   s" error[E-UNREQUIRED]: `RQC-WORD:LATER` is defined in `test/fixtures/require-check/word.f`, which `test/fixtures/require-check/is-bad.f` does not require" ;

: TRUSTED-AT ( -- str str )
   s" error[E-UNREQUIRED]: `RQC-WORD:SEVEN` is defined in `test/fixtures/require-check/word.f`, which `test/fixtures/require-check/trusted-bad.f` does not require"
   s" require-check/trusted-bad.f:6:23" ;

: TRUSTED-HEAD-AT ( -- str str )
   s" error[E-UNREQUIRED]: `RQC-WORD:SEVEN` is defined in `test/fixtures/require-check/word.f`, which `test/fixtures/require-check/trusted-head.f` does not require"
   s" require-check/trusted-head.f:6:24" ;

: TRUSTED-CLAUSE-AT ( -- str str )
   s" error[E-UNREQUIRED]: `RQC-WORD:SEVEN` is defined in `test/fixtures/require-check/word.f`, which `test/fixtures/require-check/trusted-clause.f` does not require"
   s" require-check/trusted-clause.f:6:50" ;

: EXPORT-AFTER-HEAD ( -- str )
   s" error[E-UNREQUIRED]: `RQC-WORD:SEVEN` is defined in `test/fixtures/require-check/word.f`, which `test/fixtures/require-check/export-after.f` does not require" ;

: EXISTS-AGAIN-HEAD ( -- str )
   s" error[E-UNREQUIRED]: `FS:EXISTS?` is defined in `src/lib/fs.f`, which `test/fixtures/require-check/exists-again.f` does not require" ;

: PROV-MAIN-HEAD ( -- str )
   s" error[E-UNREQUIRED]: `RQC-WORD:SEVEN` is defined in `test/fixtures/require-check/word.f`, which `test/fixtures/require-check/prov-main.f` does not require" ;

: PROV-BAD-HEAD ( -- str )
   s" error[E-UNREQUIRED]: `RQC-PROV-WORD:SEVEN` is defined in `test/fixtures/require-check/prov-sib.f`, which `test/fixtures/require-check/prov-bad.f` does not require" ;

: DEFINER-HEAD ( -- str )
   s" error[E-UNREQUIRED]: `RQC-DEFINER:MAKE` is defined in `test/fixtures/require-check/definer.f`, which `test/fixtures/require-check/definer-bad.f` does not require" ;

: USER-HEAD ( -- str )
   s" error[E-UNREQUIRED]: `TASK:+USER` is defined in `src/lib/task.f`, which `test/fixtures/require-check/user-bad.f` does not require" ;

: FUNCTION-HEAD ( -- str )
   s" error[E-UNREQUIRED]: `FUNCTION:` is defined in `src/lib/ffi-abi.f`, which `test/fixtures/require-check/function-bad.f` does not require" ;

: SHADOW-HEAD ( -- str )
   s" error[E-UNREQUIRED]: `CWORK-BUFFER` is defined in `src/compiler/workspace.f`, which `test/fixtures/require-check/shadow-bad.f` does not require" ;

: PLAIN-HEAD ( -- str )
   s" error[E-UNREQUIRED]: `RQC-GLOBAL` is defined in `test/fixtures/require-check/global.f`, which `test/fixtures/require-check/shadow-plain.f` does not require" ;

: TEXT-HEAD ( -- str )
   s" error[E-UNREQUIRED]: `RQC-WORD:SEVEN` is defined in `test/fixtures/require-check/word.f`, which `test/fixtures/require-check/text-bad.f` does not require" ;

: TEXT-MADE-HEAD ( -- str )
   s" error[E-UNREQUIRED]: `RQC-TEXT-MADE` is defined in `test/fixtures/require-check/text-made.f`, which `test/fixtures/require-check/text-made-bad.f` does not require" ;

: STRETCH-HEAD ( -- str )
   s" error[E-UNREQUIRED]: `RQC-DEFINER:MAKE` is defined in `test/fixtures/require-check/definer.f`, which `test/fixtures/require-check/stretch-bad.f` does not require" ;

: BUILT-HEAD ( -- str )
   s" error[E-UNREQUIRED]: `RQC-WORD:SEVEN` is defined in `test/fixtures/require-check/word.f`, which `test/fixtures/require-check/built-bad.f` does not require" ;

: CLAUSE-HEAD ( -- str )
   s" error[E-UNREQUIRED]: `VARIADIC` is defined in `src/lib/ffi-abi.f`, which `test/fixtures/require-check/shadow-clause.f` does not require" ;

: GLOBAL-HEAD ( -- str )
   s" error[E-USING-SHADOW-GLOBAL]: `TYPED-VARIABLE` is both a global word and a public of used package `RQC-SHADOW-GLOBAL`" ;

: REST-HEAD ( -- str )
   s" error[E-UNREQUIRED]: `TASK:+USER` is defined in `src/lib/task.f`, which `test/fixtures/require-check/definer-rest.f` does not require" ;

: REST-LAST-HEAD ( -- str )
   s" error[E-UNREQUIRED]: `RQC-DEFINER:MAKE` is defined in `test/fixtures/require-check/definer.f`, which `test/fixtures/require-check/definer-rest.f` does not require" ;

PATH-CAP BUFFER: DIR   variable DIR-U
PATH-CAP BUFFER: PROG  variable PROG-U
: DIR$ ( -- str ) DIR DIR-U @ >str ;
: PROG$ ( -- str ) PROG PROG-U @ >str ;

\ hb-build.f, with flag if it is not empty, builds program.f to PROG$.
: BUILD ( CMD:command str -- CMD:command n )
   {: f:str :}
   CMD:RESET
   s" --load" CMD:ARG+  s" tools/hb-build.f" CMD:ARG+  s" --" CMD:ARG+
   f STR:LEN 0<> if f CMD:ARG+ then
   s" test/fixtures/require-check/program.f" CMD:ARG+
   s" -o" CMD:ARG+  PROG$ CMD:ARG+
   ENGINE-CANDIDATE:PATH$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N ;

: BUILT-RUNS ( CMD:command str str -- CMD:command )
   {: f:str w:str :}
   f BUILD {: built:n :}
   built 0<> if CMD:ERR$ type cr then
   w T-LABEL  built 0 T=
   built 0<> if exit then
   CMD:RESET  PROG$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N {: rc:n :}
   CMD:OUT$ s" ran" STR:CONTAINS? TTRUE  rc 0 T= ;

: PROGRAM-CASES ( CMD:command -- CMD:command )
   s" " s" a program whose MAIN is in a file it requires builds and runs"
   BUILT-RUNS
   s" --repl" s" so does the hb --repl builds with it" BUILT-RUNS ;

: REMOVE ( -- ) DIR$ FS:REMOVE-TREE ;

PATH-CAP BUFFER: JOINED   PATH-CAP BUFFER: FIX-PATH

\ The file of that name in DIR$, valid until the next.
: IN-DIR ( str -- str )
   {: name:str :}
   DIR$ name JOINED FS:JOIN-PATH {: n:n :}
   JOINED n >str ;

: FIXTURE ( str -- str )
   {: name:str :}
   s" test/fixtures/require-check/warm" name FIX-PATH FS:JOIN-PATH {: n:n :}
   FIX-PATH n >str ;

\ The warm fixture a copied to DIR$'s file b.
: COPY-AS ( str str -- )
   {: a:str b:str :}
   a FIXTURE  b IN-DIR  FS:COPY-FILE-STREAM ;

: COPY-IN ( str -- )
   {: a:str :}
   a a COPY-AS ;

: WARM-SETUP ( -- )
   s" m.f" COPY-IN  s" sib.f" COPY-IN
   s" inc-main.f" COPY-IN  s" inc.f" COPY-IN
   s" drop-main.f" COPY-IN  s" drop-part.f" COPY-IN
   s" move-main.f" COPY-IN  s" move-a.f" COPY-IN  s" move-part.f" COPY-IN
   s" order-main.f" COPY-IN  s" order-x.f" COPY-IN  s" order-e.f" COPY-IN
   s" top-main.f" COPY-IN  s" top-part.f" COPY-IN  s" top-a.f" COPY-IN
   s" top-word.f" COPY-IN
   s" text-main.f" COPY-IN  s" text-a.f" COPY-IN  s" text-b.f" COPY-IN
   s" text-own.f" COPY-IN  s" text-inc.f" COPY-IN  s" text-part.f" COPY-IN ;

\ The build command the file d runs builds DIR$'s file a, its out root DIR$'s.
: BUILD-BY ( CMD:command str str -- CMD:command n )
   {: d:str a:str :}
   CMD:RESET
   s" --load" CMD:ARG+  d CMD:ARG+  s" --" CMD:ARG+
   a IN-DIR CMD:ARG+  s" -o" CMD:ARG+  s" prog" IN-DIR CMD:ARG+
   s" HABU_OUT" s" out" IN-DIR CMD:ENV+
   ENGINE-CANDIDATE:PATH$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N ;

: WARM-BUILD ( CMD:command str -- CMD:command n )
   {: a:str :}
   s" tools/hb-build.f" a BUILD-BY ;

: COLD-BUILDS ( CMD:command str -- CMD:command )
   WARM-BUILD {: rc:n :}
   rc 0<> if CMD:ERR$ type cr then
   rc 0 T= ;

: ERR-HAS ( CMD:command str -- CMD:command )
   {: s:str :}
   CMD:ERR$ s STR:CONTAINS? TTRUE ;

\ DIR$'s file of that name, which the build's working directory is not above.
: NAME+ ( str -- )
   {: name:str :}
   DIR$ SOURCE-ROOT:CANONICAL drop SB-APPEND  s" /" SB-APPEND
   name SB-APPEND ;

\ The head of the rejection of a call of word, of file f, in file w.
: WARM-HEAD ( str str str -- str )
   {: word:str f:str w:str :}
   SB-RESET s" error[E-UNREQUIRED]: `" SB-APPEND word SB-APPEND
   s" ` is defined in `" SB-APPEND  f NAME+
   s" `, which `" SB-APPEND  w NAME+  s" ` does not require" SB-APPEND
   SB$ ;

\ The main file a builds warm and is rejected, the rejection naming the
\ call (word), the file of its word (f) and the file that does not require
\ it (w).
: REJECTS ( CMD:command str str str str -- CMD:command )
   {: a:str word:str f:str w:str :}
   a WARM-BUILD EXIT-STATUS:REJECTED T=
   word f w WARM-HEAD ERR-HAS ;

: M-REJECTS ( CMD:command str str -- CMD:command )
   {: a:str w:str :}
   a s" RQC-WARM-M:SEVEN" s" m.f" w REJECTS ;

PATH-CAP BUFFER: FOUND   variable FOUND-U

: FOUND! ( str -- )
   {: p:str :}
   p STR:DATA FOUND p STR:LEN BYTE-COPY  p STR:LEN FOUND-U ! ;

: PART-SEEN ( str -- )
   {: p:str :}
   p s" /top-part.f+1.hfo" STR:ENDS-WITH? if p FOUND! then ;

: INPUTS-SEEN ( str -- )
   {: p:str :}
   p s" /kernel-app.inputs" STR:ENDS-WITH? if p FOUND! then ;

\ The change time of the file found.
: FOUND-CHANGED ( -- n n )
   FOUND FOUND-U @ >str FS:FILE-META {: size:n ms:n mns:n cs:n cns:n :}
   cs cns ;

\ A build that makes the kernel anew writes its inputs file.
: KERNEL-CHANGED ( -- n n )
   0 FOUND-U !  s" out/obj" IN-DIR ['] INPUTS-SEEN FS:WALK-FILES
   FOUND-CHANGED ;

: KERNEL-CASE ( CMD:command -- CMD:command )
   s" a build keeps the kernel of a builder a text ran" T-LABEL
   s" test/fixtures/require-check/text-build.f" s" inc-main.f" BUILD-BY 0 T=
   KERNEL-CHANGED {: cs:n cns:n :}
   s" inc-main.f" COLD-BUILDS  KERNEL-CHANGED cns T= cs T= ;

: INC-CASE ( CMD:command -- CMD:command )
   s" an included file's dropped require rejects warm" T-LABEL
   s" inc-main.f" COLD-BUILDS  s" inc-edit.f" s" inc.f" COPY-AS
   s" inc-main.f" s" inc-main.f" M-REJECTS
   s" /inc-main.f:4:15" ERR-HAS ;

: DROP-CASE ( CMD:command -- CMD:command )
   s" an includer's dropped require rejects its included call warm" T-LABEL
   s" drop-main.f" COLD-BUILDS  s" drop-main-edit.f" s" drop-main.f" COPY-AS
   s" drop-main.f" s" drop-main.f" M-REJECTS
   s" /drop-part.f:2:15" ERR-HAS ;

: MOVE-CASE ( CMD:command -- CMD:command )
   s" an include made a require rejects its includer's caller warm" T-LABEL
   s" move-main.f" COLD-BUILDS  s" move-a-edit.f" s" move-a.f" COPY-AS
   s" move-main.f"
   s" RQC-WARM-P:SEVEN" s" move-part.f" s" move-main.f" REJECTS
   s" /move-main.f:3:15" ERR-HAS ;

: ORDER-CASE ( CMD:command -- CMD:command )
   s" a call before the file's own require rejects warm" T-LABEL
   s" order-main.f" COLD-BUILDS  s" order-e-edit.f" s" order-e.f" COPY-AS
   s" order-main.f" s" order-x.f" M-REJECTS
   s" /order-x.f:4:15" ERR-HAS ;

\ The change time of the object of top-part.f's part after its require: a
\ rebuild writes the same bytes, but moves it.
: PART-CHANGED ( -- n n )
   0 FOUND-U !  s" out/obj" IN-DIR ['] PART-SEEN FS:WALK-FILES
   FOUND-CHANGED ;

: TOP-CASE ( CMD:command -- CMD:command )
   s" a part's top-level call builds warm while its file is required" T-LABEL
   s" top-main.f" COLD-BUILDS  PART-CHANGED {: cs:n cns:n :}
   s" top-main.f" COLD-BUILDS
   s" and the part's object is not rebuilt" T-LABEL
   PART-CHANGED cns T= cs T=
   s" and rejects warm once it is not" T-LABEL
   s" top-a-edit.f" s" top-a.f" COPY-AS
   s" top-main.f"
   s" RQC-WARM-T:SEVEN" s" top-word.f" s" top-part.f" REJECTS
   s" /top-part.f:3:1" ERR-HAS
   s" and the part's object is not rebuilt there either" T-LABEL
   PART-CHANGED cns T= cs T= ;

: TEXT-CASE ( CMD:command -- CMD:command )
   s" a call only a text's require would grant rejects warm" T-LABEL
   s" text-main.f" COLD-BUILDS  s" text-main-edit.f" s" text-main.f" COPY-AS
   s" text-main.f" s" text-main.f" M-REJECTS
   s" /text-b.f:2:15" ERR-HAS
   s" and so does a call added to the file that evaluates it" T-LABEL
   s" text-own.f" COLD-BUILDS  s" text-own-edit.f" s" text-own.f" COPY-AS
   s" text-own.f" s" text-own.f" M-REJECTS
   s" /text-own.f:4:15" ERR-HAS
   s" and so does a call in a file its text included, once its own text does"
   T-LABEL
   s" text-inc.f" COLD-BUILDS  s" text-inc-edit.f" s" text-inc.f" COPY-AS
   s" text-inc.f" s" text-inc.f" M-REJECTS
   s" /text-part.f:2:15" ERR-HAS
   s" and it builds again once a text does" T-LABEL
   s" text-inc.f" COPY-IN  s" text-inc.f" COLD-BUILDS ;

: WARM-CASES ( CMD:command -- CMD:command )
   KERNEL-CASE  INC-CASE  DROP-CASE  MOVE-CASE  ORDER-CASE  TOP-CASE  TEXT-CASE ;

\ The text of s past the first b.
: AFTER ( str str -- str )
   {: s:str b:str :}
   s b STR:FIND-SUB MATCH option
     none OF s" " ENDOF
     some OF IDX>N b STR:LEN + {: k:n :}
        s STR:DATA k +  s STR:LEN k - >str ENDOF
   ;MATCH ;

\ The text of s before the first b.
: BEFORE ( str str -- str )
   {: s:str b:str :}
   s b STR:FIND-SUB MATCH option
     none OF s" " ENDOF
     some OF IDX>N {: k:n :}  s STR:DATA k >str ENDOF
   ;MATCH ;

\ The require line the last child's help suggests.
: HELP-LINE$ ( CMD:command -- CMD:command str )
   CMD:ERR$ s" help: add `" AFTER s" ` to `" BEFORE ;

: LOAD-IN ( CMD:command str -- CMD:command n )
   {: f:str :}
   CMD:RESET  s" --load" CMD:ARG+  f IN-DIR CMD:ARG+
   ENGINE-CANDIDATE:PATH$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N ;

\ DIR$'s file f of text t is rejected, and loads after the require suggested.
: FOLLOWED ( CMD:command str str -- CMD:command )
   {: f:str t:str :}
   f IN-DIR t FS:WRITE-ALL
   f LOAD-IN EXIT-STATUS:REJECTED T=
   SB-RESET  HELP-LINE$ SB-APPEND  s\" \n" SB-APPEND  t SB-APPEND
   f IN-DIR SB$ FS:WRITE-ALL
   f LOAD-IN {: rc:n :}
   rc 0<> if CMD:ERR$ type cr then
   rc 0 T= ;

: OWN-NAME-CASE ( CMD:command -- CMD:command )
   s" the require suggested of an engine file the root holds the name of loads"
   T-LABEL
   s" own/src/lib" IN-DIR FS:MAKE-DIRS
   s" own/src/lib/fs.f" IN-DIR s\" \\ the root's own\n" FS:WRITE-ALL
   s" own/main.f" s\" : RQC-OWN ( -- bool ) s\q .\q FS:EXISTS? ;\n" FOLLOWED ;

: BLANK-CASE ( CMD:command -- CMD:command )
   s" and of a path that holds blanks" T-LABEL
   s\" blank/my \qlib" IN-DIR FS:MAKE-DIRS  s" blank/app" IN-DIR FS:MAKE-DIRS
   s\" blank/my \qlib/w f.f" IN-DIR s\" : RQC-BLANK ( -- n ) 7 ;\n" FS:WRITE-ALL
   s" blank/app/sib.f" IN-DIR s\" s\\\q ../my \\qlib/w f.f\q required\n" FS:WRITE-ALL
   s" blank/app/main.f"
   s\" require sib.f\n: RQC-BLANK-M ( -- n ) RQC-BLANK ;\n" FOLLOWED ;

: PATH-CASES ( CMD:command -- CMD:command )
   OWN-NAME-CASE  BLANK-CASE ;

\ check.f --verify-only of the file f: its rc.
: VERIFY ( CMD:command str -- CMD:command n )
   {: f:str :}
   CMD:RESET
   s" --load" CMD:ARG+  s" tools/check.f" CMD:ARG+  s" --" CMD:ARG+
   s" --verify-only" CMD:ARG+
   f CMD:ARG+
   ENGINE-CANDIDATE:PATH$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N ;

: VERIFIES ( CMD:command str str -- CMD:command )
   {: f:str w:str :}
   w T-LABEL
   f VERIFY {: rc:n :}
   rc 0<> if CMD:ERR$ type cr then
   rc 0 T= ;

\ check.f --verify-only rejects the file f, naming head and place.
: VERIFY-REJECTS ( CMD:command str str str -- CMD:command )
   {: f:str h:str p:str :}
   f VERIFY EXIT-STATUS:REJECTED T=
   h ERR-HAS  p ERR-HAS ;

: USE-CASES ( CMD:command -- CMD:command )
   s" check.f --verify-only rejects a top-level token as the load does" T-LABEL
   s" test/fixtures/require-check/top-bad.f" TOP-HEAD
   s" require-check/top-bad.f:4:1" VERIFY-REJECTS
   s" and a tick" T-LABEL
   s" test/fixtures/require-check/tick-bad.f" TICK-HEAD
   s" require-check/tick-bad.f:3:3" VERIFY-REJECTS
   s" and an EXPORT operand" T-LABEL
   s" test/fixtures/require-check/export-bad.f" EXPORT-HEAD
   s" require-check/export-bad.f:5:8" VERIFY-REJECTS
   s" and an is target" T-LABEL
   s" test/fixtures/require-check/is-bad.f" IS-HEAD
   s" require-check/is-bad.f:6:26" VERIFY-REJECTS
   s" and a call in a trusted body" T-LABEL
   s" test/fixtures/require-check/trusted-bad.f" TRUSTED-AT VERIFY-REJECTS
   s" and one in a trusted definer before its does>" T-LABEL
   s" test/fixtures/require-check/trusted-head.f" TRUSTED-HEAD-AT VERIFY-REJECTS
   s" and one in its clause" T-LABEL
   s" test/fixtures/require-check/trusted-clause.f" TRUSTED-CLAUSE-AT VERIFY-REJECTS
   s" and an EXPORT operand after a body" T-LABEL
   s" test/fixtures/require-check/export-after.f" EXPORT-AFTER-HEAD
   s" require-check/export-after.f:7:8" VERIFY-REJECTS
   s" and a word hb carries that another file called first" T-LABEL
   s" test/fixtures/require-check/exists-again.f" EXISTS-AGAIN-HEAD
   s" require-check/exists-again.f:6:29" VERIFY-REJECTS
   s" and a learned definer at top level" T-LABEL
   s" test/fixtures/require-check/definer-bad.f" DEFINER-HEAD
   s" require-check/definer-bad.f:4:3" VERIFY-REJECTS
   s" and a library definer at top level" T-LABEL
   s" test/fixtures/require-check/user-bad.f" USER-HEAD
   s" require-check/user-bad.f:2:1" VERIFY-REJECTS
   s" and FUNCTION:" T-LABEL
   s" test/fixtures/require-check/function-bad.f" FUNCTION-HEAD
   s" require-check/function-bad.f:2:1" VERIFY-REJECTS
   s" and a definer a using shadows, as unrequired" T-LABEL
   s" test/fixtures/require-check/shadow-bad.f" SHADOW-HEAD
   s" require-check/shadow-bad.f:8:4" VERIFY-REJECTS
   s" and such a plain call" T-LABEL
   s" test/fixtures/require-check/shadow-plain.f" PLAIN-HEAD
   s" require-check/shadow-plain.f:8:1" VERIFY-REJECTS
   s" and a call in the file after text that requires its file" T-LABEL
   s" test/fixtures/require-check/text-bad.f" TEXT-HEAD
   s" require-check/text-bad.f:6:1" VERIFY-REJECTS
   s" and a call of a word another file's text makes" T-LABEL
   s" test/fixtures/require-check/text-made-bad.f" TEXT-MADE-HEAD
   s" require-check/text-made-bad.f:3:1" VERIFY-REJECTS
   s" and a definer after a name only evaluated text defines" T-LABEL
   s" test/fixtures/require-check/stretch-bad.f" STRETCH-HEAD
   s" require-check/stretch-bad.f:4:10" VERIFY-REJECTS
   s" and a call in the file in a body another file's definer opened" T-LABEL
   s" test/fixtures/require-check/built-bad.f" BUILT-HEAD
   s" require-check/built-bad.f:3:34" VERIFY-REJECTS
   s" and such a clause" T-LABEL
   s" test/fixtures/require-check/shadow-clause.f" CLAUSE-HEAD
   s" require-check/shadow-clause.f:7:3" VERIFY-REJECTS
   s" and as shadowed where it needs no require" T-LABEL
   s" test/fixtures/require-check/shadow-global.f" GLOBAL-HEAD
   s" require-check/shadow-global.f:7:1" VERIFY-REJECTS
   s" and each rejected token after a definer it rejects" T-LABEL
   s" test/fixtures/require-check/definer-rest.f" REST-HEAD
   s" require-check/definer-rest.f:4:1" VERIFY-REJECTS
   s" error[E-UNDEFINED]: cannot find word `RQC-REST-NAME`" ERR-HAS
   REST-LAST-HEAD ERR-HAS
   s" test/fixtures/require-check/text-good.f"
   s" and passes a call only evaluated text makes" VERIFIES
   s" test/fixtures/require-check/built-good.f"
   s" and a call only a definer renders" VERIFIES
   s" test/fixtures/require-check/definer-good.f"
   s" and the definers once their files are required" VERIFIES
   s" test/fixtures/require-check/function-good.f"
   s" and FUNCTION: once its file is required" VERIFIES
   s" test/fixtures/require-check/names-row.f"
   s" and passes a names: row's target, which is no call" VERIFIES
   s" test/fixtures/require-check/names-lit.f"
   s" and a name in a literal, which is no call" VERIFIES ;

\ The verifier child's --index of a source that calls a library word.
: INDEX-CASE ( CMD:command -- CMD:command )
   s" an --index, which follows no requires, judges no call" T-LABEL
   CMD:RESET
   s" --load" CMD:ARG+  s" tools/check-verify-child.f" CMD:ARG+
   s" --" CMD:ARG+  s" test/fixtures/require-check/exists-bad.f" CMD:ARG+
   s" --index" CMD:ARG+
   s\" \n: RQC-INDEXED ( -- bool ) s\" .\" FS:EXISTS? ;\n" CMD:IN!
   ENGINE-CANDIDATE:PATH$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N 0 T=
   CMD:OUT$ s" check-verify: verified" STR:CONTAINS? TTRUE ;

: PROV-VERIFY-CASES ( CMD:command -- CMD:command )
   s" check.f --verify-only rejects a call in a provided file it includes"
   T-LABEL
   s" test/fixtures/require-check/prov-main.f" PROV-MAIN-HEAD
   s" require-check/prov-part.f:5:18" VERIFY-REJECTS
   s" and a call of such a file's word without its includer's require"
   T-LABEL
   s" test/fixtures/require-check/prov-bad.f" PROV-BAD-HEAD
   s" require-check/prov-bad.f:6:18" VERIFY-REJECTS
   s" test/fixtures/require-check/prov-good.f"
   s" and passes the file that requires its includer" VERIFIES ;

: VERIFY-CASES ( CMD:command -- CMD:command )
   s" check.f --verify-only rejects a call only another file's require loaded"
   T-LABEL
   s" test/fixtures/require-check/bad.f" BAD-HEAD
   s" require-check/bad.f:5:18" VERIFY-REJECTS
   s" test/fixtures/require-check/good.f"
   s" and passes the file that requires it" VERIFIES
   s" check.f --verify-only rejects a call of a word hb carries with no require"
   T-LABEL
   s" test/fixtures/require-check/exists-bad.f" VERIFY EXIT-STATUS:REJECTED T=
   s" error[E-UNREQUIRED]: `FS:EXISTS?` is defined in `src/lib/fs.f`, which `test/fixtures/require-check/exists-bad.f` does not require"
   ERR-HAS
   s" test/fixtures/require-check/exists-good.f"
   s" and passes the file that requires it" VERIFIES
   USE-CASES  INDEX-CASE  PROV-VERIFY-CASES ;

public

: UNREQUIRED-CASE ( -- )
   s" a call of a word only another file's require loaded is rejected" T-LABEL
   REJECTION:CAPTURE  BAD-RC  REJECTION:BUFFER-OFF
   EXIT-STATUS:REJECTED T=
   BAD-HEAD HAS
   s" require-check/bad.f:5:18" HAS
   s" = expected: `require test/fixtures/require-check/word.f` in `test/fixtures/require-check/bad.f`"
   HAS
   s" = found: no such require; one made by another file does not count" HAS
   s" = help: add `require test/fixtures/require-check/word.f` to `test/fixtures/require-check/bad.f`"
   HAS ;

: PROGRAM-CASE ( -- )
   s" habu-require-check" FS:MKTEMP-DIR {: d:str :}
   d STR:DATA DIR d STR:LEN BYTE-COPY  d STR:LEN DIR-U !
   DIR$ s" program" PROG FS:JOIN-PATH PROG-U !
   [: [: PROGRAM-CASES ;] CMD:WITH-COMMAND ;] ['] REMOVE finally ;

: VERIFY-CASE ( -- ) [: VERIFY-CASES ;] CMD:WITH-COMMAND ;

: LOAD-USE-CASE ( -- )
   s" a top-level call of such a word is rejected" T-LABEL
   REJECTION:CAPTURE  TOP-RC  REJECTION:BUFFER-OFF
   TOP-HEAD s" require-check/top-bad.f:4:1" REJECTED-AT
   s" and a tick of one" T-LABEL
   REJECTION:CAPTURE  TICK-RC  REJECTION:BUFFER-OFF
   TICK-HEAD s" require-check/tick-bad.f:3:3" REJECTED-AT
   s" and an EXPORT of one" T-LABEL
   REJECTION:CAPTURE  EXPORT-RC  REJECTION:BUFFER-OFF
   EXPORT-HEAD s" require-check/export-bad.f:5:8" REJECTED-AT
   s" and an is of one" T-LABEL
   REJECTION:CAPTURE  IS-RC  REJECTION:BUFFER-OFF
   IS-HEAD s" require-check/is-bad.f:6:26" REJECTED-AT
   s" and a call in a trusted body" T-LABEL
   REJECTION:CAPTURE  TRUSTED-RC  REJECTION:BUFFER-OFF
   TRUSTED-AT REJECTED-AT
   s" and one in a trusted definer before its does>" T-LABEL
   REJECTION:CAPTURE  TRUSTED-HEAD-RC  REJECTION:BUFFER-OFF
   TRUSTED-HEAD-AT REJECTED-AT
   s" and one in its clause" T-LABEL
   REJECTION:CAPTURE  TRUSTED-CLAUSE-RC  REJECTION:BUFFER-OFF
   TRUSTED-CLAUSE-AT REJECTED-AT
   s" and an EXPORT of one after a body" T-LABEL
   REJECTION:CAPTURE  EXPORT-AFTER-RC  REJECTION:BUFFER-OFF
   EXPORT-AFTER-HEAD s" require-check/export-after.f:7:8" REJECTED-AT
   s" and a call of a word hb carries that another file called first" T-LABEL
   REJECTION:CAPTURE  EXISTS-AGAIN-RC  REJECTION:BUFFER-OFF
   EXISTS-AGAIN-HEAD s" require-check/exists-again.f:6:29" REJECTED-AT
   s" and a learned definer at top level" T-LABEL
   REJECTION:CAPTURE  DEFINER-RC  REJECTION:BUFFER-OFF
   DEFINER-HEAD s" require-check/definer-bad.f:4:3" REJECTED-AT
   s" and a library definer at top level" T-LABEL
   REJECTION:CAPTURE  USER-RC  REJECTION:BUFFER-OFF
   USER-HEAD s" require-check/user-bad.f:2:1" REJECTED-AT
   s" and FUNCTION:" T-LABEL
   REJECTION:CAPTURE  FUNCTION-RC  REJECTION:BUFFER-OFF
   FUNCTION-HEAD s" require-check/function-bad.f:2:1" REJECTED-AT
   s" and a definer a using shadows, as unrequired" T-LABEL
   REJECTION:CAPTURE  SHADOW-RC  REJECTION:BUFFER-OFF
   SHADOW-HEAD s" require-check/shadow-bad.f:8:4" REJECTED-AT
   s" and such a plain call" T-LABEL
   REJECTION:CAPTURE  PLAIN-RC  REJECTION:BUFFER-OFF
   PLAIN-HEAD s" require-check/shadow-plain.f:8:1" REJECTED-AT
   s" and a call in the file after text that requires its file" T-LABEL
   REJECTION:CAPTURE  TEXT-RC  REJECTION:BUFFER-OFF
   TEXT-HEAD s" require-check/text-bad.f:6:1" REJECTED-AT
   s" and a call of a word another file's text makes" T-LABEL
   REJECTION:CAPTURE  TEXT-MADE-RC  REJECTION:BUFFER-OFF
   TEXT-MADE-HEAD s" require-check/text-made-bad.f:3:1" REJECTED-AT
   s" and a definer after a name only evaluated text defines" T-LABEL
   REJECTION:CAPTURE  STRETCH-RC  REJECTION:BUFFER-OFF
   STRETCH-HEAD s" require-check/stretch-bad.f:4:10" REJECTED-AT
   s" and a call in the file in a body another file's definer opened" T-LABEL
   REJECTION:CAPTURE  BUILT-RC  REJECTION:BUFFER-OFF
   BUILT-HEAD s" require-check/built-bad.f:3:34" REJECTED-AT
   s" and such a clause" T-LABEL
   REJECTION:CAPTURE  CLAUSE-RC  REJECTION:BUFFER-OFF
   CLAUSE-HEAD s" require-check/shadow-clause.f:7:3" REJECTED-AT
   s" and as shadowed where it needs no require" T-LABEL
   REJECTION:CAPTURE  GLOBAL-RC  REJECTION:BUFFER-OFF
   GLOBAL-HEAD s" require-check/shadow-global.f:7:1" REJECTED-AT
   s" and a library definer before other rejected tokens" T-LABEL
   REJECTION:CAPTURE  REST-RC  REJECTION:BUFFER-OFF
   REST-HEAD s" require-check/definer-rest.f:4:1" REJECTED-AT
   s" a buffer's scan, which follows no requires, judges no definer" T-LABEL
   REJECTION:CAPTURE  BUFFER-RC  REJECTION:BUFFER-OFF
   0 T=  REJECTION:BUFFER$ STR:LEN 0 T=
   s" and no call in a body" T-LABEL
   REJECTION:CAPTURE  BUFFER-BODY-RC  REJECTION:BUFFER-OFF
   0 T=  REJECTION:BUFFER$ STR:LEN 0 T=
   s" a names: row's target is no call" T-LABEL
   NAMES-ROW-RC 0 T=
   s" nor is a name in a literal" T-LABEL
   NAMES-LIT-RC 0 T= ;

: INCLUDED-CASE ( -- )
   s" a call in an included file is rejected as its includer's" T-LABEL
   REJECTION:CAPTURE  INCLUDE-RC  REJECTION:BUFFER-OFF
   EXIT-STATUS:REJECTED T=
   s" error[E-UNREQUIRED]: `RQC-WORD:SEVEN` is defined in `test/fixtures/require-check/word.f`, which `test/fixtures/require-check/part-main.f` does not require"
   HAS
   s" require-check/part.f:4:21" HAS ;

: PROV-MAIN-CASE ( -- )
   s" a call in a provided file it includes is rejected as its includer's"
   T-LABEL
   REJECTION:CAPTURE  PROV-MAIN-RC  REJECTION:BUFFER-OFF
   PROV-MAIN-HEAD s" require-check/prov-part.f:5:18" REJECTED-AT ;

: PROV-BAD-CASE ( -- )
   s" and its words are rejected for a file not requiring its includer"
   T-LABEL
   REJECTION:CAPTURE  PROV-BAD-RC  REJECTION:BUFFER-OFF
   PROV-BAD-HEAD s" require-check/prov-bad.f:6:18" REJECTED-AT ;

: WARM-CASE ( -- )
   s" habu-require-warm" FS:MKTEMP-DIR {: d:str :}
   d STR:DATA DIR d STR:LEN BYTE-COPY  d STR:LEN DIR-U !
   [: WARM-SETUP  [: WARM-CASES ;] CMD:WITH-COMMAND ;] ['] REMOVE finally ;

: PATH-CASE ( -- )
   s" habu-require-path" FS:MKTEMP-DIR {: d:str :}
   d STR:DATA DIR d STR:LEN BYTE-COPY  d STR:LEN DIR-U !
   [: [: PATH-CASES ;] CMD:WITH-COMMAND ;] ['] REMOVE finally ;

: CARRIED-CASE ( -- )
   s" a call of a word of a file hb carries needs its require" T-LABEL
   REJECTION:CAPTURE  CARRIED-RC  REJECTION:BUFFER-OFF
   EXIT-STATUS:REJECTED T=
   s" error[E-UNREQUIRED]: `BUF:N>BLEN` is defined in `src/lib/byte-buffer.f`, which `test/fixtures/require-check/carried-bad.f` does not require"
   HAS ;

;package

T-RESET
RQC-TEST:UNREQUIRED-CASE
RQC-TEST:LOAD-USE-CASE
RQC-TEST:INCLUDED-CASE
s" it loads once it requires the file, with prefix and included words" T-LABEL
require test/fixtures/require-check/good.f
RQC-GOOD:SUM 21 T=
RQC-GOOD:SAME? TTRUE
RQC-TEST:PROV-MAIN-CASE
s" a provided file's words it includes load for its includer's requirer"
T-LABEL
require test/fixtures/require-check/prov-good.f
RQC-PROV-GOOD:SEVEN 7 T=
RQC-TEST:PROV-BAD-CASE
s" the definers run once their files are required" T-LABEL
require test/fixtures/require-check/definer-good.f
RQC-GOOD-FIVE 5 T=
require test/fixtures/require-check/function-good.f
RQC-GOOD-PID 0 > TTRUE
s" and words of text or a definer's body that call it load" T-LABEL
require test/fixtures/require-check/text-good.f
require test/fixtures/require-check/built-good.f
RQC-BUILT-SEVEN 7 T=  RQC-BUILT-TEXT 7 T=
RQC-TEST:CARRIED-CASE
s" it loads once it requires the carried file" T-LABEL
require test/fixtures/require-check/carried-good.f
RQC-CARRIED-GOOD:TOUCH TTRUE
RQC-TEST:PROGRAM-CASE
RQC-TEST:WARM-CASE
RQC-TEST:PATH-CASE
RQC-TEST:VERIFY-CASE
T-REPORT
