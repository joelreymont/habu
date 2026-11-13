\ Build and run one x86-64 program twice in the same output directory.
\ The ELF must stay byte-identical across the cold and warm builds.
require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-command.f
require src/lib/engine-candidate.f
require src/core/sha256.f

package X64-LINK-RUN
private

PATH-CAP BUFFER: ROOT  variable ROOT-U
PATH-CAP BUFFER: SOURCE  variable SOURCE-U
PATH-CAP BUFFER: EXE  variable EXE-U
SHA256-FILE-CTX-BYTES BUFFER: HASH
32 BUFFER: COLD
32 BUFFER: WARM

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: SOURCE$ ( -- ptr u8 n ) SOURCE SOURCE-U @ ;
: EXE$ ( -- ptr u8 n ) EXE EXE-U @ ;

: SHOW ( CMD:command n -- CMD:command )
   s" x64-link-run: exit " type . cr
   CMD:OUT$ type CMD:ERR$ type ;

: BUILD ( CMD:command -- CMD:command )
   CMD:RESET
   s" --load" CMD:ARG+ s" tools/hb-build.f" CMD:ARG+
   s" --" CMD:ARG+ s" --target" CMD:ARG+
   s" linux-x86-64" CMD:ARG+ SOURCE$ CMD:ARG+
   s" -o" CMD:ARG+ EXE$ CMD:ARG+
   s" HABU_OUT" ROOT$ CMD:ENV+
   ENGINE-CANDIDATE:PATH$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N {: rc:n :}
   rc 0<> if rc SHOW then
   s" x86-64 build succeeds" T-LABEL rc 0 T= ;

: RUN-IMAGE ( CMD:command -- CMD:command )
   CMD:RESET
   EXE$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N {: rc:n :}
   rc 0<> if rc SHOW then
   s" x86-64 program exits successfully" T-LABEL rc 0 T=
   s" package, does>, defer, captured quotations and DATA run" T-LABEL
   CMD:OUT$ s\" Hi\n" T$= ;

: DIGEST ( ptr u8 -- )
   {: dst:ptr :}
   HASH EXE$ dst SHA256-FILE-IN
   s" built image is readable" T-LABEL 0 T= ;

: CHECK ( -- )
   [: BUILD RUN-IMAGE COLD DIGEST
      BUILD RUN-IMAGE WARM DIGEST
      s" cold and warm images are byte-identical" T-LABEL
      COLD 32 WARM 32 T$= ;] CMD:WITH-COMMAND ;

public
: RUN ( -- )
   T-RESET
   s" habu-x64-link" FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   ROOT$ s" program.f" SOURCE FS:JOIN-PATH SOURCE-U !
   ROOT$ s" program" EXE FS:JOIN-PATH EXE-U !
   s" test/fixtures/x64-link-main.f" SOURCE$ FS:COPY-FILE-STREAM
   ['] CHECK [: ROOT$ FS:REMOVE-TREE ;] finally
   T-REPORT ;

;package

X64-LINK-RUN:RUN
