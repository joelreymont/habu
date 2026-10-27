\ check-drivers-test.f - standalone build-driver regressions: run through
\ the shared CHECK core with no arguments, each reaches its own exit
\ (tools/check-exit-test-lib.f says what fails a case). The out and err each
\ driver left, saved under the printed tree, are the row's artifact.
require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require tools/check-exit-test-lib.f

package CHECK-DRIVERS-TEST
private
create ROOT FS-PATH-CAP allot variable ROOT-U
create PATH FS-PATH-CAP allot
create TARGET FS-PATH-CAP allot
: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: AT ( ptr u8 n -- ptr u8 n )
   {: a:ptr u:n :}
   ROOT$ a u PATH JOIN-PATH PATH swap ;
: SAVE ( ptr u8 n -- )
   {: src:ptr srcu:n :}
   src srcu BASENAME {: stem:ptr u:n :}
   stem u AT MAKE-DIRS
   stem u s" out" TARGET JOIN-PATH TARGET swap AT CHECK-EXIT:OUT$ WRITE-ALL
   stem u s" err" TARGET JOIN-PATH TARGET swap AT CHECK-EXIT:ERR$ WRITE-ALL ;
: DRIVER ( ptr u8 n n -- )
   {: src:ptr srcu:n want:n :}
   src srcu ROOT$ CHECK-EXIT:DIRECT {: oc :}
   src srcu SAVE
   src srcu oc want CHECK-EXIT:WANT ;
: CLI-DRIVER ( ptr u8 n n -- )
   {: src:ptr srcu:n want:n :}
   src srcu CHECK-EXIT:RUN {: oc :}
   src srcu SAVE
   src srcu oc want CHECK-EXIT:WANT ;
: SETUP ( -- )
   s" check-drivers" TMPDIR-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   s" check drivers tree: " type ROOT$ type cr ;
public
: RUN ( -- )
   T-RESET SETUP
   s" tools/build-fixpoint-main.f" 64 DRIVER
   \ The bootstrap refusal needs a fresh CLI process with HABU_ALLOW_BOOTSTRAP=0;
   \ the running process's startup environment cannot be changed by PROC-ENV.
   s" tools/ddc-drive.f" 79 CLI-DRIVER
   s" tools/imagedisasm.f" 0 DRIVER
   s" tools/json-only.f" 64 DRIVER
   s" tools/app-build.f" 0 DRIVER
   s" tools/build-profile.f" 67 DRIVER
   T-REPORT ;
;package
CHECK-DRIVERS-TEST:RUN
