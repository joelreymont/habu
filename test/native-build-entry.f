\ The production entry refuses a bad command line before it loads the build
\ closure (tools/native-build-args.f). No output argument is refused by name;
\ so is a bad second argument:
\ the class the build is being asked for is settled before the target load, so
\ a typo cannot quietly produce a product. So are an unknown `--target` profile
\ and one whose machine this engine has no backend for. With that backend
\ loaded, the selected action refuses an unavailable writer before the full
\ compiler window loads; an x86-64 window's writer is loaded before that window,
\ so a build for it writes an x86-64 ELF from either host. The entry's accepted
\ path, the closure compiling in the executable-build scope, is every gate's
\ whitebox engine build (test/whitebox-engine.f runs this entry with
\ `whitebox`).
require src/lib/test.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/le.f
require src/lib/string.f
require test/suite-budget.f              \ CHILD-MS, every child's hang guard

package NATIVE-BUILD-ENTRY-TEST

$4000 constant CAP
create OUT CAP allot
create ERR CAP allot
variable OUT-U
variable ERR-U
variable RC
create FOREIGN-ROOT FS-PATH-CAP allot
variable FOREIGN-ROOT-U
create FOREIGN-OUT FS-PATH-CAP allot
variable FOREIGN-OUT-U
create CONTENT 64 allot
: FOREIGN-OUT$ ( -- ptr u8 n ) FOREIGN-OUT FOREIGN-OUT-U @ ;

: FOREIGN-OUT! ( ptr u8 n -- ) {: name:ptr size:n :}
   FOREIGN-ROOT FOREIGN-ROOT-U @ name size FOREIGN-OUT JOIN-PATH
      FOREIGN-OUT-U ! ;

: FOREIGN-ROOT! ( -- )
   s" native-build-foreign" FS:MKTEMP-DIR {: a:ptr u:n :}
   a FOREIGN-ROOT u BYTE-COPY u FOREIGN-ROOT-U !
   s" native-build foreign artifacts: " type a u type cr ;

: ARG ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: ENTRY-ARGS ( -- )
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG
   s" tools/native-build.f" ARG ;

: DRIVE ( -- )
   ENGINE-CANDIDATE:PATH$ >LEN
   OUT CAP >LEN ERR CAP >LEN SUITE-BUDGET:CHILD-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U !
   erru LEN>N ERR-U !
   rc RC ! ;

: ERR$ ( -- ptr u8 n )
   ERR ERR-U @ ;

\ Every refusal here is the same shape: exit 74, nothing on stdout, and one
\ named line on stderr. Show the capture when the code is wrong, so a refusal
\ that moved is read from its own output instead of guessed at.
: REFUSED ( ptr u8 n -- ) {: want:ptr wantu:n :}
   RC @ 74 <> if
      OUT OUT-U @ type ERR$ type
   then
   RC @ 74 T=
   OUT-U @ 0 T=
   ERR$ want wantu T$= ;

: NO-OUTPUT-CASE ( -- )
   s" the entry refuses a missing output path before the build closure loads" T-LABEL
   ENTRY-ARGS
   DRIVE
   S\" native-build: one explicit output path is required, then an optional `whitebox`, then an optional `--target <target>`\n" REFUSED ;

\ The build class is an argument and not an inherited variable, so the argument
\ has exactly one other spelling and everything else is refused by name.
: BAD-CLASS-CASE ( -- )
   s" and refuses a second argument that is neither `whitebox` nor `--target`" T-LABEL
   ENTRY-ARGS
   s" --" ARG
   s" /dev/null/native-build-entry-test" ARG
   s" wightbox" ARG
   DRIVE
   S\" native-build: after the output path come only `whitebox`, then `--target <target>`\n" REFUSED ;

: TARGET-ARGS ( -- )
   ENTRY-ARGS
   s" --" ARG
   s" /dev/null/native-build-entry-test" ARG
   s" --target" ARG ;

: TARGET-UNKNOWN-CASE ( -- )
   s" and refuses an unknown target profile" T-LABEL
   TARGET-ARGS
   s" linux-x86" ARG
   DRIVE
   S\" native-build: unknown --target profile\n" REFUSED ;

\ A product carries its own machine's backend and no other, so the target on the
\ other machine is the one this engine cannot emit for.
: FOREIGN$ ( -- ptr u8 n )
   HB-TARGET-LINUX-X86-64? if s" linux-aarch64" exit then
   s" linux-x86-64" ;

\ The profile resolves and has an architecture, but its native CORE ABI has
\ no implementation. This refusal precedes backend and target-window work.
: WINDOWS-CORE-CASE ( -- )
   s" and refuses the declared Windows profile before native compilation" T-LABEL
   TARGET-ARGS
   s" aarch64-pc-windows-msvc" ARG
   DRIVE
   S\" native-build: the --target profile has no native CORE ABI\n" REFUSED ;

: TARGET-UNLOADED-CASE ( -- )
   s" and refuses a target whose machine has no backend loaded here" T-LABEL
   TARGET-ARGS
   FOREIGN$ ARG
   DRIVE
   S\" native-build: the --target machine has no backend loaded; load its backend module before tools/native-build.f\n" REFUSED ;

: FOREIGN-BACKEND$ ( -- ptr u8 n )
   HB-TARGET-LINUX-X86-64? if s" src/arch/arm64/passes.f" exit then
   s" src/arch/x86-64/passes.f" ;

\ The selected action refuses a foreign-machine writer before opening the
\ compiler window; a writer loaded after that window would target its machine.
\ The output path is writable, so an earlier filesystem refusal cannot satisfy
\ the assertion and the other-machine build must leave it absent. An AArch64
\ host preloads the x86-64 writer instead and must produce an actual x86-64 ELF.
: FOREIGN-WINDOW-CASE ( -- )
   s" a host with the retired observer handles the foreign window at its writer boundary" T-LABEL
   s" other-machine-hb" FOREIGN-OUT!
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG
   s" test/native-build-host-observer.f" ARG
   FOREIGN-BACKEND$ ARG
   s" tools/native-build.f" ARG
   s" --" ARG
   FOREIGN-OUT$ ARG
   s" --target" ARG
   FOREIGN$ ARG
   DRIVE
   HB-TARGET-LINUX-X86-64? if
      S\" native-build: no writer is loaded for a --target on another machine; a writer loaded after the capture would compile for the window's machine\n" REFUSED
      FOREIGN-OUT$ FILE? 0= TTRUE
   else
      RC @ 0<> if OUT OUT-U @ type ERR$ type then
      RC @ 0 T=
      FOREIGN-OUT$ FILE? TTRUE
      FOREIGN-OUT$ FS-PATHZ open-rd {: fd:n :}
      fd 0 >= TTRUE
      fd CONTENT 64 read 64 T=
      fd close
      CONTENT 18 + LE:U16@ $3E T=
   then ;

: BAD-HOST-CASE ( -- )
   s" an unrelated fixed CODE registration remains incompatible" T-LABEL
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG
   s" test/native-build-host-unknown-code.f" ARG
   s" tools/native-build.f" ARG
   s" --" ARG
   s" /dev/null/native-build-host-row" ARG
   DRIVE
   S\" native-build: incompatible fixed engine layout\n" REFUSED ;

: ACTION-CASE ( -- )
   s" nested build entries preserve an active action" T-LABEL
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG
   s" test/native-build-action-child.f" ARG
   DRIVE
   RC @ 0<> if OUT OUT-U @ type ERR$ type then
   RC @ 0 T=
   OUT OUT-U @ s" native-build-action: ok" CONTAINS? TTRUE ;

: ABI-TARGET$ ( -- ptr u8 n )
   HB-TARGET-MACOS? if s" linux-aarch64" exit then
   s" macos-aarch64" ;

: FOREIGN-SEED ( ptr u8 n -- )
   FOREIGN-OUT!
   FOREIGN-OUT$ s" retained native output" WRITE-ALL ;

: FOREIGN-RESULT ( -- )
   RC @ 74 <> if OUT OUT-U @ type ERR$ type then
   RC @ 74 T=
   ERR-U @ 0 T=
   OUT OUT-U @ s" native-build: target process ABI or image format is not executable here" CONTAINS? TTRUE
   OUT OUT-U @ s" native-build: uncaught throw code 74" CONTAINS? TTRUE
   FOREIGN-OUT$ CONTENT 64 READ-ALL 22 T=
   CONTENT 22 s" retained native output" T$= ;

: FOREIGN-DEFAULT ( -- )
   s" same-ISA foreign default writer reaches process ABI refusal" T-LABEL
   s" default-hb" FOREIGN-SEED
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG
   s" tools/native-build.f" ARG
   s" --" ARG
   FOREIGN-OUT$ ARG
   s" --target" ARG
   ABI-TARGET$ ARG
   DRIVE
   FOREIGN-RESULT ;

: FOREIGN-ABI-CASE ( -- )
   HB-TARGET-MACOS? HB-TARGET-LINUX? or 0= if
      s" native-build foreign ABI: no supported same-ISA counterpart on this host" type cr
      exit
   then
   FOREIGN-DEFAULT ;

: RUN ( -- )
   T-RESET
   FOREIGN-ROOT!
   NO-OUTPUT-CASE
   BAD-CLASS-CASE
   TARGET-UNKNOWN-CASE
   WINDOWS-CORE-CASE
   TARGET-UNLOADED-CASE
   FOREIGN-WINDOW-CASE
   BAD-HOST-CASE
   ACTION-CASE
   FOREIGN-ABI-CASE
   T-REPORT ;

RUN
;package
