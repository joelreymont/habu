\ codegen-compare-cc.f - the host C toolchain the clang reference column is
\ built with. One concern: running clang and the binary-inspection tools, and
\ saying loudly when they are not there.
\
\ WHAT THIS BUILDS. tools/clang/twins.c, once per run, into a temporary
\ directory of this process's own: one object file, whose per-symbol code sizes
\ are the reference column's byte counts, and one dynamic library linked FROM
\ THAT OBJECT, which is what the harness calls. The library is linked from the
\ object rather than compiled a second time, so the bytes the report prints are
\ the bytes that run.
\
\ THE FLAGS, AND WHY EACH ONE IS THERE.
\
\   -O2               what the reference is asked to be: an ordinary release
\                     build, not a hand-tuned one.
\   -arch arm64       the architecture both other columns emit.
\   -fno-math-errno   a square root becomes the fsqrt instruction, as the habu
\                     word is, instead of a libm call with an errno test around
\                     it. Without it the float rows would be measuring a call
\                     into a shared library.
\
\ AND WHAT IS NOT PASSED: -ffast-math. The recorded float answers are compared
\ bit for bit against the same pins the other two columns are held to, and
\ reassociating a sum changes them. A reference that answered differently would
\ not be a reference.
\
\ ABSENT IS DECIDED BY RUNNING THE REAL PATH, NOT BY ASKING A TOOL ITS VERSION.
\ READY? probes clang, compiles, links, and reads the object back with nm and
\ size, and it is that whole pipeline that decides whether there is a third
\ column. A version flag would have been a heuristic - Apple's `size` has no
\ --version and refuses one, which is exactly the kind of answer a heuristic
\ reads as absence - so what is asked instead is the question that matters: does
\ the toolchain do the job.
\
\ AND THE TWO ANSWERS ARE TOLD APART STRUCTURALLY. A command /usr/bin/env cannot
\ find exits 127, which POSIX reserves for that and nothing else, so 127 means
\ the tool is not on this host and the column is absent with the tool named.
\ Any OTHER nonzero code came from a tool that WAS there and refused, which
\ means the reference does not build - a failure of this harness, thrown with
\ the tool's own diagnosis printed, never absorbed into "no clang here".
\
\ WHEN THE TOOLCHAIN IS NOT THERE THE COLUMN IS ABSENT AND SAYS SO. A missing
\ clang is not an error and it is not silence: READY? answers false,
\ ABSENT-WHY$ names what was missing, and the caller prints one line. A run on a
\ host without a C compiler therefore reports two columns and says why there are
\ not three, which is a result; a run that quietly printed two columns would be
\ a comparison nobody could tell had lost its reference.
\
\ THE OWNER OF THE TREE, AND WHY A FORKED CHILD IS NOT IT. The gate runs this
\ harness as a forked pool member, and fork copies the whole address space:
\ a child inherits ROOT-U, the built tree's path, and the mapped library,
\ having built none of them. Two rules follow, and both are enforced rather
\ than assumed.
\
\ It may not REMOVE. The tree holds the library other processes have mapped,
\ so a child unlinking it is a child deciding for everyone. REMOVE therefore
\ asks OWNER? -- the recorded pid against this one, because fork copies a flag
\ and does not copy a pid -- and does nothing in a process that did not build.
\
\ It may not MAP a library the parent did not map. dlopen of an image that is
\ not already loaded runs dyld's loader, and dyld is not fork-safe: measured on
\ macOS 26.5.1, a forked child that dlopens the freshly built twins.dylib takes
\ SIGBUS inside /usr/lib/dyld and the member exits 134, while the identical
\ call in the parent, or in the child when the parent had already mapped it,
\ returns normally. So the mapping is the FORKING process's job:
\ CODEGEN-CABI:PREPARE builds and maps before the fork, children inherit both,
\ and CODEGEN-CABI:OPEN refuses -- named, not aborted -- if it is ever reached
\ in a process that would have to load the image itself.
\
\ THE HOST THIS READS. The byte column comes out of a Mach-O object, through
\ nm and size, so the reference column is built on a Mach-O host. On any other
\ host the column is absent for that reason, named the same way a missing
\ compiler is. Dot habu-read-elf-symbol-090540c1 carries the ELF reader that
\ retires the restriction.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/fmt.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process-command.f

package CODEGEN-CC

private

128 constant PATH-CAP
160 constant WHY-CAP
$8000 constant NM-CAP                 \ the reference object's whole symbol table as text
$1000 constant SIZE-CAP
120000 constant TOOL-MS               \ a cold clang on a loaded host still finishes well inside this
127 constant NOT-FOUND-RC             \ POSIX: what a shell or env exits when the command is not there

PATH-CAP BUFFER: ROOT-BYTES
PATH-CAP BUFFER: OBJ-BYTES
PATH-CAP BUFFER: LIB-BYTES
WHY-CAP BUFFER: WHY-BYTES
NM-CAP BUFFER: NM-BYTES
SIZE-CAP BUFFER: SIZE-BYTES

variable ROOT-U
variable OBJ-U
variable LIB-U
variable WHY-U
variable NM-U
variable SIZE-U
variable READY-FLAG                   \ -1 ready, 0 absent, 1 not yet decided
variable OWNER-PID                    \ pid of the process that built the tree (0 before MAKE-ROOT)

: TRUE? ( -- bool )
   0 0= ;

: FALSE? ( -- bool )
   TRUE? 0= ;

: ENV$ ( -- ptr u8 n )
   s" /usr/bin/env" ;

: CC$ ( -- ptr u8 n )
   s" clang" ;

: NM$ ( -- ptr u8 n )
   s" nm" ;

: SIZE$ ( -- ptr u8 n )
   s" size" ;

: SOURCE$ ( -- ptr u8 n )
   s" tools/clang/twins.c" ;

\ Why the column is absent, kept as this file's own text: the caller prints it
\ and the reason has to outlive the command that produced it.
: WHY! ( ptr u8 n -- ) {: a:ptr u:n :}
   u WHY-CAP > if E-CODEGEN-CLANG-TOOL throw then
   a WHY-BYTES u STR-LEN BYTE-COPY-LEN
   u WHY-U ! ;

: COPY-INTO ( ptr u8 n ptr u8 n ptr n -- ) {: a:ptr u:n dst:ptr cap:n lenp:ptr :}
   u cap > if E-CODEGEN-CLANG-TOOL throw then
   a dst u STR-LEN BYTE-COPY-LEN
   u lenp ! ;

: ARG ( ptr u8 n -- )
   >LEN PROC-CMD:ARG+ ;

\ One staged command run through /usr/bin/env, so a tool is found the way any
\ other program on this host finds it, and its completion code comes back
\ unjudged: telling "not there" from "there and refused" is the caller's job and
\ the whole point of the distinction.
: RUN-TOOL ( -- n )
   ENV$ >LEN TOOL-MS >MS PROC-CMD:RUN-RC
   MATCH result
     ok  OF ENDOF
     err OF ENDOF
   ;MATCH ;

\ What a tool that was there and refused gets to say. Its own diagnosis is the
\ only thing that names which line of C, or which symbol, it choked on, so it is
\ printed at the failure rather than dropped.
: SAY-REFUSAL ( ptr u8 n n -- ) {: a:ptr u:n rc:n :}
   s" codegen-compare: " type a u type
   s"  refused with status " type rc FMT:.U cr
   PROC-CMD:ERR$ type cr ;

: NOT-FOUND! ( ptr u8 n -- ) {: a:ptr u:n :}
   SB-RESET
   a u SB-APPEND
   s"  is not on this host" SB-APPEND
   SB$ WHY! ;

\ Run the staged command and answer whether it worked. False means the tool is
\ not on this host, with the reason recorded; a tool that was there and refused
\ throws, because a reference that half-built is worse than none.
: TOOL-OK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   RUN-TOOL {: rc:n :}
   rc 0 = if TRUE? exit then
   rc NOT-FOUND-RC = if a u NOT-FOUND! FALSE? exit then
   a u rc SAY-REFUSAL
   E-CODEGEN-CLANG-TOOL throw ;

: ROOT$ ( -- ptr u8 n )
   ROOT-BYTES ROOT-U @ ;

: OBJ$ ( -- ptr u8 n )
   OBJ-BYTES OBJ-U @ ;

public

: LIB$ ( -- ptr u8 n )
   LIB-BYTES LIB-U @ ;

\ What the reference column is missing, when it is missing something.
: ABSENT-WHY$ ( -- ptr u8 n )
   WHY-BYTES WHY-U @ ;

\ What nm and size said about the reference object, kept here because the
\ command runner's own capture buffer is overwritten by the next command and
\ two readers want two different captures.
: NM-TEXT$ ( -- ptr u8 n )
   NM-BYTES NM-U @ ;

: SIZE-TEXT$ ( -- ptr u8 n )
   SIZE-BYTES SIZE-U @ ;

: FLAGS$ ( -- ptr u8 n )
   s" -O2 -arch arm64 -fno-math-errno" ;

private

: MAKE-ROOT ( -- )
   getpid OWNER-PID !
   s" habu-ccref" TMPDIR-MKDIR ROOT-BYTES PATH-CAP ROOT-U COPY-INTO
   ROOT$ s" twins.o" OBJ-BYTES JOIN-PATH OBJ-U !
   ROOT$ s" twins.dylib" LIB-BYTES JOIN-PATH LIB-U ! ;

: PROBE-CC ( -- bool )
   PROC-CMD:RESET
   CC$ ARG
   s" --version" ARG
   CC$ TOOL-OK? ;

: COMPILE ( -- bool )
   PROC-CMD:RESET
   CC$ ARG
   s" -O2" ARG
   s" -arch" ARG
   s" arm64" ARG
   s" -fno-math-errno" ARG
   s" -c" ARG
   SOURCE$ ARG
   s" -o" ARG
   OBJ$ ARG
   CC$ TOOL-OK? ;

: LINK ( -- bool )
   PROC-CMD:RESET
   CC$ ARG
   s" -dynamiclib" ARG
   s" -arch" ARG
   s" arm64" ARG
   s" -o" ARG
   LIB$ ARG
   OBJ$ ARG
   CC$ TOOL-OK? ;

: READ-NM ( -- bool )
   PROC-CMD:RESET
   NM$ ARG
   s" -m" ARG
   OBJ$ ARG
   NM$ TOOL-OK? 0= if FALSE? exit then
   PROC-CMD:OUT$ NM-BYTES NM-CAP NM-U COPY-INTO
   TRUE? ;

: READ-SIZE ( -- bool )
   PROC-CMD:RESET
   SIZE$ ARG
   s" -m" ARG
   OBJ$ ARG
   SIZE$ TOOL-OK? 0= if FALSE? exit then
   PROC-CMD:OUT$ SIZE-BYTES SIZE-CAP SIZE-U COPY-INTO
   TRUE? ;

: DECIDE ( -- bool )
   HB-TARGET-MACOS? 0= if
      s" this host is not Mach-O and the byte column reads Mach-O symbols" WHY!
      FALSE? exit
   then
   SOURCE$ FILE? 0= if
      s" tools/clang/twins.c is missing" WHY!
      FALSE? exit
   then
   PROBE-CC 0= if FALSE? exit then
   MAKE-ROOT
   COMPILE 0= if FALSE? exit then
   LINK 0= if FALSE? exit then
   READ-NM 0= if FALSE? exit then
   READ-SIZE ;

public

\ Is there a reference column on this host? Decided once, by building it: the
\ object, the library, and both readings of the object are all in place when
\ this answers true, and ABSENT-WHY$ names what stopped it when it answers
\ false.
: READY? ( -- bool )
   READY-FLAG @ 0 <= if READY-FLAG @ 0<> exit then
   DECIDE dup if -1 else 0 then READY-FLAG ! ;

\ Did THIS process build the tree? A forked child inherits ROOT-U and the
\ mapped library but built neither, so it answers false and must not touch
\ either. The question is the pid rather than a flag because fork copies flags
\ and does not copy a pid.
: OWNER? ( -- bool )
   ROOT-U @ 0= if 0 0= 0= exit then
   OWNER-PID @ getpid = ;

\ Take the temporary tree away again, and only the process that made it may.
\ The tree carries the library a caller has mapped; a forked child shares that
\ mapping and would be pulling the file out from under every process that holds
\ it, so REMOVE in a child is a no-op rather than a race. In the process that
\ DID build it this is called after the last measured pass: unlinking a mapped
\ file leaves the mapping valid, so the ordering is about the tree's other
\ contents, not about the library.
: REMOVE ( -- )
   OWNER? 0= if exit then
   ROOT$ EXISTS? 0= if exit then
   ROOT$ REMOVE-TREE ;

private

: INIT ( -- )
   1 READY-FLAG !
   0 OWNER-PID !
   0 ROOT-U !
   0 WHY-U !
   0 NM-U !
   0 SIZE-U ! ;

INIT

;package
