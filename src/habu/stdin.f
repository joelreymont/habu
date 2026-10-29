\ stdin.f — internal driver: emit the stdin-program engine used while building
\ the single installed `bin/hb`. The output reads its program from stdin; on a
\ tty it runs the baked repl.f instead, which turns it into the interactive REPL.
\ Swapped in for stage2.f by tools/build-fixpoint.f's stdin build path.

\ output path — the single knob; the build-fixpoint driver owns the artifact
: STDIN-OUT ( -- ptr u8 n ) s" hb-stdin-got" TMP-PATH ;

\ The REPL source this driver captures (repo-root relative). The debugger trio
\ stays out: this recovery engine reaches it with `require src/habu/debug.f`, and
\ a captured trio would carry a second BPW-MAX that the require dies on. The
\ product bakes the trio through src/habu/native-runtime.f.
: REPL-SRC ( -- ptr u8 n ) s" src/habu/repl.f" ;
PTR-VARIABLE HB  variable HL  variable HFD  variable HRD
$20000 constant HMAX
\ HB exposes the raw baked-source buffer cell.
: HB@ ( -- ptr u8 ) HB @ ;
s" HB@" s" -- ptr u8" TRUST

: HNL ( -- )
   HL @ 1 + HMAX > IF s" hb: repl sources exceed buffer" 74 die THEN
   10 HB@ HL @ + c!  HL @ 1 + HL ! ;

: RD-1 ( ptr u8 -- )
   0 0 open HFD !
   HFD @ 0 < IF s" hb: cannot open repl source" 74 die THEN
   BEGIN                                                 \ read() may return short
     HFD @  HB@ HL @ +  HMAX HL @ -  read HRD !
     HRD @ 0 >
   WHILE  HL @ HRD @ + HL !  REPEAT
   HFD @ close
   HRD @ 0 < IF s" hb: repl source read failed" 74 die THEN
   HNL ;

: HFILE ( ptr u8 n -- ) PATH0 RD-1 ;

: HUNKNOWN-TARGET ( -- )
   s" hb: unknown target" 76 die ;

: READ-REPL-TARGET ( -- )
   HB-TARGET-LINUX? IF
      s" src/os/linux/repl-term.f" HFILE  exit
   THEN
   HB-TARGET-MACOS? IF
      s" src/os/macos/repl-term.f" HFILE  exit
   THEN
   HB-TARGET-LINUX-X86-64? IF
      s" src/os/linux-x86-64/repl-term.f" HFILE  exit
   THEN
   HUNKNOWN-TARGET ;

: READ-REPL ( -- )
   here HB !  HMAX allot  0 HL !
   READ-REPL-TARGET
   REPL-SRC PATH0 RD-1
   HL @ 2 > 0= IF s" hb: repl sources missing" 74 die THEN
   HL @ HMAX = IF s" hb: sources exceed buffer" 74 die THEN ;

\ The REPL is compiled in this metabuild host and captured (code, records, call,
\ DATA and CODE relocations), so bin/hb seeds it at boot as code with no baked
\ source: EMIT-DICT bakes only the emit-builder #PL cold prefix, and EM-SEED-AOT
\ copies the blob, registers the records, relocates it and calls the boot-run
\ list (INSTALL). Other drivers leave their AOT buffers empty and EM-SEED-AOT
\ skips. The seed runs at every boot's prefix end,
\ so every global name captured here is in every
\ program's dictionary: a program defining the same spelling dies `duplicate
\ definition`, and a required file whose names the engine already carries cannot
\ load. Check a new name against the tree before capturing it.
package STDIN-DRIVER
public

\ An empty artifact makes this emit the capture host, a declared one the product:
\ the only difference, with no mode flag and no second prefix. The build driver
\ splices both paths in as a call (tools/build-fixpoint.f
\ BF-EMIT-STDIN-RUN-SOURCE); a build source must not read them from the
\ environment. The producer key is two readings of one fact: the SHA-256 the
\ capture stamped into the artifact and the hash of the engine named here. A
\ mismatch is the reader's refusal.
PATH-CAP constant APATH-CAP       \ the tree's path capacity (src/core/util.f)
create ART-P APATH-CAP allot   variable ART-U
create ENG-P APATH-CAP allot   variable ENG-U
create PROD 32 allot
create FSHA-CTX SHA256-FILE-CTX-BYTES allot   \ this driver's own file-digest context

: PATH-COPY ( ptr u8 n ptr u8 -- ) {: a:ptr u:n d:ptr :}
   u APATH-CAP > IF s" hb: artifact path exceeds the driver's buffer" 74 die THEN
   u 0 ?do a i + c@  d i + c!  loop ;

\ ( artifact-path engine-path -- ): the artifact to merge and the engine that
\ produced it. Called with two empty spans, this emit is the capture host.
: ARTIFACT! ( ptr u8 n ptr u8 n -- ) {: a:ptr au:n e:ptr eu:n :}
   a au ART-P PATH-COPY  au ART-U !
   e eu ENG-P PATH-COPY  eu ENG-U ! ;

private

\ CAPTURE-REPL latches `here` as the window's DATA base, and an interpret-mode
\ `s"` allots there, so a parameter spliced as a top-level literal moves the base
\ and every literal in the window: two builds differing only in artifact path
\ length differed in 12081 bytes. A spliced parameter belongs inside a colon
\ body, below the window, where the seed's canonical CODE-B0 absorbs it
\ (tools/aot-chain-bake.f). DP-MARK is this file's last act: whatever the driver
\ appends runs between it and RUN, and RUN refuses by name if the cursor moved.
\ Unmarked, DP0 is 0 and no boot can match it.
PTR-VARIABLE DP0

: ?DP ( -- )
   here DP0 @ = IF exit THEN
   s" hb: the DATA cursor moved after the driver marked it; this emit would not reproduce"
   74 die ;

\ The window's four coordinates are AOT-ARM's, latched by the two words that
\ name the moments. The widened re-captures in test/aot-wid-build.f start their
\ windows where this one started its own and read the same cells.
: CAPTURE-REPL ( -- )
   READ-REPL                                     \ REPL sources -> HB scratch buffer
   AOT-ARM:WINDOW-OPEN                            \ the engine declines to inline pre-window chains from here on
   AOT-ARM:R0 @ AOT-ARM:D0 @ AOT-CAPTURE:PRELUDE-MARK  \ no prelude: this host compiles only what the target's prefix carries
   HB@ HL @ evaluate-closed                       \ compile the REPL in the host dictionary
   AOT-ARM:WINDOW-CLOSE
   AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE
   s" INSTALL" AOT-CAPTURE:BOOTRUN+ ;              \ repl.f -> REPL read hook + termios save

\ Append the declared artifact to what CAPTURE-REPL just captured, in the
\ coordinates of that capture, so EMIT-AOT-SEED still bakes one of everything.
\ A declared artifact is mandatory once declared: an engine this build cannot key
\ or read is a refusal, never a quietly unseeded product.
: MERGE-ARTIFACT ( -- )
   ART-U @ 0= IF exit THEN
   ENG-U @ 0= IF
      s" hb: an artifact was declared with no engine to key it against" 74 die
   THEN
   FSHA-CTX ENG-P ENG-U @ PROD SHA256-FILE-IN 0 <> IF
      s" hb: cannot hash the engine that produced the artifact" 74 die
   THEN
   PROD ART-P ART-U @ AOT-FILE:MERGE ;

;package

package STDIN-DRIVER
public

: DP-MARK ( -- ) here DP0 ! ;

: RUN ( -- )
   ?DP
   CAPTURE-REPL
   MERGE-ARTIFACT
   0 0= STDIN? !
   HB@ 0 ENGINE-EMIT:FORTH                        \ empty LSRC: the REPL is seeded, not re-parsed
   SIGN-ID:ENGINE$ STDIN-OUT DRV-EMIT-IMAGE
   DRV-EXIT-OK ;

;package

STDIN-DRIVER:DP-MARK
STDIN-DRIVER:RUN
