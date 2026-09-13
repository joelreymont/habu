\ aot-chain-capture-suite.f - the AOT artifact format and the chain capture tool,
\ both through the real load path (dot habu-retire-the-s-4fbc244f).
\
\ WHY A SUITE AT ALL. src/habu/aot-file.f and tools/aot-chain-capture.f were loaded
\ by exactly two things, neither of them a registered check: the stdin metabuild
\ host, which assembles the format into its own generated stage source, and the
\ capture tool, which the build runs inside its capture host. So a word one of them
\ named and nothing defined was not a red suite - it was a build dying much later,
\ in generated source, on a name whose reader could not place it. That is what
\ happened: the format kept a protected-WID bitmap section for months after its
\ producer became a live derivation, and the two buffer readers it still had named
\ AOT-PWID-BUF@, which no filler defines. Two lanes measured it as `hb-stdin-mk`
\ dying E-UNDEFINED in the Gforth recovery chain and as the native refresh's
\ certify pass rejecting the assembled stdin source. Both files are loaded here, so
\ that class of residue is a red suite now.
\
\ TWO CHILDREN, BECAUSE THE PRODUCT CANNOT CAPTURE ITS OWN CHAIN.
\   test/aot-artifact-roundtrip.f captures a small window in this engine and drives
\   AOT-FILE:WRITE, READ and WRITE again over it, which is the whole format: the
\   header, the section table, both digest passes, every section's length
\   arithmetic and the closure walk. It also executes the declared-address-cell
\   predicate tools/aot-chain-capture.f ?XTOFF asserts, which only the capture host
\   could otherwise reach - hence the `xtcells=2` line pinned below: one cell inside
\   that window and one outside it targeting it. Its refusals are its assertions.
\   tools/aot-chain-capture.f is run as the build runs it, and must refuse by name:
\   bin/hb provides every file the compiler chain's closure names, so `require` is
\   a no-op there and the window comes up empty (the capture host is the only
\   engine that can run it - LESSONS.md 2026-08-17). Reaching that refusal is what
\   proves the tool's whole closure - the window prelude, the capture, the identity
\   and src/habu/aot-file.f - compiled in a booted engine; an undefined word
\   anywhere in it answers E-UNDEFINED with a different code instead.
\
\ WHAT THIS FILE ADDS THAT NEITHER CHILD CAN SAY ABOUT ITSELF. The artifact's
\ header is read back as bytes and checked against the format's identity written
\ down a second time here - magic, version, section count - and against the
\ arithmetic it promises (136 + payload length = the file). Two files having to
\ agree is what makes a version bump or a dropped section a deliberate change. And
\ the PRODUCER KEY the artifact carries is compared against a SHA-256 this process
\ takes of bin/hb from the outside, so the key is a reading of a file pinned here
\ rather than a claim the writer made about itself.
\
\ WHAT IT DOES NOT COVER. Every refusal src/habu/aot-file.f lists in its own header
\ is still unforged: nothing here truncates a payload, doctors a section table or
\ moves a chain source behind a stored digest. Those cases belong in this suite
\ when they are written. Nor does anything here capture the compiler chain - that
\ needs the build's capture host, and the capture tool's own assertions over that
\ window run in tools/build-fixpoint.f's artifact fixpoint.
\
\ Cost: two child engine runs, both under two seconds. Registered as
\ `TEST:SUITE aot-chain-capture` in test/gate-stdlib-cases.f. Run standalone:
\   bin/hb --load test/aot-chain-capture-suite.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f

package AOT-CHAIN-SUITE

$8000 constant CAP
60000 constant CHILD-TIMEOUT-MS

\ The header this reads back is src/habu/aot-file.f's, and these are its offsets
\ and its identity. They are written out again rather than imported because that
\ file cannot be loaded here - its closure is a capture's, not a test's - and the
\ two spellings agreeing is the thing being checked.
136 constant HDR-BYTES
0 constant O-MAGIC
8 constant O-VERSION
24 constant O-SECTIONS
32 constant O-PAYLEN
40 constant O-PRODUCER
$00544F4155424148 constant MAGIC     \ "HABUAOT\0" in LE byte order
6 constant VERSION
17 constant SECTIONS
64 constant HEX-LEN

\ tools/aot-chain-capture.f's refusal code and the sentence the product must die
\ with, which is a different exit from every undefined-word death.
$4A constant REFUSE-RC

create OUT CAP allot     variable OUT-U
create ERR CAP allot     variable ERR-U
create EMPTY 1 allot                          \ zero-length stdin
variable RC

create ROOT-BUF FS-PATH-CAP allot   variable ROOT-U
create ART-BUF FS-PATH-CAP allot    variable ART-U
create HDR HDR-BYTES allot
create PROD-HEX HEX-LEN allot        \ sha256(bin/hb), taken here
create ART-HEX HEX-LEN allot         \ the producer key the artifact carries

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: ART$ ( -- ptr u8 n ) ART-BUF ART-U @ ;
: HB$ ( -- ptr u8 n ) s" bin/hb" ;
: OUT$ ( -- ptr u8 n ) OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n ) ERR ERR-U @ ;

\ One tree per run, registered for cleanup, so "the artifact exists" is a statement
\ about the capture that just ran and never about a leftover.
: SETUP ( -- )
   s" habu-aot-chain" TMPDIR-MKDIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY  u ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" small.aot" ART-BUF JOIN-PATH ART-U ! ;

: RUN-CHILD ( -- )
   HB$ >LEN  EMPTY 0 >LEN  OUT CAP >LEN  ERR CAP >LEN  CHILD-TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  0 RC ! ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  c RC>N RC ! ENDOF
   ;MATCH ;

: RUN-ROUNDTRIP ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/aot-artifact-roundtrip.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   ART$ >LEN PROC-ARGV+
   RUN-CHILD ;

\ The argv the build uses (tools/build-fixpoint.f BF-PREPARE-CAPTURE-ARGV), with
\ the artifact path this suite would have taken if the product could capture.
: RUN-CAPTURE-TOOL ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/aot-chain-capture.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   ROOT$ >LEN PROC-ARGV+
   RUN-CHILD ;

: SAID? ( ptr u8 n -- ) {: m:ptr mu:n :}
   m mu T-LABEL
   OUT$ m mu CONTAINS? TTRUE ;

: ERR-SAID? ( ptr u8 n -- ) {: m:ptr mu:n :}
   m mu T-LABEL
   ERR$ m mu CONTAINS? TTRUE ;

: CHILD-FAILED. ( -- )
   RC @ 0 = if exit then
   s" aot-chain-capture-suite: child stdout:" type cr OUT$ type cr
   s" aot-chain-capture-suite: child stderr:" type cr ERR$ type cr ;

\ Exactly the header, read with the engine's own descriptor words: the artifact is
\ larger than a header and READ-ALL refuses a cap smaller than the whole file.
: HEADER@ ( -- )
   ART$ FS-PATHZ open-rd {: fd:n :}
   fd 0 < if
      s" aot-chain-capture-suite: cannot open the artifact" type cr
      E-FS-OPEN throw
   then
   fd HDR HDR-BYTES read {: got:n :}
   fd close
   s" the artifact is at least a header long" T-LABEL
   got HDR-BYTES T= ;

: HDR-U64@ ( n -- n ) {: at:n :} HDR at + FS-U64@ ;

: PROBE-ROUNDTRIP ( -- )
   SETUP
   RUN-ROUNDTRIP
   s" a capture writes, reads and rewrites its artifact byte for byte" T-LABEL
   CHILD-FAILED.
   RC @ 0 T=
   s" roundtrip: recs=" SAID?
   s" pwin=1" SAID?
   s" xtcells=2" SAID?
   s" roundtrip=ok" SAID? ;

: PROBE-ARTIFACT ( -- )
   s" the artifact is at the path the capture was given" T-LABEL
   ART$ FILE? TTRUE
   HEADER@
   s" its magic is this format's" T-LABEL
   O-MAGIC HDR-U64@ MAGIC T=
   s" its version is the one the reader accepts" T-LABEL
   O-VERSION HDR-U64@ VERSION T=
   s" its section count is the one the reader accepts" T-LABEL
   O-SECTIONS HDR-U64@ SECTIONS T=
   s" the header's payload length accounts for the whole file" T-LABEL
   ART$ FILE-SIZE  HDR-BYTES O-PAYLEN HDR-U64@ +  T= ;

\ The producer key, hashed here rather than believed.
: PROBE-PRODUCER ( -- )
   s" bin/hb hashes at the path this suite reads it from" T-LABEL
   HB$ PROD-HEX SHA256-FILE-HEX 0 T=
   s" the artifact names that hash as its producer" T-LABEL
   HDR O-PRODUCER + ART-HEX SHA256>HEX
   ART-HEX HEX-LEN  PROD-HEX HEX-LEN  T$= ;

\ The tool loads in a booted engine and refuses the one way the product must.
: PROBE-CAPTURE-TOOL ( -- )
   RUN-CAPTURE-TOOL
   s" the capture tool refuses the product by its own code" T-LABEL
   RC @ REFUSE-RC T=
   s" and names the empty window rather than a missing word" T-LABEL
   s" the window is empty - the chain did not load" ERR-SAID? ;

: BODY ( -- )
   PROBE-ROUNDTRIP
   PROBE-ARTIFACT
   PROBE-PRODUCER
   PROBE-CAPTURE-TOOL ;

public

: RUN ( -- )
   T-RESET
   CLEANUP-RESET
   [: BODY ;] catch {: code:n :}
   CLEANUP-RUN
   code 0 <> if code throw then
   T-REPORT
   s" aot-chain-capture: ok" type cr ;

;package

AOT-CHAIN-SUITE:RUN
