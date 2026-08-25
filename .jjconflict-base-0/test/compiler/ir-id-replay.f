\ ir-id-replay.f - child load-path fixture for require replay.
\
\ The parity gate runs this in its own `bin/hb` children and looks only at the
\ exit status, so every check here throws rather than reporting. It is a real
\ load path and not a numeric predicate on purpose: what is being proved is a
\ property of loading, namely that the module allocator is one process-wide
\ counter that a replay cannot restart.
\
\ Loading the file is the first mode. It mints an identity, keeps it, and after
\ each replay mints another and refuses it if it repeats the one it kept. A
\ counter that restarted would hand back the first identity again, because the
\ first identity is the first serial the counter ever issues.
\
\ The second mode runs the same load path with `HABU_IR_ID_REPLAY_FORCE` set in
\ the environment. It then asks for the production source to be included
\ outright, bypassing the require memo the first mode exercises. The engine must
\ refuse that with its package-seal status; if it did not, the source would
\ re-run and its `0 NEXT-SERIAL !` would restart the counter. A mode switch on
\ the environment rather than on an argument is what the loader leaves
\ available: `--load` treats every further argument as another file to load.
\
\ The reset below is INCLUDE-RESET-SCRATCH, which resets the loader scratch the
\ registry replay actually needs and nothing else. It used to be
\ INCLUDE-SNAPSHOT-PREPARE, which additionally zeroed INCLUDE-DEPTH - harmless
\ while `--load` inlined this file into the top-level stream at depth 0, and a
\ silent corruption of the caller's frame once `--load` began loading argv files
\ through the registry (dot habu-make-load-consult-85c88fb3): this file's own
\ load underflowed at INCLUDE-POP. Snapshot preparation now refuses to run under
\ an open load rather than zeroing its way through one, so this file names the
\ half it means.

require lib/errors.f
require src/compiler/ir/id.f

package IR-ID-REPLAY
public

\ Is this child the forced-replay arm? The variable's presence is the switch;
\ its value is not read.
: FORCED? ( -- bool )
   s" HABU_IR_ID_REPLAY_FORCE" GETENV nip 0 > ;

\ Keep the identity handed in, mint another, and refuse a repeat.
: STILL-FRESH ( IR-ID:ir-module-id -- IR-ID:ir-module-id )
   IR-ID:NEW-MODULE nip
   {: first:IR-ID:ir-module-id fresh:IR-ID:ir-module-id :}
   first fresh IR-ID:MODULE-SAME? if E-CID-REPLAY throw then
   first ;

: MAYBE-FORCE-REPLAY ( -- )
   FORCED? 0= if exit then
   s" src/compiler/ir/id.f" included ;

;package

\ One identity before any replay, kept for the rest of the run, and one more, so
\ a counter that never advances fails here rather than later.
IR-ID:NEW-MODULE nip
IR-ID-REPLAY:STILL-FRESH

\ Reset the loader scratch and ask for the identity authority again, twice.
INCLUDE-RESET-SCRATCH
require src/compiler/ir/id.f
IR-ID-REPLAY:STILL-FRESH

INCLUDE-RESET-SCRATCH
require src/compiler/ir/id.f
IR-ID-REPLAY:STILL-FRESH

drop

IR-ID-REPLAY:MAYBE-FORCE-REPLAY
