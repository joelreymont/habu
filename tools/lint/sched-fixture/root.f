\ root.f - the seeded root of the disk-audit fixture tree. Nothing loads this
\ file; the fixture hands its path to SCHEDULE-LINT:REACH-ROOT and lets the
\ shipped closure walk out of it, so what the fixture tests is the production
\ reference grammar and not a copy of it.
\
\ Three reaching shapes, one per reachable neighbour, and two decoys.

require tools/lint/sched-fixture/linked.f

package SCHED-FIXTURE-ROOT

\ DECOY 1, for the reach side: this comment names
\ tools/lint/sched-fixture/dangling.f in the exact spelling a require would use.
\ The lexer consumes `\` lines before the token table exists, so a scanner that
\ searched text would grant it coverage and this one cannot see it at all.

: SPAWN-BARE ( -- ptr u8 n )
   s" tools/lint/sched-fixture/spawned.f" ;

\ The path inside a longer literal, which is how a generated load line carries
\ one. Reading the payload whole would miss it; reading its words finds it.
: SPAWN-EMBEDDED ( -- ptr u8 n )
   s" require tools/lint/sched-fixture/embedded.f" ;

\ DECOY 2: a literal naming a path that is not on disk. Existence is part of the
\ rule, so this grants nothing and cannot inflate the reached count.
: SPAWN-ABSENT ( -- ptr u8 n )
   s" tools/lint/sched-fixture/no-such-file.f" ;

;package
