\ root-set.f - sets its source root to its own directory, then requests root-mid.f by the tree's name and
\ root-dep.f by a name only that root resolves: every part links when warm.

SOURCE-ROOT:ACTIVE-SOURCE 2drop 2drop SOURCE-ROOT:DIRNAME SOURCE-ROOT:ROOT!
require test/fixtures/file-objects/root-mid.f
require root-dep.f
package FILE-OBJECTS-ROOT-SET public
: V ( -- n ) FILE-OBJECTS-ROOT-MID:M FILE-OBJECTS-ROOT-DEP:D + ;
;package
