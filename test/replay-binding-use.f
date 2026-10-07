\ replay-binding-use.f - the second file of the reopen reproducer, checked by
\ test/replay-binding.f through tools/check.f. It reopens REOPEN-BIND, whose
\ defining file test/reopen-binding-lib.f gives the package its own `@`, and
\ spells the engine's fetch bare: the compiler calls the package word, so the
\ checker must refuse GET-REOPENED by name.

require test/reopen-binding-lib.f

package REOPEN-BIND

public

: GET-REOPENED ( -- n ) REC 2 cells + @ ;

;package
