\ engine-error.f - authoritative engine failure ABI.

package ENGINE-ERROR
public
82 constant AOT-SEED
83 constant SEAL-VIOLATION
84 constant SEAL-PACKAGE
85 constant BAD-TAG
86 constant CALLABLE-ABI
87 constant CATCH-STACK
88 constant CODE-CERT
\ `using NAME` / `;using` consumer-import failures (dot habu-using-import-pkg-a07dd7ba).
\ Each is recoverable inside `evaluate` (a catchable throw of the same code) and a
\ fail-closed process exit at top level, mirroring the package-keyword failure ABI.
89 constant USING-NO-NAME       \ `using` with no following token
90 constant USING-BAD-NAME      \ `using` package name contains ':'
91 constant USING-UNKNOWN       \ `using` names no known package
92 constant USING-OVERFLOW      \ more than USE-MAX concurrent usings
93 constant USING-UNBALANCED    \ `;using` with no using open
94 constant USING-AMBIGUOUS     \ bare tail resolves in more than one used public wordlist
;package
