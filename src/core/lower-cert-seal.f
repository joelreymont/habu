\ lower-cert-seal.f — erase lowering-certificate producer authority.
\ Loaded after xref installs `undefine`; compiled calls retain direct xts.
\
\ Both producers are now reached through declared dispatch cells (`defer`), so
\ this file retires the two things that can still GRANT authority: the name of
\ each dispatch cell, without which no later source can write `is` at it, and
\ the flag that records the grant. Undefining a name never removes the cell or
\ the code, which is why the already-compiled callers below the seal keep
\ dispatching and why the cells stay in the snapshot address-cell table and go
\ on being relocated (dot habu-declare-persisted-producer-76fbce09).

undefine CHECKER-CERT:INSTALL
undefine CHECKER-CERT:PRODUCE

package CHECKER-CERT
undefine PRODUCER-XT
undefine PRODUCER-SET
;package

package LOWER-CERT
undefine FULL-INSTALL
undefine FULL-PRODUCE-INSTALL
undefine FULL-PRODUCE
undefine DISPATCH-INSTALL
undefine DISPATCH
undefine FULL-XT
undefine FULL-SET
;package
