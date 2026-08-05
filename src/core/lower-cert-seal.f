\ lower-cert-seal.f — erase lowering-certificate producer authority.
\ Loaded after xref installs `undefine`; compiled calls retain direct xts.
\
\ Both producers are reached through declared dispatch cells (`defer`), so this
\ file retires their target, provider, and installer names. Undefining a name
\ never removes the cell or code, which is why already-compiled callers keep
\ dispatching and why the cells stay in the snapshot address-cell table and go
\ on being relocated (dot habu-declare-persisted-producer-76fbce09).

undefine CHECKER-CERT:PRODUCE
undefine CHECKER-CERT:PRODUCER-XT

package CHECKER-CERT
undefine PRODUCER-DEFAULT
undefine PRODUCER-UNAVAILABLE
;package

package LOWER-CERT
undefine FULL-DEFAULT-INSTALL
undefine FULL-DEFAULT
undefine FULL-PRODUCE-INSTALL
undefine FULL-PRODUCE
undefine DISPATCH-INSTALL
undefine DISPATCH
;package

undefine LOWER-CERT:FULL-XT
