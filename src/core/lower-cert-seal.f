\ lower-cert-seal.f — erase lowering-certificate producer authority.
\ Loaded after xref installs `undefine`; compiled calls retain direct xts.

undefine CHECKER-CERT:INSTALL
undefine CHECKER-CERT:PRODUCE

package CHECKER-CERT
undefine PRODUCER-XT
end-package

package LOWER-CERT
undefine FULL-INSTALL
undefine FULL-PRODUCE
undefine DISPATCH
undefine FULL-XT
end-package
