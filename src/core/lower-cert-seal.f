\ lower-cert-seal.f — retire the certificate producer's public words once boot
\ has installed the producer. Loaded after xref installs `undefine`.

undefine CHECKER-CERT:INSTALL
undefine CHECKER-CERT:PRODUCE
