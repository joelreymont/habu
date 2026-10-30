\ jws-fixture.f - the RSA key of RFC 7515 appendix A.2, for tests that sign
\ or verify RS256: the public JWK members n and e as base64url text, as the RFC
\ prints them, and the private components as an unencrypted PKCS#8 PEM.
\ STORAGE CLASS. PROCESS-WIDE: two byte rows written at load and only read
\ after.

package JWS-FIXTURE
private

$180 constant MODULUS-CAP
$800 constant PEM-CAP
MODULUS-CAP BUFFER: MODULUS-ROW
variable MODULUS-U
PEM-CAP BUFFER: PEM-ROW
variable PEM-U

: MODULUS+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   a MODULUS-ROW MODULUS-U @ + u BYTE-COPY
   MODULUS-U @ u + MODULUS-U ! ;

: PEM+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   a PEM-ROW PEM-U @ + u BYTE-COPY
   10 PEM-ROW PEM-U @ u + + c!
   PEM-U @ u + 1+ PEM-U ! ;


: FILL ( -- )
   s" ofgWCuLjybRlzo0tZWJjNiuSfb4p4fAkd_wWJcyQoTbji9k0l8W26mPddxHmfHQp" MODULUS+
   s" -Vaw-4qPCJrcS2mJPMEzP1Pt0Bm4d4QlL-yRT-SFd2lZS-pCgNMsD1W_YpRPEwOW" MODULUS+
   s" vG6b32690r2jZ47soMZo9wGzjb_7OMg0LOL-bSf63kpaSHSXndS5z5rexMdbBYUs" MODULUS+
   s" LA9e-KXBdQOS-UTo7WTBEMa2R2CapHg665xsmtdVMTBQY4uDZlxvb3qCo5ZwKh9k" MODULUS+
   s" G4LT6_I5IhlJH7aGhyxXFvUK-DWNmoudF8NAco9_h9iaGNj8q2ethFkMLs91kzk2" MODULUS+
   s" PAcDTW9gb54h4FRWyuXpoQ" MODULUS+
   s" -----BEGIN PRIVATE KEY-----" PEM+
   s" MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQCh+BYK4uPJtGXO" PEM+
   s" jS1lYmM2K5J9vinh8CR3/BYlzJChNuOL2TSXxbbqY913EeZ8dCn5VrD7io8ImtxL" PEM+
   s" aYk8wTM/U+3QGbh3hCUv7JFP5IV3aVlL6kKA0ywPVb9ilE8TA5a8bpvfbr3SvaNn" PEM+
   s" juygxmj3AbONv/s4yDQs4v5tJ/reSlpIdJed1LnPmt7Ex1sFhSwsD174pcF1A5L5" PEM+
   s" ROjtZMEQxrZHYJqkeDrrnGya11UxMFBji4NmXG9veoKjlnAqH2QbgtPr8jkiGUkf" PEM+
   s" toaHLFcW9Qr4NY2ai50Xw0Byj3+H2JoY2PyrZ62EWQwuz3WTOTY8BwNNb2BvniHg" PEM+
   s" VFbK5emhAgMBAAECggEAEq5xpGnNCivDflJsRQBXHx1hdR1k6Ulwe2JZD50LpXyW" PEM+
   s" PEAeP88vLNO97IjlA7/GQ5sLKMgvfTeXZx9SE+7YwVol2NXOoAJe46sui395IW/G" PEM+
   s" O+pWJ1O0BkTGoVEn2bKVRUCgu+GjBVaYLU6f3l9kJfFNS3E0QbVdxzubSu3Mkqzj" PEM+
   s" kn439X0M/V51gfpRLI9JYanrC4D4qAdGcopV/0ZHHzQlBjudU2QvXt4ehNYTCBr6" PEM+
   s" XCLQUShb1juUO1ZdiYoFaFQT5Tw8bGUl/x/jTj3ccPDVZFD9pIuhLhBOneufuBiB" PEM+
   s" 4cS98l2SR/RQyGWSeWjnczT0QU91p1DhOVRuOopznQKBgQDgHMQQ60imZV1URk0K" PEM+
   s" pLttoLhyt3SmqdEf/kgHePDdtzEafpAu+cS191R2JiuoF2yzWXnwFDcqGigp5BNr" PEM+
   s" 0AHQfD8/Lk8l1Mk09jxxCfui5nYooNyac8YFjm3vItzVCVDnEd3BbVWG8qf6deqE" PEM+
   s" lMGAg+C2V0L4oNXnP7LZcPAZRwKBgQC5A8R+CZW2MvRTLLHzwZkUXV866cff7tx6" PEM+
   s" kqa0finGG0KgeqlaZPxgCZnFp7AeAcCMtiynVlJ7vMVgePsLq6XtON4tB5kP9jAP" PEM+
   s" rq3rbAOal78eUH5OcED6eNuCV8ixEu1eWcPNCS/l1OW1EnXUEoEHKl54Xrrz2uNw" PEM+
   s" kgMTP1FZ1wKBgAcCn1dwJKufzBWQxWQp1vsM5fggqPN1qGb5y0MAk3g7/Ls5bkUp" PEM+
   s" 5u9SN0Ai3Ya6hNnvWJMb7sXQX6U/zyO2M/hTip7tUeh7CXgwo59dkpN75gJLVds2" PEM+
   s" 9+DAncu3KXU4f2Fa+7bLNrur53k8KwPOq2bbuTG69QtV7Jr5MR0AHWKNAoGBAIf/" PEM+
   s" evpitUf+4JYbLpvNXWcY052Mpz22aR84mY3nh3F2LF2mjMJDpTg7FmuyPcVw6EcG" PEM+
   s" yoAe9fa65iNqCq+jdw6PVNGo2hxfjSiZ8IIzHdsPXI89//pMjZcQK9r+CCoRjaZj" PEM+
   s" OYiIDktVWZzmevJuv6WywUqd57LE3Zar3dLSIkx1AoGAIYd7DHOhrWvxkwPQsRM2" PEM+
   s" tOgrjbcrfvtQJipd+DlcxyVuuM9sQLdgjVk2oy26F0EmpScGLq2MowX7fhd/QJQ3" PEM+
   s" ydy5cY7YIBi87w93IKLEdfnbJtoOPLUW0ITrJReOgo1cq9SbsxYawBgfp/gh6A56" PEM+
   s" 03k2+ZQwVK0JKSHuLFkuQ3U=" PEM+
   s" -----END PRIVATE KEY-----" PEM+
   ;

FILL

public

: MODULUS$ ( -- ptr u8 n ) MODULUS-ROW MODULUS-U @ ;
: EXPONENT$ ( -- ptr u8 n ) s" AQAB" ;
: PEM$ ( -- ptr u8 n ) PEM-ROW PEM-U @ ;

;package
