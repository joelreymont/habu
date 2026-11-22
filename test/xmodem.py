#!/usr/bin/env python3
"""Exercise the checked native XMODEM codec with independent wire packets."""

import binascii
from pathlib import Path
import subprocess


ROOT = Path(__file__).resolve().parents[1]
SETUP = '''
require src/lib/fs.f
require src/lib/heap.f
require src/lib/span.f
require src/lib/vector.f
using XMODEM
TYPED-VARIABLE INPUT ^VEC:vector<u8>
TYPED-VARIABLE OUTPUT ^VEC:vector<u8>
: FRESH ( -- ^VEC:vector<u8> ) HEAP:ALLOCATOR 0 [type] u8 VEC:NEW ;

: INPUT-READ ( str n -- ) {: path:str capacity:n :}
   HEAP:ALLOCATOR capacity [type] u8 VEC:NEW INPUT !
   INPUT @ VEC:SPAN SPAN:$ str> drop {: data :}
   path data capacity FS:READ-ALL INPUT @ swap VEC:RESIZE ;
'''


def packet(data, sequence, size, crc):
    payload = data + b'\x1a' * (size - len(data))
    trailer = (binascii.crc_hqx(payload, 0).to_bytes(2, 'big') if crc
               else bytes([sum(payload) & 255]))
    return bytes([1 if size == 128 else 2, sequence, sequence ^ 255]) + payload + trailer


class Checks:
    def __init__(self):
        self.out = ROOT / 'tmp/test-xmodem'
        self.out.mkdir(parents=True, exist_ok=True)
        self.count = 0

    def run(self, source, *, rejected=None):
        path = self.out / 'case.f'
        path.write_text(source)
        result = subprocess.run([str(ROOT / 'bin/hb'), '--load', 'src/lib/xmodem.f', str(path)],
                                cwd=ROOT, input='', text=True, capture_output=True, timeout=20)
        assert result.returncode == (70 if rejected else 0), (result.stdout, result.stderr)
        if rejected:
            lines = [line.strip() for line in result.stderr.splitlines()]
            assert lines[0] == rejected[0] and '= help: ' + rejected[1] in lines, result.stderr
        self.count += 1
        return result.stdout.split()

    def input(self, data):
        path = self.out / 'input.bin'
        path.write_bytes(data)
        return SETUP + f'\ns" {path}" >str {len(data)} INPUT-READ\n'

    def encode(self, data, sequence, size, kind, *, error=0):
        output = self.out / 'packet.bin'
        source = self.input(data) + f'''
FRESH OUTPUT !
s" preserved" OUTPUT @ VEC:APPEND
: EXERCISE ( -- )
   [: INPUT @ VEC:SPAN SPAN:$ {sequence} >SEQUENCE {size} >PAYLOAD-SIZE {kind} >CHECK-KIND OUTPUT @ ENCODE ;] catch .
   s" {output}" >str OUTPUT @ VEC:SPAN SPAN:$ FS:WRITE-ALL INPUT @ VEC:DELETE OUTPUT @ VEC:DELETE ;
EXERCISE
'''
        assert self.run(source) == [str(error)]
        expected = b'preserved' if error else packet(data, sequence, size, kind == 1)
        assert output.read_bytes() == expected, (sequence, size, kind)

    def decode(self, data, kind, *, error=0):
        output = self.out / 'payload.bin'
        source = self.input(data) + f'''
: SAVE ( str sequence -- )
   SEQUENCE>N . {{: payload:str :}}
   s" {output}" >str payload FS:WRITE-ALL ;


: EXERCISE ( -- )
   [: INPUT @ VEC:SPAN SPAN:$ {kind} >CHECK-KIND DECODE SAVE ;] catch . INPUT @ VEC:DELETE ;
EXERCISE
'''
        expected = [str(error)] if error else [str(data[1]), '0']
        assert self.run(source) == expected, (len(data), kind)
        if not error:
            assert output.read_bytes() == data[3:-1-kind]

    def cases(self):
        for size in (128, 1024):
            for kind in (0, 1):
                for data, sequence in [(b'', 1), (b'123456789', 255),
                                       (bytes(i & 255 for i in range(size)), 0)]:
                    self.encode(data, sequence, size, kind)
                    self.decode(packet(data, sequence, size, kind == 1), kind)
        for sequence, size, kind, data in [
            (-1, 128, 1, b'a'), (256, 128, 1, b'a'), (1, 127, 1, b'a'),
            (1, 129, 1, b'a'), (1, 0, 1, b'a'), (1, 128, -1, b'a'),
            (1, 128, 2, b'a'), (1, 128, 1, bytes(129)), (1, 1024, 1, bytes(1025)),
        ]:
            self.encode(data, sequence, size, kind, error=-9120)
        valid = packet(bytes(range(128)), 1, 128, True)
        for length in (0, 1, 2, 3, 4, 127, 128, 131, 132):
            self.decode(valid[:length], 1, error=-9121)
        for index, value in [(0, 0), (0, 2), (1, 2), (2, 0), (3, 200), (131, 0), (132, 0)]:
            changed = bytearray(valid)
            assert changed[index] != value
            changed[index] = value
            self.decode(bytes(changed), 1, error=-9121)
        self.decode(valid + b'\0', 1, error=-9121)
        self.decode(valid, 0, error=-9121)
        self.decode(valid, 2, error=-9120)
        changed = bytearray(packet(b'binary\x1a\0\xff', 7, 1024, False))
        changed[-1] ^= 1
        self.decode(bytes(changed), 0, error=-9121)
        for effect, first, help in [
            ('XMODEM:sequence -- XMODEM:payload-size',
             'error[E-MISMATCH]: BAD ends with xmodem:sequence on the stack; its signature promises xmodem:payload-size',
             'leave xmodem:payload-size in the body, or declare ( xmodem:sequence -- xmodem:sequence )'),
            ('XMODEM:payload-size -- XMODEM:check-kind',
             'error[E-MISMATCH]: BAD ends with xmodem:payload-size on the stack; its signature promises xmodem:check-kind',
             'leave xmodem:check-kind in the body, or declare ( xmodem:payload-size -- xmodem:payload-size )'),
            ('n -- XMODEM:sequence',
             'error[E-MISMATCH]: BAD ends with n on the stack; its signature promises xmodem:sequence',
             'leave xmodem:sequence in the body, or declare ( n -- n )')]:
            self.run(f': BAD ( {effect} ) ;\n', rejected=(first, help))
        source = '''
using XMODEM
: CRC-KIND ( -- check-kind ) CRC ;
: SUM-KIND ( -- check-kind ) SUM ;
: SIZE ( -- NUM:byte-len ) 1024 BLOCK CRC-KIND PACKET-BYTES ;
CAST: BLEN>N ( NUM:byte-len -- n )
SIZE BLEN>N . 128 BLOCK SUM-KIND PACKET-BYTES BLEN>N .
'''
        assert self.run(source) == ['1029', '132']


if __name__ == '__main__':
    checks = Checks()
    checks.cases()
    print(f'xmodem: {checks.count} checks passed')
