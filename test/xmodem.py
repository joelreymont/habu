#!/usr/bin/env python3
"""Exercise the checked native XMODEM codec with independent wire packets."""

import binascii
from pathlib import Path
import subprocess


ROOT = Path(__file__).resolve().parents[1]
SETUP = '''
require lib/fs.f
using XMODEM
using BUF
create INPUT HDR-BYTES allot
create OUTPUT HDR-BYTES allot
CAST: BLEN>N ( CAD-NUM:byte-len -- n )

: INPUT-READ ( ptr u8 n n -- ) {: path size:n capacity:n :}
   INPUT capacity 1 max BYTES INIT
   INPUT SPAN$ drop {: data :}
   path size data capacity READ-ALL data swap BYTES INPUT REPLACE ;
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

    def run(self, source, *, refused=False):
        path = self.out / 'case.f'
        path.write_text(source)
        result = subprocess.run([str(ROOT / 'bin/hb'), '--load', 'lib/xmodem.f', str(path)],
                                cwd=ROOT, input='', text=True, capture_output=True, timeout=20)
        assert result.returncode == (70 if refused else 0), (result.stdout, result.stderr)
        if refused:
            assert 'non-certified' in result.stderr, result.stderr
        self.count += 1
        return result.stdout.split()

    def input(self, data):
        path = self.out / 'input.bin'
        path.write_bytes(data)
        return SETUP + f'\ns" {path}" {len(data)} INPUT-READ\n'

    def encode(self, data, sequence, size, kind, *, error=0):
        output = self.out / 'packet.bin'
        source = self.input(data) + f'''
OUTPUT 8 BYTES INIT
s" preserved" BYTES OUTPUT REPLACE
: EXERCISE ( -- )
   [: INPUT SPAN$ {sequence} >SEQUENCE {size} >PAYLOAD-SIZE {kind} >CHECK-KIND OUTPUT ENCODE ;] catch .
   s" {output}" OUTPUT SPAN$ BLEN>N WRITE-ALL INPUT DISPOSE OUTPUT DISPOSE ;
EXERCISE
'''
        assert self.run(source) == [str(error)]
        expected = b'preserved' if error else packet(data, sequence, size, kind == 1)
        assert output.read_bytes() == expected, (sequence, size, kind)

    def decode(self, data, kind, *, error=0):
        output = self.out / 'payload.bin'
        source = self.input(data) + f'''
: SAVE ( ptr u8 CAD-NUM:byte-len sequence -- )
   SEQUENCE>N . {{: payload size:CAD-NUM:byte-len :}}
   s" {output}" payload size BLEN>N WRITE-ALL ;


: EXERCISE ( -- )
   [: INPUT SPAN$ {kind} >CHECK-KIND DECODE SAVE ;] catch . INPUT DISPOSE ;
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
        for effect in ['XMODEM:sequence -- XMODEM:payload-size',
                       'XMODEM:payload-size -- XMODEM:check-kind',
                       'n -- XMODEM:sequence']:
            self.run(f': BAD ( {effect} ) ;\n', refused=True)
        source = '''
using XMODEM
: CRC-KIND ( -- check-kind ) CRC ;
: SUM-KIND ( -- check-kind ) SUM ;
: SIZE ( -- CAD-NUM:byte-len ) 1024 BLOCK CRC-KIND PACKET-BYTES ;
CAST: BLEN>N ( CAD-NUM:byte-len -- n )
SIZE BLEN>N . 128 BLOCK SUM-KIND PACKET-BYTES BLEN>N .
'''
        assert self.run(source) == ['1029', '132']


if __name__ == '__main__':
    checks = Checks()
    checks.cases()
    print(f'xmodem: {checks.count} checks passed')
