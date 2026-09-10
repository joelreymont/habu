#!/usr/bin/env python3
"""Exercise UDP/serial cleanup through saved native images and fresh processes."""

import argparse
from concurrent.futures import ThreadPoolExecutor
import os
from pathlib import Path
import pty
import select
import socket
import subprocess
import tempfile
from threading import Event
import time

ROOT = Path(__file__).resolve().parents[1]
SUBJECT = r'''
require lib/net/udp4.f
require lib/serial.f

package HOST-IO-IMAGE-TEST
private

CAST: BLEN>N ( CAD-NUM:byte-len -- n )
here FFI:>CELL 7 and 8 swap - 7 and allot
variable ENTERED
variable DONE
variable PATH-SIZE
variable PEER-PORT
create PATH 64 allot
create UDP-BYTE 1 allot
create SERIAL-BYTE 1 allot
TASK:MIN-STACK TASK:TASK UDP-WORKER
TASK:MIN-STACK TASK:TASK SERIAL-WORKER

: GATE ( -- )
   1 ENTERED atomic-add drop
   begin ENTERED atomic@ 2 < while TASK:PAUSE repeat ;


: UDP-OK ( UDP4:status -- )
   MATCH UDP4:status
      ok OF ENDOF failed OF UDP4:ERRNO>N throw ENDOF
   ;MATCH ;


: UDP-RUN ( -- )
   GATE
   $7F000001 UDP4:ADDRESS 0 UDP4:PORT UDP4:BIND MATCH UDP4:open-result
      opened OF ENDOF failed OF UDP4:ERRNO>N throw ENDOF
   ;MATCH {: handle:UDP4:socket :}
   $19 UDP-BYTE c!
   handle $7F000001 UDP4:ADDRESS PEER-PORT @ UDP4:PORT
   UDP-BYTE 1 UDP4:PAYLOAD-BYTES UDP4:SEND UDP-OK
   handle UDP-BYTE 1 UDP4:PAYLOAD-BYTES 2000 >MS UDP4:RECEIVE
   MATCH UDP4:receive-result
      packet OF
         {: size:CAD-NUM:byte-len address:UDP4:address port:UDP4:port :}
         size BLEN>N 1 <> if -1 throw then
         address UDP4:ADDRESS>N $7F000001 <> if -1 throw then
         port UDP4:PORT>N PEER-PORT @ <> if -1 throw then
         UDP-BYTE c@ $E6 <> if -1 throw then
      ENDOF
      truncated OF drop drop drop -1 throw ENDOF
      timeout OF -1 throw ENDOF
      failed OF UDP4:ERRNO>N throw ENDOF
   ;MATCH
   handle UDP4:CLOSE UDP-OK
   1 DONE atomic-add drop ;


: SERIAL-ONE ( SERIAL:io-result -- )
   MATCH SERIAL:io-result
      transferred OF BLEN>N 1 <> if -1 throw then ENDOF
      timeout OF -1 throw ENDOF closed OF -1 throw ENDOF
      failed OF SERIAL:ERRNO>N throw ENDOF
   ;MATCH ;


: SERIAL-RUN ( -- )
   GATE
   PATH PATH-SIZE @ 115200 SERIAL:BAUD SERIAL:OPEN8N1 MATCH SERIAL:open-result
      opened OF ENDOF failed OF SERIAL:ERRNO>N throw ENDOF
      unsupported OF -1 throw ENDOF
   ;MATCH {: handle:SERIAL:handle :}
   $A5 SERIAL-BYTE c!
   handle SERIAL-BYTE 1 SERIAL:BYTES 2000 >MS SERIAL:WRITE SERIAL-ONE
   handle SERIAL-BYTE 1 SERIAL:BYTES 2000 >MS SERIAL:READ SERIAL-ONE
   SERIAL-BYTE c@ $5A <> if -1 throw then
   handle SERIAL:CLOSE MATCH SERIAL:status
      ok OF ENDOF failed OF SERIAL:ERRNO>N throw ENDOF
   ;MATCH
   1 DONE atomic-add drop ;


: WAIT-DONE ( ptr a -- ) {: worker:ptr :}
   begin worker TASK:DONE? 0= while TASK:PAUSE repeat ;

public


: RUN ( ptr u8 n UDP4:port -- ) {: path size:n port:UDP4:port :}
   size 1 < size 64 > or if -1 throw then
   path PATH size BYTE-COPY size PATH-SIZE ! port UDP4:PORT>N PEER-PORT !
   0 ENTERED atomic! 0 DONE atomic!
   ['] UDP-RUN UDP-WORKER TASK:ACTIVATE
   ['] SERIAL-RUN SERIAL-WORKER TASK:ACTIVATE
   UDP-WORKER WAIT-DONE SERIAL-WORKER WAIT-DONE
   UDP-WORKER TASK:KILL SERIAL-WORKER TASK:KILL
   DONE atomic@ 2 <> if -1 throw then
   s" io=ok" type cr ;

;package
'''


def literal(path):
    value = str(path)
    assert '"' not in value and '\n' not in value, value
    return f's" {value}"'


def serial_peer(master, rounds, stopped):
    deadline = time.monotonic() + 45
    for _ in range(rounds):
        while not select.select([master], [], [], 0.1)[0]:
            if stopped.is_set():
                return False
            assert time.monotonic() < deadline, 'serial peer timed out'
        assert os.read(master, 1) == b'\xa5', 'wrong serial request'
        assert os.write(master, b'\x5a') == 1
    return True


def udp_peer(peer, rounds, stopped):
    deadline = time.monotonic() + 45
    for _ in range(rounds):
        while True:
            try:
                data, endpoint = peer.recvfrom(2)
                break
            except TimeoutError:
                if stopped.is_set():
                    return False
                assert time.monotonic() < deadline, 'UDP peer timed out'
        assert data == b'\x19', ('wrong UDP request', data)
        assert peer.sendto(b'\xe6', endpoint) == 1
    return True


def exercise(executable, cwd, subject=None, destination=None, *, reset=True):
    """Use both transports, optionally reset/reuse them, then capture if requested."""
    master, slave = pty.openpty()
    try:
        with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as peer:
            peer.bind(('127.0.0.1', 0))
            peer.settimeout(0.1)
            command = [str(executable)]
            run = f'{literal(os.ttyname(slave))} {peer.getsockname()[1]} UDP4:PORT HOST-IO-IMAGE-TEST:RUN\n'
            script = (f'{literal(subject)} required\n' if subject is not None else '') + run
            rounds = 1
            if reset:
                # An empty second PREPARE must be harmless. RUN then registers
                # both owners again, before capture's own PREPARE.
                script += 'IMAGE-LIFECYCLE:PREPARE\nIMAGE-LIFECYCLE:PREPARE\n' + run
                rounds = 2
            if destination is not None:
                # SAVE must run from the outer input stream, after loads return.
                script += f'{literal(destination)} APP-IMAGE:SAVE\n'
            with ThreadPoolExecutor(max_workers=2) as pool:
                stopped = Event()
                serial = pool.submit(serial_peer, master, rounds, stopped)
                udp = pool.submit(udp_peer, peer, rounds, stopped)
                started = time.monotonic()
                with subprocess.Popen(command, cwd=cwd, stdin=subprocess.PIPE,
                                      stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                      text=True) as process:
                    try:
                        stdout, stderr = process.communicate(script, timeout=60)
                    except subprocess.TimeoutExpired:
                        process.kill()
                        stdout, stderr = process.communicate()
                        stopped.set()
                        raise AssertionError(('host-io-image timeout', command, cwd,
                                              time.monotonic() - started, stdout, stderr))
                    if process.returncode != 0 or stderr:
                        stopped.set()
                        raise AssertionError(('host-io-image failed', command, cwd,
                                              process.returncode, stdout, stderr))
                    stopped.set()
                    assert stdout.split() == ['io=ok'] * rounds, stdout
                assert serial.result(), 'serial exchanges incomplete'
                assert udp.result(), 'UDP exchanges incomplete'
            if destination is not None:
                assert destination.is_file() and os.access(destination, os.X_OK), destination
    finally:
        os.close(master)
        os.close(slave)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--habu', type=Path, default=ROOT / 'bin/hb')
    args = parser.parse_args()
    output = ROOT / 'tmp/test-host-io-image'
    output.mkdir(parents=True, exist_ok=True)
    run = Path(tempfile.mkdtemp(prefix='run-', dir=output))
    foreign = run / 'outside-source'
    foreign.mkdir(exist_ok=True)
    subject = run / 'subject.f'
    subject.write_text('require src/habu/app-image.f\n' + SUBJECT)
    first = run / 'first'
    second = run / 'second'
    exercise(args.habu.resolve(), ROOT, subject, first)
    exercise(first, foreign, destination=second)
    exercise(second, foreign)
    print('host-io-image: 3 native generations passed with concurrent UDP/serial I/O')


if __name__ == '__main__':
    main()
