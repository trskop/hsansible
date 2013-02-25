#!/usr/bin/env python

# This file was generated using $program$ $version$.

$if(documentation)$
DOCUMENTATION = \'\'\'
$documentation$
\'\'\'
$endif$

import base64
import os
import stat
import subprocess
import sys
import tempfile

def main():
    fd, fn = tempfile.mkstemp()
    os.fchmod(fd, stat.S_IEXEC)
    os.write(fd, base64.b64decode(encodedBinary))
    os.fsync(fd)
    os.close(fd)
    subprocess.call([fn] + sys.argv[1:])
    os.remove(fn)

encodedBinary = \'\'\'
$encodedBinary$
\'\'\'

if __name__ == '__main__':
    main()

