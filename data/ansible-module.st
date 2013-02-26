#!/usr/bin/env python

# This file was generated using $program$ $version$ from a template
# with following copyringht notice.
#
# Copyright (c) 2013, Peter Trsko <peter.trsko@gmail.com>
# 
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# 
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
# 
#     * Redistributions in binary form must reproduce the above
#       copyright notice, this list of conditions and the following
#       disclaimer in the documentation and/or other materials provided
#       with the distribution.
# 
#     * Neither the name of Peter Trsko nor the names of other
#       contributors may be used to endorse or promote products derived
#       from this software without specific prior written permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

$if(documentation)$
DOCUMENTATION = \'\'\'
$documentation$
\'\'\'
$endif$

import atexit
import base64
import os
import stat
import subprocess
import sys
import tempfile
try:
    import json
except ImportError:
    import simplejson as json


def remove_temporary_file(fn):
    if fn is not None:
        os.remove(fn)

def fail_json(msg):
    print json.dumps(dict(failed=True, msg=msg))
    sys.exit(1)
    
def main():
    try:
        fd, fn = tempfile.mkstemp()
        atexit.register(remove_temporary_file, fn)
    except Exception as e:
        fail_json("Error creating temporary file: %s" % str(e))

    try:
        os.fchmod(fd, stat.S_IEXEC)
        os.write(fd, base64.b64decode(encodedBinary))
        os.fsync(fd)
        os.close(fd)
    except Exception as e:
        fail_json("Error recreating executable: %s" % str(e))

    try:
        subprocess.call([fn] + sys.argv[1:])
    except Exception as e:
        fail_json("Error while calling executable: %s" % str(e))

encodedBinary = \'\'\'
$encodedBinary$
\'\'\'

if __name__ == '__main__':
    main()

