#!/bin/bash

# Copyright (c) 2013, 2015, Peter Trško <peter.trsko@gmail.com>
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

printHelp() {
    cat << EOF
Usage: ${0##*/} SOURCE_DIR [TARGET_DIR]

SOURCE_DIR
  Directory where Markdown sources, that ends with \`.[1-8].md', are stored.

TARGET_DIR
  This is where the output will be stored, but it has to be noted that
  directory structure in form man/man[1-8] will be created to store the
  generated manual pages. Directory "\$TARGET_DIR/man" can be used as a value
  in MANPATH environment variable. If not specified then current directory is
  used.
EOF
}

# Default values:
SOURCE_DIR=''
TARGET_DIR='.'

if [ $# -eq 0 -o $# -gt 2 ]; then
    echo "ERROR: Wrong number of arguments." 1>&2
    echo 1>&2
    printHelp 1>&2
    exit 1
elif [ $# -gt 0 ]; then
    if [ "$1" = "-h" -o "$1" = "--help" ]; then
        printHelp
        exit 0
    fi

    SOURCE_DIR="$1"
    if [ $# -eq 2 ]; then
        TARGET_DIR="$2"
    fi
fi

if ! hash pandoc 2> /dev/null; then
    cat 1>&2 << EOF
ERROR: Command \`pandoc' wasn't found.

On Debian/Ubuntu you can install package named \`pandoc', but generally it is
possible to install it via \`cabal-install':

    cabal install pandoc

Then you have to ensure that bin directory where Cabal puts executables is in
your PATH, or use \`--symlink-bindir=\$HOME/bin' or different directory that is
in your PATH.
EOF
    exit 1
fi

for SRC in `ls "$SOURCE_DIR"/*.[0-9].md`; do
    # .../<some-file-base-name>.<section-num>.md -> <section-num>
    SECTION="${SRC##*/}"
    SECTION="${SECTION#*.}"
    SECTION="${SECTION%.md}"

    OUT_DIR="${TARGET_DIR}/man/man$SECTION"
    if [ ! -e "$OUT_DIR" ]; then
        mkdir -p "$OUT_DIR"
    fi
    OUT="${SRC##*/}"
    OUT="$OUT_DIR/${OUT%.md}"

    printf "Generating \`$OUT'... "
    pandoc -s -f markdown -t man "$SRC" -o "$OUT"
    gzip -9 --no-name --force "$OUT"
    echo 'done.'
done
