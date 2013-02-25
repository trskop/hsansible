The Hsansible Package
=====================

[Ansible](http://ansible.cc) is a software for deploying, managing, and
orchestrating computer systems over SSH. It has a modular design that allows
users to create modules for various parts of the system.

This package provides simple framework for rapid development of
[Ansible](http://ansible.cc) modules in [Haskell](http://haskell.org)
programming language. Haskell is flexible when it comes to being compiled or
interpreted and so it's possible to use it either way, but each has it's own
trade-offs.


Installation
============

Currently `hsansible` is not available on
[Hackage](http://hackage.haskell.org/), hopefully soon it will be, and so the
only way how to get it is from GitHub:

    git clone git://github.com/trskop/hsansible.git


Installation: The Standard Cabal Way
------------------------------------

Hsansible is a cabalized package, so using `cabal-install` in a cloned
repository will work just fine:

    cabal install

Standard `Setup.hs` way is also available:

    runhaskell Setup.hs configure
    runhaskell Setup.hs build
    runhaskell Setup.hs install

See [Cabal User Guide: Building and installing
packages](http://www.haskell.org/cabal/users-guide/installing-packages.html)
for details.


Installation: Sandboxing
------------------------

It might be useful to create separate sandbox for packages that will be used by
Ansbile modules. This can be done using `cabal-dev`, which its self can be
installed using `cabal-install`. In such case it's a good idea to place the
sandbox in a Ansible specific directory:

    cabal-dev install --sandbox=$HOME/.ansible/sandbox

Feel free to change `$HOME/.ansible/sandbox` to a directory of your choice.
Since `cabal-dev` has it's own dependencies it might be useful to use only
`cabal-install` and GHC tools to do the same thing:

    INSTALL_DIR="$HOME/.ansible/sandbox"
    PACKAGE_CONF="$INSTALL_DIR/packages-`ghc --numeric-version`.conf"
    mkdir "$INSTALL_DIR"
    ghc-pkg init "$PACKAGE_CONF"
    cabal install --prefix="$INSTALL_DIR" --package-db="$PACKAGE_CONF"

You might also want to consider using option like `--disable-documentation`.
Newer version of `cabal-install` should be able to work with sandboxes
directly, but that functionality is currently in HEAD and I'm not familiar with
it.

When `hsansible` is installed in sandbox, then interpreted Ansible modules
written in Haskell have to know where to find it. To do so you need to modify
`#!` line to look like this:

    #!/usr/bin/env runhaskell -package-conf=<path-to-packages-conf-goes-here>
