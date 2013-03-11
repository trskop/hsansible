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
sandbox in a Ansible specific directory, in example:

    cabal-dev install --sandbox=$HOME/.ansible/sandbox

Feel free to change `$HOME/.ansible/sandbox` to a directory of your choice.
Since `cabal-dev` has it's own dependencies it might be useful to use only
`cabal-install` and GHC tools to do the same thing:

    INSTALL_DIR="$HOME/.ansible/sandbox"
    PACKAGES_CONF="$INSTALL_DIR/packages-`ghc --numeric-version`.conf"
    mkdir "$INSTALL_DIR"
    ghc-pkg init "$PACKAGES_CONF"
    cabal install --prefix="$INSTALL_DIR" --package-db="$PACKAGES_CONF"

You might also want to consider using option like `--disable-documentation`.
Newer version of `cabal-install` should be able to work with sandboxes
directly, but that functionality is currently in HEAD and I'm not familiar with
it.

When `hsansible` is installed in sandbox, then interpreted Ansible modules
written in Haskell have to know where to find it. To do so you need to modify
`#!` line to look like this:

    #!/usr/bin/env runhaskell -package-conf=<path-to-packages-conf-goes-here>


Installation: Ansible Playbook
------------------------------

As expected, it's possible to use Ansible to install Hsansible. Following is an
example of Ansible playbook that does that.

    ---
    - hosts: debian ubuntu
      # ^ Setup does determine OS of targeted machine, but I sometimes found it
      # simpler to just have special groups for Debian and Ubuntu systems.
      # Operating system doesn't change that often and therefore it's possible to
      # get away with this and it really does simplify playbooks.
    
      sudo: no
      # ^ It's not necessary to install Hsansible via sudo since it may be
      # installed in user's home directory. Only the haskell-platform installation
      # requires sudo, see included tasks file.
    
      vars:
        - hsansible_dir: /home/ansible/hsansible
        - haskell_sandbox: /home/ansible/haskell-sandbox
    
      tasks:
        # Tag "update-hsansible" is here to provide easy way to just update Hsansible.
    
        - name: Install Haskell Platform
          apt: pkg=haskell-platform state=installed update_cache=yes
          tags: [install-hsansible]
          # This is the only step that will always require sudo, others can work
          # without it, if the "hsansible_dir" and "haskell_sandbox" can be created by
          # the user Ansible uses for SSH login.
          sudo: yes
    
        - name: Clone/pull Hsansible repository
          # Depending on your firewall/proxy configuration you might want to swith to
          # "git:" protocol or create a tunel before doing this. Another possibility is
          # to have internal "Git Cache", a host that pulls Git repositories from the
          # internet and provides access to them from internal network. In such case
          # you need to provide correct repository URL.
          git: repo=https://github.com/trskop/hsansible.git dest=$hsansible_dir
          tags: [install-hsansible, update-hsansible]
    
        # This step is required if it wasn't ever executed before on target host under
        # the user you'll going to install Hsansible, particulary right after
        # first-time haskell-platform installation.
        - name: Update Cabal package cache
          command: /usr/bin/cabal update
          tags: [install-hsansible]
    
        # Relies on script provided by Hsansible in it's Git repository.
        - name: Install Hsansible in sandbox
          command: $hsansible_dir/tools/sandbox-install.sh --working-dir="$hsansible_dir" "$haskell_sandbox"
          tags: [install-hsansible, update-hsansible]

You can find it in `examples/playbooks/install-hsansible-on-debian-and-ubuntu.yaml`.
