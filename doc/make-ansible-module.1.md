% MAKE-ANSIBLE-MODULE(1) Hsansile 0.3.0.0
% Peter Trško
% March 13, 2013

# NAME

make-ansible-module - Create Ansible module out of binary executable.


# SYNOPSIS

make-ansible-module \[-o *OUTPUT\_FILE*] \[-t *TEMPLATE\_FILE*]
\[-d *DOC\_FILE*] *BINARY\_FILE*

make-ansible-module {-h|-V|--numeric-version|--print-template}


# DESCRIPTION

Ansible is a software for deploying, managing, and orchestrating computer
systems over SSH. It has a modular design that allows users to create modules
for various parts of the system.

While Ansible allows it's users to develop modules in any scripting language
it's sometimes useful to be able to use compiled languages. This program allows
us to do so, by wrapping binary executable in to Python wrapper. Such wrapper
contains Base 64 encoded binary as part of its body. Wrapped executable has to
follow Ansible module interface, but other than that there aren't any
additional limitations. Python wrapper is defined using StringTemplate
templating language, see section *TEMPLATES* for details.


# OPTIONS

-t *TEMPLATE\_FILE*, \--template=*TEMPLATE\_FILE*
:   Set custom template instead of the default.

-h, \--help
:   Show this help and exit.

-V, \--version
:   Show version string and exit.

\--numeric-version
:   Show machine readable version number and exit.

-o *OUTPUT\_FILE*, \--output=*OUTPUT\_FILE*
:   Set output to *OUTPUT\_FILE* instead of stdout.

-d *DOC\_FILE*, \--doc=*DOC\_FILE*, \--documentation=*DOC\_FILE*
:   Read module documentation from *DOC\_FILE*, it has to be a Ansible-style
    module documentation in YAML.

\--print-template
:   Print default template and exit. This is great as a starting point for your
    own template file.


# TEMPLATES

Choosen template language is StringTemplate in it's Haskell implementation called
HStringTemplate. StringTemplate language description and other documentation is
available on *http://www.stringtemplate.org*. To print default template use command:

    make-ansible-module --print-template

You'll notice usage of following variables:

program
:   Contains name of generator application, in this case it's
    "make-ansible-module".

version
:   Contains version of generator, same as output of:

    make-ansible-module --numeric-version

documentation
:   This variable is defined when *DOC\_FILE* is passed, see *--documentation*
    option.

encodedBinary
:   Contains Base 64 encoded binary executable.


# ENVIRONMENT VARIABLES

hsansible\_datadir
:   When set, *make-ansible-module* uses its value as a base directory where it
    searches for default template. This is a standard Cabal
    (*http://www.haskell.org/cabal/*) definition.


# FILES

Default Ansible module template, i.e. Python wrapper:
*$hsansible_datadir/data/ansible-module.st*


# SEE ALSO

`ansible`(1), *http://ansible.cc*

`test-ansible-module`(1)

*http://www.stringtemplate.org*,
*http://www.haskell.org/haskellwiki/HStringTemplate*
