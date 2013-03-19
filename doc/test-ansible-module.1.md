% TEST-ANSIBLE-MODULE(1) Hsansile 0.3.1.1
% Peter Trško
% March 19, 2013

# NAME

test-ansible-module - Simple tool for testing Hsansible based Ansible modules.


# SYNOPSIS

test-ansible-module \[-r\] \[-p FILE\] \[-a ARGUMENTS|-A FILE]
\[-c JSON|-C FILE] FILE

make-ansible-module {-h|-V|--numeric-version|--print-template}


# DESCRIPTION

Simple tool for testing Hsansible based Ansible modules. Should also work for
any Ansible module that doesn't use Ansible module common code and accept
arguments via temporary file.

It takes module file and copies it in to a temporary file. Creates temporary
files for simple and complex module arguments passed via *-a* or *-A*, and *-c*
or *-C* options. After doing so it executes copy of the module and passes
argument files to it as command line options.


# OPTIONS

-h, \--help
:   Show this help and exit.

-V, \--version
:   Show version string and exit.

\--numeric-version
:   Show machine readable version number and exit.

-a *ARGUMENTS*, \--arguments=*ARGUMENTS*, \--args=*ARGUMENTS*
:   Pass *ARGUMENTS* to the module via temporary file. Don't mix with *-A*
    option.

-A *FILE*, \--arguments-file=*FILE*, \--args-file=*FILE*
:   Copy *FILE* to a temporary file that will be passed to the module as
    arguments file. Don't mix with -a option.

-c *JSON*, \--complex-arguments=*JSON*
:   Pass *JSON* to the module via temporary file. Don't mix with *-C* option.

-C *FILE*, \--complex-arguments-file=*FILE*
:   Copy *FILE* to a temporary file that will be passed to the module as
    complex arguments file. Don't mix with *-c* option.

-i *COMMAND*, \--interpret=*COMMAND*
:   Don't execute module directly, but using *COMMAND* as interpret.

-r, \--runhaskell
:   Don't execute module directly, but using runhaskell.

-p *FILE*, \--package-conf=*FILE*
:   Use alternative GHC package configuration. Implies *--runhaskell*.


# SEE ALSO

`ansible`(1), *http://ansible.cc*

`make-ansible-module`(1)
