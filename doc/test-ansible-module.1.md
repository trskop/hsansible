% TEST-ANSIBLE-MODULE(1) Hsansile 0.3.0.0
% Peter Trško
% March 13, 2013

# NAME

test-ansible-module - Simple tool for testing Hsansible based Ansible modules.


# SYNOPSIS

test-ansible-module \[-a MODULE\_ARGUMENTS|-A FILE] \[-c COMPLEX\_ARGUMENTS|-C
FILE] MODULE\_FILE

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

-a *MODULE\_ARGUMENTS*, \--arguments=*MODULE\_ARGUMENTS*, \--args=*MODULE\_ARGUMENTS*
:   Pass *MODULE\_ARGUMENTS* to the module via temporary file. Don't mix with
    *-A* option.

-A *FILE*, \--arguments-file=*FILE*, \--args-file=*FILE*
:   Copy *FILE* to a temporary file that will be passed to the module as
    arguments file. Don't mix with -a option.

-c *COMPLEX_ARGUMENTS*, \--complex-arguments=*COMPLEX_ARGUMENTS*
:   Pass *COMPLEX\_ARGUMENTS* to the module via temporary file. Don't mix with
    *-C* option.

-C *FILE*, \--complex-arguments-file=*FILE*
:   Copy *FILE* to a temporary file that will be passed to the module as
    complex arguments file. Don't mix with *-c* option.


# SEE ALSO

`ansible`(1), *http://ansible.cc*

`make-ansible-module`(1)
