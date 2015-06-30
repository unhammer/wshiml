Wshiml
======

Implementation of
http://nlp.stanford.edu/IR-book/html/htmledition/near-duplicates-and-shingling-1.html

Build requires [oasis](http://oasis.forge.ocamlcore.org/index.html). Do:

    ./configure    # optionally with --prefix
    make
    make install

To build the example command-line program, do

    ./configure --enable-cli
    make
    make install
    find-similar-docs --help

The command-line program requires
[cmdliner](http://erratique.ch/software/cmdliner). The rest of the
software has no dependencies apart from Oasis for building from git.

So far the code is fairly unoptimised apart from what's described
in
http://nlp.stanford.edu/IR-book/html/htmledition/near-duplicates-and-shingling-1.html
and uses 8s (6s with super-shingling) to cluster 850 documents of
altogether 462,572 words on an old 2.8 GHz AMD.

API documentation
=================

is [here](http://unhammer.github.io/wshiml/).
