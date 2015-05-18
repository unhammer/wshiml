Wshiml
========

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

So far the code is completely unoptimised apart from what's described
in
http://nlp.stanford.edu/IR-book/html/htmledition/near-duplicates-and-shingling-1.html
and uses 12s (9s with super-shingling) to cluster 1458 documents of
altogether 556176 words on an old 2.8 GHz AMD.
