# Monadic Parser

This repository holds the implementation of the parsers as decsribed in the paper [Monadic Parser Combinators by Graham Hutton and Erik Meijer](http://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf).
A lot of things in the paper are implemented with monad comprejensions. At the moment of writting I didn't know how to do that in Haskell and so I wrote it using `bind` explicitly. Also, the parsers in the paper are in [Gofer](https://en.wikipedia.org/wiki/Gofer_(programming_language)) which is quite close to Haskell.

In order to build the project you will need to install [Stack](http://docs.haskellstack.org/en/stable/README.html).
