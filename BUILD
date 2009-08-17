1.0  ->  Requirements
2.0  ->  Getting the source code
3.0  ->  Compiling, Building & Installation


(1.0) Requirements
================================================================================
POSIX compatible system.
ghc >= 6.10         - http://haskell.org/ghc/
Cabal		    - http://haskell.org/cabal/
PostgreSQL headers  - http://www.postgresql.org/


(2.0) Getting the source code
================================================================================
$ git clone git://git.mercenariesguild.net/rivertam.git


(3.0) Compiling, Building & Installation
================================================================================
You have 2 options, either the easy way with cabal (3.0a), or the hard way (3.0b).


(3.0a) The easy way
================================================================================
 $ cd rivertam
 $ cabal install
Run the latter as root for a global installation.


(3.0b) The hard way
================================================================================
First make sure you have all dependencies listen in rivertam.cabal

Configure it for global installation:
 $ runhaskell Setup configure
Or for a local installation:
 $ runhaskell Setup configure --prefix=$HOME --user

Build:
 $ runhaskell Setup build

Install:
 $ runhaskell Setup install

NOTE: For a global installation the last command must be run with root permissions.

To get it running consult README 2.5.
(You can find the binary file in the PREFIX/bin directory or often in the /usr/local/bin directory.)
