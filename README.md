# horse-control
Version control, written in Haskell with all that monadic goodness! (Currently totally in progress and not finished in the least)

[![Build Status](https://travis-ci.org/bgwines/horse-control.svg?branch=master)](https://travis-ci.org/bgwines/horse-control)

<img src="logo.png" alt="" width="100px"/>

Differences with Git
--------------------

<!-- * `horse rebase` acts like `git rebase -p` -->
<!-- * `horse pull` acts like `git pull --rebase=preserve-merges` -->
* does not distinguish between tracked and untracked files
* `horse stage` replaces `git add` and `git rm`
* the staging area does not store changes; rather, it merely tracks filenames to commit
* `horse squash` replaces `git rebase -i`
* addition of `horse unstage`
* `horse init` doesn't overwrite existing repositories like `git init` does
* can't initialize a repo in a subdirectory of another repo
* `checkout` completely overwrites working directory (product of not distinguishing between tracked and untracked files)

Installation
------------

### horse-control: ###

    > cabal install horse-control

### non-cabal dependencies (database) ###

#### Linux ####

    > sudo apt-get update -qq
    > sudo apt-get install libsnappy-dev
    > wget https://leveldb.googlecode.com/files/leveldb-1.9.0.tar.gz
    > tar -xzf leveldb-1.9.0.tar.gz
    > cd leveldb-1.9.0
    > make
    > sudo mv libleveldb.* /usr/local/lib
    > cd include
    > sudo cp -R leveldb /usr/local/include
    > sudo ldconfig
    > cd ../..

#### OSX ####

    > brew install leveldb

Testing
-------

Testing suite: Tasty-HUnit

Number of unit tests: 88 (and rising!)

Code coverage: ![](coverage.png)

Thanksto
--------

* https://robots.thoughtbot.com/applicative-options-parsing-in-haskell
