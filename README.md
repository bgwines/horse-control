# horse-control
Version control, written in Haskell with all that monadic goodness! (Currently totally in progress)

[![Build Status](https://travis-ci.org/bgwines/horse-control.svg?branch=master)](https://travis-ci.org/bgwines/horse-control)

Dependencies
------------

* LevelDB

Differences with Git
--------------------

* `horse rebase` acts like `git rebase -p`
* `horse pull` acts like `git pull --rebase=preserve-merges`
* does not distinguish between tracked and untracked files
* `horse stage` replaces `git add` and `git rm`

Thanksto
--------

* https://robots.thoughtbot.com/applicative-options-parsing-in-haskell