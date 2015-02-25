# horse-control
A version control system! (Currently totally in progress)

[![Build Status](https://travis-ci.org/bgwines/horse-control.svg?branch=master)](https://travis-ci.org/bgwines/horse-control)

Interface
---------

* `horse mod`
* `horse checkout`
* `horse commit`
* `horse diff`
* `horse init`
* `horse config`
* `horse log`
* `horse status`
* `horse show`

Dependencies
------------

* LevelDB

Differences with Git
--------------------

* `horse rebase` acts like `git rebase -p`
* `horse pull` acts like `git pull --rebase=preserve-merges`