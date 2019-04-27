About documentation
===================

This is written in `ReStructuredText`_ because it is how the
documentation would be generated.

It was less immediate than expected, though. Calling *gnatdoc* wasn't
enough because it doesn't run automagically all the required
tools. Maybe there's a way, which I don't know (yet).

Simple compilation didn't work either: *gnatdoc* still says

    warning: cross references for file ...
  
Then I've run *gnatxref*, and it generated a 0-byte-long
*gnatinspect.db*. Running *gnatinspect* solved, but it isn't clear why
I need to do this and why *gnatxref* generate a void file.

Anyway, this wasn't the problem for *gnatdoc*. Finally, the sequence was:

* gprbuild
* gnatinspect default.gpr --exit
* gnatdoc -Pdefault.gpr

These steps did it. Then, in order to have the doc in, say, html, *cd
doc* and *make html* (requirements: sphinx).

.. _ReStructuredText: https://en.wikipedia.org/wiki/ReStructuredText
