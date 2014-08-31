# civs

A simulator of civilizations evolution written in Clojure. By simulating migrations, language development, evolution of tribes, chiefdoms and nations, creation and destruction of settlements it builds a realistic world with a complete history. That history can be observed for fun or be used as base for future games.

## Status

[![Build Status](https://travis-ci.org/ftomassetti/civs.svg?branch=master)](https://travis-ci.org/ftomassetti/civs)

[![Clojars Project](http://clojars.org/civs/latest-version.svg)](http://clojars.org/civs)

## Related projects

The project generate civilizations evolving in worlds which can be generated using the world generator [lands](https://github.com/ftomassetti/lands).
To generate languages for the different civilizations it use the names generation library [langgen](https://github.com/ftomassetti/langgen). 
Lands and langgen are written Python and wrapped in Java using Jython. The wrapper is [lands-java-lib](https://github.com/ftomassetti/lands-java-lib). 

The history files produced by civs can be explored using [civs-browser](https://github.com/ftomassetti/civs-browser).

## Unrelated projects which involves history simulation

[world-js](http://anvoz.github.io/world-js/)
