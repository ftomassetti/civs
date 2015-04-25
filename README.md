# civs

A simulator of civilizations evolution written in Clojure. By simulating migrations, language development, evolution of tribes, chiefdoms and nations, creation and destruction of settlements it builds a realistic world with a complete history. That history can be observed for fun or be used as base for future games.

## Status

[![Build Status](https://travis-ci.org/ftomassetti/civs.svg?branch=master)](https://travis-ci.org/ftomassetti/civs)

[![Clojars Project](http://clojars.org/civs/latest-version.svg)](http://clojars.org/civs)

Currently the simulation can be started with any number of bands (small groups of humans). During the turns they can:
* evolve into tribe or chiefdoms
* migrate towards better lands
* settle new cities
* develop languages
* grow or perish
* develop agriculture
* change their lifestyle from nomadic to semi-sedentary or sedentary
* split into separate groups
* and so on... I am constantly improving the simulation

## How to use it: related projects

The project generate civilizations evolving in worlds which can be generated using the world generator [lands](https://github.com/ftomassetti/lands).
To generate languages for the different civilizations it use the names generation library [langgen](https://github.com/ftomassetti/langgen). 
Lands and langgen are written Python and wrapped in Java using Jython. The wrapper is [lands-java-lib](https://github.com/ftomassetti/lands-java-lib). 

The history files produced by civs can be explored using [civs-browser](https://github.com/ftomassetti/civs-browser).

## Include it as a dependency

Leiningen

```
[civs "0.2.2"]
```

Gradle

```
compile "civs:civs:0.2.2"
```

Maven

```
<dependency>
  <groupId>civs</groupId>
  <artifactId>civs</artifactId>
  <version>0.2.2</version>
</dependency>
```

Dev guidelines
==============

Use [kibit](https://github.com/jonase/kibit) and [eastwood](https://github.com/jonase/eastwood) to verify code quality.

When runnng eastwood exclude the check for unlimited use of namespaces:

```
lein eastwood "{:exclude-linters [:unlimited-use]}"
```

## Interesting links about human history and history simulation

[world-js](http://anvoz.github.io/world-js/) *an attempt to show you a brief history of humankind via a simulation game.*

[A Brief History of Humankind](https://www.coursera.org/course/humankind) *a Coursera course on human history*
