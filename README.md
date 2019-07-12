# PharoGs

**PharoGs** is an open-source project to host [Pharo](http://pharo.org) in [GemStone](https://gemtalksystems.com/products/gs64/).

## Background

At the lowest level, a Smalltalk class library is tightly bound to the virtual machine on which it runs. This is because Smalltalk uses _primitives_ to invoke operations that involve:
- hardware (such as arithmetic and other binary operations), 
- resources managed by the operating system (such as files and sockets), and 
- external software libraries (typically with a foreign function interface or FFI). 

With the exception of the primitives (of which there are about 535 in Pharo's [minimal image](https://files.pharo.org/get-files/80/pharo-minimal.zip)), the remaining code in standard Smalltalk is simply message sends to objects, which (in theory) is portable to any Smalltalk implementation. Because of this, there is a rich tradition of writing high-level libraries and frameworks in such a way that they are portable from one dialect to another. Perhaps the best-known current such framework in the Smalltalk community is [Seaside](http://seaside.st/download), with seven listed dialects supported.

Of course, there is a lot of code between the primitives and the portable libraries, and this leaves lots of room for [misunderstandings](https://wiki.c2.com/?DoesNotUnderstand). One approach is to provide a portability layer (of which [Grease](https://github.com/SeasideSt/Grease)) is perhaps the best known in Smalltalk. This way, if you write to the the portability layer, then you should be able to port your code to any dialect that has an equivalent version of the portability layer (customized for that dialect).

## Motivation

Writing to a portability layer is somewhat challenging since it is easy to write code that accidentally bypasses the portability layer and not discover it till the port is attempted. 

Also, a portability layer by definition contains only things that exist in all dialects, so is a subset and cannot include unique features.

## Development Process

The current approach is to export _all_ class and methods from a Pharo [minimal image](https://files.pharo.org/get-files/80/pharo-minimal.zip) (to which sources has been added) using the `exportFromPharo.sh` script. Using four `.st` files, this script generates a set of `.gs` files that can be loaded into GemStone using [Topaz](https://downloads.gemtalksystems.com/docs/GemStone64/3.5.x/GS64-Topaz-3.5.pdf).

Although the goal is to work with an standard Pharo image, some modifications may be required to the Pharo code base for this to work. These changes are being submitted back to the base, but until they are all incorporated you need to work on a local fork. To do this obtain a copy of [Pharo8.0](https://github.com/pharo-project/pharo.git) and use bootstrap to get a new minimal image:

```
BRANCH_NAME=Pharo8.0 BUILD_NUMBER=42 BOOTSTRAP_ARCH=32 \
  time ./bootstrap/scripts/bootstrap.sh # this takes about 15 minutes
```
 