# PharoGs

**PharoGs** is an open-source project to host [Pharo](http://pharo.org) in [GemStone](https://gemtalksystems.com/products/gs64/).

## Background

At the lowest level, a Smalltalk class library is tightly bound to the virtual machine on which it runs. This is because Smalltalk uses _primitives_ to invoke operations that involve:
- hardware (such as arithmetic and other binary operations in the CPU),
- resources managed by the operating system (such as files and sockets), and
- external software libraries (typically with a foreign function interface or FFI).

With the exception of the primitives (of which there are over 500 in Pharo's [minimal image](https://files.pharo.org/get-files/90/pharo-minimal.zip)), the remaining code in standard Smalltalk is simply message sends to objects, which (in theory) is portable to any Smalltalk implementation. Because of this, there is a rich tradition of writing high-level libraries and frameworks in such a way that they are portable from one dialect to another. Perhaps the best-known current such framework in the Smalltalk community is [Seaside](http://seaside.st/download), with seven listed dialects supported.

Of course, there is a lot of code between the primitives and the portable libraries, and this leaves lots of room for [misunderstandings](https://wiki.c2.com/?DoesNotUnderstand). One approach is to provide a portability layer (of which [Grease](https://github.com/SeasideSt/Grease)) is perhaps the best known in Smalltalk. This way, if you write to the the portability layer, then you should be able to port your code to any dialect that has an equivalent version of the portability layer (customized for that dialect).

## Motivation

### Portability

A portability layer, by definition, contains only things that exist in all dialects, so is a subset and cannot include unique features. Furthermore, writing to a portability layer is something that is rarely done from the start, but is an afterthought taken on after a project has achieved success in its original dialect, and is typically attempted by someone other than the original author. Enhancements made in the original code have to be repeatedly checked for portability, with changes (that are of no value in the original dialect) fed back to the source.

Instead of a portability layer, this project seeks to *host* the Pharo class library in GemStone. While this is narrower than a portability layer (since it doesn't facilitate moving Pharo code to anything except GemStone), it is much more complete since (with certain limitations discussed later) most Pharo code should be runable in GemStone. This allows a Pharo-based application to take advantage of GemStone features: allowsing many VMs to share a large object space.

### Development IDE

Another possible motivation for running Pharo code in GemStone is that any GemStone IDE can interact with the Pharo code. This allows us to run a "headless" image with a full GUI since we can use the GemStone tools to look at Pharo code. This is an attractive alternative to trying to debug in an environment without a debugger.

## Approach

Perhaps the most transparent approach to support Pharo in GemStone would be to modify GemStone's virtual machine (VM) to match Pharo's VM. While initially appealing, this would be more complex than simply remapping primitives and it would mean that existing GemStone code (including its built-in class library) would no longer work. Initially, we are using an alternate approach that takes advantage of existing GemStone features and (so far) doesn't require any changes to the GemStone VM.

Like other Smalltalks, message sends in GemStone use dynamic binding to a chain of MethodDictionary instances associated with the Class of the receiver. In order to support other dialects and languages (originally [Ruby](https://maglev.github.io)), GemStone method lookup uses not just a selector (as in traditional Smalltalks), but a selector plus an integer **environment** (where the default is 0 for the GemStone Smalltalk class library). Each environment has its own MethodDictionary for each Class, and each environment can specify a superclass for method lookup (this does not affect instance variables or other class attributes that are inherited). By default, a method will send messages using its own environment, but a special syntax exists to explicitly call another environment.

Our general approach is to install the Pharo class library into environment 2 (environments 0 and 1 are reserved), and rewrite any methods required to get things to work. The rewrite starts with primitives by (1) changing just the number if an equivalent primitive exists; (2) writing equivalent code if possible; and (3) reporting an error if an equivalent operation has not been implemented yet.

## Limitation

As in any Smalltalk, there is some low-level Pharo code that is tied to the VM and is not easily reproduced in GemStone. Beyond the obvious case of over 500 primitives, this includes code dealing with garbage collection and process scheduling. In many cases an attempt is made to provide equivalent high-level functionality, but using GemStone's native implementation. So, `Processor yield` is implemented by calling GemStone's equivalent code and not making any attempt to reuse the Pharo implementation. Otherwise, we generally try to keep Pharo's implementation even if a more efficient implementation is available in GemStone (such as `OrderedCollection`).

It is not anticipated that PharoGs will provide a platform for developing language tools, but will instead be a platform for running higher-level headless applications built on popular libraries, such as Seaside. In particular, supporting a GUI is beyond the anticipated scope of PharoGs at this time.

## Environment

The following instructions describe one development process using macOS Sequoia (15.1.1), GemStone/S 64 Bit 3.7.1, and Pharo 13.0. This is an attempt to describe something that is known to work and is not intended to mandate naming conventions and directories. You are welcome to adapt this to your own situation.

### Pharo

Depending on the state of development, some modifications may be required to the [Pharo code base](https://github.com/pharo-project/pharo) for PharoGs to work. These changes are being [submitted](https://github.com/pharo-project/pharo/pulls/jgfoster) back to the base, but unless and until they are all incorporated you need to use a branch (of course, if you already have a Git checkout of Pharo, you can add this repository as a remote and checkout the appropriate branch):

```
cd ~/code/ # or where you want to put the checkout
git clone https://github.com/jgfoster/pharo.git
```

Pharo's bootstrap script generates a number of images, including the one we want:

```
cd ./pharo
git checkout PharoGs
# the next step takes about 12 minutes (on a 2021 MBP with M1 Pro CPU)
time ./bootstrap/scripts/bootstrap.sh; date
```

### GemStone

On macOS it is easiest to use [GemStone.app](https://github.com/jgfoster/GemStoneApp) to install and run GemStone 3.7.1. From the Databases tab and the Login subtab, click `Terminal` to open a Terminal with appropriate GemStone environment variables set. 

To log in to GemStone with [Topaz](https://downloads.gemtalksystems.com/docs/GemStone64/3.7.x/GS64-Topaz-3.7.pdf) you will need a `.topazini` file similar to the following:

```
set user SystemUser pass swordfish
set gems gs64stone
login
```

Use this Terminal to test the GemStone connectivitiy with the following:

```
topaz -l
run
2 + 3
%
logout
exit
```

If the above does not work you need to get GemStone working (which is beyond the scope for this README.md). Continue in the terminal for the next step (skip the clone if it has already been done).

### PharoGs

Get a copy of this code:

```
cd ~/code # or where you want to install the code
git clone https://github.com/jgfoster/PharoGs.git
```

Create a symbolic link named `pharo` to the Pharo checkout:

```
cd PharoGs
ln -s ~/code/pharo pharo
```

## Development Process

* Bootstrap a Pharo image from source files (skip if already done above).
  * `time cd pharo; ./bootstrap/scripts/bootstrap.sh; cd ..; date`
  * The `time` and `date` commands aren't necessary but helpful to give some experience at what to expect on subsequent iterations.
  * If this errors, then there is a problem with Pharo's bootstrap and this should be reported back to the Pharo dev team.
* Export Pharo classes and methods and generate a set of `.gs` files that can be loaded into GemStone (next).
  * `time ./exportFromPharo.sh; date`
  * This should end with a list of globals (currently 6), pools (5), classes (1293), and methods (22470) that were exported.
  * If there are errors here, then we need to find and fix them. Typically they will involve `PharoMods` that applied in a previous version of Pharo but no longer apply. The simplest thing is to just remove them until you get past this step (we will add appropriate mods later).
* Import Pharo classes and methods to GemStone.
  * `time ./importToGemStone.sh; date`
  * This should end with an errorCount of 0 and a commit.
  * Look at `PharoGs.out` to find the import attempt. Then look at the `./output` directory to find the file with the error.
  * An error at this point typically means that there was code in Pharo that cannot be loaded into GemStone, such as two dots at the end of a line. This code needs to be fixed in Pharo (which is why we work on a separate branch) and then start over.
    * You can explicitly fix the `.gs` file as a way to postpone the bootstrap step.
  * The import script will run a number of SUnit tests from Pharo. With a few noted exceptions, all should pass! Watching the list of passing tests gives you a good idea of where we are in the process.
* Other
  * The purpose of `PharoGs.md5` is to track changes made to Pharo methods that have overrides (replacements) in PharoGs. These are typically methods that don't compile in GemStone (typically because of primitives or differences in instvar names) so have been rewritten in GemStone. When the code changes in Pharo we need to revisit our overrides to see if they are still appropriate.

### SystemUser

For most projects it is best to avoid using the SystemUser login to GemStone. In this case, however, we do all our development with the SystemUser account. This is because we are compiling primitives. Once an effective Pharo SymbolDictionary is prepared, it can be assigned to a user as the primary or even _sole_ system dictionary. Globals should not be needed or visible to most Pharo code; if it is present it should come after the Pharo SymbolDictionary so that the Pharo classes are found first.

### Testing

To run individual tests in PharoGs, try the following:

```
topaz -lq
set compile_env: 2
run
IntegerTest suite run printString
%
logout
exit
```

This should show something like the following:

```
49 ran, 49 passed, 3 skipped, 0 expected failures, 0 failures, 0 errors, 0 passed unexpected
```
