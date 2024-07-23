Plural Arguments for Curry Programs
===================================

This package contains the implementation of a tool
to support plural arguments in Curry programs.
The idea and background of this implementation is
described in the following paper:

> Michael Hanus: Adding Plural Arguments to Curry Programs,
> Tech. Comm. of the 29th Int. Conf. on Logic Programming (ICLP 2013),
> Theory and Practice of Logic Programming 13 (4-5-Online-Supplement), 2013 


Installation:
-------------

The tool can be directly installed by the command

    > cypm install plural-arguments

This installs the executable `curry-plural` in the bin directory of CPM.
The tool implements the necessary transformation on Curry programs.


Usage:
------

To use plural arguments in a Curry program,
import the library `Language.Curry.Plural` (available in the `src` directory)
and mark the plural arguments of an operation by wrapping
their type with the type constructor `Plural` in their
type signature, e.g.:

    dupp :: Plural Int -> (Int,Int)
    dupp x = (x,x)

Currently, this is restricted to top-level
operations. As an example, consider the operation `pali`
of the Curry program [`Palindrome`](examples/Palindrome.curry).

Then execute the program transformation by the command

    > curry-plural -r prog.curry

This performs the transformation and loads the transformed
program into the Curry system. To see a list of
other options, just execute the command

    > curry-plural -h


Restrictions:
-------------

The transformation is restricted to a single module, i.e.,
the operations with plural arguments and their usage
must be contained in a single module.

Relaxing this restriction requires the transformation of
all modules referring the operations with plural arguments.


Contact:
--------

In case of problems or comments, contact
[Michael Hanus](http://www.informatik.uni-kiel.de/~mh/).
