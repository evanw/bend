# The Bend Programming Language

Bend is a statically-typed programming language for the web. It compiles down to readable JavaScript with no runtime library and easily integrates with other JavaScript code.

An example Bend program:

    extern class console {
      static void log(var[] data...)
    }
    int[] nums = { 1, 2, 3 }
    for n in nums {
      console.log('hello', n)
    }

# Live Demo

Please see [http://evanw.github.com/bend/](http://evanw.github.com/bend/) for a live demo and more information.

# Installation

Bend can be installed through [npm](https://npmjs.org/):

    npm install -g bend
