[![build status](https://secure.travis-ci.org/evanw/bend.png)](http://travis-ci.org/evanw/bend)
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

The live demo is available at [http://evanw.github.com/bend/](http://evanw.github.com/bend/) and includes many more examples.

# Installation

Bend can be installed through [npm](https://npmjs.org/):

    npm install -g bend
