# Compiler

## Error Handling

Design and implement a strategy for error handling and recovery. Follow the principles outlined in sections 4.1.3 and 4.1.4. Hand in a design document containing the general principles and detailed choices of your error-handling strategy. Implement error handling in the source code.

We utilized the built-in error recovery function of chunksy, our parser-generator library of choice. It provides a few methods of recovery that are in-line with that specified in the book.

The book mentions 4 types of error recovery methods: panic-mode, phrase-level, error productions, and global corrections. The chosen chumsky recovery method of choice is similar to that of panic-mode and phrase-level recovery.

The chumsky parser recovery of choice is able to understand which tokens may have been missed or accidentally added during the parse process, and is able to skip tokens until previously opened AST objects may close cleanly. This method does not rely on synchronizing tokens (like parenthesis or braces), as it will match on any continuing expression already defined in the library. This does have some drawbacks however; if there are too many tokens missed, our parser would throw multiple errors until the AST is synchronized as a whole.

This approach allows us to have partial ASTs that are still useful for the developer as majority of the code should be able to parse properly.

We additionally utilized the ariadne library [https://github.com/zesterer/ariadne](https://github.com/zesterer/ariadne) in order to pretty-print our errors and more clearly point out where the AST failed to parse.

# Refactoring Statements

You can find proof that our previous compiler works by looking at this sample output:
[https://gist.github.com/404Wolf/1634b37d35e81df15c553f132b1a32dd](https://gist.github.com/404Wolf/1634b37d35e81df15c553f132b1a32dd)
Which was run for `{ 12 + 12; boolean foo; foo = true; !foo; }`

We added support for unary NOT "!" and fixed the issue with the final ";' not printing for blocks.

# Textbook Output

Our textbook output can be found here:
[https://gist.github.com/404Wolf/6757d0716cd374d72e65f4735e339604](https://gist.github.com/404Wolf/6757d0716cd374d72e65f4735e339604)

# Building and running

We suggest that if you use the Nix build system to build our project, you simply run

`nix build –extra-experimental-features flakes --extra-experimental-features nix-command`

Rather than using the development shell
