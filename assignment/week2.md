### Objectives

The objective of the second assignment is to develop a **lexical analyzer**.
In future assignments, you will reuse the functions developed here.

---

### Questions

Solve the following exercises in the textbook:

- 3.3.2 (a), (b), (c), (d)
- 3.3.5 (c)
- 3.4.1 (a), (b), (c), (d)
- 3.4.2 (c)
- 3.6.2 (c)
- 3.7.3 (a)

---

### Refactoring

Make modifications and improvements to the **symbol table** as suggested by the feedback you received on Assignment 1.\
If your symbol table did not receive any comment, you can skip this section.

Hand in the revised code and any supporting material to demonstrate your code refactoring (for example, test cases and their output).

---

### Toolkit

In this assignment, the instructions assume the use of **lex** for consistency with the book.\
You are encouraged to transition to **flex** and **bison** (http://www.gnu.org/software/bison/) or a compiler generator with bindings for your target programming language.

---

### Lexical Analyzer

#### Source Language

The source language for all homework assignments is defined by the grammar in Appendix A.1.

#### Regular Expressions

Write regular expressions for the following tokens:

- `id` (like Figure 3.23)
- `basic` (basic tokens are `int`, `float`, or `boolean`)
- `num` (like Section 2.6.3)
- `real` (like Figure 3.23)

Hand in the regular expressions for these tokens. You are required to submit **ordinary regular expressions**. Additionally, the next question will require you to submit their corresponding **lex source code**.

#### Lexical Analyzer Implementation

Write a lexical analyzer with **lex (or flex-bison)** for the language in Appendix A.1. The lexical analyzer should:

- **Symbol table:** All `id`s should be installed in the chained symbol table implemented in Assignment 1.
  - The key is a string representing the identifier, copied from `yytext`.
  - The parser does not yet distinguish scopes, so all identifiers can be inserted into the top-level symbol table.

- **Output:** Print out the name of the token whenever it recognizes one.
  Example program:

  ```c
  int i;
  while (true)
      i=i+1;
  ```
