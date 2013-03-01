#Cobol programs analyzer#

This tool is basically a Lexer for Cobol Programs. It process the source code and generate a list of tokens.
This list includes variable definition, string literal, numeric literal, etc.; in general, all cobol words
that are used in real programs.

The list of tokens is full of metadata, in a way that you may know the exact position of a token in the source
code (line and col numbers) and the value of the token (string value, numeric value, etc., depending on the type
of token).

This tool has been proved against thousands of test programs, in order to find exceptional syntactical cases. For
example: string continued in next line and so on.

As a way to prove the tool I made a simple viewer that presents the program in a web browser. It´s a basic syntax
highlighter that mimic the common colors of Microfocus MainFrame.

##Possible applications##

A first use that I envisioned for this, is to construct a little DSL to search patterns in source code repositories
for analysis. A next step may be perform simple program transformations.

I proved this kind of stuff in a huge repository, and it performed good search in half of a minute.

##Future improvements##

Some day, when I have some time and inspiration, I will port it to Java language. After that, it could be possible
to use this tool with ANTLR library, to make real complex trasformations on cobol source code.

## License
Copyright (c) 2010 Nolvis Urquiza Elías
You can use it as you like.

## Contact
For any question, please contact me at: nurquiza@grm.desoft.cu


