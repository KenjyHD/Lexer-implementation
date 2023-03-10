# Lexer implementation

The above .bsp/ and .idea/ reoresents run configuration when using IntelliJ IDEA 2022.2.4
## What is a lexer?

A lexer is a program that divides a string of characters into substrings called lexemes, each of which is classified as a token, based on a specification.

## Lexer input

The input of a lexer consists of two components:

1. a **specification**
2. a **text** that will be analyzed lexically, more precisely, divided into lexemes.
The specification has the following structure:
```
TOKEN1 : REGEX1;

TOKEN2 : REGEX2;

TOKEN3 : REGEX3;

...
```
where each ***TOKENi*** is a name given to a token, and ***REGEXi*** is a regex describing that token. You can think of this specification as a configuration file that describes how the lexer will work on various text files.

## Lexer output

The lexer outputs a list of the form: ***[(lexeme1, TOKEN_lexeme_1), (lexeme2, TOKEN_lexeme_2), …]***, where ***TOKEN_lexeme_N*** is the name of the token associated with lexeme n, based on the specification.

## The standard form of regular expressions
```
<regex> ::= <regex><regex> | 
            <regex> '|' <regex> | 
            <regex>'*' | <regex>'+' | <regex>'?' | 
            '(' <regex> ')' | 
            "[A-Z]" |
            "[a-z]" |
            "[0-9]" |
            "eps" | <character> 
```
In the above description, the elements between angle brackets <> are non-terminals to be generated, characters are always enclosed in single quotes, and strings are enclosed in double quotes.

<character> refers to any regular character that is not part of the control characters (such as * or |), or any string of length three of the form 'c' , where c can be any character including the control.

"eps" represents the Epsilon character.

## Lexer implementation details
There are several ways you can implement a lexer. The conventional approach (and the one we recommend) consists of the following stages:

1. each regex is converted into an AFN, keeping at the same time the information about the related token and the position at which it appears in the spec.
2. a unique AFN is built, introducing an initial state and epsilon-transitions from it to all the initial states of the AFNs above. Thus, this AFN will accept any of the tokens described in the specification. The final state visited will indicate the token found.
3. The AFN is converted to an AFD (which can optionally be minimized). In this machine:
    * when we visit a group of states that contains (AFN-)final states, it means that one or more corresponding tokens have been identified.
    * when we visit a sink-state (if it exists), it means that the current substring is not described by any token. In this case, we must return the longest accepted prefix and continue lexing the remaining word
    * when we visit a non-final state and which is not a sink-state, we continue by passing to the next state of the AFD consuming a character from the word

## When does a lexer finish its execution?
The purpose of a lexer is to identify the **longest substring** that satisfies a regex from the given specification. If a longest substring satisfies **two or more regexes**, the first related token will be reported, in the order in which they are written in the specification.

To identify the longest substring using an AFD like the one described in the previous section, we must note that:

1. visiting a group of states that contains a final (AFN-) state **does not** necessarily indicate that we have found the longest accepted substring.
2. if a group of states containing a (AFN-)final state has been **previously visited**:
    * visiting a group of states that does not contain final states **does not** necessarily indicate that we have found the longest substring (the automaton may accept in the future)
    * visiting the sink-state of the AFD (if it exists), indicates that the machine will no longer accept it in the future.
    * if there is no sink state in AFD, then the lexical analysis must continue until the input is exhausted, to decide on the longest substring.

Once the longest substring has been identified:

1. The AFD will be reset - brought to the initial state to resume the lexical analysis.
2. the lexical analysis will continue from the position where the longest substring ended, and this can precede the current position where the analysis reached by **any number of positions**.


### Example

Let the following specification be:
```
TOKEN1 -> abbc*;
TOKEN2 -> ab+;
TOKEN3 -> a*d;
```
and the ***abbd*** input. The lexical analysis will stop at character ***d*** (the previously described AFD will reach this character in sink state). The substring abb is the longest that satisfies both ***TOKEN1*** and ***TOKEN2***, and ***TOKEN1*** will be reported, since it precedes ***TOKEN2*** in the specification. Afterwards, the lexer will advance by one character the current position in the input, and will identify the substring d as ***TOKEN3***.

For further clarifications and more examples that include the longest substring, revisit the course on lexers.

## Project structure
```
...
└── src
    └── main
        └── scala
            ├── Dfa.scala - DFA class
            ├── Nfa.scala - NFA class
            ├── Regex.scala - Regex preprocessing object
            ├── Lexer.scala - lexer class
            ├── AST.scala - abstract syntax tree class (used to build an AST from a given prenex) 
            └── configuration - a configuration file for a simple programming language
    └── test
		└── prog_tests - folder with source code files in that simple programming language
			    ...
		└── scala - folder with tests of every component of this project
				...
```

## Copyright
Test file and project implementation idea belongs to the LFA team(2022-2023), UBP, Automatic and Computers faculty.
