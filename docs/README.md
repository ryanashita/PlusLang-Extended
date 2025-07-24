# PlusLang-Extended
PlusLang-Extended is a simple programming language with variables, types, lexical scope, functions, arithmetic, and higher-order support. It started as a simple interpreter but now includes type-checking and code generation/translation from .pled programs to MIPS Assembly. 

## How to run examples and tests
To run examples...
1. Clone this repository and navigate to ```code/lang```.
2. Run the terminal command ```dotnet build```, making sure the .NET project builds correctly.
3. Run the command ```dotnet run example.pled```, replacing 'example' with an existing ```.pled``` file, located at ```code/programs```.

To test the project...
1. Navigate to ```code```.
2. Run the commands ```dotnet build```, followed by ```dotnet test```. 

## Elements of the compiler
This simple plus-lang extended interpreter has three parts: Abstract Syntax Tree, Parser, and Evaluator. The AST defines the language's syntax and the tokens. The parser takes .pled source code and parses the code into a AST. The evaluator takes the AST and recursively evaluates (recursive descent) to ultimately return the value of the program.

Taking it one step further is the code generation. It can translate a plus-lang AST into MIPS assembly. The resulting code can be tested on these (limited) interactive websites: https://www.csfieldguide.org.nz/en/interactives/mips-assembler/, https://www.csfieldguide.org.nz/en/interactives/mips-simulator/. 

Another element of the compiler is the type-checker, which is still in progress.

## Roadmap
- Improve the type-checker
- Add more control flow structures


