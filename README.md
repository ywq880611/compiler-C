# compiler-C

take care of ?? in comment cause I didn't know why when I write this code.

Functions intruduction:
1. next(): next just dispose source code, it's called lexical analysis. In this function we never emit any code.

2. expression(): it's a part of semantic analysis, it also emit codes.

3. statement(): it's a higher level semantic analysis than expression(), expression() was only invoked in this function, it also emit codes.

4. match(): it's a encapsulation of next(), we check whether the token is what we want and call next() function.

5. enum_declaration(): Parse the enum declaration and save as ID in symbol tables.

6. function_parameter(): We use this function to parse parameter in function define.it needs to save the paramet in symbol table as local variable.

7. function_body(): We use this to parse function body, there are two steps, first is dispose local variable in function body, and emit code for function call and function return. It also call statement() function to emit the code of what the function really do.

8. function_declaration(): Use for parse function and call above function to emit code.

9. global_declaration(): Parse enmu, variable and function.

10. program(): Using for parsing all source code, it will call global_declaration().