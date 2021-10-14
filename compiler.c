#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <string.h>
#define int long long 

int debug;    // print the executed instructions
int assembly; // print out the assembly and source

int token; // current token

// instruction set
enum { LEA ,IMM ,JMP ,CALL,JZ  ,JNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PUSH,
       OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,
       OPEN,READ,CLOS,PRTF,MALC,MSET,MCMP,EXIT };

// tokens and classes (operators last and in precedence order)
enum {
  Num = 128, Fun, Sys, Glo, Loc, Id,
  Char, Else, Enum, If, Int, Return, Sizeof, While,
  Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc, Dec, Brak
};

// fields of identifier
enum {Token, Hash, Name, Type, Class, Value, BType, BClass, BValue, IdSize};

// types of variable/function
enum { CHAR, INT, PTR };

// type of declaration.
enum {Global, Local};

int *text, // text segment
    *stack;// stack
int * old_text; // for dump text segment
char *data; // data segment
int *idmain;

char *src, *old_src;  // pointer to source code string;

int poolsize; // default size of text/data/stack
int *pc, *bp, *sp, ax, cycle; // virtual machine registers

int *current_id, // current parsed ID
    *symbols,    // symbol table
    line,        // line number of source code
    token_val;   // value of current token (mainly for number)

int basetype;    // the type of a declaration, make it global for convenience
int expr_type;   // the type of an expression

// function frame
//
// 0: arg 1
// 1: arg 2
// 2: arg 3
// 3: return address
// 4: old bp pointer  <- index_of_bp
// 5: local var 1
// 6: local var 2
int index_of_bp; // index of bp pointer on stack

void next(){
    char *last_pos;
    int hash;

    while(token = *src){
        src++;

        if(token == '\n'){
            if(assembly){
                printf("%d: %.*s", line, src - old_src, old_src);
                old_src = src;

                while(old_text < text){
                    printf("%8.4s", & "LEA ,IMM ,JMP ,CALL,JZ  ,JNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PUSH,"
                                      "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
                                      "OPEN,READ,CLOS,PRTF,MALC,MSET,MCMP,EXIT"[*++old_text * 5]);

                    if(*old_text <= ADJ) printf("%d", *++old_text);
                    printf("\n");
                }
            }
            line++;
        }
        else if(token = '#'){
            while(*src != '\n' && *src != 0) src++;
        }
        else if((token >= 'a' && token <= 'z') || (token >= 'A' && token <= 'Z') || (token == '_')){
            //identifier
            last_pos = src - 1;
            hash = token;
            while((*src >= 'a' && *src <= 'z') || (*src >= 'A' && *src <= 'Z') || (*src == '_') || (*src >=0 && *src <= 9)){
                hash = hash * 147 + *src;
                src++;
            }

            //find existing id in symbols or allocate a new id
            current_id = symbols;
            while(current_id[Token]){
                //compare hash value firstly, than compare the string value to avoid hash confilict
                if(current_id[Hash] == hash && !memcmp((char *)current_id[Name], last_pos, src - last_pos)){
                    token = current_id[Token];
                    return;
                }
                current_id = current_id + IdSize;
            }

            current_id[Name] = (int)last_pos;
            // we need to resave this name cause this address store the src code,
            // we may remove src code in the future
            current_id[Hash] = hash;
            current_id[Token] = Id;
            return;
        }
        else if(token >= '0' && token <= '9'){
            //dec(123), hex(0xa), oct(011)
            token_val = token - '0';
            if(token_val > 0){
                //dec
                while(*src >= '0' && *src <= '9'){
                    token_val = token_val * 10 + *src - '0';
                    src ++;
                }
            }
            else if(*src == 'x' || *src == 'X'){
                //hex
                token = *++src;
                while ((token >= '0' && token <= '9') || (token >= 'a' && token <= 'f') || (token >= 'A' && token <= 'F')) {
                    token_val = token_val * 16 + (token & 15) + (token >= 'A' ? 9 : 0);
                    token = *++src;
                }
            }
            else{
                //oct
                while (*src >= '0' && *src <= '7') {
                    token_val = token_val*8 + *src++ - '0';
                }
            }

            token = Num;
            return;
        }
        else if(token == '/'){
            if(*src == '/'){
                //comment
                while(*src != 0 && *src != '\n') src++;
                // line++;
                // we must commented line++ here, cause we will continue to
                // '\n' char in next while loop to parse it and then add the line.
            }
            else{
                //div
                token = Div;
                return;
            }
        }
        else if(token == '"' || token == '\''){
            last_pos = data;
            while(*src != 0 && *src != token){
                token_val = *src++;
                if(token_val = '\\'){
                    token_val = *src++;
                    if(token_val == 'n'){
                        token_val = '\n';
                    }
                }

                if(token == '"'){
                    *data++ = token_val;
                }
            }
            src++;
            if(token == '"'){
                token_val = (int)last_pos;
            }
            else{
                token = Num;
            }
            return;
        }
        else if(token == '='){
            if(*src == '='){
                token = Eq;
                src++;
            }
            else{
                token = Assign;
            }
            return;
        }
        else if(token == '+'){
            if(*src == '+'){
                token = Inc;
                src++;
            }
            else token = Add;
            return;
        }
        else if(token == '-'){
            if(*src == '-'){
                token = Dec;
                src++;
            }
            else token = Sub;
            return;
        }
        else if(token == '!'){
            if(*src == '='){
                token = Ne;
                src++;
            }
            return;
        }
        else if(token == '<'){
            if(*src == '='){
                token = Le;
                src++;
            }
            else if(*src == '<'){
                token = Shl;
                src++;
            }
            else token = Lt;
            return;
        }
        else if(token == '>'){
            if(*src == '='){
                token = Ge;
                src++;
            }
            else if(*src == '>'){
                token = Shr;
                src ++;
            }
            else token = Gt;
            return;
        }
        else if(token == '|'){
            if(*src == '|'){
                token = Lor;
                src ++;
            }
            else token == Or;
            return;
        }
        else if(token == '&'){
            if(*src == '&'){
                token = Lan;
                src ++;
            }
            else token = And;
            return;
        }
        else if(token = '^'){
            token = Xor;
            return;
        }
        else if(token =='%'){
            token = Mod;
            return;
        }
        else if(token == '*'){
            token = Mul;
            return;
        }
        else if(token == '['){
            // we just take care '[' singer here
            // ']' symbol just deal in later while loop.
            token = Brak;
            return;
        }
        else if(token == '?'){
            token = Cond;
            return;
        }
        else if (token == '~' || token == ';' || token == '{' || token == '}' || token == '(' || token == ')' || token == ']' || token == ',' || token == ':') {
            // directly return the character as token;
            return;
        }
    }
}


void match(int tk){
    if(tk != token){
        printf("%d: expected token: %d\n", line, tk);
        exit(-1);
    }
    next();
}

void expression(int level){
    int *id;
    int tmp;
    int *addr;

    {
        if(!token){
            printf("%d: unexpected token EOF of expression\n", line);
            exit(-1);
        }
        if(token == Num){
            match(Num);
            *++text = IMM;
            *++text = token_val;
            expr_type = Int;
        }
        else if(token == '"'){
            //string, there may be continous, like "abc""bcd"
            *++text = IMM;
            *++text = token_val;
            match('"');

            while(token == '"'){
                // deal with continous string, and store them in data segement.
                match('"');
            }

            data = (char*)(((int)data + sizeof(int)) & (-sizeof(int)));
            expr_type = PTR;
        }
        else if(token == Sizeof){
            // Size of is one of identify in symbol table.
            // we will initialize sizeof in main function later!!
            match(Sizeof);
            match('(');
            expr_type = INT;

            if(token == Int){
                match(Int);
            }
            else if(token == Char){
                match(Char);
                expr_type = CHAR;
            }

            while(token == Mul){
                match(Mul);
                expr_type = expr_type + PTR;
            }

            match(')');

            *++text = IMM;
            *++text = (expr_type == CHAR) ? sizeof(char) : sizeof(int);

            expr_type = INT; 
        }
        else if(token == Id){
            // three kinds of Id:
            // 1.function call
            // 2.Enum variable
            // 3.global/local variable
            // 4.keywords which were initialized in main function !!

            match(Id);
            id = current_id;

            if(token == '('){
                // function, I am not very clear??
                match('(');
                // deal with arguments;
                tmp = 0;// number of arguments
                while(token != ')'){
                    expression(Assign);
                    // all arithmetic or logical character is bigger than Assign
                    // So we will deal with them in this expression function in arguments parse 
                    *++text = PUSH;
                    tmp ++;

                    if(token == ','){
                        match(',');
                    }
                }
                match(')');

                if(id[Class] == Sys){
                    // It's the address the sys function
                    *++text = id[Value];
                }
                else if(id[Class] == Fun){
                    // function call
                    *++text = CALL;
                    *++text = id[Value];
                }
                else {
                    printf("%d: bad function call\n", line);
                    exit(-1);
                }

                if(tmp > 0){
                    *++text = ADJ;
                    *++text = tmp;
                }
                expr_type = id[Type];
            }
            else if(id[Class] == Num){
                //enum
                *++text = IMM;
                *++text = id[Value];
                expr_type = INT;
            }
            else{
                // variable


                // The layout of function call
                // ------------------   Address
                // Argument2            High Address
                // Argument1                ||
                // return addr              ||
                // last bp                  ||  <- now bp
                // Local varibale 1         \/
                // Local varibale 2     Low Address    
                if(id[Class] == Loc){
                    *++text = LEA;
                    *++text = index_of_bp - id[Value];
                }
                else if(id[Class] == Glo){
                    *++text = IMM;
                    *++text = id[Value];
                }
                else {
                    printf("%d: undefined variable\n", line);
                    exit(-1);
                }

                // why we still need emit other codes
                // we have used IMM or LEA.
                // we use LC and LI here for what
                // because the value in Value attribute is the address of variable !!
                expr_type = id[Type];
                *++text = (expr_type == CHAR) ? LC : LI;
            }
        }
        else if(token == '('){
            // cast or parenthesis
            match('(');

            if(token == CHAR || token == INT){
                // cast type
                tmp = (token == CHAR) ? CHAR : INT;
                match(tmp);
                while(token == Mul){
                    match(Mul);
                    tmp = tmp + PTR;
                }
                match(')');
                
                expression(Inc);
                // may be we just support number and variable in type cast
                // but we use expression here may cause function address cast
                // there may be something wrong with function ??
                expr_type = tmp;
            }
            else{
                // just parenthesis
                expression(Assign);
                match(')');
            }
        }
        else if(token == Mul){
            match(Mul);
            expression(Inc);// dereference has the same precendence as Inc.
            // here is a regression call it will derenference all '*' token.
            // if there are 3 '*' token, we will add 3 instrctions(LC or LI) in text segements
            // which means three times of dereference operation.           

            if(expr_type >= PTR){
                expr_type = expr_type - PTR;
            }
            else{
                printf("%d: bad dereference\n", line);
                exit(-1);
            }

            *++text = (expr_type == CHAR) ? LC: LI;
        }
        else if(token == And){
            // It's just about the &variable, this is a refrence operator.
            // we want to get the address of variable.
            // the reason why we didn't deal with bitwise and operator here is
            // they are postfix character we will deal with them in later while loop
            match(And);
            expression(Inc);

            if(*text == LC || *text == LI){
                text --;
            }
            else{
                printf("%d: bad address of\n", line);
                exit(-1);
            }

            expr_type = expr_type + PTR;
        }
        else if(token == '!'){
            match('!');
            expression(Inc);

            *++text = PUSH;
            *++text = IMM;
            *++text = 0;
            *++text = EQ;

            expr_type = INT;
        }
        else if(token == '~'){
            // bitwise not
            match('~');
            expression(Inc);

            // use xor -1 to get not
            *++text = PUSH;
            *++text = IMM;
            *++text = -1;
            *++text = XOR;

            expr_type = INT;
        }
        else if(token == Add){
            // we do nothing when deal with '+'
            match(Add);
            expression(Inc);

            expr_type = INT;
        }
        else if(token == Sub){
            match(Sub);
            
            if(token == Num){
                // for number
                *++text = IMM;
                *++text = -token_val;
                match(Num);
            }
            else{
                // for variable
                *++text = IMM;
                *++text = -1;
                *++text = PUSH;
                expression(Inc);
                *++text = MUL;
            }
            
            expr_type = INT;
        }
        else if(token == Inc || token == Dec){
            // we just care about prefix '++' here.
            // we didn't support '++++variable' here.
            tmp = token;
            match(token);
            expression(Inc);

            if(*text == LC){
                *text = PUSH; // save the address in stack
                *++text = LC;
            }
            else if(*text = LI){
                *text = PUSH;
                *++text = LI;
            }
            else{
                printf("%d: bad lvalue of pre-increment\n", line);
                exit(-1);
            }
            *++text = PUSH;
            *++text = IMM;
            *++text = (expr_type > PTR) ? sizeof(int) : sizeof(char);
            *++text = (tmp == Inc) ? ADD : SUB;
            *++text = (expr_type == CHAR) ? SC : SI; 
        }
    }


    
    {
        // binary operator and postfix operators


        // there are some tricks about precedence
        // very important !! ??
        // for example, like var = 3 + 4 * 5;
        // 1. first we use expression(var), var is Gol or Loc
        // 2. then we meet '=', it's Assign it is bigger than Gol or Loc,
        // we need to do the while loop in next.
        // 3. then we meet '+', it's also big than Assign.
        // 4. Last we meet '*' it's bigger than '+', so we deal with '*' firstly.

        // we can handler ';' in later statement function maybe ?? 
        // I suppose ';' is an ASCII character so it is less than 128,
        // So it is less than any character we define in previous
        // So we will end this loop when we meet ';'
        // It will be next token after next next() function call??


        while(token >= level){
            tmp = expr_type;
            if(token == Assign){
                // we deal with var = expr here,
                // the expr could be num or variable or any combine of previous two types
                // but if we meet any kind of them we will not modify the sp register and the stack
                // So we push the address of var before we call expression
                // And we store the value in 'ax' after we call expression to the address store on the stack
                match(Assign);
                if(*text == LC || *text == LI){
                    *text = PUSH;
                }
                else{
                    printf("%d: bad lvalue in assignment\n", line);
                    exit(-1);
                }
                expression(Assign);

                expr_type = tmp;
                *++text = (expr_type == CHAR) ? SC : SI;
            }
            else if(token == Cond){
                // expr ? var or num : var or num
                match(Cond);
                *++text = JZ;
                addr = ++text;
                // why Assign here !!
                // Maybe Assign is bigger than 128
                // So we will not deal with ':' symbol in while loop in next recursion
                // We need to take care of ':' sym bol in this recursion
                expression(Assign);
                if(token == ':'){
                    match(':');
                }
                else{
                    printf("%d: missing colon in conditional\n", line);
                    exit(-1);
                }
                // The text segment layout of Cond
                // 0    JZ                                  Low Addr
                // 1    6                                     || <- first addr pointer 
                // 2    IMM or LEA           var or num       ||   
                // 3    num or id[value]                      ||   
                // 4    JMP                                   ||
                // 5    8                                     || <- second addr pointer
                // 6    IMM or LEA                            ||
                // 7    num or id[value]                      \/
                // 8                                        High Addr

                // We didn't need to identify the whether the type is var or num
                // Because the instruction length of each type is same.
                // Maybe I have miss some cases here, 
                // but I suppose the instruction length of all case is same.
                *addr = (int)(text + 3);
                *++text = JMP;
                addr = ++text;
                expression(Cond);
                *addr = (int)(text + 1);
            }
            else if(token == Lor){
                // if first var or num is zero, we will not call jnz
                // We will then modify ax register by second var or num
                // otherwise we will not care about the second var or num
                // this part just deal with logical calculate
                // It didn't deal with If or While etc. control flow.
                match(Lor);
                *++text = JNZ;
                addr = ++text;
                expression(Lor + 1);
                *addr = (int)(text + 1);
                expr_type = INT;
            }
            else if(token == Lan){
                match(Lan);
                *++text = JZ;
                addr = ++text;
                expression(Lan + 1);
                *addr = (int)(text + 1);
                expr_type = INT;
            }
            else if(token == Or){
                // bitwise or
                match(Or);
                *++text = PUSH;
                expression(Or + 1);
                *++text = OR;
                expr_type = INT;
            }
            else if(token == Xor){
                match(Xor);
                *++text = PUSH;
                expression(Xor + 1);
                *++text = XOR;
                expr_type = INT;
            }
            else if(token == And){
                // bitwise and
                match(And);
                *++text = PUSH;
                expression(And + 1);
                *++text = AND;
                expr_type = INT;
            }
            else if(token == Eq){
                // equal ==
                match(Eq);
                *++text = PUSH;
                expression(Eq + 1);
                *++text = EQ;
                expr_type = INT;
            }
            else if(token == Ne){
                // not equal !=
                match(Ne);
                *++text = PUSH;
                expression(Ne + 1);
                *++text = NE;
                expr_type = INT;
            }
            else if(token == Lt){
                // less than <
                match(Lt);
                *++text = PUSH;
                expression(Lt + 1);
                *++text = LT;
                expr_type = INT;
            }
            else if(token == Gt){
                // great than
                match(Gt);
                *++text = PUSH;
                expression(Gt + 1);
                *++text = GT;
                expr_type = INT;
            }
            else if(token == Le){
                // less than or equal <=
                match(Le);
                *++text = PUSH;
                expression(Le + 1);
                // maybe we need use Le + 2 here to ignore Ge in future??
                *++text = LE;
                expr_type = INT;
            }
            else if (token == Ge) {
                // greater than or equal to
                match(Ge);
                *++text = PUSH;
                expression(Ge + 1);
                *++text = GE;
                expr_type = INT;
            }
            else if (token == Shl) {
                // shift left
                match(Shl);
                *++text = PUSH;
                expression(Shl + 1);
                // maybe shl + 2 here ??
                *++text = SHL;
                expr_type = INT;
            }
            else if (token == Shr) {
                // shift right
                match(Shr);
                *++text = PUSH;
                expression(Shr + 1);
                *++text = SHR;
                expr_type = INT;
            }
            else if(token == Add){
                // add +
                match(Add);
                *++text = PUSH;
                expression(Add + 1);
                
                expr_type = tmp;
                if(expr_type > PTR){
                    // we need to take care pointer, and not 'char*'
                    // this if statement means we care about pinter plus !!
                    // If we have a int pointer int* ip here;
                    // We do ip = ip + 3;
                    // we need to add the point ip by 3 * 4 * (sizeof(int)) bytes
                    // And we didn't need to deal with char* here
                    // Because char* just 1 byte when it added.

                    *++text = PUSH;
                    *++text = IMM;
                    *++text = sizeof(int);
                    *++text = MUL;
                }
                *++text = ADD;
            }
            else if(token == Sub){
                // sub
                match(Sub);
                *++text = PUSH;
                expression(Mul);
                if(tmp > PTR && tmp == expr_type){
                    // same type pointer subtraction
                    *++text = SUB;
                    *++text = PUSH;
                    *++text = IMM;
                    *++text = sizeof(int);
                    *++text = DIV;
                    expr_type = INT;
                }
                else if(tmp > PTR){
                    // previous type is pointer and not 'char*'
                    // the second type is not the same type
                    *++text = PUSH;
                    *++text = IMM;
                    *++text = sizeof(int);
                    *++text = MUL;
                    *++text = SUB;
                    expr_type = tmp;
                }
                else{
                    //numeral subtractiom
                    *++text = SUB;
                    expr_type = tmp;
                }
            }
            else if (token == Mul) {
                // multiply
                match(Mul);
                *++text = PUSH;
                expression(Mul + 1);
                *++text = MUL;
                expr_type = tmp;
            }
            else if (token == Div) {
                // divide
                match(Div);
                *++text = PUSH;
                expression(Div + 1);
                *++text = DIV;
                expr_type = tmp;
            }
            else if (token == Mod) {
                // Modulo
                match(Mod);
                *++text = PUSH;
                expression(Mod + 1);
                *++text = MOD;
                expr_type = tmp;
            }
            else if(token == Inc || token == Dec){
                // postfix ++ or --
                // the previous token must be variable,
                // So we have to use LC or LI instruction to fetch the var.

                if (*text == LI) {
                    *text = PUSH;// push the address in stack to be used in later.
                    // it will be poped by '*++text = (expr_type == CHAR) ? SC : SI;' in later
                    *++text = LI;
                }
                else if (*text == LC) {
                    *text = PUSH;
                    *++text = LC;
                }
                else {
                    // if there is not a variable, there will be an error
                    printf("%d: bad value in increment\n", line);
                    exit(-1);
                }

                // there are two steps
                // 1. add or sub the value in variable address,
                // 2. sub or add the value in register 'ax'
                *++text = PUSH; // push number in stack 
                *++text = IMM;
                *++text = (expr_type > PTR) ? sizeof(int) : sizeof(char);
                *++text = (token == Inc) ? ADD : SUB; // pop the num in stack
                *++text = (expr_type == CHAR) ? SC : SI; // pop the address in stack
                *++text = PUSH;
                *++text = IMM;
                *++text = (expr_type > PTR) ? sizeof(int) : sizeof(char);
                *++text = (token == Inc) ? SUB : ADD;

                match(token);
            }
            else if(token == Brak){
                // '[' array access
                match(Brak);
                *++text = PUSH;
                // here we push the address which the pointer pointed.
                // We will use it later in future memory access
                expression(Assign);
                match(']');

                if (tmp > PTR) {
                    // pointer, `not char *`
                    *++text = PUSH;
                    // we push the index which we want to access.
                    *++text = IMM;
                    *++text = sizeof(int);
                    *++text = MUL;
                }
                else if (tmp < PTR) {
                    // num or char can not be accessed as an array.
                    printf("%d: pointer type expected\n", line);
                    exit(-1);
                }
                expr_type = tmp - PTR;
                *++text = ADD;
                *++text = (expr_type == CHAR) ? LC : LI;
            }
            else {
                printf("%d: compiler error, token = %d\n", line, token);
                exit(-1);
            }
        }
    }
}

void statement(){
    // there are 8 kinds of statements here:
    // 1. if (...) <statement> [else <statement>]
    // 2. while (...) <statement>
    // 3. { <statement> }
    // 4. return xxx;
    // 5. <empty statement>;
    // 6. expression; (expression end with semicolon)

    int *a, *b; // for control flow

    if(token == If){
        // if (expr) <statement> [else <statement>]
        //
        //  Layout:
        //      if (expr)           <cond>
        //                          JZ a
        //      <statement>         <statement>
        //      else:                JMP b
        // a:
        //      <statement>         <statement>
        // b:                       b:

        match(If);
        match('(');
        expression(Assign); // parse condition
        match(')');

        // emit code for it
        *++text = JZ;
        // There is a trick here, this pointer b not same as the b in above layout.
        // it means the next instruction address of the whole loop either there is Else or not.
        b = ++text;

        statement(); // parse statement later
        // but I am not very clear about how to deal with no control flow code later 
        // the '6. expression; (expression end with semicolon)' statement can dispose this situation.
        if(token == Else){
            // for else statement
            match(Else);

            *b = (int)(text + 3);
            *++text = JMP;
            b = ++text;

            statement();
        }

        *b = (int)(text + 1);
    }
    else if(token == While){
        //
        // a:                     a:
        //    while (<cond>)        <cond>
        //                          JZ b
        //     <statement>          <statement>
        //                          JMP a
        // b:                     b:
        match(While);

        a = text + 1;
        match('(');
        expression(Assign);
        match(')');

        *++text = JZ;
        b = ++text;

        statement();

        *++text = JMP;
        *++text = (int)a;
        *b = (int)(text + 1);
    }
    else if (token == '{') {
        // { <statement> ... }
        match('{');

        while (token != '}') {
            statement();
        }

        match('}');
    }
    else if (token == Return) {
        // return [expression];
        match(Return);

        if (token != ';') {
            expression(Assign);
        }

        match(';');

        // emit code for return
        *++text = LEV;
    }
    else if (token == ';') {
        // empty statement
        match(';');
    }
    else {
        // a = b; or function_call();
        expression(Assign);
        match(';');
    }
}