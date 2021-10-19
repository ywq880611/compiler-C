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
            // we need to resave this name cause this address store the src code !!
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
                // To be honest, I suppose this is just function call, not define??
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
                    // Why system function didn't have JMP instruction ??
                    // what's this id[Value] represents??
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

void enum_declaration() {
    // parse enum [id] { a = 1, b = 3, ...}
    int i;
    i = 0;
    while (token != '}') {
        if (token != Id) {
            printf("%d: bad enum identifier %d\n", line, token);
            exit(-1);
        }
        next();
        if (token == Assign) {
            // like {a=10}
            next();
            if (token != Num) {
                printf("%d: bad enum initializer\n", line);
                exit(-1);
            }
            i = token_val;
            next();
        }

        current_id[Class] = Num;
        current_id[Type] = INT;
        current_id[Value] = i++;

        if (token == ',') {
            next();
        }
    }
}


void function_parameter(){
    int type;
    int params;
    params = 0;
    while(token != ')'){
        // int or char type argument
        type = INT;
        if (token == Int) {
            match(Int);
        } else if (token == Char) {
            type = CHAR;
            match(Char);
        }

        // pointer argument
        while (token == Mul) {
            match(Mul);
            type = type + PTR;
        }

        // parameter name
        if (token != Id) {
            printf("%d: bad parameter declaration\n", line);
            exit(-1);
        }
        if (current_id[Class] == Loc) {
            printf("%d: duplicate parameter declaration\n", line);
            exit(-1);
        }

        match(Id);

        // store local variable
        // BClass means backup class,
        // if there is a global variable which has the same name as local variable
        // We will use backup to keep they original information
        // But we just support two level of nested now ??
        // I suppose maybe we can modify this by priority or linked list in the future ??

        // if there is no global variable here, the value in Class and BClass are both null,
        // after the function call we will recovery the Class and other information to null
        // The current_id will be a empty ID, we can use it to store another ID again.
        current_id[BClass] = current_id[Class]; current_id[Class]  = Loc;
        current_id[BType]  = current_id[Type];  current_id[Type]   = type;
        current_id[BValue] = current_id[Value]; current_id[Value]  = params++;   // index of current parameter

        if (token == ',') {
            match(',');
        }
    }
    index_of_bp = params+1;
}

void function_body(){
    // type func_name (...) {...}
    //                   -->|   |<--

    // ... {
    // 1. local declarations
    // 2. statements
    // }

    int pos_local; // position of local variables on the stack.
    int type;
    pos_local = index_of_bp; // we get it from above function_parameter() function

    while (token == Int || token == Char) {
        // local variable declaration, just like global ones.
        basetype = (token == Int) ? INT : CHAR;
        match(token);

        while (token != ';') {
            type = basetype;
            while (token == Mul) {
                match(Mul);
                type = type + PTR;
            }

            if (token != Id) {
                // invalid declaration
                printf("%d: bad local declaration\n", line);
                exit(-1);
            }
            if (current_id[Class] == Loc) {
                // identifier exists

                // what's this mean ??
                // If there is another function which has a parameter ID called A
                // In this function we hava a parameter ID called A too
                // Will we fail in compile ??

                // I am not very clear about this configuration.
                // This variable is not parameter, it the local variable in this function body !!
                // it is not same as the argument.
                printf("%d: duplicate local declaration\n", line);
                exit(-1);
            }
            match(Id);

            // store the local variable
            current_id[BClass] = current_id[Class]; current_id[Class]  = Loc;
            current_id[BType]  = current_id[Type];  current_id[Type]   = type;
            current_id[BValue] = current_id[Value]; current_id[Value]  = ++pos_local;   // index of current parameter

            if (token == ',') {
                match(',');
            }
        }
        match(';');
    }

    // save the address and allocate the size for local variable !!
    *++text = ENT;
    *++text = pos_local - index_of_bp;

    // statements
    while (token != '}') {
        statement();
    }

    // emit code for leaving the sub function
    *++text = LEV;
}

void function_declaration(){
    // type func_name (...) {...}
    //               | this part

    match('(');
    function_parameter();
    match(')');
    match('{');
    function_body();

    // unwind local variable to global variable
    current_id = symbols;
    while (current_id[Token]) {
        if (current_id[Class] == Loc) {
            current_id[Class] = current_id[BClass];
            current_id[Type]  = current_id[BType];
            current_id[Value] = current_id[BValue];
        }
        current_id = current_id + IdSize;
    }
}

void global_declaration(){
    // type [*] id [; | (...) {...}]

    int type;
    int i;

    basetype = INT;

    // parse enum, this should be treated alone.
    if (token == Enum) {
        // enum [id] { a = 10, b = 20, ... }
        match(Enum);
        if (token != '{') {
            match(Id); // skip the [id] part
        }
        if (token == '{') {
            // parse the assign part
            match('{');
            enum_declaration();
            match('}');
        }

        match(';');
        return;
    }

    // parse type information
    if (token == Int) {
        match(Int);
    }
    else if (token == Char) {
        match(Char);
        basetype = CHAR;
    }

    // parse the comma seperated variable declaration.
    while (token != ';' && token != '}') {
        type = basetype;
        // parse pointer type, note that there may exist `int ****x;`
        while (token == Mul) {
            match(Mul);
            type = type + PTR;
        }

        if (token != Id) {
            // invalid declaration
            printf("%d: bad global declaration\n", line);
            exit(-1);
        }
        if (current_id[Class]) {
            // identifier exists
            printf("%d: duplicate global declaration\n", line);
            exit(-1);
        }
        match(Id);
        current_id[Type] = type;

        if (token == '(') {
            current_id[Class] = Fun;
            current_id[Value] = (int)(text + 1); // the memory address of function
            function_declaration();
        } else {
            // variable declaration
            current_id[Class] = Glo; // global variable
            current_id[Value] = (int)data; // assign memory address
            data = data + sizeof(int);
        }

        if (token == ',') {
            match(',');
        }
    }
    next();
}

void program() {
    // Parse all global declaration.

    // get next token
    next();
    while (token > 0) {
        global_declaration();
    }
}

int eval(){
    int op, *tmp;
    cycle = 0;
    while(1){
        cycle ++;
        op = *pc++; // get opcode of next instruction

        // print debug info
        if (debug) {
            printf("%d> %.4s", cycle,
                   & "LEA ,IMM ,JMP ,CALL,JZ  ,JNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PUSH,"
                   "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
                   "OPEN,READ,CLOS,PRTF,MALC,MSET,MCMP,EXIT"[op * 5]);
            if (op <= ADJ)
                printf(" %d\n", *pc);
            else
                printf("\n");
        }

        if (op == IMM)       {ax = *pc++;}                                     // load immediate value to ax
        else if (op == LC)   {ax = *(char *)ax;}                               // load character to ax, address in ax
        else if (op == LI)   {ax = *(int *)ax;}                                // load integer to ax, address in ax
        else if (op == SC)   {ax = *(char *)*sp++ = ax;}                       // save character to address, value in ax, address on stack
        else if (op == SI)   {*(int *)*sp++ = ax;}                             // save integer to address, value in ax, address on stack
        else if (op == PUSH) {*--sp = ax;}                                     // push the value of ax onto the stack
        else if (op == JMP)  {pc = (int *)*pc;}                                // jump to the address
        else if (op == JZ)   {pc = ax ? pc + 1 : (int *)*pc;}                   // jump if ax is zero
        else if (op == JNZ)  {pc = ax ? (int *)*pc : pc + 1;}                   // jump if ax is not zero
        else if (op == CALL) {*--sp = (int)(pc+1); pc = (int *)*pc;}           // call subroutine
        //else if (op == RET)  {pc = (int *)*sp++;}                              // return from subroutine;
        else if (op == ENT)  {*--sp = (int)bp; bp = sp; sp = sp - *pc++;}      // make new stack frame
        else if (op == ADJ)  {sp = sp + *pc++;}                                // add esp, <size>
        else if (op == LEV)  {sp = bp; bp = (int *)*sp++; pc = (int *)*sp++;}  // restore call frame and PC
        else if (op == LEA)  {ax = (int)(bp + *pc++);}                         // load address for arguments.

        else if (op == OR)  ax = *sp++ | ax;
        else if (op == XOR) ax = *sp++ ^ ax;
        else if (op == AND) ax = *sp++ & ax;
        else if (op == EQ)  ax = *sp++ == ax;
        else if (op == NE)  ax = *sp++ != ax;
        else if (op == LT)  ax = *sp++ < ax;
        else if (op == LE)  ax = *sp++ <= ax;
        else if (op == GT)  ax = *sp++ >  ax;
        else if (op == GE)  ax = *sp++ >= ax;
        else if (op == SHL) ax = *sp++ << ax;
        else if (op == SHR) ax = *sp++ >> ax;
        else if (op == ADD) ax = *sp++ + ax;
        else if (op == SUB) ax = *sp++ - ax;
        else if (op == MUL) ax = *sp++ * ax;
        else if (op == DIV) ax = *sp++ / ax;
        else if (op == MOD) ax = *sp++ % ax;

        else if (op == EXIT) { printf("exit(%d)", *sp); return *sp;}
        else if (op == OPEN) { ax = open((char *)sp[1], sp[0]); }
        else if (op == CLOS) { ax = close(*sp);}
        else if (op == READ) { ax = read(sp[2], (char *)sp[1], *sp); }
        else if (op == PRTF) { tmp = sp + pc[1]; ax = printf((char *)tmp[-1], tmp[-2], tmp[-3], tmp[-4], tmp[-5], tmp[-6]); }
        else if (op == MALC) { ax = (int)malloc(*sp);}
        else if (op == MSET) { ax = (int)memset((char *)sp[2], sp[1], *sp);}
        else if (op == MCMP) { ax = memcmp((char *)sp[2], (char *)sp[1], *sp);}
        else {
            printf("unknown instruction:%d\n", op);
            return -1;
        }
    }
}

#undef int // Mac/clang needs this to compile

int main(int argc, char **argv)
{
    #define int long long // to work with 64bit address

    int i, fd;
    int *tmp;

    argc--;
    argv++;

    // parse arguments
    // if there is -d -s ??
    // we may ingore -s ??
    if (argc > 0 && **argv == '-' && (*argv)[1] == 's') {
        assembly = 1;
        --argc;
        ++argv;
    }
    if (argc > 0 && **argv == '-' && (*argv)[1] == 'd') {
        debug = 1;
        --argc;
        ++argv;
    }
    if (argc < 1) {
        printf("usage: xc [-s] [-d] file ...\n");
        return -1;
    }

    if ((fd = open(*argv, 0)) < 0) {
        printf("could not open(%s)\n", *argv);
        return -1;
    }

    poolsize = 256 * 1024; // arbitrary size
    line = 1;

    // allocate memory
    if (!(text = malloc(poolsize))) {
        printf("could not malloc(%d) for text area\n", poolsize);
        return -1;
    }
    if (!(data = malloc(poolsize))) {
        printf("could not malloc(%d) for data area\n", poolsize);
        return -1;
    }
    if (!(stack = malloc(poolsize))) {
        printf("could not malloc(%d) for stack area\n", poolsize);
        return -1;
    }
    if (!(symbols = malloc(poolsize))) {
        printf("could not malloc(%d) for symbol table\n", poolsize);
        return -1;
    }

    memset(text, 0, poolsize);
    memset(data, 0, poolsize);
    memset(stack, 0, poolsize);
    memset(symbols, 0, poolsize);

    old_text = text;

    // define keyword in symbol table firstly.
    src = "char else enum if int return sizeof while "
          "open read close printf malloc memset memcmp exit void main";

     // add keywords to symbol table
    i = Char;
    while (i <= While) {
        next();
        current_id[Token] = i++;
    }

    // add library to symbol table
    i = OPEN;
    while (i <= EXIT) {
        next();
        current_id[Class] = Sys;
        current_id[Type] = INT;
        current_id[Value] = i++;
    }

    next(); current_id[Token] = Char; // handle void type
    next(); idmain = current_id; // keep track of main

    if (!(src = old_src = malloc(poolsize))) {
        printf("could not malloc(%d) for source area\n", poolsize);
        return -1;
    }
    // read the source file
    if ((i = read(fd, src, poolsize-1)) <= 0) {
        printf("read() returned %d\n", i);
        return -1;
    }
    src[i] = 0; // add EOF character
    close(fd);

    program();

    if (!(pc = (int *)idmain[Value])) {
        printf("main() not defined\n");
        return -1;
    }

    // dump_text();
    if (assembly) {
        // only for compile
        return 0;
    }

    // setup stack
    sp = (int *)((int)stack + poolsize);
    *--sp = EXIT; // call exit if main returns
    *--sp = PUSH; tmp = sp;
    *--sp = argc;
    *--sp = (int)argv;
    *--sp = (int)tmp;

    return eval();
}
