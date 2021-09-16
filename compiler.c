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
            // why '[' singer here? we didn't analyze ']'?
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