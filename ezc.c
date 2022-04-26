#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/******************************************************************************/
/**                                  UTILS                                   **/
/******************************************************************************/

#define MALLOC_TYPE(_Type_) (_Type_ *)malloc(sizeof(_Type_))

#include <stdarg.h>

void
error(char *fmt, ...)
{
    va_list ap;

    printf("[!] ERROR: ");
    va_start(ap, fmt);
    vprintf(fmt, ap);
    va_end(ap);
    printf("\n");
}

void
fatal(char *fmt, ...)
{
    va_list ap;

    printf("[!] ERROR: ");
    va_start(ap, fmt);
    vprintf(fmt, ap);
    va_end(ap);
    printf("\n");

    exit(1);
}

/******************************************************************************/
/**                            STRING INTERNING                              **/
/******************************************************************************/

#include <stddef.h> /* offsetof macro */

typedef struct
StrInterned
{
    struct StrInterned *next;
    int len;
    char str[1];
} StrInterned;

StrInterned *str_intern_table;

StrInterned *
str_intern_create(char *s)
{
    StrInterned *res;
    int s_len = strlen(s);
    res = (StrInterned *)malloc(sizeof(StrInterned) + s_len);
    res->next = 0;
    res->len = s_len;
    strcpy(res->str, s);
    res->str[s_len] = 0;
    return(res);
}

void
str_intern_add(StrInterned *si)
{
    StrInterned *s;

    if(str_intern_table)
    {
        s = str_intern_table;
        while(s->next)
        {
            s = s->next;
        }
        s->next = si;
    }
    else
    {
        str_intern_table = si;
    }
}

int
str_intern_len(char *s)
{
    int res = 0;
    if(s)
    {
        StrInterned *tmp = (StrInterned *)(s - offsetof(StrInterned, str));
        res = tmp->len;
    }
    else
    {
        res = 0;
    }

    return(res);
}

char *
str_intern(char *s)
{
    char *res = 0;
    int found;
    StrInterned *ptr;
    int s_len;

    if(s && *s)
    {
        found = 0;
        ptr = str_intern_table;
        s_len = strlen(s);
        while(!found && ptr)
        {
            found = (ptr->len == s_len && strcmp(ptr->str, s) == 0);
            if(!found)
            {
                ptr = ptr->next;
            }
        }

        if(!found)
        {
            ptr = str_intern_create(s);
            str_intern_add(ptr);
        }

        res = (char *)ptr->str;
    }
    else
    {
        res = 0;
    }

    return(res);
}

/******************************************************************************/
/**                                 TYPES                                    **/
/******************************************************************************/

/*
 * Types:
 *
 * We have 4 built-in types (for now): void, char, int, pointer.
 * Upon these 4 types other composed types can be built (structs, unions).
 *
 * We use type uniquing (you can't have 2 different (int*), they must be
 * the same. In this way we can compare two types by doing a simple ==.
 * 
 */

enum
{
    TYPE_VOID,
    TYPE_CHAR,
    TYPE_INT,
    TYPE_PTR,

    TYPE_COUNT
};

typedef struct
Type
{
    int kind;
    int size;
    struct Type *base_type;
} Type;

Type _type_void = { TYPE_VOID, 0, 0 };
Type _type_char = { TYPE_CHAR, 1, 0 };
Type _type_int  = { TYPE_INT,  4, 0 };

Type *
type_void()
{
    return(&_type_void);
}

Type *
type_char()
{
    return(&_type_char);
}

Type *
type_int()
{
    return(&_type_int);
}

#define TYPE_PTR_CACHE_SIZE 1000
Type type_ptr_cache[TYPE_PTR_CACHE_SIZE];
int type_ptr_cache_count = 0;

Type *
type_ptr(Type *base_type)
{
    Type *res = 0;
    int i;

    for(i = 0;
        i < type_ptr_cache_count;
        ++i)
    {
        if(type_ptr_cache[i].base_type == base_type)
        {
            res = &(type_ptr_cache[i]);
            break;
        }
    }

    if (!res)
    {
        res = &(type_ptr_cache[type_ptr_cache_count]);
        ++type_ptr_cache_count;
        res->kind = TYPE_PTR;
        res->size = 4;
        res->base_type = base_type;
    }

    return(res);
}

/******************************************************************************/
/**                                SYM TABLE                                 **/
/******************************************************************************/

typedef struct
{
    char *id;
    Type *type;
    int global;
    int offset;
} Sym;

#define SYM_TABLE_SIZE 1000
Sym sym_table[SYM_TABLE_SIZE];
int sym_table_count = 0;

Sym *
sym_add(char *id, Type *type)
{
    Sym *res = 0;

    res = &(sym_table[sym_table_count]);
    ++sym_table_count;
    res->id = id;
    res->type = type;
    res->global = 0;
    res->offset = 0;

    return(res);
}

Sym *
sym_get(char *id)
{
    Sym *res = 0;
    int i;

    for(i = 0;
        i < sym_table_count;
        ++i)
    {
        if(id == sym_table[i].id)
        {
            res = &(sym_table[i]);
            break;
        }
    }

    return(res);
}

/******************************************************************************/
/**                                  AST                                     **/
/******************************************************************************/

enum
{
    EXPR_INTLIT,
    EXPR_ID,

    EXPR_NEG,

    EXPR_MUL,
    EXPR_DIV,
    EXPR_MOD,
    EXPR_ADD,
    EXPR_SUB,

    EXPR_COUNT
};

typedef struct
Expr
{
    int kind;
    int value;
    char *id;
    struct Expr *l;
    struct Expr *r;
} Expr;

Expr *
make_expr_intlit(int value)
{
    Expr *res = 0;

    res = MALLOC_TYPE(Expr);
    if(res)
    {
        res->kind = EXPR_INTLIT;
        res->value = value;
    }

    return(res);
}

Expr *
make_expr_id(char *id)
{
    Expr *res = 0;

    res = MALLOC_TYPE(Expr);
    if(res)
    {
        res->kind = EXPR_ID;
        res->id = id;
    }

    return(res);
}

Expr *
make_expr_unary(int kind, Expr *l)
{
    Expr *res = 0;

    res = MALLOC_TYPE(Expr);
    if(res)
    {
        res->kind = kind;
        res->l = l;
        res->r = 0;
    }

    return(res);
}

Expr *
make_expr_binary(int kind, Expr *l, Expr *r)
{
    Expr *res = 0;

    res = MALLOC_TYPE(Expr);
    if(res)
    {
        res->kind = kind;
        res->l = l;
        res->r = r;
    }

    return(res);
}

void
print_expr(Expr *expr)
{
    char *op = 0;
    switch(expr->kind)
    {
        case EXPR_ID:
        {
            printf("%s", expr->id);
        } break;

        case EXPR_INTLIT:
        {
            printf("%d", expr->value);
        } break;

        case EXPR_MUL: { op = "*"; } break;
        case EXPR_DIV: { op = "/"; } break;
        case EXPR_MOD: { op = "%"; } break;
        case EXPR_ADD: { op = "+"; } break;
        case EXPR_SUB: { op = "-"; } break;

        default:
        {
            fatal("Invalid expression");
        } break;
    }

    if(op)
    {
        printf("(%s ", op);
        print_expr(expr->l);
        printf(" ");
        print_expr(expr->r);
        printf(")");
    }
}

typedef struct
Decl
{
    Type *type;
    char *id;
} Decl;

Decl *
make_decl(Type *type, char *id)
{
    Decl *res = 0;

    res = MALLOC_TYPE(Decl);
    if(res)
    {
        res->type = type;
        res->id = id;
    }

    return(res);
}

void
print_type(Type *type)
{
    if(type == type_void())
    {
        printf("void");
    }
    else if(type == type_char())
    {
        printf("char");
    }
    else if(type == type_int())
    {
        printf("int");
    }
    else if(type->kind == TYPE_PTR)
    {
        printf("ptr to ");
        print_type(type->base_type);
    }
    else
    {
        fatal("Invalid type to print");
    }
}

void
print_decl(Decl *decl)
{
    printf("(var %s ", decl->id);
    print_type(decl->type);
    printf(")");
}

enum
{
    STMT_DECL,
    STMT_EXPR,
    STMT_BLOCK,

    STMT_COUNT
};

typedef struct
Stmt
{
    int kind;
    union
    {
        Decl *decl;
        Expr *expr;
        struct Stmt *block;
    } u;
    struct Stmt *next;
} Stmt;

Stmt *
make_stmt(int kind)
{
    Stmt *res = 0;

    res = MALLOC_TYPE(Stmt);
    if(res)
    {
        res->kind = kind;
        res->next = 0;
    }

    return(res);
}

void
print_stmt(Stmt *stmt)
{
    Stmt *sub_stmt;

    switch(stmt->kind)
    {
        case STMT_DECL:
        {
            print_decl(stmt->u.decl);
        } break;

        case STMT_EXPR:
        {
            print_expr(stmt->u.expr);
        } break;

        case STMT_BLOCK:
        {
            printf("(\n");
            sub_stmt = stmt->u.block;
            while(sub_stmt)
            {
                printf("  ");
                print_stmt(sub_stmt);
                sub_stmt = sub_stmt->next;
            }
            printf(")");
        } break;

        default:
        {
            fatal("Invalid statement to print");
        } break;
    }

    printf("\n");
}

enum
{
    GLOB_DECL_VAR,
    GLOB_DECL_FUNC,

    GLOB_DECL_COUNT
};

typedef struct
GlobDecl
{
    int kind;
    struct GlobDecl *next;
    char *id;
    Type *type;
    Stmt *func_def;
} GlobDecl;

GlobDecl *
make_glob_decl(int kind)
{
    GlobDecl *res = 0;

    res = MALLOC_TYPE(GlobDecl);
    if(res)
    {
        res->kind = kind;
        res->next = 0;
    }

    return(res);
}

GlobDecl *
make_glob_decl_var(char *id, Type *type)
{
    GlobDecl *res = make_glob_decl(GLOB_DECL_VAR);

    if(res)
    {
        res->id = id;
        res->type = type;
    }

    return(res);
}

GlobDecl *
make_glob_decl_func(char *id, Type *type, Stmt *func_def)
{
    GlobDecl *res = make_glob_decl(GLOB_DECL_FUNC);

    if(res)
    {
        res->id = id;
        res->type = type;
        res->func_def = func_def;
    }

    return(res);
}

void
print_glob_decl(GlobDecl *decl)
{
    switch(decl->kind)
    {
        case GLOB_DECL_VAR:
        {
            printf("(var %s ", decl->id);
            print_type(decl->type);
            printf(")");
        } break;

        case GLOB_DECL_FUNC:
        {
            printf("func %s ", decl->id);
            print_type(decl->type);
            printf("\n");
            print_stmt(decl->func_def);
        } break;

        default:
        {
            fatal("Invalid global declaration to print");
        } break;
    }

    printf("\n");
}

void
print_unit(GlobDecl *unit)
{
    GlobDecl *curr;

    curr = unit;
    while(curr)
    {
        print_glob_decl(curr);
        curr = curr->next;
    }
}

/******************************************************************************/
/**                                 PARSER                                   **/
/******************************************************************************/

#include <ctype.h>
#include <assert.h>

char *source;
int source_line;

int func_var_offset;

char *kword_int;

#include <stdarg.h>

void
syntax_error(char *fmt, ...)
{
    va_list ap;

    printf("[!] ERROR: Line %d: ", source_line);
    va_start(ap, fmt);
    vprintf(fmt, ap);
    va_end(ap);
    printf("\n");
}

void
syntax_fatal(char *fmt, ...)
{
    va_list ap;

    printf("[!] ERROR: Line %d: ", source_line);
    va_start(ap, fmt);
    vprintf(fmt, ap);
    va_end(ap);
    printf("\n");

    exit(1);
}

void
parser_init(char *src)
{
    source = src;
    source_line = 1;

    kword_int = str_intern("int");
}

enum
{
    TOK_EOF,

    TOK_ID,
    TOK_INTLIT,

    TOK_LPAREN,
    TOK_RPAREN,
    TOK_LBRACE,
    TOK_RBRACE,

    TOK_SEMI,

    /* Binary operators */
    TOK_BIN_OP,

    TOK_STAR = TOK_BIN_OP,
    TOK_SLASH,
    TOK_PERCENT,
    TOK_PLUS,
    TOK_MINUS,

    TOK_BIN_OP_END,

    TOK_KW_INT,

    TOK_COUNT
};

#define MAX_ID_LEN 33

typedef struct Token
{
    int kind;
    int value;
    char *id;
} Token;

Token
_tok_next(int update_source)
{
    Token tok;
    char *src = source;
    int srcl = source_line;

    if(!*src)
    {
        tok.kind = TOK_EOF;
    }
    else
    {
        while(isspace(*src))
        {
            if(*src == '\n')
            {
                ++srcl;
            }
            ++src;
        }

        switch(*src)
        {
            case '_':
            case 'a':case 'b':case 'c':case 'd':case 'e':case 'f':case 'g':
            case 'h':case 'i':case 'j':case 'k':case 'l':case 'm':case 'n':
            case 'o':case 'p':case 'q':case 'r':case 's':case 't':case 'u':
            case 'v':case 'w':case 'x':case 'y':case 'z':
            case 'A':case 'B':case 'C':case 'D':case 'E':case 'F':case 'G':
            case 'H':case 'I':case 'J':case 'K':case 'L':case 'M':case 'N':
            case 'O':case 'P':case 'Q':case 'R':case 'S':case 'T':case 'U':
            case 'V':case 'W':case 'X':case 'Y':case 'Z':
            {
                char buff[MAX_ID_LEN];
                int buffsize = 0;

                tok.kind = TOK_ID;

                while(isalnum(*src) || *src == '_')
                {
                    buff[buffsize++] = *src;
                    ++src;
                    assert(buffsize < MAX_ID_LEN);
                }
                buff[buffsize] = 0;

                tok.id = str_intern(buff);

                if(tok.id == kword_int) { tok.kind = TOK_KW_INT; }
            } break;

            case '0':case '1':case '2':case '3':case '4':
            case '5':case '6':case '7':case '8':case '9':
            {
                tok.kind = TOK_INTLIT;

                /* TODO: Hexadecimal */
                tok.value = 0;
                while(isdigit(*src))
                {
                    tok.value = tok.value*10 + (*src - '0');
                    ++src;
                }
            } break;

            case '(': { ++src; tok.kind = TOK_LPAREN; } break;
            case ')': { ++src; tok.kind = TOK_RPAREN; } break;
            case '{': { ++src; tok.kind = TOK_LBRACE; } break;
            case '}': { ++src; tok.kind = TOK_RBRACE; } break;

            case ';': { ++src; tok.kind = TOK_SEMI; } break;

            case '*': { ++src; tok.kind = TOK_STAR; } break;
            case '/': { ++src; tok.kind = TOK_SLASH; } break;
            case '%': { ++src; tok.kind = TOK_PERCENT; } break;
            case '+': { ++src; tok.kind = TOK_PLUS; } break;
            case '-': { ++src; tok.kind = TOK_MINUS; } break;

            default:
            {
                syntax_fatal("Invalid token");
            } break;
        }
    }

    if(update_source)
    {
        source = src;
        source_line = srcl;
    }

    return(tok);
}

Token
tok_peek()
{
    return(_tok_next(0));
}

Token
tok_next()
{
    return(_tok_next(1));
}

Token
tok_expect(int tok_kind)
{
    Token tok;
    tok = tok_next();
    assert(tok.kind == tok_kind);
    return(tok);
}

int
tok_is_type(Token tok)
{
    int is_type = 0;

    switch(tok.kind)
    {
        case TOK_KW_INT:
        {
            is_type = 1;
        } break;

        default:
        {
            is_type = 0;
        } break;
    }

    return(is_type);
}

/*
 * <expr> ::= <expr_binary>
 *
 * <expr_binary> ::= <expr_binary> <bin_op> <expr_unary>
 *                 | <expr_unary>
 *
 * <expr_unary> ::= <un_op> <expr_unary>
 *                | <expr_base>
 *
 * <expr_base> ::= '(' <expr> ')'
 *               | <int_lit>
 *               | <ident>
 *
 * <bin_op> ::= [/ *]
 *            | [-+]
 * <un_op> ::= [-+]
 */

Expr *parse_expr();

Expr *
parse_expr_base()
{
    Expr *expr = 0;
    Token tok;

    tok = tok_next();
    switch(tok.kind)
    {
        case TOK_INTLIT:
        {
            expr = make_expr_intlit(tok.value);
        } break;

        case TOK_ID:
        {
            expr = make_expr_id(tok.id);
        } break;

        case TOK_LPAREN:
        {
            expr = parse_expr();
            tok_expect(TOK_RPAREN);
        } break;

        default:
        {
            syntax_fatal("Invalid base expression");
        } break;
    }

    return(expr);
}

Expr *
parse_expr_unary()
{
    Expr *expr = 0;
    Token tok;

    tok = tok_peek();
    switch(tok.kind)
    {
        case TOK_MINUS:
        {
            tok_next();
            expr = parse_expr_unary();
            expr = make_expr_unary(EXPR_NEG, expr);
        } break;

        case TOK_PLUS:
        {
            tok_next();
            expr = parse_expr_base();
        } break;

        default:
        {
            expr = parse_expr_base();
        } break;
    }

    return(expr);
}

int op_precedence_table[TOK_BIN_OP_END - TOK_BIN_OP] = {
    0, /* TOK_STAR */
    0, /* TOK_SLASH */
    0, /* TOK_PERCENT */
    1, /* TOK_PLUS */
    1, /* TOK_MINUS */
};

Expr *
parse_expr_binary(int precedence)
{
    Expr *l = 0;
    Expr *r = 0;
    Token tok;
    int new_precedence;

    l = parse_expr_unary();
    tok = tok_peek();
    while(tok.kind >= TOK_BIN_OP && tok.kind < TOK_BIN_OP_END)
    {
        new_precedence = op_precedence_table[tok.kind - TOK_BIN_OP];
        if(new_precedence < precedence)
        {
            tok_next();
            r = parse_expr_binary(new_precedence);
        }
        else
        {
            break;
        }

        switch(tok.kind)
        {
            case TOK_STAR:    { l = make_expr_binary(EXPR_MUL, l, r); } break;
            case TOK_SLASH:   { l = make_expr_binary(EXPR_DIV, l, r); } break;
            case TOK_PERCENT: { l = make_expr_binary(EXPR_MOD, l, r); } break;
            case TOK_PLUS:    { l = make_expr_binary(EXPR_ADD, l, r); } break;
            case TOK_MINUS:   { l = make_expr_binary(EXPR_SUB, l, r); } break;

            default:
            {
                syntax_fatal("Invalid binary expression");
            } break;
        }

        tok = tok_peek();
    }

    return(l);
}

Expr *
parse_expr()
{
    return(parse_expr_binary(999));
}

/*
 * <decl> ::= <type> <ident> ';'
 */

Type *
get_base_type()
{
    Type *type = 0;
    Token tok;

    tok = tok_peek();
    switch(tok.kind)
    {
        case TOK_KW_INT:
        {
            tok_next();
            type = type_int();
        } break;
    }

    return(type);
}

Type *
parse_type(Type *base_type)
{
    Type *type = 0;
    Token tok;

    type = base_type;

    tok = tok_peek();
    while(tok.kind == TOK_STAR)
    {
        tok_next();
        type = type_ptr(type);
        tok = tok_peek();
    }

    return(type);
}

Decl *
parse_decl()
{
    Decl *decl;
    Type *type;
    Token tok;
    Sym *sym;

    type = get_base_type();
    if(!type)
    {
        syntax_fatal("Invalid type for variable declaration");
    }

    type = parse_type(type);

    tok = tok_expect(TOK_ID);

    decl = make_decl(type, tok.id);

    sym = sym_add(tok.id, type);
    sym->global = 0;
    sym->offset = func_var_offset;
    func_var_offset += type->size;

    /* TODO: Parse variable initialization */

    tok_expect(TOK_SEMI);

    return(decl);
}

/*
 * <stmt> ::= <decl>
 *          | <expr>
 *          | <stmt_block>
 */

Stmt *parse_stmt();

Stmt *
parse_stmt_block()
{
    Stmt *stmt;
    Stmt *sub_stmt;
    Token tok;

    tok_expect(TOK_LBRACE);

    stmt = make_stmt(STMT_BLOCK);

    tok = tok_peek();
    sub_stmt = 0;
    while(tok.kind != TOK_RBRACE)
    {
        if(sub_stmt)
        {
            sub_stmt->next = parse_stmt();
            sub_stmt = sub_stmt->next;
        }
        else
        {
            sub_stmt = parse_stmt();
            stmt->u.block = sub_stmt;
        }
        tok = tok_peek();
    }

    tok_expect(TOK_RBRACE);

    return(stmt);
}

Stmt *
parse_stmt()
{
    Stmt *stmt;
    Token tok;

    tok = tok_peek();
    while(tok.kind == TOK_SEMI)
    {
        tok_next();
        tok = tok_peek();
    }

    if(tok_is_type(tok))
    {
        stmt = make_stmt(STMT_DECL);
        stmt->u.decl = parse_decl();
    }
    else
    {
        switch(tok.kind)
        {
            case TOK_LBRACE:
            {
                stmt = parse_stmt_block();
            } break;

            default:
            {
                stmt = make_stmt(STMT_EXPR);
                stmt->u.expr = parse_expr();
                tok_expect(TOK_SEMI);
            } break;
        }
    }

    return(stmt);
}

/*
 * <glob_decl> ::= <type> <ident> ['(' <func_params> ')' <stmt_block>]?
 */

GlobDecl *
parse_glob_decl()
{
    GlobDecl *glob_decl = 0;

    Type *type;
    Token tok;
    char *id;
    Sym *sym;

    type = get_base_type();
    if(!type)
    {
        syntax_fatal("Invalid type for global declaration");
    }

    type = parse_type(type);

    tok = tok_expect(TOK_ID);
    id = tok.id;

    tok = tok_peek();
    if(tok.kind == TOK_SEMI)
    {
        tok_expect(TOK_SEMI);
        glob_decl = make_glob_decl_var(id, type);

        sym = sym_add(id, type);
        sym->global = 1;
    }
    else
    {
        tok_expect(TOK_LPAREN);
        /* TODO: Parse func params */
        tok_expect(TOK_RPAREN);

        func_var_offset = 0;
        glob_decl = make_glob_decl_func(id, type, parse_stmt_block());
    }

    return(glob_decl);
}

/*
 * <unit> ::= <glob_decl>*
 */

GlobDecl *
parse_unit()
{
    GlobDecl *first = 0;
    GlobDecl *last = 0;
    Token tok;

    tok = tok_peek();
    while(tok.kind != TOK_EOF)
    {
        if(last)
        {
            last->next = parse_glob_decl();
            last = last->next;
        }
        else
        {
            last = parse_glob_decl();
        }

        if(!first)
        {
            first = last;
        }

        tok = tok_peek();
    }

    return(first);
}

/******************************************************************************/
/**                                   IRC                                    **/
/******************************************************************************/

#include <assert.h>

int lbl_count = 0;

void
lbl_gen(char *lbl)
{
    int n;

    lbl[0] = '.';
    lbl[1] = 'L';
    n = sprintf(lbl+2, "%d", lbl_count);
    lbl[n+2] = 0;
    ++lbl_count;
}

enum
{
    IRC_SYM_FUNC,
    IRC_SYM_VAR_INT,
    IRC_SYM_VAR_PTR,

    IRC_SYM_TYPES_COUNT
};

void
compile_expr(FILE *fout, Expr *expr)
{
    Sym *sym = 0;

    switch(expr->kind)
    {
        case EXPR_INTLIT:
        {
            fprintf(fout, "\tmovl $%d,%%eax\n", expr->value);
        } break;

        case EXPR_ID:
        {
            sym = sym_get(expr->id);
            if(!sym)
            {
                fatal("Invalid symbol");
            }

            /* TODO: Only ints for now */
            assert(sym->type == type_int());

            if(sym->global)
            {
                fprintf(fout, "\tmovl %s,%%ebx\n", sym->id);
            }
            else
            {
                fprintf(fout, "\tmovl %d(%%ebp),%%ebx\n", sym->offset);
            }
            fprintf(fout, "\tmovl (%%ebx),%%eax\n");
        } break;

        case EXPR_MUL:
        {
            compile_expr(fout, expr->r);
            fprintf(fout, "\tmovl %%eax,%%ecx\n");
            compile_expr(fout, expr->l);
            fprintf(fout, "\timull %%ecx,%%eax\n");
        } break;

        case EXPR_DIV:
        {
            compile_expr(fout, expr->r);
            fprintf(fout, "\tmovl %%eax,%%ecx\n");
            compile_expr(fout, expr->l);
            fprintf(fout, "\tidivl %%ecx\n");
        } break;

        case EXPR_ADD:
        {
            compile_expr(fout, expr->r);
            fprintf(fout, "\tmovl %%eax,%%ecx\n");
            compile_expr(fout, expr->l);
            fprintf(fout, "\taddl %%ecx,%%eax\n");
        } break;

        case EXPR_SUB:
        {
            compile_expr(fout, expr->r);
            fprintf(fout, "\tmovl %%eax,%%ecx\n");
            compile_expr(fout, expr->l);
            fprintf(fout, "\tsubl %%ecx,%%eax\n");
        } break;

        default:
        {
            fatal("Invalid expression");
        } break;
    }
}

void
compile_decl(FILE *fout, Decl *decl)
{
    assert(decl->type && decl->type->size > 0);
    fprintf(fout, "\tsubl %%esp,%d\n", decl->type->size);
}

void
compile_stmt(FILE *fout, Stmt *stmt)
{
    /* TODO: What about the scope */
    /*int scope;*/
    Stmt *substmt;

    switch(stmt->kind)
    {
        case STMT_DECL:
        {
            compile_decl(fout, stmt->u.decl);
        } break;

        case STMT_EXPR:
        {
            compile_expr(fout, stmt->u.expr);
        } break;

        case STMT_BLOCK:
        {
            /*scope = sym_table_count;*/
            substmt = stmt->u.block;
            while(substmt)
            {
                compile_stmt(fout, substmt);
                substmt = substmt->next;
            }
            /*sym_table_count = scope;*/
        } break;

        default:
        {
            fatal("Invalid statement");
        } break;
    }
}

void
compile_glob_decl(FILE *fout, GlobDecl *decl)
{
    switch(decl->kind)
    {
        case GLOB_DECL_VAR:
        {
            fprintf(fout, "%s:\n", decl->id);
            fprintf(fout, "\t.zero $%d\n", decl->type->size);
        } break;

        case GLOB_DECL_FUNC:
        {
            fprintf(fout, "%s:\n", decl->id);
            fprintf(fout, "\tpushl %%ebp\n");
            fprintf(fout, "\tmovl %%esp,%%ebp\n");
            compile_stmt(fout, decl->func_def);
            fprintf(fout, "\tleave\n");
            fprintf(fout, "\tret\n");
        } break;

        default:
        {
            fatal("Invalid global declaration");
        } break;
    }
}

void
compile_unit(FILE *fout, GlobDecl *unit)
{
    GlobDecl *curr;

    curr = unit;
    while(curr)
    {
        compile_glob_decl(fout, curr);
        curr = curr->next;
    }
}

#if 0
#define ASMORG_API
#include "asmorg.c"
#endif

#include <assert.h>

int
main(int argc, char *argv[])
{
    FILE *fout;
    char *src;
    GlobDecl *unit;

    fout = fopen("a.out.asm", "w");

    src = "int globalvar;\n"
          "int main() {\n"
          "    int a;\n"
          "    int b;\n"
          "    int c;\n"
          "    a*84/b+c;\n"
          "}";
    parser_init(src);
    unit = parse_unit();
    print_unit(unit);
    compile_unit(fout, unit);

    fclose(fout);

    return(0);
}
