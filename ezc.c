#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define MALLOC_TYPE(_Type_) (_Type_ *)malloc(sizeof(_Type_))

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
    int i;

    for(i = 0;
        i < sym_table_count;
        ++i)
    {
        if(id == sym_table[i].id)
        {
            /* TODO: Log error (the symbol with that id already exists) */
            break;
        }
    }

    res = &(sym_table[sym_table_count]);
    ++sym_table_count;
    res->id = id;
    res->type = type;

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

    res = (Expr *)malloc(sizeof(Expr));
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

    res = (Expr *)malloc(sizeof(Expr));
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

    res = (Expr *)malloc(sizeof(Expr));
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

    res = (Expr *)malloc(sizeof(Expr));
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
            /* TODO: Log error */
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

/******************************************************************************/
/**                                 PARSER                                   **/
/******************************************************************************/

#include <ctype.h>
#include <assert.h>

char *source;

char *kword_int;

void
parser_init(char *src)
{
    source = src;

    kword_int = str_intern("int");
}

enum
{
    TOK_EOF,

    TOK_ID,
    TOK_INTLIT,

    TOK_LPAREN,
    TOK_RPAREN,

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

    if(!*src)
    {
        tok.kind = TOK_EOF;
    }
    else
    {
        while(isspace(*src))
        {
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

            case '*': { ++src; tok.kind = TOK_STAR; } break;
            case '/': { ++src; tok.kind = TOK_SLASH; } break;
            case '%': { ++src; tok.kind = TOK_PERCENT; } break;
            case '+': { ++src; tok.kind = TOK_PLUS; } break;
            case '-': { ++src; tok.kind = TOK_MINUS; } break;

            default:
            {
                /* TODO: Log error */
            } break;
        }
    }

    if(update_source)
    {
        source = src;
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

void
tok_expect(int tok_kind)
{
    Token tok;
    tok = tok_next();
    assert(tok.kind == tok_kind);
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

        case TOK_LPAREN:
        {
            expr = parse_expr();
            tok_expect(TOK_RPAREN);
        } break;

        default:
        {
            /* TODO: Log error */
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
                /* TODO: Log error */
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

/* TODO: HERE */
/*
 * <declaration> ::=
 */

/******************************************************************************/
/**                                   IRC                                    **/
/******************************************************************************/

#include <assert.h>

int lblcount = 0;

void
genlbl(char *lbl)
{
    int n;

    lbl[0] = '.';
    lbl[1] = 'L';
    n = sprintf(lbl+2, "%d", lblcount);
    lbl[n+2] = 0;
    ++lblcount;
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
                /* TODO: Log error */
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
            /* TODO: Log error */
        } break;
    }
}

#if 0

#define MAX_ID_LEN 33

typedef struct
{
    char id[MAX_ID_LEN];
    int global;
    int type;
    int subtype;
    int offset;
} IrcSym;

IrcSym ircsymtbl[1000];
int ircsymtblcount = 0;

IrcSym *
addircsym(char *id, int global, int type)
{
    IrcSym *sym;

    sym = &(ircsymtbl[ircsymtblcount]);
    ++ircsymtblcount;

    strcpy(sym->id, id);
    sym->global = global;
    sym->type = type;

    return(sym);
}

IrcSym *
getircsym(char *id)
{
    int i;
    IrcSym *sym;

    sym = 0;
    if(id)
    {
        for(i = ircsymtblcount - 1;
            i >= 0;
            --i)
        {
            if(strcmp(id, ircsymtbl[i].id) == 0)
            {
                sym = &(ircsymtbl[i]);
                break;
            }
        }
    }

    return(sym);
}

enum
{
    IRC_EXPR_INTLIT,
    IRC_EXPR_ID,

    IRC_EXPR_MUL,
    IRC_EXPR_DIV,
    IRC_EXPR_ADD,
    IRC_EXPR_SUB,

    IRC_EXPR_COUNT
};

typedef struct
IrcExpr
{
    int kind;
    int value;
    char *id;
    struct IrcExpr *l;
    struct IrcExpr *r;
} IrcExpr;

enum
{
    IRC_STMT_EXPR,
    IRC_STMT_ASSIGN,
    IRC_STMT_GOTO,
    IRC_STMT_IF,
    IRC_STMT_LBL,
    IRC_STMT_RET,

    IRC_STMT_COUNT
};

typedef struct
IrcStmt
{
    struct IrcStmt *next;
    int kind;
    char *lbl;
    char *id;
    int deref;
    IrcExpr *expr;
} IrcStmt;

enum
{
    IRC_GLOB_DECL_FUNC,
    IRC_GLOB_DECL_VAR,

    IRC_GLOB_DECL_COUNT
};

typedef struct
IrcGlobDecl
{
    int kind;
    char *id;
    int type;
    int subtype;
    int init;
    IrcStmt *stmts;
    struct IrcGlobDecl *next;
} IrcGlobDecl;

void
compexpr(FILE *fout, IrcExpr *expr)
{
    IrcSym *sym = 0;

    switch(expr->kind)
    {
        case IRC_EXPR_INTLIT:
        {
            fprintf(fout, "\tmovl $%d,%%eax\n", expr->value);
        } break;

        case IRC_EXPR_ID:
        {
            sym = getircsym(expr->id);
            if(!sym)
            {
                /* TODO: Log error */
            }
            if(sym->type == IRC_SYM_FUNC)
            {
                /* TODO: Log error */
            }
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

        case IRC_EXPR_MUL:
        {
            compexpr(fout, expr->r);
            fprintf(fout, "\tmovl %%eax,%%ecx\n");
            compexpr(fout, expr->l);
            fprintf(fout, "\timull %%ecx,%%eax\n");
        } break;

        case IRC_EXPR_DIV:
        {
            compexpr(fout, expr->r);
            fprintf(fout, "\tmovl %%eax,%%ecx\n");
            compexpr(fout, expr->l);
            fprintf(fout, "\tidivl %%ecx\n");
        } break;

        case IRC_EXPR_ADD:
        {
            compexpr(fout, expr->r);
            fprintf(fout, "\tmovl %%eax,%%ecx\n");
            compexpr(fout, expr->l);
            fprintf(fout, "\taddl %%ecx,%%eax\n");
        } break;

        case IRC_EXPR_SUB:
        {
            compexpr(fout, expr->r);
            fprintf(fout, "\tmovl %%eax,%%ecx\n");
            compexpr(fout, expr->l);
            fprintf(fout, "\tsubl %%ecx,%%eax\n");
        } break;

        default:
        {
            /* TODO: Log error */
        } break;
    }
}

void
compstmt(FILE *fout, IrcStmt *stmt)
{
    IrcSym *sym;
    char tmplbl[MAX_ID_LEN];

    switch(stmt->kind)
    {
        case IRC_STMT_EXPR:
        {
            compexpr(fout, stmt->expr);
        } break;

        case IRC_STMT_ASSIGN:
        {
            compexpr(fout, stmt->expr);
            sym = getircsym(stmt->id);
            if(!sym)
            {
                /* TODO: Log error */
            }
            if(sym->type == IRC_SYM_FUNC)
            {
                /* TODO: Log error */
            }
            if(sym->global)
            {
                fprintf(fout, "\tmovl %s,%%ebx\n", sym->id);
            }
            else
            {
                fprintf(fout, "\tmovl %d(%%ebp),%%ebx\n", sym->offset);
            }
            if(stmt->deref)
            {
                fprintf(fout, "\tmovl (%%ebx),%%ebx\n");
            }
            fprintf(fout, "\tmovl %%eax,(%%ebx)\n");
        } break;

        case IRC_STMT_GOTO:
        {
            fprintf(fout, "\tjmp %s\n", stmt->lbl);
        } break;

        case IRC_STMT_IF:
        {
            compexpr(fout, stmt->expr);
            genlbl(tmplbl);
            fprintf(fout, "\tcmpl %%eax,0\n");
            fprintf(fout, "\tje %s\n", tmplbl);
            fprintf(fout, "\tjmp %s\n", stmt->lbl);
            fprintf(fout, "%s:\n", tmplbl);
        } break;

        case IRC_STMT_LBL:
        {
            fprintf(fout, "%s:\n", stmt->lbl);
        } break;

        case IRC_STMT_RET:
        {
            compexpr(fout, stmt->expr);
            fprintf(fout, "\tleave\n");
            fprintf(fout, "\tret\n");
        } break;

        default:
        {
            /* TODO: Log error */
        } break;
    }
}

void
compdecl(FILE *fout, IrcGlobDecl *decl)
{
    IrcSym *sym;
    IrcStmt *stmt;

    fprintf(fout, "%s:\n", decl->id);
    switch(decl->kind)
    {
        case IRC_GLOB_DECL_FUNC:
        {
            fprintf(fout, "\tpushl %%ebp\n");
            fprintf(fout, "\tmovl %%esp,%%ebp\n");

            sym = addircsym(decl->id, 1, IRC_SYM_FUNC);

            stmt = decl->stmts;
            while(stmt)
            {
                compstmt(fout, stmt);
                stmt = stmt->next;
            }

            fprintf(fout, "\tleave\n");
            fprintf(fout, "\tret\n");
        } break;

        case IRC_GLOB_DECL_VAR:
        {
            if(decl->init == 0)
            {
                fprintf(fout, "\t.zero $4\n");
            }
            else
            {
                fprintf(fout, "\t.long $%d\n", decl->init);
            }
            sym = addircsym(decl->id, 1, decl->type);
            sym->subtype = decl->subtype;
        } break;

        default:
        {
            /* TODO: Log error */
        } break;
    }
}

void
compunit(FILE *fout, IrcGlobDecl *decls)
{
    while(decls)
    {
        compdecl(fout, decls);
        decls = decls->next;
    }
}

IrcGlobDecl *
mkglobdecl(char *id, int kind)
{
    IrcGlobDecl *res = 0;

    res = (IrcGlobDecl *)malloc(sizeof(IrcGlobDecl));
    if(res)
    {
        res->id = id;
        res->kind = kind;
    }

    return(res);
}

IrcStmt *
mkstmt(int kind)
{
    IrcStmt *res = 0;

    res = (IrcStmt *)malloc(sizeof(IrcStmt));
    if(res)
    {
        res->kind = kind;
        res->next = 0;
    }

    return(res);
}

IrcExpr *
mkbinexpr(int kind, IrcExpr *l, IrcExpr *r)
{
    IrcExpr *res = 0;

    res = (IrcExpr *)malloc(sizeof(IrcExpr));
    if(res)
    {
        res->kind = kind;
        res->l = l;
        res->r = r;
    }

    return(res);
}

IrcExpr *
mkunexpr(int kind, IrcExpr *l)
{
    IrcExpr *res = 0;

    res = (IrcExpr *)malloc(sizeof(IrcExpr));
    if(res)
    {
        res->kind = kind;
        res->l = l;
        res->r = 0;
    }

    return(res);
}

IrcExpr *
mkexprintlit(int value)
{
    int kind = IRC_EXPR_INTLIT;
    IrcExpr *res = 0;

    res = (IrcExpr *)malloc(sizeof(IrcExpr));
    if(res)
    {
        res->kind = kind;
        res->value = value;
    }

    return(res);
}

IrcExpr *
mkexprid(char *id)
{
    int kind = IRC_EXPR_ID;
    IrcExpr *res = 0;

    res = (IrcExpr *)malloc(sizeof(IrcExpr));
    if(res)
    {
        res->kind = kind;
        res->id = id;
    }

    return(res);
}

/******************************************************************************/
/**                                CODE GEN                                  **/
/******************************************************************************/

IrcExpr *
cgexpr(Expr *expr)
{
    IrcExpr *res = 0;

    switch(expr->kind)
    {
        case EXPR_INTLIT:
        {
            res = mkexprintlit(expr->value);
        } break;

        case EXPR_ID:
        {
            res = mkexprid(expr->id);
        } break;

        case EXPR_MUL:
        {
            res = mkbinexpr(IRC_EXPR_MUL, cgexpr(expr->l), cgexpr(expr->r));
        } break;

        case EXPR_DIV:
        {
            res = mkbinexpr(IRC_EXPR_DIV, cgexpr(expr->l), cgexpr(expr->r));
        } break;

        case EXPR_ADD:
        {
            res = mkbinexpr(IRC_EXPR_ADD, cgexpr(expr->l), cgexpr(expr->r));
        } break;

        case EXPR_SUB:
        {
            res = mkbinexpr(IRC_EXPR_SUB, cgexpr(expr->l), cgexpr(expr->r));
        } break;

        default:
        {
            /* TODO: Log error */
        } break;
    }

    return(res);
}

#endif

#define ASMORG_API
#include "asmorg.c"

#include <assert.h>

int
main(int argc, char *argv[])
{
#if 0
    FILE *fout;
    IrcGlobDecl *decl;
    IrcStmt *stmt;
#if 1
    IrcStmt *stmt2;
    IrcStmt *stmt3;
#endif

    fout = fopen("a.out.asm", "w");

    /* Bootstrap (call main function) */
    fprintf(fout, "\tcall main\n");
    fprintf(fout, "\tmovl %%eax,%%ebx\n");
    fprintf(fout, "\tmovl $1,%%eax\n");
    fprintf(fout, "\tsyscall\n");

#if 1
    decl = mkglobdecl("var1", IRC_GLOB_DECL_VAR);
    decl->type = IRC_SYM_VAR_INT;
    decl->init = 0;
    compdecl(fout, decl);
    decl->id = "var2";
    decl->type = IRC_SYM_VAR_INT;
    decl->init = 88;
    compdecl(fout, decl);

    stmt = mkstmt(IRC_STMT_ASSIGN);
    stmt->id = "var1";
    stmt->expr = mkbinexpr(IRC_EXPR_ADD, mkexprid("var2"), mkexprintlit(666));

    stmt2 = mkstmt(IRC_STMT_ASSIGN);
    stmt2->id = "var2";
    stmt2->expr = mkbinexpr(IRC_EXPR_SUB, mkexprintlit(999), mkexprintlit(994));
    stmt->next = stmt2;

    stmt3 = mkstmt(IRC_STMT_RET);
    stmt3->expr = mkexprid("var2");
    stmt2->next = stmt3;
#endif

    decl = mkglobdecl("main", IRC_GLOB_DECL_FUNC);
    decl->stmts = stmt;
    compdecl(fout, decl);

    fclose(fout);

    assemble("a.out.asm", "a.out");
#endif

    FILE *fout;
    char *src;
    Expr *expr;

    src = "3+2*1";
    parser_init(src);
    expr = parse_expr();
    printf("%s => ", src);
    print_expr(expr);
    printf("\n");

    src = "3*2+1";
    parser_init(src);
    expr = parse_expr();
    printf("%s => ", src);
    print_expr(expr);
    printf("\n");

    src = "3+2+1";
    parser_init(src);
    expr = parse_expr();
    printf("%s => ", src);
    print_expr(expr);
    printf("\n");

    src = "3*2+8/4/9";
    parser_init(src);
    expr = parse_expr();
    printf("%s => ", src);
    print_expr(expr);
    printf("\n");

    src = "3*(2+8)/4/9";
    parser_init(src);
    expr = parse_expr();
    printf("%s => ", src);
    print_expr(expr);
    printf("\n");

    fout = fopen("a.out.asm", "w");

    compile_expr(fout, expr);

    return(0);
}
