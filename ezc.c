/******************************************************************************/
/**                                  TODOS                                   **/
/******************************************************************************/

/*
 * [x] Better types system
 * [x] Type check
 * [x] Type check of arguments on function call
 * [x] Type inference
 * [x] Code generation based on type width
 * [x] Arrays
 * [x] Array subscription operator
 * [x] Cast operator
 * [x] Arrays decay into pointers
 * [x] Structs
 * [ ] Unions
 * [/] Finite/Non-finite types
 * [ ] Function prototypes/Function definitions
 * [x] Better AST => IR-C Translation
 * [x] Post-position of labels
 * [x] If-Else
 * [x] While
 * [ ] Do-While
 * [ ] For
 * [ ] Switch
 * [ ] Constants
 * [ ] Enums
 * [ ] Variable initialization on declaration
 * [ ] Multiple variable declaration (comma separated)
 * [x] >,>=,<,<=
 */

/******************************************************************************/
/**                                  UTILS                                   **/
/******************************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#define ALIGN(n, a) ((((n)%(a))>0)?((n)+((a)-((n)%(a)))):(n))

#define MALLOC_TYPE(_Type_) ((_Type_ *)malloc(sizeof(_Type_)))
#define DUP_OBJ(_Type_, dest, src)\
    ((_Type_*)\
        (dest=MALLOC_TYPE(_Type_),\
        memcpy((void*)(dest),(const void *)(src),sizeof(_Type_))))

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

typedef struct Type Type;

typedef struct
FuncParam
{
    char *id;
    Type *type;
    struct FuncParam *next;
} FuncParam;

FuncParam *
make_func_param(char *id, Type *type)
{
    FuncParam *res;

    res = MALLOC_TYPE(FuncParam);
    if(res)
    {
        res->id = id;
        res->type = type;
        res->next = 0;
    }

    return(res);
}

FuncParam *
reverse_func_params_list(FuncParam *params)
{
    FuncParam *res;

    res = 0;
    if(!params || !(params->next))
    {
        res = params;
    }
    else
    {
        res = reverse_func_params_list(params->next);
        params->next->next = params;
        params->next = 0;
    }

    return(res);
}

typedef struct
AggrElement
{
    char *id;
    Type *type;
    int offset;
    struct AggrElement *next;
} AggrElement;

AggrElement *
make_aggr_element(char *id, Type *type, int offset)
{
    AggrElement *res;

    res = MALLOC_TYPE(AggrElement);
    if(res)
    {
        res->id = id;
        res->type = type;
        res->offset = offset;
        res->next = 0;
    }

    return(res);
}

enum
{
    TYPE_VOID,
    TYPE_CHAR,
    TYPE_INT,
    TYPE_PTR,
    TYPE_ARRAY,
    TYPE_FUNC,
    TYPE_STRUCT,

    TYPE_COUNT
};

struct
Type
{
    int kind;
    int size;
    struct Type *base_type;
    int length;
    FuncParam *params;
    char *id;
    AggrElement *def;
};

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

int
type_is_arithmetic(Type *type)
{
    return(type == type_char() || type == type_int());
}

#define TYPE_PTR_CACHE_SIZE 100
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

Type *
type_func(Type *ret_type, FuncParam *params)
{
    Type *res;

    res = MALLOC_TYPE(Type);
    if(res)
    {
        res->kind = TYPE_FUNC;
        res->base_type = ret_type;
        res->params = params;
    }

    return(res);
}

#define TYPE_ARRAY_CACHE_SIZE 100
Type type_array_cache[TYPE_ARRAY_CACHE_SIZE];
int type_array_cache_count = 0;

Type *
type_array(Type *base_type, int length)
{
    Type *res = 0;
    int i;

    for(i = 0;
        i < type_array_cache_count;
        ++i)
    {
        if(type_array_cache[i].base_type == base_type &&
           type_array_cache[i].length == length)
        {
            res = &(type_array_cache[i]);
            break;
        }
    }

    if (!res)
    {
        res = &(type_array_cache[type_array_cache_count]);
        ++type_array_cache_count;
        res->kind = TYPE_ARRAY;
        res->base_type = base_type;
        res->length = length;
        res->size = length*base_type->size;
    }

    return(res);
}

#define TYPE_STRUCT_CACHE_SIZE 100
Type type_struct_cache[TYPE_STRUCT_CACHE_SIZE];
int type_struct_cache_count = 0;

Type *
type_struct(char *id, AggrElement *def)
{
    Type *res = 0;
    AggrElement *e;
    int i;

    for(i = 0;
        i < type_struct_cache_count;
        ++i)
    {
        if(type_struct_cache[i].id == id)
        {
            res = &(type_struct_cache[i]);
            break;
        }
    }

    if (!res)
    {
        res = &(type_struct_cache[type_struct_cache_count]);
        ++type_struct_cache_count;
        res->kind = TYPE_STRUCT;
        res->id = id;
    }
    else if(res->def && def)
    {
        fatal("Cannot redefine a structure");
    }

    if(!res->def && def)
    {
        res->def = def;

        /* Compute struct size */
        res->size = 0;
        e = def;
        while(e)
        {
            if(e->type->size == 0)
            {
                fatal("Invalid structure member type");
            }
            res->size += ALIGN(e->type->size, 4);
            e = e->next;
        }

        assert(res->size);
    }

    return(res);
}

AggrElement *
get_struct_member(Type *stype, char *id)
{
    AggrElement *res;
    AggrElement *curr;

    res = 0;
    if(stype->def)
    {
        curr = stype->def;
        while(curr)
        {
            if(curr->id == id)
            {
                res = curr;
                break;
            }
            curr = curr->next;
        }
    }

    return(res);
}

int
get_struct_member_offset(Type *stype, char *id)
{
    int offset;
    AggrElement *el;

    offset = -1;
    el = get_struct_member(stype, id);
    if(el)
    {
        offset = el->offset;
    }

    return(offset);
}

/******************************************************************************/
/**                                SYM TABLE                                 **/
/******************************************************************************/

enum
{
    LABEL_UNDEFINED,
    LABEL_DEFINED,

    LABEL_STATUS_COUNT
};

typedef struct
{
    int status;
    char *id;
} Label;

#define LBL_TABLE_SIZE 1000
Label label_table[LBL_TABLE_SIZE];
int label_table_count = 0;

Label *
label_add(char *id)
{
    Label *res = 0;

    res = &(label_table[label_table_count]);
    ++label_table_count;
    res->id = id;
    res->status = LABEL_UNDEFINED;

    return(res);
}

Label *
label_get(char *id)
{
    Label *res = 0;
    int i;

    for(i = 0;
        i < label_table_count;
        ++i)
    {
        if(id == label_table[i].id)
        {
            res = &(label_table[i]);
            break;
        }
    }

    return(res);
}

Label *
label_get_or_add(char *id)
{
    Label *res = 0;

    res = label_get(id);
    if(!res)
    {
        res = label_add(id);
        res->status = LABEL_UNDEFINED;
    }

    return(res);
}

typedef struct
{
    char *id;
    Type *type;
    int global;
    int offset;
    void *func;
    int is_const;
    int value;
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
    res->func = 0;
    res->is_const = 0;
    res->value = 0;

    return(res);
}

Sym *
sym_get(char *id)
{
    Sym *res = 0;
    int i;

    for(i = sym_table_count - 1;
        i >= 0;
        --i)
    {
        if(id == sym_table[i].id)
        {
            res = &(sym_table[i]);
            break;
        }
    }

    return(res);
}

Sym *
sym_add_func_param(char *id, Type *type, int offset)
{
    Sym *res = 0;

    res = &(sym_table[sym_table_count]);
    ++sym_table_count;
    res->id = id;
    res->type = type;
    res->global = 0;
    res->offset = offset;
    res->func = 0;
    res->is_const = 0;
    res->value = 0;

    return(res);
}

void
init_builtin_sym()
{
#if 0
    /* TODO: This is a type function not int */
    sym_add("putchar", type_int());
#endif
}

void
sym_reset()
{
    sym_table_count = 0;
    init_builtin_sym();
}

/******************************************************************************/
/**                                  AST                                     **/
/******************************************************************************/

enum
{
    EXPR_INVALID,

    EXPR_INTLIT,
    EXPR_ID,

    EXPR_CALL,
    EXPR_ARR_SUB,
    EXPR_MEMB_ACCESS,
    EXPR_MEMB_ACCESS_PTR,

    EXPR_UNARY,
    EXPR_INC_PRE = EXPR_UNARY,
    EXPR_DEC_PRE,
    EXPR_NEG,
    EXPR_CAST,
    EXPR_DEREF,
    EXPR_ADDR_OF,
    EXPR_UNARY_END,

    EXPR_BINARY,
    EXPR_MUL = EXPR_BINARY,
    EXPR_DIV,
    EXPR_MOD,
    EXPR_ADD,
    EXPR_SUB,
    EXPR_LT,
    EXPR_LE,
    EXPR_GT,
    EXPR_GE,
    EXPR_BINARY_END,

    EXPR_TERNARY,
    EXPR_ASSIGN,

    EXPR_COMPOUND,

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
    struct Expr *m;
    Type *cast_to;
    struct Expr *next;
} Expr;

Expr *
dup_expr(Expr *expr)
{
    Expr *new;
    new = DUP_OBJ(Expr, new, expr);
    return(new);
}

Expr *
make_expr_intlit(int value)
{
    Expr *res = 0;

    res = MALLOC_TYPE(Expr);
    if(res)
    {
        res->kind = EXPR_INTLIT;
        res->value = value;
        res->next = 0;
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
        res->next = 0;
    }

    return(res);
}

Expr *
make_expr_cast(Expr *l, Type *type)
{
    Expr *res = 0;

    res = MALLOC_TYPE(Expr);
    if(res)
    {
        res->kind = EXPR_CAST;
        res->l = l;
        res->cast_to = type;
        res->next = 0;
    }

    return(res);
}

Expr *
make_expr_member_access(Expr *l, char *member)
{
    Expr *res = 0;

    res = MALLOC_TYPE(Expr);
    if(res)
    {
        res->kind = EXPR_MEMB_ACCESS;
        res->l = l;
        res->id = member;
        res->next = 0;
    }

    return(res);
}

Expr *
make_expr_member_access_ptr(Expr *l, char *member)
{
    Expr *res = 0;

    res = MALLOC_TYPE(Expr);
    if(res)
    {
        res->kind = EXPR_MEMB_ACCESS_PTR;
        res->l = l;
        res->id = member;
        res->next = 0;
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
        res->next = 0;
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
        res->next = 0;
    }

    return(res);
}

Expr *
make_expr_ternary(Expr *l, Expr *m, Expr *r)
{
    Expr *res = 0;

    res = MALLOC_TYPE(Expr);
    if(res)
    {
        res->kind = EXPR_TERNARY;
        res->l = l;
        res->m = m;
        res->r = r;
        res->next = 0;
    }

    return(res);
}

Expr *
make_expr_compound(Expr *l)
{
    Expr *res = 0;

    res = MALLOC_TYPE(Expr);
    if(res)
    {
        res->kind = EXPR_COMPOUND;
        res->l = l;
        res->next = 0;
    }

    return(res);
}

void print_type(Type *type);

void
print_sym_table()
{
    int i;
    Sym *s;

    for(i = 0;
        i < sym_table_count;
        ++i)
    {
        s = &sym_table[i];
        printf("%d - %s => ", i, s->id);
        print_type(s->type);
        printf("\n");
    }
    print_type(s->type);
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

        case EXPR_CALL:
        {
            printf("(call %s)", expr->l->id);
        } break;

        case EXPR_ARR_SUB:
        {
            printf("(");
            print_expr(expr->l);
            printf("[");
            print_expr(expr->r);
            printf("])");
        } break;

        case EXPR_MEMB_ACCESS:
        {
            printf("(");
            print_expr(expr->l);
            printf(".%s)", expr->id);
        } break;

        case EXPR_MEMB_ACCESS_PTR:
        {
            printf("(");
            print_expr(expr->l);
            printf("->%s)", expr->id);
        } break;

        case EXPR_INC_PRE:
        {
            printf("(inc ");
            print_expr(expr->l);
            printf(")");
        } break;

        case EXPR_DEC_PRE:
        {
            printf("(dec ");
            print_expr(expr->l);
            printf(")");
        } break;

        case EXPR_CAST:
        {
            printf("(cast (");
            print_type(expr->cast_to);
            printf(") ");
            print_expr(expr->l);
            printf(")");
        } break;

        case EXPR_DEREF:
        {
            printf("(deref ");
            print_expr(expr->l);
            printf(")");
        } break;

        case EXPR_ADDR_OF:
        {
            printf("(addrof ");
            print_expr(expr->l);
            printf(")");
        } break;

        case EXPR_NEG:
        {
            printf("(-");
            print_expr(expr->l);
            printf(")");
        } break;

        case EXPR_MUL: { op = "*"; } break;
        case EXPR_DIV: { op = "/"; } break;
        case EXPR_MOD: { op = "%"; } break;
        case EXPR_ADD: { op = "+"; } break;
        case EXPR_SUB: { op = "-"; } break;

        case EXPR_LT: { op = "<"; } break;
        case EXPR_LE: { op = "<="; } break;
        case EXPR_GT: { op = ">"; } break;
        case EXPR_GE: { op = ">="; } break;

        case EXPR_TERNARY:
        {
            printf("(");
            print_expr(expr->l);
            printf(" ? ");
            print_expr(expr->m);
            printf(" : ");
            print_expr(expr->r);
            printf(")");
        } break;

        case EXPR_ASSIGN: { op = "="; } break;

        case EXPR_COMPOUND:
        {
            expr = expr->l;
            while(expr)
            {
                print_expr(expr);
                if(expr->next)
                {
                    printf(", ");
                }
                expr = expr->next;
            }
        } break;

        default:
        {
            assert(0);
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
    FuncParam *param;

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
    else if(type->kind == TYPE_ARRAY)
    {
        printf("array of %d of ", type->length);
        print_type(type->base_type);
    }
    else if(type->kind == TYPE_STRUCT)
    {
        printf("struct %s", type->id);
    }
    else if(type->kind == TYPE_FUNC)
    {
        printf("func (");
        param = type->params;
        while(param)
        {
            print_type(param->type);
            if(param->next)
            {
                printf(", ");
            }
            param = param->next;
        }
        printf(") => ");
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
    STMT_RET,
    STMT_LABEL,
    STMT_GOTO,
    STMT_IF,

    STMT_WHILE,
    STMT_FOR,

    STMT_COUNT
};

typedef struct
Stmt
{
    int kind;
    union
    {
        char *label;
        Decl *decl;
        Expr *expr;
        struct Stmt *block;
    } u;
    Expr *init;
    Expr *cond;
    Expr *post;
    struct Stmt *then_stmt;
    struct Stmt *else_stmt;
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

Stmt *
dup_stmt(Stmt *stmt)
{
    Stmt *new;
    new = DUP_OBJ(Stmt, new, stmt);
    return(new);
}

Stmt *
make_stmt_decl(Decl *decl)
{
    Stmt *res = 0;

    res = make_stmt(STMT_DECL);
    if(res)
    {
        res->u.decl = decl;
    }

    return(res);
}

Stmt *
make_stmt_expr(Expr *expr)
{
    Stmt *res = 0;

    res = make_stmt(STMT_EXPR);
    if(res)
    {
        res->u.expr = expr;
    }

    return(res);
}

Stmt *
make_stmt_if(Expr *cond, Stmt *then_stmt, Stmt *else_stmt)
{
    Stmt *res = 0;

    res = make_stmt(STMT_IF);
    if(res)
    {
        res->cond = cond;
        res->then_stmt = then_stmt;
        res->else_stmt = else_stmt;
    }

    return(res);
}

Stmt *
make_stmt_while(Expr *cond, Stmt *then_stmt)
{
    Stmt *res = 0;

    res = make_stmt(STMT_WHILE);
    if(res)
    {
        res->cond = cond;
        res->then_stmt = then_stmt;
        res->else_stmt = 0;
    }

    return(res);
}

Stmt *
make_stmt_for(Expr *init, Expr *cond, Expr *post, Stmt *then_stmt)
{
    Stmt *res = 0;

    res = make_stmt(STMT_FOR);
    if(res)
    {
        res->init = init;
        res->cond = cond;
        res->post = post;
        res->then_stmt = then_stmt;
        res->else_stmt = 0;
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

        case STMT_RET:
        {
            printf("(ret ");
            if(stmt->u.expr)
            {
                print_expr(stmt->u.expr);
            }
            printf(")");
        } break;

        case STMT_LABEL:
        {
            printf("(label %s)", stmt->u.label);
        } break;

        case STMT_GOTO:
        {
            printf("(goto %s)", stmt->u.label);
        } break;

        case STMT_IF:
        {
            printf("(if ");
            print_expr(stmt->cond);
            if(stmt->then_stmt)
            {
                print_stmt(stmt->then_stmt);
                if(stmt->else_stmt)
                {
                    printf(" else ");
                    print_stmt(stmt->else_stmt);
                }
                printf(")");
            }
            else
            {
                printf(" goto %s)", stmt->u.label);
            }
        } break;

        case STMT_WHILE:
        {
            printf("(while ");
            print_expr(stmt->cond);
            printf(" ");
            print_stmt(stmt->then_stmt);
            printf(")");
        } break;

        case STMT_FOR:
        {
            printf("(for ");
            print_expr(stmt->init);
            printf(" ");
            print_expr(stmt->cond);
            printf(" ");
            print_expr(stmt->post);
            print_stmt(stmt->then_stmt);
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
            printf("(func %s ", decl->id);
            print_type(decl->type->base_type);
            if(decl->func_def)
            {
                printf("\n");
                print_stmt(decl->func_def);
            }
            printf(")\n");
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

char *kword_void;
char *kword_char;
char *kword_int;
char *kword_struct;
char *kword_return;
char *kword_goto;
char *kword_if;
char *kword_else;
char *kword_while;
char *kword_for;

#include <stdarg.h>

void
syntax_error(char *fmt, ...)
{
    va_list ap;

    printf("[!] SYNTAX ERROR: Line %d: ", source_line);
    va_start(ap, fmt);
    vprintf(fmt, ap);
    va_end(ap);
    printf("\n");
}

void
syntax_fatal(char *fmt, ...)
{
    va_list ap;

    printf("[!] SYNTAX ERROR: Line %d: ", source_line);
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

    kword_void = str_intern("void");
    kword_char = str_intern("char");
    kword_int = str_intern("int");
    kword_struct = str_intern("struct");
    kword_return = str_intern("return");
    kword_goto = str_intern("goto");
    kword_if = str_intern("if");
    kword_else = str_intern("else");
    kword_while = str_intern("while");
    kword_for = str_intern("for");
}

enum
{
    TOK_EOF,

    TOK_ID,
    TOK_INTLIT,

    TOK_LPAREN,
    TOK_RPAREN,
    TOK_LBRACK,
    TOK_RBRACK,
    TOK_LBRACE,
    TOK_RBRACE,

    TOK_SEMI,
    TOK_COLON,
    TOK_COMMA,
    TOK_DOT,
    TOK_QMARK,
    TOK_AMPERSAND,
    TOK_MEMB_ACCESS_PTR,

    TOK_PLUS_PLUS,
    TOK_MINUS_MINUS,

    /* Binary operators */
    TOK_BIN_OP,

    TOK_ASTERISK = TOK_BIN_OP,
    TOK_SLASH,
    TOK_PERCENT,
    TOK_PLUS,
    TOK_MINUS,
    TOK_LT,
    TOK_LE,
    TOK_GT,
    TOK_GE,

    TOK_BIN_OP_END,

    TOK_EQUAL,

    TOK_KW_VOID,
    TOK_KW_CHAR,
    TOK_KW_INT,
    TOK_KW_STRUCT,

    TOK_KW_RETURN,
    TOK_KW_GOTO,
    TOK_KW_IF,
    TOK_KW_ELSE,
    TOK_KW_WHILE,
    TOK_KW_FOR,

    TOK_COUNT
};

#define MAX_ID_LEN 33

typedef struct Token
{
    int kind;
    int value;
    char *id;
} Token;

Token putback;

void
tok_putback(Token tok)
{
    putback = tok;
}

Token
_tok_next(int update_source)
{
    Token tok;
    char *src = source;
    int srcl = source_line;

    if(putback.kind != TOK_EOF)
    {
        tok = putback;
        if(update_source)
        {
            putback.kind = TOK_EOF;
        }
        return(tok);
    }

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

                     if(tok.id == kword_void) { tok.kind = TOK_KW_VOID; }
                else if(tok.id == kword_char) { tok.kind = TOK_KW_CHAR; }
                else if(tok.id == kword_int) { tok.kind = TOK_KW_INT; }
                else if(tok.id == kword_struct) { tok.kind = TOK_KW_STRUCT; }
                else if(tok.id == kword_return) { tok.kind = TOK_KW_RETURN; }
                else if(tok.id == kword_goto) { tok.kind = TOK_KW_GOTO; }
                else if(tok.id == kword_if) { tok.kind = TOK_KW_IF; }
                else if(tok.id == kword_else) { tok.kind = TOK_KW_ELSE; }
                else if(tok.id == kword_while) { tok.kind = TOK_KW_WHILE; }
                else if(tok.id == kword_for) { tok.kind = TOK_KW_FOR; }
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
            case '[': { ++src; tok.kind = TOK_LBRACK; } break;
            case ']': { ++src; tok.kind = TOK_RBRACK; } break;
            case '{': { ++src; tok.kind = TOK_LBRACE; } break;
            case '}': { ++src; tok.kind = TOK_RBRACE; } break;

            case ';': { ++src; tok.kind = TOK_SEMI; } break;
            case ':': { ++src; tok.kind = TOK_COLON; } break;
            case ',': { ++src; tok.kind = TOK_COMMA; } break;
            case '.': { ++src; tok.kind = TOK_DOT; } break;
            case '?': { ++src; tok.kind = TOK_QMARK; } break;
            case '&': { ++src; tok.kind = TOK_AMPERSAND; } break;

            case '*': { ++src; tok.kind = TOK_ASTERISK; } break;
            case '/': { ++src; tok.kind = TOK_SLASH; } break;
            case '%': { ++src; tok.kind = TOK_PERCENT; } break;

            case '+':
            {
                ++src;
                tok.kind = TOK_PLUS;
                if(*src == '+')
                {
                    ++src;
                    tok.kind = TOK_PLUS_PLUS;
                }
            } break;

            case '-':
            {
                ++src;
                tok.kind = TOK_MINUS;
                if(*src == '-')
                {
                    ++src;
                    tok.kind = TOK_MINUS_MINUS;
                }
                else if(*src == '>')
                {
                    ++src;
                    tok.kind = TOK_MEMB_ACCESS_PTR;
                }
            } break;

            case '<':
            {
                ++src;
                tok.kind = TOK_LT;
                if(*src == '=')
                {
                    ++src;
                    tok.kind = TOK_LE;
                }
            } break;

            case '>':
            {
                ++src;
                tok.kind = TOK_GT;
                if(*src == '=')
                {
                    ++src;
                    tok.kind = TOK_GE;
                }
            } break;

            case '=': { ++src; tok.kind = TOK_EQUAL; } break;

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
    if(tok.kind != tok_kind)
    {
        syntax_fatal("Expected token %d, found %d\n", tok_kind, tok.kind);
    }
    assert(tok.kind == tok_kind);
    return(tok);
}

int
tok_is_type(Token tok)
{
    int is_type = 0;

    switch(tok.kind)
    {
        case TOK_KW_VOID:
        case TOK_KW_CHAR:
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
 * <expr> ::= <expr_assign> (',' <expr_assign>)*
 *
 * <expr_assign> ::= <expr_ternary> <assign_op> <expr_assign>
 *
 * <expr_ternary> ::= <expr_binary> '?' <expr> ':' <expr_ternary>
 *
 * <expr_binary> ::= <expr_binary> <bin_op> <expr_unary>
 *                 | <expr_unary>
 *
 * <expr_unary> ::= <un_op> <expr_unary>
 *                | <expr_first_level>
 *
 * <expr_first_level> ::= <expr_base>
 *                      | <expr_base> '(' (<expr_assign> (',' <expr_assign>)*)? ')'
 *                      | <expr_base> '[' <expr> ']'
 *                      | <expr_base> '.' TOK_ID
 *                      | <expr_base> '->' TOK_ID
 *
 * <expr_base> ::= '(' <expr> ')'
 *               | <int_lit>
 *               | <ident>
 *
 * <bin_op> ::= [/ * %]
 *            | [-+]
 *            | [< <= > >=]
 *
 * <un_op> ::= [-+]
 *
 * <assign_op> ::= '='
 */

int op_precedence_table[TOK_BIN_OP_END - TOK_BIN_OP] = {
    3, /* TOK_ASTERISK */
    3, /* TOK_SLASH */
    3, /* TOK_PERCENT */
    4, /* TOK_PLUS */
    4, /* TOK_MINUS */
    6, /* TOK_LT */
    6, /* TOK_LE */
    6, /* TOK_GT */
    6, /* TOK_GE */
};

int
expr_is_const(Expr *expr)
{
    int res = 0;
    Sym *sym;

    if(expr->kind > EXPR_TERNARY)
    {
        res = 0;
    }
    else if(expr->kind == EXPR_INTLIT)
    {
        res = 1;
    }
    else if(expr->kind == EXPR_ID)
    {
        sym = sym_get(expr->id);
        if(!sym)
        {
            fatal("Invalid symbol '%s'", expr->id);
        }

        res = sym->is_const;
    }
    else if(expr->kind >= EXPR_UNARY && expr->kind <= EXPR_UNARY_END)
    {
        switch(expr->kind)
        {
            case EXPR_NEG:
            {
                res = expr_is_const(expr->l);
            } break;

            default:
            {
                res = 0;
            } break;
        }
    }
    else if(expr->kind >= EXPR_BINARY && expr->kind <= EXPR_BINARY_END)
    {
        switch(expr->kind)
        {
            case EXPR_MUL: case EXPR_DIV: case EXPR_MOD:
            case EXPR_ADD: case EXPR_SUB:
            case EXPR_LT: case EXPR_LE:
            case EXPR_GT: case EXPR_GE:
            {
                res = expr_is_const(expr->l);
                res = res && expr_is_const(expr->r);
            } break;

            default:
            {
                res = 0;
            } break;
        }
    }
    else if(expr->kind == EXPR_TERNARY)
    {
        res = expr_is_const(expr->l);
        res = res && expr_is_const(expr->m);
        res = res && expr_is_const(expr->r);
    }

    return(res);
}

int
eval_expr(Expr *expr)
{
    int res = 0;
    int l = 0;
    int r = 0;
    int m = 0;
    Sym *sym;

    if(expr->kind > EXPR_TERNARY)
    {
        assert(0);
    }
    else if(expr->kind == EXPR_INTLIT)
    {
        res = expr->value;
    }
    else if(expr->kind == EXPR_ID)
    {
        sym = sym_get(expr->id);
        if(!sym)
        {
            fatal("Invalid symbol '%s'", expr->id);
        }

        assert(sym->is_const);
        res = sym->value;
    }
    else if(expr->kind >= EXPR_UNARY && expr->kind <= EXPR_UNARY_END)
    {
        switch(expr->kind)
        {
            case EXPR_NEG:
            {
                res = -eval_expr(expr->l);
            } break;

            default:
            {
                assert(0);
            } break;
        }
    }
    else if(expr->kind >= EXPR_BINARY && expr->kind <= EXPR_BINARY_END)
    {
        l = eval_expr(expr->l);
        r = eval_expr(expr->r);
        switch(expr->kind)
        {
            case EXPR_MUL: { res = l*r; } break;
            case EXPR_DIV: { res = l/r; } break;
            case EXPR_MOD: { res = l%r; } break;
            case EXPR_ADD: { res = l+r; } break;
            case EXPR_SUB: { res = l-r; } break;

            case EXPR_LT: { res = ((l<r)?(1):0); } break;
            case EXPR_LE: { res = ((l<=r)?(1):0); } break;
            case EXPR_GT: { res = ((l>r)?(1):0); } break;
            case EXPR_GE: { res = ((l>=r)?(1):0); } break;

            default:
            {
                assert(0);
            } break;
        }
    }
    else if(expr->kind == EXPR_TERNARY)
    {
        l = eval_expr(expr->l);
        m = eval_expr(expr->m);
        r = eval_expr(expr->r);
        if(l)
        {
            res = m;
        }
        else
        {
            res = r;
        }
    }
    else
    {
        assert(0);
    }

    return(res);
}

Expr *parse_expr_assign();
Expr *parse_expr();
Type *parse_type(Type *type);

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
reverse_args_list(Expr *args)
{
    Expr *res;

    res = 0;
    if(!args || !(args->next))
    {
        res = args;
    }
    else
    {
        res = reverse_args_list(args->next);
        args->next->next = args;
        args->next = 0;
    }

    return(res);
}

Expr *
parse_expr_first_level()
{
    Expr *expr = 0;
    Expr *args = 0;
    Expr *arg = 0;
    Token tok;

    expr = parse_expr_base();

    tok = tok_peek();
    switch(tok.kind)
    {
        case TOK_LPAREN:
        {
            tok_next();
            tok = tok_peek();
            if(tok.kind != TOK_RPAREN)
            {
                do
                {
                    if(tok.kind == TOK_COMMA)
                    {
                        tok_next();
                    }
                    if(arg)
                    {
                        arg->next = parse_expr_assign();
                        arg = arg->next;
                    }
                    else
                    {
                        arg = parse_expr_assign();
                        args = arg;
                    }
                    tok = tok_peek();
                }
                while(tok.kind == TOK_COMMA);
            }
            tok_expect(TOK_RPAREN);

            if(args)
            {
                args = reverse_args_list(args);
            }
            expr = make_expr_binary(EXPR_CALL, expr, args);
        } break;

        case TOK_LBRACK:
        {
            tok_next();
            expr = make_expr_binary(EXPR_ARR_SUB, expr, parse_expr());
            tok_expect(TOK_RBRACK);
        } break;

        case TOK_DOT:
        {
            tok_next();

            tok = tok_expect(TOK_ID);
            expr = make_expr_member_access(expr, tok.id);
        } break;

        case TOK_MEMB_ACCESS_PTR:
        {
            tok_next();

            tok = tok_expect(TOK_ID);
            expr = make_expr_member_access_ptr(expr, tok.id);
        } break;
    }

    return(expr);
}

Expr *
parse_expr_unary()
{
    Expr *expr = 0;
    Token tok;
    Type *type;

    tok = tok_peek();
    switch(tok.kind)
    {
        case TOK_PLUS_PLUS:
        {
            tok_next();
            expr = parse_expr_unary();
            expr = make_expr_unary(EXPR_INC_PRE, expr);
        } break;

        case TOK_MINUS_MINUS:
        {
            tok_next();
            expr = parse_expr_unary();
            expr = make_expr_unary(EXPR_DEC_PRE, expr);
        } break;

        case TOK_PLUS:
        {
            tok_next();
            expr = parse_expr_unary();
        } break;

        case TOK_MINUS:
        {
            tok_next();
            expr = parse_expr_unary();
            expr = make_expr_unary(EXPR_NEG, expr);
        } break;

        case TOK_LPAREN:
        {
            tok_next();

            tok = tok_peek();
            if(tok_is_type(tok))
            {
                type = parse_type(0);
                tok_expect(TOK_RPAREN);

                expr = make_expr_cast(parse_expr_unary(), type);
            }
            else
            {
                expr = parse_expr();
                tok_expect(TOK_RPAREN);
            }
        } break;

        case TOK_ASTERISK:
        {
            tok_next();
            expr = parse_expr_unary();
            expr = make_expr_unary(EXPR_DEREF, expr);
        } break;

        case TOK_AMPERSAND:
        {
            tok_next();
            expr = parse_expr_unary();
            expr = make_expr_unary(EXPR_ADDR_OF, expr);
        } break;

        default:
        {
            expr = parse_expr_first_level();
        } break;
    }

    return(expr);
}

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
            case TOK_ASTERISK:{ l = make_expr_binary(EXPR_MUL, l, r); } break;
            case TOK_SLASH:   { l = make_expr_binary(EXPR_DIV, l, r); } break;
            case TOK_PERCENT: { l = make_expr_binary(EXPR_MOD, l, r); } break;
            case TOK_PLUS:    { l = make_expr_binary(EXPR_ADD, l, r); } break;
            case TOK_MINUS:   { l = make_expr_binary(EXPR_SUB, l, r); } break;
            case TOK_LT:      { l = make_expr_binary(EXPR_LT, l, r); } break;
            case TOK_LE:      { l = make_expr_binary(EXPR_LE, l, r); } break;
            case TOK_GT:      { l = make_expr_binary(EXPR_GT, l, r); } break;
            case TOK_GE:      { l = make_expr_binary(EXPR_GE, l, r); } break;

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
parse_expr_ternary()
{
    Expr *l;
    Expr *m;
    Expr *r;
    Token tok;

    l = parse_expr_binary(999);
    tok = tok_peek();
    if(tok.kind == TOK_QMARK)
    {
        tok_next();
        m = parse_expr();
        tok_expect(TOK_COLON);
        r = parse_expr_ternary();
        l = make_expr_ternary(l, m, r);
    }

    return(l);
}

Expr *
parse_expr_assign()
{
    Expr *expr;
    Token tok;

    expr = parse_expr_ternary();
    tok = tok_peek();
    if(tok.kind == TOK_EQUAL)
    {
        tok_next();
        expr = make_expr_binary(EXPR_ASSIGN, expr, parse_expr_assign());
    }

    return(expr);
}

Expr *
parse_expr()
{
    Expr *expr = 0;
    Expr *curr = 0;
    Token tok;

    expr = parse_expr_assign();
    curr = expr;

    tok = tok_peek();
    while(tok.kind == TOK_COMMA)
    {
        tok_next();

        curr->next = parse_expr_assign();
        curr = curr->next;

        tok = tok_peek();
    }
    if(expr != curr)
    {
        expr = make_expr_compound(expr);
    }

    return(expr);
}

/*
 * <decl> ::= <type> <ident> ('[' <expr> ']')? ';'
 *
 * <type> ::= <base_type> '*'*
 *
 * <base_type> ::= 'void' | 'char' | 'int'
 *          | 'struct' <ident> <struct-definition>?
 */

Type *parse_base_type();
Type *parse_type(Type *base_type);
AggrElement *parse_struct_def();

AggrElement *
parse_struct_def()
{
    AggrElement *res = 0;
    AggrElement *curr = 0;
    Type *type;
    Token tok;
    char *id;
    int offset;

    tok_expect(TOK_LBRACE);

    offset = 0;
    tok = tok_peek();
    while(tok.kind != TOK_RBRACE)
    {
        type = parse_type(0);
        tok = tok_expect(TOK_ID);
        id = tok.id;
        if(curr)
        {
            curr->next = make_aggr_element(id, type, offset);
            curr = curr->next;
        }
        else
        {
            curr = make_aggr_element(id, type, offset);
            res = curr;
        }
        offset += ALIGN(type->size, 4);
        tok_expect(TOK_SEMI);
        tok = tok_peek();
    }

    tok_expect(TOK_RBRACE);

    if(!res)
    {
        syntax_fatal("Invalid struct definition");
    }

    return(res);
}

Type *
parse_base_type()
{
    Type *type = 0;
    Token tok;
    char *id;
    AggrElement *sdef;

    tok = tok_peek();
    switch(tok.kind)
    {
        case TOK_KW_VOID:
        {
            tok_next();
            type = type_void();
        } break;

        case TOK_KW_CHAR:
        {
            tok_next();
            type = type_char();
        } break;

        case TOK_KW_INT:
        {
            tok_next();
            type = type_int();
        } break;

        case TOK_KW_STRUCT:
        {
            tok_next();
            tok = tok_expect(TOK_ID);
            id = tok.id;

            sdef = 0;
            tok = tok_peek();
            if(tok.kind == TOK_LBRACE)
            {
                sdef = parse_struct_def();
            }

            type = type_struct(id, sdef);
        } break;
    }

    return(type);
}

Type *
parse_type(Type *base_type)
{
    Type *type = 0;
    Token tok;

    if(!base_type)
    {
        base_type = parse_base_type();
    }
    type = base_type;

    tok = tok_peek();
    while(tok.kind == TOK_ASTERISK)
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
    char *id;
    Expr *expr;
    int length;

    type = parse_base_type();
    if(!type)
    {
        syntax_fatal("Invalid type for variable declaration");
    }

    type = parse_type(type);

    tok = tok_expect(TOK_ID);
    id = tok.id;

    tok = tok_peek();
    if(tok.kind == TOK_LBRACK)
    {
        tok_next();

        expr = parse_expr();
        if(!expr_is_const(expr))
        {
            syntax_fatal("Invalid constant expression for array length");
        }
        length = eval_expr(expr);

        type = type_array(type, length);

        tok_expect(TOK_RBRACK);
    }

    decl = make_decl(type, id);

    /* TODO: Parse variable initialization */

    tok_expect(TOK_SEMI);

    return(decl);
}

/*
 * <stmt> ::= <label> ':'
 *          | <decl>
 *          | <expr>
 *          | <stmt_block>
 *          | 'return' <expr>? ';'
 *          | 'goto' <ident> ';'
 *          | 'if' '(' <expr> ')' <stmt> 'else' <stmt>
 *          | 'while' '(' <expr> ')' <stmt>
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
    Token tok2;
    char *label;
    Expr *expr;
    Stmt *then_stmt;
    Stmt *else_stmt;

    Expr *init;
    Expr *cond;
    Expr *post;

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
            case TOK_ID:
            {
                label = tok.id;
                tok_next();

                tok2 = tok_peek();
                if(tok2.kind == TOK_COLON)
                {
                    tok_next();
                    stmt = make_stmt(STMT_LABEL);
                    stmt->u.label = label;
                }
                else
                {
                    tok_putback(tok);
                    stmt = make_stmt(STMT_EXPR);
                    stmt->u.expr = parse_expr();
                    tok_expect(TOK_SEMI);
                }
            } break;

            case TOK_LBRACE:
            {
                stmt = parse_stmt_block();
            } break;

            case TOK_KW_RETURN:
            {
                tok_next();
                stmt = make_stmt(STMT_RET);
                stmt->u.expr = 0;
                tok = tok_peek();
                if(tok.kind != TOK_SEMI)
                {
                    stmt->u.expr = parse_expr();
                }
                tok_expect(TOK_SEMI);
            } break;

            case TOK_KW_GOTO:
            {
                tok_next();
                tok = tok_expect(TOK_ID);
                stmt = make_stmt(STMT_GOTO);
                stmt->u.label = tok.id;
                tok_expect(TOK_SEMI);
            } break;

            case TOK_KW_IF:
            {
                tok_next();

                tok_expect(TOK_LPAREN);
                expr = parse_expr();
                tok_expect(TOK_RPAREN);

                then_stmt = parse_stmt();
                else_stmt = 0;

                tok = tok_peek();
                if(tok.kind == TOK_KW_ELSE)
                {
                    tok_next();
                    else_stmt = parse_stmt();
                }

                stmt = make_stmt_if(expr, then_stmt, else_stmt);
            } break;

            case TOK_KW_WHILE:
            {
                tok_next();

                tok_expect(TOK_LPAREN);
                expr = parse_expr();
                tok_expect(TOK_RPAREN);

                then_stmt = parse_stmt();

                stmt = make_stmt_while(expr, then_stmt);
            } break;

            case TOK_KW_FOR:
            {
                tok_next();

                tok_expect(TOK_LPAREN);
                init = parse_expr();
                tok_expect(TOK_SEMI);
                cond = parse_expr();
                tok_expect(TOK_SEMI);
                post = parse_expr();
                tok_expect(TOK_RPAREN);

                then_stmt = parse_stmt();

                stmt = make_stmt_for(init, cond, post, then_stmt);
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
 * <glob_decl> ::= <type> <ident> ['(' <func_params>? ')' [<stmt_block>|';']]?
 * <func_params> ::= <type> <ident>
 */

FuncParam *
parse_func_param()
{
    FuncParam *param;
    char *id;
    Type *type;
    Token tok;

    type = parse_type(0);

#if 0
    type = parse_base_type();
    if(!type)
    {
        syntax_fatal("Invalid type for function parameter");
    }

    type = parse_type(type);
#endif

    tok = tok_expect(TOK_ID);
    id = tok.id;

    param = make_func_param(id, type);

    return(param);
}

GlobDecl *
parse_glob_decl()
{
    GlobDecl *glob_decl = 0;

    Type *type;
    Token tok;
    char *id;
    FuncParam *params;
    FuncParam *curr_param;
    Stmt *func_def;


    type = parse_base_type();
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
    }
    else
    {
        params = 0;
        curr_param = 0;

        tok_expect(TOK_LPAREN);
        tok = tok_peek();
        while(tok.kind != TOK_RPAREN)
        {
            if(curr_param)
            {
                curr_param->next = parse_func_param();
                curr_param = curr_param->next;
            }
            else
            {
                curr_param = parse_func_param();
                curr_param->next = 0;
                params = curr_param;
            }

            tok = tok_peek();
            if(tok.kind != TOK_COMMA)
            {
                break;
            }
            else
            {
                tok = tok_next();
            }
        }
        tok_expect(TOK_RPAREN);

        params = reverse_func_params_list(params);
        type = type_func(type, params);

        func_def = 0;
        tok = tok_peek();
        if(tok.kind == TOK_LBRACE)
        {
            func_def = parse_stmt_block();
        }
        else
        {
            tok_expect(TOK_SEMI);
        }
        glob_decl = make_glob_decl_func(id, type, func_def);
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
/**                           SEMANTIC ANALYSIS                              **/
/******************************************************************************/

#include <stdarg.h>

void
semantic_error(char *fmt, ...)
{
    va_list ap;

    printf("[!] SEMANTIC ERROR: ");
    va_start(ap, fmt);
    vprintf(fmt, ap);
    va_end(ap);
    printf("\n");
}

void
semantic_fatal(char *fmt, ...)
{
    va_list ap;

    printf("[!] SEMANTIC ERROR: ");
    va_start(ap, fmt);
    vprintf(fmt, ap);
    va_end(ap);
    printf("\n");

    exit(1);
}

Sym *curr_func;

Type *
resolve_expr_type(Expr *expr, Type *wanted)
{
    Type *type = 0;
    Sym *sym;
    Type *lt;
    Type *mt;
    Type *rt;
    Expr *curr;
    AggrElement *curr_el;

    switch(expr->kind)
    {
        case EXPR_INTLIT:
        {
            if(wanted && wanted == type_char())
            {
                type = type_char();
            }
            else
            {
                type = type_int();
            }
        } break;

        case EXPR_ID:
        {
            sym = sym_get(expr->id);
            if(!sym)
            {
                /* DEBUG TODO: to delete */
                print_sym_table();
                printf("\n\n");

                semantic_fatal("Invalid symbol %s in expression", expr->id);
            }
            type = sym->type;
        } break;

        case EXPR_CALL:
        {
            type = resolve_expr_type(expr->l, wanted);
            type = type->base_type;
        } break;

        case EXPR_ARR_SUB:
        {
            type = resolve_expr_type(expr->l, 0);
            if(!type || (type->kind != TYPE_ARRAY && type->kind != TYPE_PTR))
            {
                semantic_fatal("Cannot operate array subscription on non-array or non-ptr");
            }
            type = type->base_type;
        } break;

        case EXPR_MEMB_ACCESS:
        {
            type = resolve_expr_type(expr->l, 0);
            if(!type || type->kind != TYPE_STRUCT)
            {
                semantic_fatal("Cannot access a member of a non-struct");
            }
            if(!type->def)
            {
                semantic_fatal("Cannot access a member of an undefined struct");
            }

            curr_el = get_struct_member(type, expr->id);
            type = 0;
            if(curr_el)
            {
                type = curr_el->type;
            }
            if(!type)
            {
                semantic_fatal("Tried to access an invalid struct member");
            }
        } break;

        case EXPR_MEMB_ACCESS_PTR:
        {
            type = resolve_expr_type(expr->l, 0);
            if(!type || type->kind != TYPE_PTR || type->base_type->kind != TYPE_STRUCT)
            {
                semantic_fatal("Cannot access a member of a non-pointer-to-struct");
            }
            if(!type->base_type->def)
            {
                semantic_fatal("Cannot access a member of an undefined struct");
            }

            curr_el = get_struct_member(type->base_type, expr->id);
            type = 0;
            if(curr_el)
            {
                type = curr_el->type;
            }
            if(!type)
            {
                semantic_fatal("Tried to access an invalid struct member");
            }
        } break;

        default:
        {
            if(expr->kind == EXPR_CALL)
            {
                type = resolve_expr_type(expr->l, wanted);
                type = type->base_type;
            }
            else if(expr->kind == EXPR_CAST)
            {
                type = resolve_expr_type(expr->l, wanted);
                type = expr->cast_to;
            }
            else if(expr->kind == EXPR_DEREF)
            {
                type = resolve_expr_type(expr->l, wanted);
                type = type->base_type;
            }
            else if(expr->kind == EXPR_ADDR_OF)
            {
                type = resolve_expr_type(expr->l, wanted);
                type = type_ptr(type);
            }
            else if(expr->kind >= EXPR_UNARY && expr->kind < EXPR_UNARY_END)
            {
                type = resolve_expr_type(expr->l, wanted);
            }
            else if(expr->kind >= EXPR_BINARY && expr->kind < EXPR_BINARY_END)
            {
                lt = resolve_expr_type(expr->l, wanted);
                rt = resolve_expr_type(expr->l, lt);

                if(lt->kind == TYPE_PTR && (rt == type_char() || rt == type_int()))
                {
                    type = lt;
                }
                else if(rt->kind == TYPE_PTR && (lt == type_char() || lt == type_int()))
                {
                    type = rt;
                }
                else if(lt != rt)
                {
                    semantic_fatal("Cannot operate on 2 different types");
                }

                type = lt;
            }
            else if(expr->kind == EXPR_TERNARY)
            {
                mt = resolve_expr_type(expr->m, wanted);
                rt = resolve_expr_type(expr->r, mt);

                if(mt != rt)
                {
                    semantic_fatal("Type mismatch in ternary expression");
                }

                type = rt;
            }
            else if(expr->kind == EXPR_ASSIGN)
            {
                lt = resolve_expr_type(expr->l, wanted);
                rt = resolve_expr_type(expr->l, lt);

                if( lt->kind == TYPE_PTR && (rt == type_char() || rt == type_int()) &&
                    expr_is_const(expr->r) && eval_expr(expr->r) == 0)
                {
                    rt = lt;
                }
                else if(lt != rt)
                {
                    semantic_fatal("Cannot assign a different type");
                }

                type = lt;
            }
            else if(expr->kind == EXPR_COMPOUND)
            {
                curr = expr->l;
                while(curr)
                {
                    type = resolve_expr_type(curr, 0);
                    curr = curr->next;
                }
            }
            else
            {
                assert(0);
            }
        } break;
    }

    if(!type)
    {
        semantic_fatal("Invalid expression type");
    }

    if(type && wanted && type == type_char() && wanted == type_int())
    {
        type = type_int();
    }

    /* Array decays into a pointer */
    if(type->kind == TYPE_ARRAY && expr->kind != EXPR_ADDR_OF)
    {
        type = type_ptr(type->base_type);
    }

    return(type);
}

int
check_lvalue(Expr *expr)
{
    int res = 0;

    if(expr->kind == EXPR_ID)
    {
        res = 1;
    }
    else if(expr->kind == EXPR_DEREF)
    {
        res = 1;
    }
    else if(expr->kind == EXPR_ARR_SUB)
    {
        res = 1;
    }
    else if(expr->kind == EXPR_MEMB_ACCESS)
    {
        res = 1;
    }
    else if(expr->kind == EXPR_MEMB_ACCESS_PTR)
    {
        res = 1;
    }

    return(res);
}

void
check_expr(Expr *expr)
{
    Type *type;
    Type *lt;
    Type *mt;
    Type *rt;

    Expr *arg;
    FuncParam *param;
    AggrElement *aggr_el;

    switch(expr->kind)
    {
        case EXPR_INTLIT:
        case EXPR_ID:
        {
            /* Nothing */
        } break;

        case EXPR_CALL:
        {
            type = resolve_expr_type(expr->l, 0);
            if(type->kind != TYPE_FUNC &&
               type->kind != TYPE_PTR &&
               type->base_type->kind != TYPE_FUNC)
            {
                semantic_fatal("Invalid function call");
            }
            if(type->kind == TYPE_PTR)
            {
                type = type->base_type;
            }

            arg = expr->r;
            param = type->params;

            if((arg != 0 && param == 0) ||
               (arg == 0 && param != 0))
            {
                semantic_fatal("Invalid number of arguments in function call");
            }
            while(arg)
            {
                lt = param->type;
                rt = resolve_expr_type(arg, lt);
                if(lt != rt)
                {
                    semantic_fatal("Invalid type of argument in function call");
                }

                arg = arg->next;
                param = param->next;
                if((arg != 0 && param == 0) ||
                   (arg == 0 && param != 0))
                {
                    semantic_fatal("Invalid number of arguments in function call");
                }
            }
        } break;

        case EXPR_ARR_SUB:
        {
            lt = resolve_expr_type(expr->r, 0);
            if(lt->kind != TYPE_ARRAY && lt->kind != TYPE_PTR)
            {
                semantic_fatal("Array subscription used to non-array and non-pointer");
            }

            if(lt->base_type == type_void())
            {
                semantic_fatal("Subscription on a void pointer");
            }

            rt = resolve_expr_type(expr->r, type_int());
            if(rt != type_int())
            {
                semantic_fatal("Array subscription operand must be an integer");
            }
        } break;

        case EXPR_MEMB_ACCESS:
        {
            lt = resolve_expr_type(expr->l, 0);
            if(!lt || lt->kind != TYPE_STRUCT)
            {
                semantic_fatal("Cannot access a member of a non-struct");
            }
            if(!lt->def)
            {
                semantic_fatal("Cannot access a member of an undefined struct");
            }

            aggr_el = lt->def;
            lt = 0;
            while(aggr_el)
            {
                if(aggr_el->id == expr->id)
                {
                    lt = aggr_el->type;
                    break;
                }
                aggr_el = aggr_el->next;
            }
            if(!lt)
            {
                semantic_fatal("Tried to access an invalid struct member");
            }
        } break;

        case EXPR_MEMB_ACCESS_PTR:
        {
            lt = resolve_expr_type(expr->l, 0);
            if(!lt || lt->kind != TYPE_PTR || lt->base_type->kind != TYPE_STRUCT)
            {
                semantic_fatal("Cannot access a member of a non-pointer-to-struct");
            }
            if(!lt->base_type->def)
            {
                semantic_fatal("Cannot access a member of an undefined struct");
            }

            aggr_el = lt->base_type->def;
            lt = 0;
            while(aggr_el)
            {
                if(aggr_el->id == expr->id)
                {
                    lt = aggr_el->type;
                    break;
                }
                aggr_el = aggr_el->next;
            }
            if(!lt)
            {
                semantic_fatal("Tried to access an invalid struct member");
            }
        } break;

        case EXPR_INC_PRE:
        {
            if(!check_lvalue(expr->l))
            {
                semantic_fatal("Invalid lvalue (operand of pre-increment '++')");
            }
        } break;

        case EXPR_DEC_PRE:
        {
            if(!check_lvalue(expr->l))
            {
                semantic_fatal("Invalid lvalue (operand of pre-decrement '--')");
            }
        } break;

        case EXPR_NEG:
        {
            lt = resolve_expr_type(expr->l, 0);

            if(!type_is_arithmetic(lt))
            {
                semantic_fatal("Invalid arithmetic expression operand");
            }
        } break;

        case EXPR_CAST:
        {
            lt = resolve_expr_type(expr->l, 0);
        } break;

        case EXPR_DEREF:
        {
            type = resolve_expr_type(expr->l, 0);
            if(type->kind != TYPE_PTR)
            {
                semantic_fatal("Invalid pointer for dereference");
            }
        } break;

        case EXPR_ADDR_OF:
        {
            if(!check_lvalue(expr->l))
            {
                semantic_fatal("Invalid lvalue (operand of unary '&')");
            }
        } break;

        case EXPR_MUL: case EXPR_DIV: case EXPR_MOD:
        case EXPR_ADD: case EXPR_SUB:
        case EXPR_LT: case EXPR_LE:
        case EXPR_GT: case EXPR_GE:
        {
            lt = resolve_expr_type(expr->l, 0);
            rt = resolve_expr_type(expr->r, lt);

            if((!type_is_arithmetic(lt) && lt->kind != TYPE_PTR) ||
               (!type_is_arithmetic(rt) && rt->kind != TYPE_PTR))
            {
                semantic_fatal("Invalid arithmetic expression operand");
            }
        } break;

        case EXPR_TERNARY:
        {
            lt = resolve_expr_type(expr->l, 0);
            if(lt != type_char() && lt != type_int() && lt->kind != TYPE_PTR)
            {
                semantic_fatal("Invalid condition for ternary expression");
            }

            mt = resolve_expr_type(expr->m, 0);
            rt = resolve_expr_type(expr->r, mt);
            if(mt != rt)
            {
                semantic_fatal("Type mismatch in ternary expression");
            }
        } break;

        case EXPR_ASSIGN:
        {
            if(!check_lvalue(expr->l))
            {
                semantic_fatal("Invalid lvalue (left operand of assignment)");
            }

            lt = resolve_expr_type(expr->l, 0);
            if(lt->kind == TYPE_ARRAY)
            {
                semantic_fatal("Cannot assign to an array variable (only to its elements)");
            }

            rt = resolve_expr_type(expr->r, lt);
            if(lt != rt)
            {
                semantic_fatal("Invalid assignment expression (types mismatch)");
            }
        } break;

        case EXPR_COMPOUND:
        {
            arg = expr->l;
            while(arg)
            {
                check_expr(arg);
                arg = arg->next;
            }
        } break;

        default:
        {
            assert(0);
        } break;
    }

    resolve_expr_type(expr, 0);
}

void
check_stmt(Stmt *stmt)
{
    Type *type;
    Decl *decl;
    Stmt *curr;
    Sym *sym;
    Label *lbl;

    switch(stmt->kind)
    {
        case STMT_DECL:
        {
            decl = stmt->u.decl;

            if(decl->type == type_void())
            {
                semantic_fatal("You cannot declare a void type variable");
            }

            sym = sym_add(decl->id, decl->type);
            sym->global = 0;
            sym->offset = func_var_offset;
            func_var_offset -= decl->type->size;
        } break;

        case STMT_EXPR:
        {
            check_expr(stmt->u.expr);
        } break;

        case STMT_BLOCK:
        {
            curr = stmt->u.block;
            while(curr)
            {
                check_stmt(curr);
                curr = curr->next;
            }
        } break;

        case STMT_RET:
        {
            if(stmt->u.expr)
            {
                type = resolve_expr_type(stmt->u.expr, curr_func->type->base_type);
            }
            else
            {
                type = type_void();
            }

            if(type != curr_func->type->base_type)
            {
                semantic_fatal("Return expression does not match function return type");
            }
        } break;

        case STMT_LABEL:
        {
            lbl = label_get(stmt->u.label);
            if(lbl)
            {
                if(lbl->status == LABEL_DEFINED)
                {
                    semantic_fatal("Cannot redefine the label '%s'", lbl->id);
                }
            }
            else
            {
                lbl = label_add(stmt->u.label);
                lbl->status = LABEL_DEFINED;
            }
        } break;

        case STMT_GOTO:
        {
            label_get_or_add(stmt->u.label);
        } break;

        case STMT_IF:
        {
            check_expr(stmt->cond);
            check_stmt(stmt->then_stmt);
            if(stmt->else_stmt)
            {
                check_stmt(stmt->else_stmt);
            }
        } break;

        case STMT_WHILE:
        {
            check_expr(stmt->cond);
            check_stmt(stmt->then_stmt);
        } break;

        case STMT_FOR:
        {
            check_expr(stmt->init);
            check_expr(stmt->cond);
            check_expr(stmt->post);
            check_stmt(stmt->then_stmt);
        } break;

        default:
        {
            assert(0);
        } break;
    }
}

void
check_glob_decl(GlobDecl *decl)
{
    Sym *sym;
    FuncParam *param;
    int sym_count;
    int offset;

    switch(decl->kind)
    {
        case GLOB_DECL_VAR:
        {
            sym = sym_get(decl->id);
            if(sym)
            {
                semantic_fatal("Global variable '%s' already declared", decl->id);
            }

            sym = sym_add(decl->id, decl->type);
            sym->global = 1;
        } break;

        case GLOB_DECL_FUNC:
        {
            sym = sym_get(decl->id);
            if(sym)
            {
                semantic_fatal("Function '%s' already declared", decl->id);
            }

            sym = sym_add(decl->id, decl->type);
            sym->global = 1;

            curr_func = sym;

            if(decl->func_def)
            {
                func_var_offset = -8;

                sym_count = sym_table_count;
                param = decl->type->params;
                offset = 8;
                while(param)
                {
                    sym_add_func_param(param->id, param->type, offset);
                    offset += ALIGN(param->type->size, 4);
                    param = param->next;
                }

                check_stmt(decl->func_def);

                sym_table_count = sym_count;
            }
        } break;

        default:
        {
            assert(0);
        } break;
    }
}

void
check_unit(GlobDecl *unit)
{
    GlobDecl *curr;
    int i;
    Label *lbl;

    sym_reset();

    curr = unit;
    while(curr)
    {
        check_glob_decl(curr);
        curr = curr->next;
    }

    for(i = 0;
        i < label_table_count;
        ++i)
    {
        lbl = &(label_table[i]);
        if(lbl->status != LABEL_DEFINED)
        {
            semantic_error("Label '%s' used but not defined", lbl->id);
        }
    }
}

/******************************************************************************/
/**                              AST -> IR-C                                 **/
/******************************************************************************/

/*
 * We use the AST to represent our Intermediate Representation tree since
 * our IR language is a subset of the C language (hence IR-C).
 *
 * IR-C Description:
 *
 * There are only 5 kinds of statement:
 * 1. Return statements
 * 2. Expressions
 * 3. Jumps
 * 4. Labels definition
 * 5. Branches
 *
 * Sources:
 * [1] https://ls12-www.cs.tu-dortmund.de/daes/media/documents/publications/downloads/2003-samosIII.pdf
 */

int lbl_count = 0;

char *
lbl_gen()
{
    char *res;
    char lbl[128];
    int n;

    lbl[0] = '_';
    lbl[1] = '_';
    lbl[2] = '_';
    lbl[3] = 'L';
    n = sprintf(lbl+4, "%d", lbl_count);
    lbl[n+4] = 0;
    ++lbl_count;

    res = str_intern(lbl);
    return(res);
}

char tmp_var_buff[64];
int tmp_vars_count = 0;

char *
tmp_var()
{
    int n;

    tmp_var_buff[0] = '_';
    tmp_var_buff[1] = '_';
    tmp_var_buff[2] = '_';
    tmp_var_buff[3] = 't';
    n = sprintf(tmp_var_buff+4, "%d", tmp_vars_count);
    tmp_var_buff[n+4] = 0;
    ++tmp_vars_count;

    return(str_intern(tmp_var_buff));
}

Stmt *curr_block;

void
add_stmt(Stmt *stmt)
{
    Stmt *last;

    last = curr_block->u.block;
    while(last && last->next)
    {
        last = last->next;
    }

    if(last)
    {
        last->next = stmt;
    }
    else
    {
        curr_block->u.block = stmt;
    }
}

char *
declare_tmp_var(Type *type)
{
    char *res;
    Stmt *stmt;

    res = tmp_var();
    assert(type);
    stmt = make_stmt_decl(make_decl(type, res));
    stmt->next = 0;
    add_stmt(stmt);

    return(res);
}

void block_to_irc(Stmt *block);
Stmt *func_def_to_irc(Stmt *block);

int
expr_is_atom(Expr *expr)
{
    if(expr->kind == EXPR_INTLIT || expr->kind == EXPR_ID)
    {
        return(1);
    }
    return(0);
}

void expr_to_irc(Expr *expr);
Expr *reduce_expr_to_atom(Expr *expr);

char *
store_expr_temp_var(Expr *expr)
{
    char *res;
    Stmt *stmt;
    Expr *rvalue;

    Expr *l;
    Expr *m;
    Expr *r;

    Expr *arg;
    Expr *args;
    Expr *tmp;

    Type *lt;
    char *t1;
    char *t2;
    char *t3;
    AggrElement *aggr_el;

    char *lbl1;
    char *lbl2;
    char *lbl3;

    res = tmp_var();
    stmt = make_stmt(STMT_DECL);
    stmt->u.decl = make_decl(resolve_expr_type(expr, 0), res);
    add_stmt(stmt);

    rvalue = 0;
    if(expr_is_atom(expr))
    {
        if(expr->kind == EXPR_ID)
        {
            lt = resolve_expr_type(expr, 0);

            /* Array decays into a pointer */
            if(lt->kind == TYPE_ARRAY)
            {
                stmt->u.decl->type = type_ptr(lt->base_type);
                rvalue = make_expr_id(expr->id);
            }
            else
            {
                rvalue = dup_expr(expr);
            }
        }
        else
        {
            rvalue = dup_expr(expr);
        }
    }
    else if(expr->kind == EXPR_CALL)
    {
        l = reduce_expr_to_atom(expr->l);

        args = 0;
        arg = 0;
        r = expr->r;
        while(r)
        {
            tmp = reduce_expr_to_atom(r);
            tmp->next = 0;

            if(arg)
            {
                arg->next = tmp;
                arg = arg->next;
            }
            else
            {
                arg = tmp;
                args = arg;
            }

            r = r->next;
        }

        rvalue = make_expr_binary(EXPR_CALL, l, args);
    }
    else if(expr->kind == EXPR_ARR_SUB)
    {
        lt = resolve_expr_type(expr->l, 0);

        t1 = store_expr_temp_var(expr->r);
        stmt = make_stmt(STMT_EXPR);
        stmt->u.expr = make_expr_binary(
            EXPR_ASSIGN,
            make_expr_id(t1), 
            make_expr_binary(
                EXPR_MUL,
                make_expr_id(t1),
                make_expr_intlit(lt->base_type->size)));
        stmt->next = 0;
        add_stmt(stmt);

        t2 = store_expr_temp_var(expr->l);

        t3 = tmp_var();
        stmt = make_stmt(STMT_DECL);
        stmt->u.decl = make_decl(type_ptr(type_char()), t3);
        stmt->next = 0;
        add_stmt(stmt);

        stmt = make_stmt(STMT_EXPR);
        stmt->u.expr = make_expr_binary(
            EXPR_ASSIGN,
            make_expr_id(t3),
            make_expr_cast(make_expr_id(t2), type_ptr(type_char())));
        stmt->next = 0;
        add_stmt(stmt);

        stmt = make_stmt(STMT_EXPR);
        stmt->u.expr = make_expr_binary(
            EXPR_ASSIGN,
            make_expr_id(t3),
            make_expr_binary(
                EXPR_ADD,
                make_expr_id(t3),
                make_expr_id(t1)));
        stmt->next = 0;
        add_stmt(stmt);

        stmt = make_stmt(STMT_EXPR);
        stmt->u.expr = make_expr_binary(
            EXPR_ASSIGN,
            make_expr_id(t2),
            make_expr_cast(make_expr_id(t3), type_ptr(lt->base_type)));
        stmt->next = 0;
        add_stmt(stmt);

        rvalue = make_expr_unary(EXPR_DEREF, make_expr_id(t2));
    }
    else if(expr->kind == EXPR_MEMB_ACCESS)
    {
        lt = resolve_expr_type(expr->l, 0);
        assert(lt->kind == TYPE_STRUCT);
        l = reduce_expr_to_atom(expr->l);

        t1 = declare_tmp_var(type_ptr(lt));

        stmt = make_stmt_expr(make_expr_binary(
            EXPR_ASSIGN,
            make_expr_id(t1),
            make_expr_unary(EXPR_ADDR_OF, dup_expr(l))));
        add_stmt(stmt);

        t2 = declare_tmp_var(type_ptr(type_char()));

        stmt = make_stmt_expr(make_expr_binary(
            EXPR_ASSIGN,
            make_expr_id(t2),
            make_expr_cast(make_expr_id(t1), type_ptr(type_char()))));
        add_stmt(stmt);

        stmt = make_stmt_expr(make_expr_binary(
            EXPR_ASSIGN,
            make_expr_id(t2),
            make_expr_binary(
                EXPR_ADD,
                make_expr_id(t2),
                make_expr_intlit(get_struct_member_offset(lt, expr->id)))));
        add_stmt(stmt);

        aggr_el = get_struct_member(lt, expr->id);
        assert(aggr_el);

        t3 = declare_tmp_var(type_ptr(aggr_el->type));

        stmt = make_stmt_expr(make_expr_binary(
            EXPR_ASSIGN,
            make_expr_id(t3),
            make_expr_cast(make_expr_id(t2), type_ptr(aggr_el->type))));
        add_stmt(stmt);

        rvalue = make_expr_unary(EXPR_DEREF, make_expr_id(t3));
    }
    else if(expr->kind == EXPR_MEMB_ACCESS_PTR)
    {
        lt = resolve_expr_type(expr->l, 0);
        assert(lt->kind == TYPE_PTR && lt->base_type->kind == TYPE_STRUCT);
        l = reduce_expr_to_atom(expr->l);

        t1 = declare_tmp_var(type_ptr(type_char()));

        stmt = make_stmt_expr(make_expr_binary(
            EXPR_ASSIGN,
            make_expr_id(t1),
            make_expr_cast(dup_expr(l), type_ptr(type_char()))));
        add_stmt(stmt);

        stmt = make_stmt_expr(make_expr_binary(
            EXPR_ASSIGN,
            make_expr_id(t1),
            make_expr_binary(
                EXPR_ADD,
                make_expr_id(t1),
                make_expr_intlit(get_struct_member_offset(lt, expr->id)))));
        add_stmt(stmt);

        aggr_el = get_struct_member(lt, expr->id);
        assert(aggr_el);

        t2 = declare_tmp_var(type_ptr(aggr_el->type));

        stmt = make_stmt_expr(make_expr_binary(
            EXPR_ASSIGN,
            make_expr_id(t2),
            make_expr_cast(make_expr_id(t1), type_ptr(aggr_el->type))));
        add_stmt(stmt);

        rvalue = make_expr_unary(EXPR_DEREF, make_expr_id(t2));
    }
    else if(expr->kind >= EXPR_UNARY && expr->kind < EXPR_UNARY_END)
    {
        l = reduce_expr_to_atom(expr->l);
        rvalue = make_expr_unary(expr->kind, l);
    }
    else if(expr->kind >= EXPR_BINARY && expr->kind < EXPR_BINARY_END)
    {
        l = reduce_expr_to_atom(expr->l);
        r = reduce_expr_to_atom(expr->r);
        rvalue = make_expr_binary(expr->kind, l, r);
    }
    else if(expr->kind == EXPR_TERNARY)
    {
        lbl1 = lbl_gen();
        lbl2 = lbl_gen();
        lbl3 = lbl_gen();

        l = reduce_expr_to_atom(expr->l);

        stmt = make_stmt_if(l, 0, 0);
        stmt->u.label = lbl1;
        add_stmt(stmt);

        stmt = make_stmt(STMT_GOTO);
        stmt->u.label = lbl2;
        stmt->next = 0;
        add_stmt(stmt);

        stmt = make_stmt(STMT_LABEL);
        stmt->u.label = lbl1;
        stmt->next = 0;
        add_stmt(stmt);

        m = reduce_expr_to_atom(expr->m);

        stmt = make_stmt(STMT_EXPR);
        stmt->u.expr = make_expr_binary(EXPR_ASSIGN, make_expr_id(res), m);
        stmt->next = 0;
        add_stmt(stmt);

        stmt = make_stmt(STMT_GOTO);
        stmt->u.label = lbl3;
        stmt->next = 0;
        add_stmt(stmt);

        stmt = make_stmt(STMT_LABEL);
        stmt->u.label = lbl2;
        stmt->next = 0;
        add_stmt(stmt);

        r = reduce_expr_to_atom(expr->r);

        stmt = make_stmt(STMT_EXPR);
        stmt->u.expr = make_expr_binary(EXPR_ASSIGN, make_expr_id(res), m);
        stmt->next = 0;
        add_stmt(stmt);

        stmt = make_stmt(STMT_LABEL);
        stmt->u.label = lbl3;
        stmt->next = 0;
        add_stmt(stmt);
    }
    else if(expr->kind == EXPR_ASSIGN)
    {
        l = reduce_expr_to_atom(expr->l);
        r = reduce_expr_to_atom(expr->r);

        stmt = make_stmt(STMT_EXPR);
        stmt->u.expr = make_expr_binary(EXPR_ASSIGN, l, r);
        add_stmt(stmt);

        rvalue = l;
    }
    else if(expr->kind == EXPR_COMPOUND)
    {
        l = expr->l;
        while(l)
        {
            if(l->next)
            {
                expr_to_irc(l);
            }
            else
            {
                rvalue = reduce_expr_to_atom(l);
            }
            l = l->next;
        }
    }
    else
    {
        assert(0);
    }

    if(rvalue)
    {
        stmt = make_stmt(STMT_EXPR);
        stmt->u.expr = make_expr_binary(EXPR_ASSIGN, make_expr_id(res), rvalue);
        add_stmt(stmt);
    }

    return(res);
}

Expr *
reduce_expr_to_atom(Expr *expr)
{
    Expr *res = 0;

    Stmt *stmt;
    Expr *l;
    Type *lt;
    char *t1;
    char *t2;
    char *t3;
    AggrElement *aggr_el;

    if(expr_is_atom(expr))
    {
        res = dup_expr(expr);
    }
    else if(expr->kind == EXPR_DEREF)
    {
        res = dup_expr(expr);
    }
    else if(expr->kind == EXPR_ARR_SUB)
    {
        lt = resolve_expr_type(expr->l, 0);

        t1 = store_expr_temp_var(expr->r);
        stmt = make_stmt(STMT_EXPR);
        stmt->u.expr = make_expr_binary(
            EXPR_ASSIGN,
            make_expr_id(t1), 
            make_expr_binary(
                EXPR_MUL,
                make_expr_id(t1),
                make_expr_intlit(lt->base_type->size)));
        stmt->next = 0;
        add_stmt(stmt);

        t2 = store_expr_temp_var(expr->l);

        t3 = tmp_var();
        stmt = make_stmt(STMT_DECL);
        stmt->u.decl = make_decl(type_ptr(type_char()), t3);
        stmt->next = 0;
        add_stmt(stmt);

        stmt = make_stmt(STMT_EXPR);
        stmt->u.expr = make_expr_binary(
            EXPR_ASSIGN,
            make_expr_id(t3),
            make_expr_cast(make_expr_id(t2), type_ptr(type_char())));
        stmt->next = 0;
        add_stmt(stmt);

        stmt = make_stmt(STMT_EXPR);
        stmt->u.expr = make_expr_binary(
            EXPR_ASSIGN,
            make_expr_id(t3),
            make_expr_binary(
                EXPR_ADD,
                make_expr_id(t3),
                make_expr_id(t1)));
        stmt->next = 0;
        add_stmt(stmt);

        stmt = make_stmt(STMT_EXPR);
        stmt->u.expr = make_expr_binary(
            EXPR_ASSIGN,
            make_expr_id(t2),
            make_expr_cast(make_expr_id(t3), type_ptr(lt->base_type)));
        stmt->next = 0;
        add_stmt(stmt);

        res = make_expr_unary(EXPR_DEREF, make_expr_id(t2));
    }
    else if(expr->kind == EXPR_MEMB_ACCESS)
    {
        lt = resolve_expr_type(expr->l, 0);
        assert(lt->kind == TYPE_STRUCT);
        l = reduce_expr_to_atom(expr->l);

        t1 = declare_tmp_var(type_ptr(lt));

        stmt = make_stmt_expr(make_expr_binary(
            EXPR_ASSIGN,
            make_expr_id(t1),
            make_expr_unary(EXPR_ADDR_OF, dup_expr(l))));
        add_stmt(stmt);

        t2 = declare_tmp_var(type_ptr(type_char()));

        stmt = make_stmt_expr(make_expr_binary(
            EXPR_ASSIGN,
            make_expr_id(t2),
            make_expr_cast(make_expr_id(t1), type_ptr(type_char()))));
        add_stmt(stmt);

        stmt = make_stmt_expr(make_expr_binary(
            EXPR_ASSIGN,
            make_expr_id(t2),
            make_expr_binary(
                EXPR_ADD,
                make_expr_id(t2),
                make_expr_intlit(get_struct_member_offset(lt, expr->id)))));
        add_stmt(stmt);

        aggr_el = get_struct_member(lt, expr->id);
        assert(aggr_el);

        t3 = declare_tmp_var(type_ptr(aggr_el->type));

        stmt = make_stmt_expr(make_expr_binary(
            EXPR_ASSIGN,
            make_expr_id(t3),
            make_expr_cast(make_expr_id(t2), type_ptr(aggr_el->type))));
        add_stmt(stmt);

        res = make_expr_unary(EXPR_DEREF, make_expr_id(t3));
    }
    else if(expr->kind == EXPR_MEMB_ACCESS_PTR)
    {
        lt = resolve_expr_type(expr->l, 0);
        assert(lt->kind == TYPE_PTR && lt->base_type->kind == TYPE_STRUCT);
        l = reduce_expr_to_atom(expr->l);

        t1 = declare_tmp_var(type_ptr(type_char()));

        stmt = make_stmt_expr(make_expr_binary(
            EXPR_ASSIGN,
            make_expr_id(t1),
            make_expr_cast(dup_expr(l), type_ptr(type_char()))));
        add_stmt(stmt);

        stmt = make_stmt_expr(make_expr_binary(
            EXPR_ASSIGN,
            make_expr_id(t1),
            make_expr_binary(
                EXPR_ADD,
                make_expr_id(t1),
                make_expr_intlit(get_struct_member_offset(lt, expr->id)))));
        add_stmt(stmt);

        aggr_el = get_struct_member(lt, expr->id);
        assert(aggr_el);

        t2 = declare_tmp_var(type_ptr(aggr_el->type));

        stmt = make_stmt_expr(make_expr_binary(
            EXPR_ASSIGN,
            make_expr_id(t2),
            make_expr_cast(make_expr_id(t1), type_ptr(aggr_el->type))));
        add_stmt(stmt);

        res = make_expr_unary(EXPR_DEREF, make_expr_id(t2));
    }
    else
    {
        res = make_expr_id(store_expr_temp_var(expr));
    }

    assert(res);

    return(res);
}

void
expr_to_irc(Expr *expr)
{
    Stmt *stmt;
    Expr *l;
    Expr *m;
    Expr *r;
    Expr *final;
    Expr *arg;
    Expr *args;
    Expr *tmp;
    int op;
    int i;
    Type *lt;
    char *t1, *t2, *t3, *t4;

    final = 0;
    if(expr->kind == EXPR_INTLIT)
    {
        /* Nothing */
    }
    else if(expr->kind == EXPR_ID)
    {
        /* Nothing */
    }
    else if(expr->kind == EXPR_CALL)
    {
        l = reduce_expr_to_atom(expr->l);

        args = 0;
        arg = 0;
        r = expr->r;
        while(r)
        {
            printf("A");
            tmp = reduce_expr_to_atom(r);
            tmp->next = 0;

            if(arg)
            {
                arg->next = dup_expr(tmp);
                arg = arg->next;
            }
            else
            {
                arg = dup_expr(tmp);
                args = arg;
            }

            r = r->next;
        }

        final = make_expr_binary(EXPR_CALL, l, args);
    }
    else if(expr->kind == EXPR_ARR_SUB)
    {
        final = reduce_expr_to_atom(expr);
    }
    else if(expr->kind == EXPR_MEMB_ACCESS)
    {
        final = reduce_expr_to_atom(expr);
    }
    else if(expr->kind == EXPR_MEMB_ACCESS_PTR)
    {
        final = reduce_expr_to_atom(expr);
    }
    else if(expr->kind == EXPR_INC_PRE || expr->kind == EXPR_DEC_PRE)
    {
        if(expr->kind == EXPR_INC_PRE)
        {
            op = EXPR_ADD;
        }
        else
        {
            op = EXPR_SUB;
        }
        l = reduce_expr_to_atom(expr->l);
        final = make_expr_binary(
            EXPR_ASSIGN,
            l,
            make_expr_binary(op, l, make_expr_intlit(1)));
    }
    else if(expr->kind >= EXPR_UNARY && expr->kind < EXPR_UNARY_END)
    {
        l = reduce_expr_to_atom(expr->l);
        final = make_expr_unary(expr->kind, l);
    }
    else if(expr->kind >= EXPR_BINARY && expr->kind < EXPR_BINARY_END)
    {
        l = reduce_expr_to_atom(expr->l);
        r = reduce_expr_to_atom(expr->r);
        final = make_expr_binary(expr->kind, l, r);
    }
    else if(expr->kind == EXPR_TERNARY)
    {
        l = reduce_expr_to_atom(expr->l);
        m = reduce_expr_to_atom(expr->m);
        r = reduce_expr_to_atom(expr->r);
        final = make_expr_ternary(l, m, r);
    }
    else if(expr->kind == EXPR_ASSIGN)
    {
        lt = resolve_expr_type(expr->l, 0);
        if(lt->kind == TYPE_STRUCT)
        {
            assert(expr->l->kind == EXPR_ID);

            l = reduce_expr_to_atom(expr->l);
            r = reduce_expr_to_atom(expr->r);

            t1 = declare_tmp_var(type_ptr(lt));
            t2 = declare_tmp_var(type_ptr(lt));
            t3 = declare_tmp_var(type_ptr(type_char()));
            t4 = declare_tmp_var(type_ptr(type_char()));

            stmt = make_stmt_expr(make_expr_binary(
                EXPR_ASSIGN,
                make_expr_id(t1),
                make_expr_unary(EXPR_ADDR_OF, dup_expr(l))));
            add_stmt(stmt);

            stmt = make_stmt_expr(make_expr_binary(
                EXPR_ASSIGN,
                make_expr_id(t2),
                make_expr_unary(EXPR_ADDR_OF, dup_expr(r))));
            add_stmt(stmt);

            stmt = make_stmt_expr(make_expr_binary(
                EXPR_ASSIGN,
                make_expr_id(t3),
                make_expr_cast(make_expr_id(t1), type_ptr(type_char()))));
            add_stmt(stmt);

            stmt = make_stmt_expr(make_expr_binary(
                EXPR_ASSIGN,
                make_expr_id(t4),
                make_expr_cast(make_expr_id(t2), type_ptr(type_char()))));
            add_stmt(stmt);

            t1 = declare_tmp_var(type_ptr(type_int()));
            t2 = declare_tmp_var(type_ptr(type_int()));

            stmt = make_stmt_expr(make_expr_binary(
                EXPR_ASSIGN,
                make_expr_id(t1),
                make_expr_cast(make_expr_id(t3), type_ptr(type_int()))));
            add_stmt(stmt);

            stmt = make_stmt_expr(make_expr_binary(
                EXPR_ASSIGN,
                make_expr_id(t2),
                make_expr_cast(make_expr_id(t4), type_ptr(type_int()))));
            add_stmt(stmt);

            assert(lt->size % 4 == 0);
            for(i = 0;
                i < lt->size/4;
                ++i)
            {
                stmt = make_stmt_expr(make_expr_binary(
                    EXPR_ASSIGN,
                    make_expr_unary(EXPR_DEREF, make_expr_id(t1)),
                    make_expr_unary(EXPR_DEREF, make_expr_id(t2))));
                add_stmt(stmt);

                stmt = make_stmt_expr(make_expr_binary(
                    EXPR_ASSIGN,
                    make_expr_id(t3),
                    make_expr_binary(
                        EXPR_ADD,
                        make_expr_id(t3),
                        make_expr_intlit(4))));
                add_stmt(stmt);

                stmt = make_stmt_expr(make_expr_binary(
                    EXPR_ASSIGN,
                    make_expr_id(t4),
                    make_expr_binary(
                        EXPR_ADD,
                        make_expr_id(t4),
                        make_expr_intlit(4))));
                add_stmt(stmt);
                
                stmt = make_stmt_expr(make_expr_binary(
                    EXPR_ASSIGN,
                    make_expr_id(t1),
                    make_expr_cast(make_expr_id(t3), type_ptr(type_int()))));
                add_stmt(stmt);

                stmt = make_stmt_expr(make_expr_binary(
                    EXPR_ASSIGN,
                    make_expr_id(t2),
                    make_expr_cast(make_expr_id(t4), type_ptr(type_int()))));
                add_stmt(stmt);
            }
        }
        else
        {
            l = reduce_expr_to_atom(expr->l);
            r = reduce_expr_to_atom(expr->r);
            final = make_expr_binary(expr->kind, l, r);
        }
    }
    else if(expr->kind == EXPR_COMPOUND)
    {
        l = expr->l;
        while(l)
        {
            expr_to_irc(l);
            l = l->next;
        }
    }
    else
    {
        assert(0);
    }

    if(final)
    {
        stmt = make_stmt(STMT_EXPR);
        stmt->u.expr = final;
        add_stmt(stmt);
    }
}

void
stmt_to_irc(Stmt *stmt)
{
    Stmt *irc_stmt;
    Sym *sym;

    Expr *cond;
    char *lbl1;
    char *lbl2;
    char *lbl3;

    switch(stmt->kind)
    {
        case STMT_DECL:
        {
            irc_stmt = dup_stmt(stmt);
            irc_stmt->next = 0;
            add_stmt(irc_stmt);

            sym = sym_add(stmt->u.decl->id, stmt->u.decl->type);
            sym->global = 0;
            sym->offset = func_var_offset;
            func_var_offset -= stmt->u.decl->type->size;
        } break;

        case STMT_EXPR:
        {
            expr_to_irc(stmt->u.expr);
        } break;

        case STMT_BLOCK:
        {
            block_to_irc(stmt);
        } break;

        case STMT_RET:
        {
            irc_stmt = make_stmt(STMT_RET);
            irc_stmt->next = 0;
            if(stmt->u.expr)
            {
                if(expr_is_atom(stmt->u.expr))
                {
                    irc_stmt->u.expr = dup_expr(stmt->u.expr);
                }
                else
                {
                    irc_stmt->u.expr = reduce_expr_to_atom(stmt->u.expr);
                }
            }
            else
            {
                irc_stmt->u.expr = 0;
            }
            add_stmt(irc_stmt);
        } break;

        case STMT_LABEL:
        {
            irc_stmt = dup_stmt(stmt);
            irc_stmt->next = 0;
            add_stmt(irc_stmt);
        } break;

        case STMT_GOTO:
        {
            irc_stmt = dup_stmt(stmt);
            irc_stmt->next = 0;
            add_stmt(irc_stmt);
        } break;

        case STMT_IF:
        {
            lbl1 = lbl_gen();
            lbl2 = lbl_gen();
            if(stmt->else_stmt)
            {
                lbl3 = lbl_gen();
            }

            cond = reduce_expr_to_atom(stmt->cond);

            irc_stmt = make_stmt_if(cond, 0, 0);
            irc_stmt->u.label = lbl1;
            add_stmt(irc_stmt);

            irc_stmt = make_stmt(STMT_GOTO);
            irc_stmt->u.label = lbl2;
            irc_stmt->next = 0;
            add_stmt(irc_stmt);

            irc_stmt = make_stmt(STMT_LABEL);
            irc_stmt->u.label = lbl1;
            irc_stmt->next = 0;
            add_stmt(irc_stmt);

            stmt_to_irc(stmt->then_stmt);

            irc_stmt = make_stmt(STMT_GOTO);
            if(stmt->else_stmt)
            {
                irc_stmt->u.label = lbl3;
            }
            else
            {
                irc_stmt->u.label = lbl2;
            }
            irc_stmt->next = 0;
            add_stmt(irc_stmt);

            irc_stmt = make_stmt(STMT_LABEL);
            irc_stmt->u.label = lbl2;
            irc_stmt->next = 0;
            add_stmt(irc_stmt);

            if(stmt->else_stmt)
            {
                stmt_to_irc(stmt->else_stmt);

                irc_stmt = make_stmt(STMT_LABEL);
                irc_stmt->u.label = lbl3;
                irc_stmt->next = 0;
                add_stmt(irc_stmt);
            }
        } break;

        case STMT_WHILE:
        {
            lbl1 = lbl_gen();
            lbl2 = lbl_gen();
            lbl3 = lbl_gen();

            irc_stmt = make_stmt(STMT_LABEL);
            irc_stmt->u.label = lbl1;
            irc_stmt->next = 0;
            add_stmt(irc_stmt);

            cond = reduce_expr_to_atom(stmt->cond);

            irc_stmt = make_stmt_if(cond, 0, 0);
            irc_stmt->u.label = lbl2;
            add_stmt(irc_stmt);

            irc_stmt = make_stmt(STMT_GOTO);
            irc_stmt->u.label = lbl3;
            irc_stmt->next = 0;
            add_stmt(irc_stmt);

            irc_stmt = make_stmt(STMT_LABEL);
            irc_stmt->u.label = lbl2;
            irc_stmt->next = 0;
            add_stmt(irc_stmt);

            stmt_to_irc(stmt->then_stmt);

            irc_stmt = make_stmt(STMT_GOTO);
            irc_stmt->u.label = lbl1;
            irc_stmt->next = 0;
            add_stmt(irc_stmt);

            irc_stmt = make_stmt(STMT_LABEL);
            irc_stmt->u.label = lbl3;
            irc_stmt->next = 0;
            add_stmt(irc_stmt);
        } break;

        case STMT_FOR:
        {
            lbl1 = lbl_gen();
            lbl2 = lbl_gen();
            lbl3 = lbl_gen();

            expr_to_irc(stmt->init);

            irc_stmt = make_stmt(STMT_LABEL);
            irc_stmt->u.label = lbl1;
            irc_stmt->next = 0;
            add_stmt(irc_stmt);

            cond = reduce_expr_to_atom(stmt->cond);

            irc_stmt = make_stmt_if(cond, 0, 0);
            irc_stmt->u.label = lbl2;
            add_stmt(irc_stmt);

            irc_stmt = make_stmt(STMT_GOTO);
            irc_stmt->u.label = lbl3;
            irc_stmt->next = 0;
            add_stmt(irc_stmt);

            irc_stmt = make_stmt(STMT_LABEL);
            irc_stmt->u.label = lbl2;
            irc_stmt->next = 0;
            add_stmt(irc_stmt);

            stmt_to_irc(stmt->then_stmt);

            expr_to_irc(stmt->post);

            irc_stmt = make_stmt(STMT_GOTO);
            irc_stmt->u.label = lbl1;
            irc_stmt->next = 0;
            add_stmt(irc_stmt);

            irc_stmt = make_stmt(STMT_LABEL);
            irc_stmt->u.label = lbl3;
            irc_stmt->next = 0;
            add_stmt(irc_stmt);
        } break;

        default:
        {
            assert(0);
        } break;
    }
}

void
block_to_irc(Stmt *block)
{
    Stmt *stmt;

    stmt = block->u.block;
    while(stmt)
    {
        stmt_to_irc(stmt);
        stmt = stmt->next;
    }
}

Stmt *
func_def_to_irc(Stmt *block)
{
    Stmt *irc_block;
    Stmt *stmt;
    Stmt *parent_block;

    irc_block = make_stmt(STMT_BLOCK);
    irc_block->u.block = 0;
    parent_block = curr_block;
    curr_block = irc_block;
    stmt = block->u.block;
    while(stmt)
    {
        stmt_to_irc(stmt);
        stmt = stmt->next;
    }

    curr_block = parent_block;

    return(irc_block);
}

GlobDecl *
unit_to_irc(GlobDecl *unit)
{
    GlobDecl *irc_unit;
    GlobDecl *irc_curr;
    GlobDecl *decl;
    Sym *sym;
    int offset;
    int sym_count;
    FuncParam *param;

    sym_reset();

    irc_unit = 0;
    irc_curr = 0;
    decl = unit;
    while(decl)
    {
        if(irc_unit)
        {
            irc_curr->next = DUP_OBJ(GlobDecl, irc_curr->next, decl);
            irc_curr = irc_curr->next;
        }
        else
        {
            irc_unit = DUP_OBJ(GlobDecl, irc_unit, decl);
            irc_curr = irc_unit;
        }
        irc_curr->next = 0;

        switch(decl->kind)
        {
            case GLOB_DECL_VAR:
            {
                sym = sym_add(decl->id, decl->type);
                sym->global = 1;
            } break;

            case GLOB_DECL_FUNC:
            {
                sym = sym_add(decl->id, decl->type);
                sym->global = 1;

                sym_count = sym_table_count;
                param = decl->type->params;
                offset = 8;
                while(param)
                {
                    sym_add_func_param(param->id, param->type, offset);
                    offset += ALIGN(param->type->size, 4);
                    param = param->next;
                }

                if(decl->func_def)
                {
                    irc_curr->func_def = func_def_to_irc(decl->func_def);
                }

                sym_table_count = sym_count;
            } break;

            default:
            {
                semantic_fatal("invalid global declaration");
            } break;
        }
        decl = decl->next;
    }

    return(irc_unit);
}

/******************************************************************************/
/**                               CODE GEN                                   **/
/******************************************************************************/

#include <assert.h>

void compile_expr(FILE *fout, Expr *expr);

void
compile_lvalue(FILE *fout, Expr *expr)
{
    Sym *sym;

    switch(expr->kind)
    {
        case EXPR_ID:
        {
            sym = sym_get(expr->id);
            if(!sym)
            {
                fatal("Invalid symbol %s", expr->id);
            }

            if(sym->global)
            {
                fprintf(fout, "\tmovl %s,%%eax\n", sym->id);
            }
            else
            {
                fprintf(fout, "\tmovl %%ebp,%%eax\n");
                fprintf(fout, "\taddl $%d,%%eax\n", sym->offset);
            }
        } break;

        case EXPR_DEREF:
        {
            compile_lvalue(fout, expr->l);
            fprintf(fout, "\tmovl (%%eax),%%eax\n");
        } break;

        default:
        {
            assert(0);
        } break;
    }
}

void
compile_expr(FILE *fout, Expr *expr)
{
    Sym *sym = 0;
    Expr *arg;
    FuncParam *param;
    int argc;
    int params_size;
    Type *type;
    char *ins;
    char *lbl1, *lbl2;

    ins = 0;
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
                fatal("Invalid symbol %s", expr->id);
            }

            type = resolve_expr_type(expr, 0);
            if(type->size == 1)
            {
                ins = "movzbl";
            }
            else if(type->size == 2)
            {
                ins = "movzwl";
            }
            else if(type->size == 4)
            {
                ins = "movl";
            }
            else
            {
                assert(0);
            }

            if(sym->type->kind == TYPE_ARRAY)
            {
                compile_lvalue(fout, expr);
            }
            else if(sym->global)
            {
                fprintf(fout, "\tmovl %s,%%ebx\n", sym->id);
                fprintf(fout, "\t%s (%%ebx),%%eax\n", ins);
            }
            else
            {
                fprintf(fout, "\t%s %d(%%ebp),%%eax\n", ins, sym->offset);
            }
        } break;

        case EXPR_CALL:
        {
            if(expr->l->kind == EXPR_ID)
            {
                sym = sym_get(expr->l->id);
                if(!sym)
                {
                    fatal("Invalid symbol %s", expr->l->id);
                }

                assert(sym->type->kind == TYPE_FUNC);

                argc = 0;
                arg = expr->r;
                params_size = 0;
                param = sym->type->params;
                while(arg)
                {
                    ++argc;
                    compile_expr(fout, arg);
                    /* TODO: Push based on args sizes */
                    fprintf(fout, "\tpushl %%eax\n");
                    params_size += ALIGN(param->type->size, 4);
                    arg = arg->next;
                    param = param->next;
                }

                fprintf(fout, "\tcall %s\n", expr->l->id);

                while(argc > 0)
                {
                    fprintf(fout, "\taddl $%d,%%esp\n", params_size);
                    --argc;
                }
            }
            else
            {
                fatal("We don't handle \"complex\" function calls");
            }
        } break;

        case EXPR_DEREF:
        {
            compile_expr(fout, expr->l);
            fprintf(fout, "\tmovl (%%eax),%%eax\n");
        } break;

        case EXPR_ADDR_OF:
        {
            compile_lvalue(fout, expr->l);
        } break;

        case EXPR_NEG:
        {
            compile_expr(fout, expr->l);
            fprintf(fout, "\tnegl %%eax\n");
        } break;

        case EXPR_CAST:
        {
            type = resolve_expr_type(expr->l, 0);
            if(type->size >= expr->cast_to->size)
            {
                /* Nothing */
            }
            else if(type->size < expr->cast_to->size)
            {
                if(type->size == 1 && expr->cast_to->size == 2)
                {
                    ins = "movzbw";
                }
                else if(type->size == 1 && expr->cast_to->size == 4)
                {
                    ins = "movzbl";
                }
                else if(type->size == 2 && expr->cast_to->size == 4)
                {
                    ins = "movzwl";
                }
                else
                {
                    assert(0);
                }
            }

            compile_expr(fout, expr->l);
            if(ins)
            {
                fprintf(fout, "\t%s %%eax,%%eax\n", ins);
            }
        } break;

        case EXPR_MUL:
        {
            compile_expr(fout, expr->r);
            fprintf(fout, "\tmovl %%eax,%%ecx\n");
            compile_expr(fout, expr->l);
            fprintf(fout, "\timull %%ecx\n");
        } break;

        case EXPR_DIV:
        {
            compile_expr(fout, expr->r);
            fprintf(fout, "\tmovl %%eax,%%ecx\n");
            compile_expr(fout, expr->l);
            fprintf(fout, "\tcltd\n");
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

        case EXPR_LT:
        case EXPR_LE:
        case EXPR_GT:
        case EXPR_GE:
        {
            if(expr->kind == EXPR_LT)
            {
                ins = "jl";
            }
            else if(expr->kind == EXPR_LE)
            {
                ins = "jle";
            }
            else if(expr->kind == EXPR_GT)
            {
                ins = "jg";
            }
            else if(expr->kind == EXPR_GE)
            {
                ins = "jge";
            }

            assert(ins);

            lbl1 = lbl_gen();
            lbl2 = lbl_gen();

            compile_expr(fout, expr->r);
            fprintf(fout, "\tmovl %%eax,%%ecx\n");
            compile_expr(fout, expr->l);
            fprintf(fout, "\tcmpl %%ecx,%%eax\n");
            fprintf(fout, "\t%s %s\n", ins, lbl1);
            fprintf(fout, "\tmovl $0,%%eax\n");
            fprintf(fout, "\tjmp %s\n", lbl2);
            fprintf(fout, "%s:\n", lbl1);
            fprintf(fout, "\tmovl $1,%%eax\n");
            fprintf(fout, "%s:\n", lbl2);
        } break;

        case EXPR_ASSIGN:
        {
            type = resolve_expr_type(expr, 0);
            if(type->size == 1)
            {
                ins = "movb";
            }
            else if(type->size == 2)
            {
                ins = "movw";
            }
            else if(type->size == 4)
            {
                ins = "movl";
            }
            else
            {
                assert(0);
            }

            compile_expr(fout, expr->r);
            fprintf(fout, "\tmovl %%eax,%%ecx\n");
            compile_lvalue(fout, expr->l);
            fprintf(fout, "\t%s %%ecx,(%%eax)\n", ins);
        } break;

        case EXPR_COMPOUND:
        {
            arg = expr->l;
            while(arg)
            {
                compile_expr(fout, arg);
                arg = arg->next;
            }
        } break;

        default:
        {
            assert(0);
        } break;
    }
}

void
compile_decl(FILE *fout, Decl *decl)
{
    Sym *sym;
    int size;

    assert(decl->type && decl->type->size > 0);

    size = ALIGN(decl->type->size, 4);
    fprintf(fout, "\tsubl $%d,%%esp\n", size);

    sym = sym_add(decl->id, decl->type);
    sym->global = 0;
    sym->offset = func_var_offset;
    func_var_offset -= decl->type->size;
}

void
compile_stmt(FILE *fout, Stmt *stmt)
{
    int scope;
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
            scope = sym_table_count;
            substmt = stmt->u.block;
            while(substmt)
            {
                compile_stmt(fout, substmt);
                substmt = substmt->next;
            }
            sym_table_count = scope;
        } break;

        case STMT_RET:
        {
            if(stmt->u.expr)
            {
                compile_expr(fout, stmt->u.expr);
            }
            fprintf(fout, "\tpopl %%ebx\n");
            fprintf(fout, "\tleave\n");
            fprintf(fout, "\tret\n");
        } break;

        case STMT_LABEL:
        {
            fprintf(fout, "%s:\n", stmt->u.label);
        } break;

        case STMT_GOTO:
        {
            fprintf(fout, "\tjmp %s\n", stmt->u.label);
        } break;

        case STMT_IF:
        {
            compile_expr(fout, stmt->cond);
            fprintf(fout, "\tcmpl $0,%%eax\n");
            fprintf(fout, "\tjne %s\n", stmt->u.label);
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
    Sym *sym;
    int size;
    FuncParam *param;
    int sym_count;
    int offset;

    switch(decl->kind)
    {
        case GLOB_DECL_VAR:
        {
            size = ALIGN(decl->type->size, 4);
            fprintf(fout, "%s:\n", decl->id);
            fprintf(fout, "\t.zero $%d\n", size);

            sym = sym_add(decl->id, decl->type);
            sym->global = 1;
        } break;

        case GLOB_DECL_FUNC:
        {
            func_var_offset = -8;

            if(decl->func_def)
            {
                fprintf(fout, "%s:\n", decl->id);
                fprintf(fout, "\tpushl %%ebp\n");
                fprintf(fout, "\tmovl %%esp,%%ebp\n");
                fprintf(fout, "\tpushl %%ebx\n");

                sym_count = sym_table_count;
                param = decl->type->params;
                offset = 8;
                while(param)
                {
                    sym_add_func_param(param->id, param->type, offset);
                    offset += ALIGN(param->type->size, 4);
                    param = param->next;
                }

                compile_stmt(fout, decl->func_def);

                fprintf(fout, "\tpopl %%ebx\n");
                fprintf(fout, "\tleave\n");
                fprintf(fout, "\tret\n");

                sym_table_count = sym_count;
            }

            sym = sym_add(decl->id, decl->type);
            sym->global = 1;
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
#ifdef DEBUG
    FILE *flibc;
    char ch;
#if 0
    FuncParam *params;
#endif
#endif

    sym_reset();

    fprintf(fout, "___entry:\n");
    fprintf(fout, "\tpushl %%ebp\n");
    fprintf(fout, "\tmovl %%esp,%%ebp\n");
    fprintf(fout, "\tcall main\n");
    fprintf(fout, "\tmovl %%eax,%%ebx\n");
    fprintf(fout, "\tmovl $1,%%eax\n");
    fprintf(fout, "\tsyscall\n");
    fprintf(fout, "\tleave\n");
    fprintf(fout, "\tret\n");

#ifdef DEBUG
    fprintf(fout, "\n");
    flibc = fopen("libc.asm", "r");
    if(flibc)
    {
        ch = (char)fgetc(flibc);
        while(ch != EOF)
        {
            fprintf(fout, "%c", ch);
            ch = (char)fgetc(flibc);
        }
        fclose(flibc);
    }

#if 0
    params = make_func_param(str_intern("c"), type_int());
    params->next = 0;
    sym_add(str_intern("putchar"), type_func(type_int(), params));

    params = make_func_param(str_intern("nbytes"), type_int());
    params->next = make_func_param(str_intern("src"), type_ptr(type_void()));
    params->next->next = make_func_param(str_intern("dst"), type_ptr(type_void()));
    params->next->next->next = 0;
    sym_add(str_intern("___memcpy_aligned"), type_func(type_int(), params));
#endif
#else
    /* TODO: Hardcode libc into a C string */
#endif

    curr = unit;
    while(curr)
    {
        compile_glob_decl(fout, curr);
        curr = curr->next;
    }
}

/************************************************/
/************************************************/
/**                 SCRATCHPAD                 **/
/************************************************/
/************************************************/


#if 0
#define ASMORG_API
#include "asmorg.c"
#endif

#include <assert.h>

int
main(int argc, char *argv[])
{
    FILE *fin;
    FILE *fout;
    char *src;
    GlobDecl *unit;
    long fsize;

    if(argc <= 1)
    {
        printf("Usage: ./ezc <input_file> [<output_file>]\n");
        return(1);
    }

    fin = fopen(argv[1], "r");
    fseek(fin, 0, SEEK_END);
    fsize = ftell(fin);
    fseek(fin, 0, SEEK_SET);

    src = (char *)malloc(fsize);
    fread(src, fsize, 1, fin);
    fclose(fin);
    src[fsize - 1] = 0;

    fout = fopen("a.out.asm", "w");

#if 0
    src = "int putchar(int c);\n"
          "int ___memcpy_aligned(void *dst, void *src, int nbytes);\n"
          "int main() {\n"
          "    int i;\n"
          "    for(i = 97; i < 123; ++i) putchar(i);\n"
          "    return 9;\n"
          "}";
#endif

    parser_init(src);
    unit = parse_unit();
    check_unit(unit);
#ifdef PRINT
    print_unit(unit);
#endif
    unit = unit_to_irc(unit);
#ifdef PRINT
    printf("\n\n+++++++++++++++\nIRC\n+++++++++++++++\n\n");
    print_unit(unit);
    printf("\n\n+++++++++++++++\nx86\n+++++++++++++++\n\n");
#endif
    compile_unit(fout, unit);

    fclose(fout);

    return(0);
}
