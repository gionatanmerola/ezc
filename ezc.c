#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define ASMORG_API
#include "asmorg.c"

#define MAX_ID_LEN 33

enum
{
    IRC_SYM_FUNC,
    IRC_SYM_VAR_INT,
    IRC_SYM_VAR_PTR,

    IRC_SYM_TYPES_COUNT
};

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
    IRC_EXPR_OP_INTLIT,
    IRC_EXPR_OP_ID,

    IRC_EXPR_OP_COUNT
};

typedef struct
{
    int kind;
    char *id;
    int val;
} IrcExprOp;

enum
{
    IRC_EXPR_BASE,

    IRC_EXPR_MUL,
    IRC_EXPR_DIV,
    IRC_EXPR_ADD,
    IRC_EXPR_SUB,

    IRC_EXPR_COUNT
};

typedef struct
{
    int kind;
    IrcExprOp *l;
    IrcExprOp *r;
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
{
    int kind;
    char *id;
    int type;
    int subtype;
    int init;
    IrcStmt *stmts;
} IrcGlobDecl;

void
compexprop(FILE *fout, IrcExprOp *op)
{
    IrcSym *sym;

    switch(op->kind)
    {
        case IRC_EXPR_OP_INTLIT:
        {
            fprintf(fout, "\tmovl $%d,%%eax\n", op->val);
        } break;

        case IRC_EXPR_OP_ID:
        {
            sym = getircsym(op->id);
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

        default:
        {
            /* TODO: Log error */
        } break;
    }
}

void
compexpr(FILE *fout, IrcExpr *expr)
{
    /* TODO: */
    switch(expr->kind)
    {
        case IRC_EXPR_BASE:
        {
            compexprop(fout, expr->l);
        } break;

        case IRC_EXPR_MUL:
        {
            compexprop(fout, expr->r);
            fprintf(fout, "\tmovl %%eax,%%ecx\n");
            compexprop(fout, expr->l);
            fprintf(fout, "\timull %%ecx,%%eax\n");
        } break;

        case IRC_EXPR_DIV:
        {
            compexprop(fout, expr->r);
            fprintf(fout, "\tmovl %%eax,%%ecx\n");
            compexprop(fout, expr->l);
            fprintf(fout, "\tidivl %%ecx\n");
        } break;

        case IRC_EXPR_ADD:
        {
            compexprop(fout, expr->r);
            fprintf(fout, "\tmovl %%eax,%%ecx\n");
            compexprop(fout, expr->l);
            fprintf(fout, "\taddl %%ecx,%%eax\n");
        } break;

        case IRC_EXPR_SUB:
        {
            compexprop(fout, expr->r);
            fprintf(fout, "\tmovl %%eax,%%ecx\n");
            compexprop(fout, expr->l);
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
mkbinexpr(int kind, IrcExprOp *l, IrcExprOp *r)
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

IrcExprOp *
mkexprop(char *id, int val)
{
    int kind = IRC_EXPR_OP_INTLIT;
    IrcExprOp *res = 0;

    if(id)
    {
        kind = IRC_EXPR_OP_ID;
    }

    res = (IrcExprOp *)malloc(sizeof(IrcExprOp));
    if(res)
    {
        res->kind = kind;
        res->id = id;
        res->val = val;
    }

    return(res);
}

int
main(int argc, char *argv[])
{
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
    stmt->expr = mkbinexpr(IRC_EXPR_ADD, mkexprop("var2", 0), mkexprop(0, 666));

    stmt2 = mkstmt(IRC_STMT_ASSIGN);
    stmt2->id = "var2";
    stmt2->expr = mkbinexpr(IRC_EXPR_SUB, mkexprop(0, 999), mkexprop(0, 997));
    stmt->next = stmt2;

    stmt3 = mkstmt(IRC_STMT_RET);
    stmt3->expr = mkbinexpr(IRC_EXPR_BASE, mkexprop("var2", 0), 0);
    stmt2->next = stmt3;
#endif

#if 0
    stmt = mkstmt(IRC_STMT_RET);
    stmt->expr = mkbinexpr(IRC_EXPR_BASE, mkexprop(0, 69), 0);
    stmt->next = 0;
#endif

    decl = mkglobdecl("main", IRC_GLOB_DECL_FUNC);
    decl->stmts = stmt;
    compdecl(fout, decl);

    fclose(fout);

    assemble("a.out.asm", "a.out");

    return(0);
}
