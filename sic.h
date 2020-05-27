#ifndef SIC_H
#define SIC_H
#include "gc.h"
typedef struct vars
{
    bool useful;
    object next;
    enum
    {
        VARS_NIL,
        VARS_CONS
    } kind;
    union {
        struct
        {
        } nil;
        struct
        {
            int x;
            struct vars *rest;
        } cons;
    } u;
} * vars;

typedef struct expr
{
    bool useful;
    object next;
    enum
    {
        EXPR_NUM,
        EXPR_VAR,
        EXPR_ABS,
        EXPR_APP
    } kind;
    union {
        struct
        {
            int n;
        } num;
        struct
        {
            int y;
        } var;
        struct
        {
            struct vars *xs;
            struct expr *e;
        } abs;
        struct
        {
            struct expr *e_1;
            struct expr *e_2;
        } app;
    } u;
} * expr;

typedef struct value
{
    bool useful;
    object next;
    enum
    {
        VALUE_NUM,
        VALUE_CLOS,
        VALUE_PRIM
    } kind;
    union {
        struct
        {
            int n;
        } num;
        struct
        {
            struct expr *e;
            struct env *E;
        } clos;
        struct
        {
            prim x;
        } prim;
    } u;
} * value;

typedef struct env
{
    bool useful;
    object next;
    enum
    {
        ENV_NIL,
        ENV_CONS
    } kind;
    union {
        struct
        {
        } nil;
        struct
        {
            struct value *v;
            struct env *rest;
        } cons;
    } u;
} * env;

typedef struct frame
{
    bool useful;
    object next;
    enum
    {
        FRAME_APP1,
        FRAME_APP2
    } kind;
    union {
        struct
        {
            struct expr *e_2;
            struct env *E;
        } app1;
        struct
        {
            struct value *v_1;
        } app2;
    } u;
} * frame;

typedef struct cont
{
    bool useful;
    object next;
    enum
    {
        CONT_NIL,
        CONT_CONS
    } kind;
    union {
        struct
        {
        } nil;
        struct
        {
            struct frame *f;
            struct cont *rest;
        } cons;
    } u;
} * cont;

typedef struct state
{
    bool useful;
    object next;
    enum
    {
        STATE_EVAL,
        STATE_CONT
    } kind;
    union {
        struct
        {
            struct expr *e;
            struct env *E;
            struct cont *k;
        } eval;
        struct
        {
            struct value *v;
            struct cont *k;
        } cont;
    } u;
} * state;
vars new_vars_nil()
{
    vars tmp = malloc(sizeof(struct vars));
    if (!tmp)
    {
        printf("Out of memory\n.");
        exit(-1);
    }
    tmp->next = gc_head;
    gc_head = (object)tmp;
    gc_head->useful = true;
    tmp->kind = VARS_NIL;
    return tmp;
}
vars new_vars_cons(int x, vars rest)
{
    vars tmp = malloc(sizeof(struct vars));
    if (!tmp)
    {
        printf("Out of memory\n.");
        exit(-1);
    }
    tmp->next = gc_head;
    gc_head = (object)tmp;
    gc_head->useful = true;
    tmp->kind = VARS_CONS;
    tmp->u.cons.x = x;
    tmp->u.cons.rest = rest;
    return tmp;
}

expr new_expr_num(int n)
{
    expr tmp = malloc(sizeof(struct expr));
    if (!tmp)
    {
        printf("Out of memory\n.");
        exit(-1);
    }
    tmp->next = gc_head;
    gc_head = (object)tmp;
    gc_head->useful = true;
    tmp->kind = EXPR_NUM;
    tmp->u.num.n = n;
    return tmp;
}
expr new_expr_var(int y)
{
    expr tmp = malloc(sizeof(struct expr));
    if (!tmp)
    {
        printf("Out of memory\n.");
        exit(-1);
    }
    tmp->next = gc_head;
    gc_head = (object)tmp;
    gc_head->useful = true;
    tmp->kind = EXPR_VAR;
    tmp->u.var.y = y;
    return tmp;
}
expr new_expr_abs(vars xs, expr e)
{
    expr tmp = malloc(sizeof(struct expr));
    if (!tmp)
    {
        printf("Out of memory\n.");
        exit(-1);
    }
    tmp->next = gc_head;
    gc_head = (object)tmp;
    gc_head->useful = true;
    tmp->kind = EXPR_ABS;
    tmp->u.abs.xs = xs;
    tmp->u.abs.e = e;
    return tmp;
}
expr new_expr_app(expr e_1, expr e_2)
{
    expr tmp = malloc(sizeof(struct expr));
    if (!tmp)
    {
        printf("Out of memory\n.");
        exit(-1);
    }
    tmp->next = gc_head;
    gc_head = (object)tmp;
    gc_head->useful = true;
    tmp->kind = EXPR_APP;
    tmp->u.app.e_1 = e_1;
    tmp->u.app.e_2 = e_2;
    return tmp;
}

value new_value_num(int n)
{
    value tmp = malloc(sizeof(struct value));
    if (!tmp)
    {
        printf("Out of memory\n.");
        exit(-1);
    }
    tmp->next = gc_head;
    gc_head = (object)tmp;
    gc_head->useful = true;
    tmp->kind = VALUE_NUM;
    tmp->u.num.n = n;
    return tmp;
}
value new_value_clos(expr e, env E)
{
    value tmp = malloc(sizeof(struct value));
    if (!tmp)
    {
        printf("Out of memory\n.");
        exit(-1);
    }
    tmp->next = gc_head;
    gc_head = (object)tmp;
    gc_head->useful = true;
    tmp->kind = VALUE_CLOS;
    tmp->u.clos.e = e;
    tmp->u.clos.E = E;
    return tmp;
}
value new_value_prim(prim x)
{
    value tmp = malloc(sizeof(struct value));
    if (!tmp)
    {
        printf("Out of memory\n.");
        exit(-1);
    }
    tmp->next = gc_head;
    gc_head = (object)tmp;
    gc_head->useful = true;
    tmp->kind = VALUE_PRIM;
    tmp->u.prim.x = x;
    return tmp;
}

env new_env_nil()
{
    env tmp = malloc(sizeof(struct env));
    if (!tmp)
    {
        printf("Out of memory\n.");
        exit(-1);
    }
    tmp->next = gc_head;
    gc_head = (object)tmp;
    gc_head->useful = true;
    tmp->kind = ENV_NIL;
    return tmp;
}
env new_env_cons(value v, env rest)
{
    env tmp = malloc(sizeof(struct env));
    if (!tmp)
    {
        printf("Out of memory\n.");
        exit(-1);
    }
    tmp->next = gc_head;
    gc_head = (object)tmp;
    gc_head->useful = true;
    tmp->kind = ENV_CONS;
    tmp->u.cons.v = v;
    tmp->u.cons.rest = rest;
    return tmp;
}

frame new_frame_app1(expr e_2, env E)
{
    frame tmp = malloc(sizeof(struct frame));
    if (!tmp)
    {
        printf("Out of memory\n.");
        exit(-1);
    }
    tmp->next = gc_head;
    gc_head = (object)tmp;
    gc_head->useful = true;
    tmp->kind = FRAME_APP1;
    tmp->u.app1.e_2 = e_2;
    tmp->u.app1.E = E;
    return tmp;
}
frame new_frame_app2(value v_1)
{
    frame tmp = malloc(sizeof(struct frame));
    if (!tmp)
    {
        printf("Out of memory\n.");
        exit(-1);
    }
    tmp->next = gc_head;
    gc_head = (object)tmp;
    gc_head->useful = true;
    tmp->kind = FRAME_APP2;
    tmp->u.app2.v_1 = v_1;
    return tmp;
}

cont new_cont_nil()
{
    cont tmp = malloc(sizeof(struct cont));
    if (!tmp)
    {
        printf("Out of memory\n.");
        exit(-1);
    }
    tmp->next = gc_head;
    gc_head = (object)tmp;
    gc_head->useful = true;
    tmp->kind = CONT_NIL;
    return tmp;
}
cont new_cont_cons(frame f, cont rest)
{
    cont tmp = malloc(sizeof(struct cont));
    if (!tmp)
    {
        printf("Out of memory\n.");
        exit(-1);
    }
    tmp->next = gc_head;
    gc_head = (object)tmp;
    gc_head->useful = true;
    tmp->kind = CONT_CONS;
    tmp->u.cons.f = f;
    tmp->u.cons.rest = rest;
    return tmp;
}

state new_state_eval(expr e, env E, cont k)
{
    state tmp = malloc(sizeof(struct state));
    if (!tmp)
    {
        printf("Out of memory\n.");
        exit(-1);
    }
    tmp->next = gc_head;
    gc_head = (object)tmp;
    gc_head->useful = true;
    tmp->kind = STATE_EVAL;
    tmp->u.eval.e = e;
    tmp->u.eval.E = E;
    tmp->u.eval.k = k;
    return tmp;
}
state new_state_cont(value v, cont k)
{
    state tmp = malloc(sizeof(struct state));
    if (!tmp)
    {
        printf("Out of memory\n.");
        exit(-1);
    }
    tmp->next = gc_head;
    gc_head = (object)tmp;
    gc_head->useful = true;
    tmp->kind = STATE_CONT;
    tmp->u.cont.v = v;
    tmp->u.cont.k = k;
    return tmp;
}

void gc_mark_int(int tmp);
void gc_mark_prim(prim tmp);
void gc_mark_char(char tmp);
void gc_mark_str(str tmp);
void gc_mark_vars(vars tmp);
void gc_mark_expr(expr tmp);
void gc_mark_value(value tmp);
void gc_mark_env(env tmp);
void gc_mark_frame(frame tmp);
void gc_mark_cont(cont tmp);
void gc_mark_state(state tmp);

void gc_mark_vars(vars tmp)
{
    tmp->useful = true;
    switch (tmp->kind)
    {
    case VARS_NIL:
        return;
    case VARS_CONS:
        gc_mark_int(tmp->u.cons.x);
        gc_mark_vars(tmp->u.cons.rest);
        return;
    }
    printf("Internal error 87.\n");
    exit(-1);
    return;
}

void gc_mark_expr(expr tmp)
{
    tmp->useful = true;
    switch (tmp->kind)
    {
    case EXPR_NUM:
        gc_mark_int(tmp->u.num.n);
        return;
    case EXPR_VAR:
        gc_mark_int(tmp->u.var.y);
        return;
    case EXPR_ABS:
        gc_mark_vars(tmp->u.abs.xs);
        gc_mark_expr(tmp->u.abs.e);
        return;
    case EXPR_APP:
        gc_mark_expr(tmp->u.app.e_1);
        gc_mark_expr(tmp->u.app.e_2);
        return;
    }
    printf("Internal error 96.\n");
    exit(-1);
    return;
}

void gc_mark_value(value tmp)
{
    tmp->useful = true;
    switch (tmp->kind)
    {
    case VALUE_NUM:
        gc_mark_int(tmp->u.num.n);
        return;
    case VALUE_CLOS:
        gc_mark_expr(tmp->u.clos.e);
        gc_mark_env(tmp->u.clos.E);
        return;
    case VALUE_PRIM:
        gc_mark_prim(tmp->u.prim.x);
        return;
    }
    printf("Internal error 22.\n");
    exit(-1);
    return;
}

void gc_mark_env(env tmp)
{
    tmp->useful = true;
    switch (tmp->kind)
    {
    case ENV_NIL:
        return;
    case ENV_CONS:
        gc_mark_value(tmp->u.cons.v);
        gc_mark_env(tmp->u.cons.rest);
        return;
    }
    printf("Internal error 88.\n");
    exit(-1);
    return;
}

void gc_mark_frame(frame tmp)
{
    tmp->useful = true;
    switch (tmp->kind)
    {
    case FRAME_APP1:
        gc_mark_expr(tmp->u.app1.e_2);
        gc_mark_env(tmp->u.app1.E);
        return;
    case FRAME_APP2:
        gc_mark_value(tmp->u.app2.v_1);
        return;
    }
    printf("Internal error 31.\n");
    exit(-1);
    return;
}

void gc_mark_cont(cont tmp)
{
    tmp->useful = true;
    switch (tmp->kind)
    {
    case CONT_NIL:
        return;
    case CONT_CONS:
        gc_mark_frame(tmp->u.cons.f);
        gc_mark_cont(tmp->u.cons.rest);
        return;
    }
    printf("Internal error 52.\n");
    exit(-1);
    return;
}

void gc_mark_state(state tmp)
{
    tmp->useful = true;
    switch (tmp->kind)
    {
    case STATE_EVAL:
        gc_mark_expr(tmp->u.eval.e);
        gc_mark_env(tmp->u.eval.E);
        gc_mark_cont(tmp->u.eval.k);
        return;
    case STATE_CONT:
        gc_mark_value(tmp->u.cont.v);
        gc_mark_cont(tmp->u.cont.k);
        return;
    }
    printf("Internal error 76.\n");
    exit(-1);
    return;
}
#endif /* SIC_H */