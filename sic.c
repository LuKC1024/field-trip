#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "ffi.h"
#include "sic.h"
#include "program.h"

value lookup(env E, int x)
{
    switch (E->kind)
    {
    case ENV_NIL:
        printf("Internal error 5\n");
        exit(-1);
        break;
    case ENV_CONS:
        if (x == 0)
        {
            return E->u.cons.v;
        }
        else
        {
            return lookup(E->u.cons.rest, x - 1);
        }
    default:
        printf("Internal error 6\n");
        exit(-1);
        break;
    }
}

void error(char *msg)
{
    printf("Error: %s\n", msg);
    exit(-1);
}

value prim_readchar(value *args)
{
    return new_value_num(getchar());
}

value prim_writechar(value *args)
{
    if (!(args[0]->kind == VALUE_NUM))
    {
        error("write-char expects its 1-th argument to be a character.");
    }
    putchar(args[0]->u.num.n);
    return new_value_num(0);
}

value prim_times(value *args)
{
    if (!(args[0]->kind == VALUE_NUM))
    {
        error("* expects its 1-th argument to be a number.");
    }
    if (!(args[1]->kind == VALUE_NUM))
    {
        error("* expects its 2-th argument to be a number.");
    }
    return new_value_num(args[0]->u.num.n * args[1]->u.num.n);
}

value prim_plus(value *args)
{
    if (!(args[0]->kind == VALUE_NUM))
    {
        error("+ expects its 1-th argument to be a number.");
    }
    if (!(args[1]->kind == VALUE_NUM))
    {
        error("+ expects its 2-th argument to be a number.");
    }
    return new_value_num(args[0]->u.num.n + args[1]->u.num.n);
}

value prim_equal_to(value *args)
{
    if (!(args[0]->kind == VALUE_NUM))
    {
        error("= expects its 1-th argument to be a number.");
    }
    if (!(args[1]->kind == VALUE_NUM))
    {
        error("= expects its 2-th argument to be a number.");
    }
    return new_value_num(args[0]->u.num.n == args[1]->u.num.n);
}
value prim_less_than(value *args)
{
    if (!(args[0]->kind == VALUE_NUM))
    {
        error("< expects its 1-th argument to be a number.");
    }
    if (!(args[1]->kind == VALUE_NUM))
    {
        error("< expects its 2-th argument to be a number.");
    }
    return new_value_num(args[0]->u.num.n < args[1]->u.num.n);
}
value prim_greater_than(value *args)
{
    if (!(args[0]->kind == VALUE_NUM))
    {
        error("> expects its 1-th argument to be a number.");
    }
    if (!(args[1]->kind == VALUE_NUM))
    {
        error("> expects its 2-th argument to be a number.");
    }
    return new_value_num(args[0]->u.num.n > args[1]->u.num.n);
}

value prim_minus(value *args)
{
    if (!(args[0]->kind == VALUE_NUM))
    {
        error("- expects its 1-th argument to be a number.");
    }
    if (!(args[1]->kind == VALUE_NUM))
    {
        error("- expects its 2-th argument to be a number.");
    }
    return new_value_num(args[0]->u.num.n - args[1]->u.num.n);
}
value prim_division(value *args)
{
    if (!(args[0]->kind == VALUE_NUM))
    {
        error("/ expects its 1-th argument to be a number.");
    }
    if (!(args[1]->kind == VALUE_NUM))
    {
        error("/ expects its 2-th argument to be a number.");
    }
    return new_value_num(args[0]->u.num.n / args[1]->u.num.n);
}
value prim_remainder(value *args)
{
    if (!(args[0]->kind == VALUE_NUM))
    {
        error("% expects its 1-th argument to be a number.");
    }
    if (!(args[1]->kind == VALUE_NUM))
    {
        error("% expects its 2-th argument to be a number.");
    }
    return new_value_num(args[0]->u.num.n % args[1]->u.num.n);
}

value prim_cnd(value *args)
{
    return (args[0]->kind == VALUE_NUM && args[0]->u.num.n == 0) ? args[2] : args[1];
}

#define PRIM_COUNT 11

struct
{
    int arity;
    value (*op)(value *args);
} oops[PRIM_COUNT] =
    {
        {.arity = 2, .op = prim_plus},
        {.arity = 2, .op = prim_minus},
        {.arity = 2, .op = prim_times},
        {.arity = 2, .op = prim_division},
        {.arity = 2, .op = prim_remainder},
        {.arity = 2, .op = prim_less_than},
        {.arity = 2, .op = prim_equal_to},
        {.arity = 2, .op = prim_greater_than},
        {.arity = 3, .op = prim_cnd},
        {.arity = 1, .op = prim_readchar},
        {.arity = 1, .op = prim_writechar},
};


void gc_mark_int(int n) {
    return;
}
void gc_mark_prim(prim p) {
    for (int i = 0; i < p->taken; i ++) {
        gc_mark_value(p->args[i]);
    }
}

env save_env(env E, vars xs)
{
    switch (xs->kind)
    {
    case VARS_NIL:
        return new_env_nil();
    case VARS_CONS:
        return new_env_cons(lookup(E, xs->u.cons.x), save_env(E, xs->u.cons.rest));
    default:
        error("Internal error 32");
        return NULL;
    }
}

int main()
{
    struct state *s[2];
    expr program = mk_expr();
    bool i = false;

    env env_init = new_env_nil();
    for (int i = PRIM_COUNT; i > 0; i--)
    {
        env_init =
            new_env_cons(
                new_value_prim(new_prim(oops[i - 1].op, oops[i - 1].arity)),
                env_init);
    }
    env_init = new_env_cons(new_value_num(EOF), env_init);

    s[i] = new_state_eval(
        program,
        env_init,
        new_cont_nil());

    for (int gas; gas <= 999999999; gas++)
    {
        if (gas % 10 == 0)
        {
            gc_mark_state(s[i]);
            gc_sweep();
        }
        switch (s[i]->kind)
        {
        case STATE_EVAL:
        {
            expr e = s[i]->u.eval.e;
            env E = s[i]->u.eval.E;
            cont k = s[i]->u.eval.k;
            switch (e->kind)
            {
            case EXPR_NUM:
                // printf("DEBUG: num %d\n", e->u.num.n);
                s[!i] = new_state_cont(
                    new_value_num(e->u.num.n),
                    k);
                break;
            case EXPR_VAR:
                s[!i] = new_state_cont(
                    lookup(E, e->u.var.y),
                    k);
                break;
            case EXPR_ABS:
                // printf("DEBUG: abs\n");
                s[!i] = new_state_cont(
                    new_value_clos(
                        e->u.abs.e,
                        save_env(E, e->u.abs.xs)),
                    k);
                break;
            case EXPR_APP:
                // printf("DEBUG: app\n");
                s[!i] = new_state_eval(
                    e->u.app.e_1,
                    E,
                    new_cont_cons(
                        new_frame_app1(e->u.app.e_2, E),
                        k));
                break;
            default:
                printf("DEBUG: Internal error 1\n");
                exit(-1);
                break;
            }
            break;
        }
        case STATE_CONT:
        {
            value v = s[i]->u.cont.v;
            cont k = s[i]->u.cont.k;
            switch (k->kind)
            {
            case CONT_NIL:
                goto EOE;
                break;
            case CONT_CONS:
            {
                frame f = (frame)k->u.cons.f;
                cont k_rest = k->u.cons.rest;
                switch (f->kind)
                {
                case FRAME_APP1:
                {
                    expr e_2 = f->u.app1.e_2;
                    env E = f->u.app1.E;
                    s[!i] = new_state_eval(
                        e_2,
                        E,
                        new_cont_cons(
                            new_frame_app2(v),
                            k_rest));
                    break;
                }
                case FRAME_APP2:
                {
                    value v_1 = f->u.app2.v_1;
                    value v_2 = v;
                    switch (v_1->kind)
                    {
                    case VALUE_CLOS:
                        s[!i] = new_state_eval(
                            f->u.app2.v_1->u.clos.e,
                            new_env_cons(v, f->u.app2.v_1->u.clos.E),
                            k_rest);
                        break;
                    case VALUE_PRIM:
                    {
                        if (v_1->u.prim.x->taken < v_1->u.prim.x->arity - 1)
                        {
                            s[!i] = new_state_cont(
                                new_value_prim(prim_consume(v_1->u.prim.x, v_2)),
                                k_rest);
                            break;
                        }
                        else if (v_1->u.prim.x->taken == v_1->u.prim.x->arity - 1)
                        {
                            v_1->u.prim.x->args[v_1->u.prim.x->taken] = v_2;
                            s[!i] = new_state_cont(
                                v_1->u.prim.x->ptr(v_1->u.prim.x->args),
                                k_rest);
                            v_1->u.prim.x->args[v_1->u.prim.x->taken] = NULL;
                            break;
                        }
                        else
                        {
                            error("Internal error 5");
                            break;
                        }
                    }
                    default:
                        goto EOE;
                    }
                    break;
                }
                default:
                    printf("Internal error 4");
                    exit(-1);
                    break;
                }
                break;
            }
            default:
                printf("Internal error 2");
                exit(-1);
                break;
            }
            break;
        }
        default:
            printf("Internal error 3");
            exit(-1);
            break;
        }
        i = i ? 0 : 1;
    }
    printf("Our of gas\n");
EOE:
    // printf("The answer is: %d\n", s[i]->u.cont.v->u.num.n);
    return 0;
}
