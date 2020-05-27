#ifndef FFI_H
#define FFI_H

#include <stdlib.h>

typedef char* str;

typedef struct prim
{
    struct value * (*ptr)(struct value **args);
    int taken;           // a number less-than or equal-to arity
    int arity;
    struct value **args; // an array of length arity
} *prim;

prim new_prim(struct value * (*ptr)(struct value **args), int arity)
{
    prim tmp = malloc(sizeof(struct prim));
    tmp->ptr = ptr;
    tmp->taken = 0;
    tmp->arity = arity;
    tmp->args  = malloc(arity * sizeof(struct value *));
    return tmp;
}

prim prim_consume(prim p, struct value *v)
{
    prim tmp = new_prim(p->ptr, p->arity);
    for (int i = 0; i < p->taken; i ++) {
        tmp->args[i] = p->args[i];
    }
    tmp->args[p->taken] = v;
    tmp->taken = p->taken + 1;
    return tmp;
}

#endif /* FFI_H */