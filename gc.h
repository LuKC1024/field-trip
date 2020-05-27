#ifndef GC_H
#define GC_H

#include <stdbool.h>
#include "ffi.h"

typedef struct object {
    bool useful;
    struct object *next;
} *object;

object gc_head = NULL;

void gc_sweep()
{
    while (gc_head != NULL && (!gc_head->useful))
    {
        object o = gc_head->next;
        free(gc_head);
        gc_head = o;
    }
    object prev = gc_head;
    while (prev != NULL)
    {
        prev->useful = false;
        object o = prev->next;
        while (o != NULL && (! o->useful))
        {
            object q = o->next;
            free(o);
            o = q;
        }
        prev->next = o;
        prev = o;
    }
}

#endif /* GC_H */
