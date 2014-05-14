#include <stdio.h>

typedef struct _TList
{
    int            value;
    struct _TList *next;
} TList;

TList* findPrevious(TList* l, int k)
{
    TList* p = l;
    while (l != NULL && l->value != k)
    {
        p = l;
        l = l->next;
    }
    return p;
}

bool insert(TList* l, int v)
{
    TList* p = findPrevious(l);
    if (p->next == NULL)
    {
        TList* n = malloc(sizeof(TList));
        n->value = v;
        n->next = Nil;
        p->next = n;
    }
}

int main(int argc, char* argv)
{
    
    return 0;
}
