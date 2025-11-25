#include "stdio.h"

int fib(int n)
{
    return (n >= 2) ? fib(n - 2) + fib(n - 1) : n;
}

int main()
{
    int n;

    printf("> ");
    scanf("%d", &n);
    printf("%d\n", fib(n));
}
