def fib(n):
    return fib(n - 2) + fib(n - 1) if n >= 2 else n

if __name__ == "__main__":
    n = int(input("> "))
    print(fib(n))
