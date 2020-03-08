def fibo(n):
    a = 0
    b = 1
    for i in range(1, n + 1):
        c = a
        a = b
        b = b + c
    return a
