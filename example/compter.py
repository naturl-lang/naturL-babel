def compter(str, ch):
    compte = 0
    for c in str:
        if c == ch:
            compte = compte + 1
    return compte
