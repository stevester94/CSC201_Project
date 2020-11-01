def twisted_prime():
    n       = None
    reverse = None
    sum     = None
    flag    = None
    j       = None
    tprime  = None
    
    sum = 0
    n   = 2 # Originally 13


    while n != 0:
        reverse = n - (10 * int(n / 10))
        sum = sum * 10 + reverse
        n = int(n / 10)

    flag = 0
    j = 2
    
    while True:
        if sum - (j * int(sum / j)) == 0:
            flag = 1
            break
        j = j + 1
        if j >= int(sum / 2):
            break
    
    if flag == 0: ans = True
    else: ans = False

    # Idk if our program is actually supposed to return anything
    print("Ans: ", ans)
twisted_prime()


