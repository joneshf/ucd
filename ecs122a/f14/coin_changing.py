def greedy_change(x):
    """
    Returns a list of quarters (Q), dimes (D), nickels(N), and pennies(P)
    representing the fewest number of coins required to give *x* change
    """
    if 25 <= x:
        return "Q " + greedy_change(x - 25)
    elif 10 <= x < 25:
        return "D " + greedy_change(x - 10)
    elif 5 <= x < 10:
        return "N " + greedy_change(x - 5)
    elif 1 <= x < 5:
        return "P " + greedy_change(x - 1)
    elif 0 == x:
        return ""


def calltree_change(x):
    """
    Returns the minimum number of coins requires *min_coins* to give *x*
    change, along with the size *calls* of the call-tree
    """
    coins = [1, 10, 25]
    if x in coins:
        return 1, 1
    else:
        subproblems = [calltree_change(x - y) for y in coins if y <= x]
        min_coins = 1 + min([s[0] for s in subproblems])
        calls = 1 + sum([s[1] for s in subproblems])
        return min_coins, calls


def memo_change(x):
    """
    Returns the minimum number of coins required to give *x* change using
    memoization
    """
    coins = [1, 10, 25]
    subproblem_solutions = [0]*(x + 1)
    for i in range(1, x + 1):
        subproblems = [y for y in coins if y <= i]
        subproblem_solutions[i] = 1 + min([subproblem_solutions[i - y]
                                           for y in subproblems])
    return subproblem_solutions[x]
