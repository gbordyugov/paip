def permutations(bag):
    if len(bag) == 0:
        return [[]]
    else:
        perms = []
        for e in bag:
            rest = list(el for el in bag if el != e)
            rest_perms = permutations(rest)
            perms = perms + [[e] + p for p in rest_perms]
        return perms
