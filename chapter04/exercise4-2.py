def mapcan(f, items):
    return [x for item in items for x in f(item)]

def permutations(bag):
    if len(bag) == 0:
        return [[]]
    else:
        def f(item):
            rest_items = list(e for e in bag if e != item)
            rest_perms = permutations(rest_items)
            return [[item] + p for p in rest_perms]
        return mapcan(f, bag)
