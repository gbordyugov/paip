def mapcan(f, items):
    return [x for item in items for x in f(item)]
