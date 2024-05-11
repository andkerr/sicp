from bisect import bisect_left
from collections import namedtuple
from pprint import pp
import sys

CodeTree = namedtuple("CodeTree",
                      ["symbols", "weight", "left", "right"],
                      defaults=[None, None])

def main():
    if len(sys.argv) != 2:
        print(f"usage: {sys.argv[0]} infile")
        sys.exit(1)

    pairs = []
    with open(sys.argv[1], "r") as f:
        for line in f.readlines():
            sym, freq = line.split()
            pairs.append((sym, int(freq)))

    t = make_huffman_tree(pairs)
    pp(t)

    with open("hamlet-sanitized.txt", "r") as f:
        m = f.read().strip().split()

    do(m, t)

def do(msg, tree):
    e = encode(msg, tree)
    assert decode(e, tree) == msg
    print(e, len(e))

def encode(msg, tree):
    def encode_one(sym, tree):
        assert sym in tree.symbols, "ERROR: symbol does not match tree"
        if is_leaf(tree):
            return []
        else:
            if sym in tree.left.symbols:
                return [0] + encode_one(sym, tree.left)
            else:
                return [1] + encode_one(sym, tree.right)
    enc = []
    for sym in msg:
        enc += encode_one(sym, tree)
    return enc

def decode(bits, tree):
    def choose_next(bit, curr):
        assert bit in [0, 1], f"ERROR: bad bit {bit}"
        if bit == 0:
            return curr.left
        return curr.right

    def decode_one(bits, curr):
        if not bits:
            return []
        branch = curr.left if bits[0] == 0 else curr.right
        if is_leaf(branch):
            return branch.symbols + decode_one(bits[1:], tree)
        else:
            return decode_one(bits[1:], branch)

    return decode_one(bits, tree)

def make_huffman_tree(pairs):
    return successive_merge(make_leaves(pairs))

def successive_merge(trees):
    if not trees:
        return []
    elif not trees[1:]:
        return trees[0]
    else:
        x, y, rest = trees[0], trees[1], trees[2:]
        return successive_merge(adjoin_tree(make_code_tree(x, y), rest))

def adjoin_tree(t, trees):
    trees.insert(bisect_left(trees, t.weight, key=lambda t: t.weight), t)
    return trees

def make_code_tree(left, right):
    return CodeTree(left.symbols + right.symbols,
                    left.weight + right.weight,
                    left,
                    right)

def in_tree(sym, tree):
    return sym in tree.symbols

def make_leaves(pairs):
    pairs = map(lambda p: ([p[0]], p[1]), pairs)
    return [CodeTree(*p) for p in sorted(pairs, key = lambda p: p[1])]

def is_leaf(tree):
    return not tree.left and not tree.right

if __name__ == "__main__":
    main()
