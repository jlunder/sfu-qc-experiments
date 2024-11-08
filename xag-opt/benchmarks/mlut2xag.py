import struct
from dataclasses import dataclass, field
import itertools
import logging
import os
import random
import sys
import time
from typing import Iterator


logger = logging.getLogger()
logger.setLevel(logging.INFO)
handler = logging.StreamHandler()
handler.setFormatter(logging.Formatter("%(message)s"))
logger.addHandler(handler)


@dataclass
class Gate:
    lut: list[bool]
    input_ids: list[int]
    inputs: list[object] | None = field(default_factory=list)
    memo_result: bool | None = None
    computing: bool = False

    def reset(self):
        assert not self.computing
        self.memo_result = None

    def compute(self):
        assert len(self.lut) == (1 << len(self.inputs))
        assert not self.computing
        if self.memo_result is None:
            index = 0
            self.computing = True
            for i, input in enumerate(self.inputs):
                index |= int(input.compute()) << i
            self.computing = False
            self.memo_result = self.lut[index]
        return self.memo_result


false_lut = [False]
true_lut = [True]
not_lut = [True, False]
and_lut = [False, False, False, True]
xor_lut = [False, True, True, False]

empty_inputs = []

false_gate = Gate(false_lut, empty_inputs, empty_inputs, False)
true_gate = Gate(true_lut, empty_inputs, empty_inputs, True)


def pack_bits(bits: list[bool]) -> int:
    result = 0
    for i, b in enumerate(bits):
        result += int(b) << i
    return result


def unpack_bits(x: int, bit_width: int) -> list[bool]:
    return [(x & (1 << i)) != 0 for i in range(bit_width)]


def read_mlut(path: str) -> tuple[list[Gate], list[Gate], list[Gate]]:
    f = open(path, "rb")

    size, regs, lut_size = struct.unpack("<iii", f.read(12))
    lut_array = []
    for i in range(size):
        lut_array.append(struct.unpack("<" + ("i" * lut_size), f.read(4 * lut_size)))
    lut_truths = []
    lut_word_num = 1 << (lut_size - 5) if lut_size > 5 else 1
    for i in range(size):
        lut_truths.append(
            struct.unpack("<" + ("I" * lut_word_num), f.read(4 * lut_word_num))
        )

    lut_array = [
        [-1 if i == 0x7FFFFFFF else (-2 if i == 0x7FFFFFFE else i) for i in ins]
        for ins in lut_array
    ]

    logger.info(f"read '{path}': size={size}, regs={regs}, lut_size={lut_size}")
    # for i, (ins, (l,)) in enumerate(zip(lut_array, lut_truths)):
    #     ins_str = ", ".join((f"{i:4}" for i in ins))
    #     print(f"  [{i:4}, {ins_str}, {[int(i) for i in unpack_bits(l, 1 << (len(ins)))]}],")

    gates = [None] * size
    for i, (ins, (l,)) in enumerate(zip(lut_array, lut_truths)):
        lut = unpack_bits(l, 1 << (len(ins)))
        gates[i] = Gate(lut, ins)

    input_gates: list[Gate] = []
    output_gates: list[Gate] = []
    for n, g in enumerate(gates):
        if g.input_ids[0] == -1:
            # an input
            assert all((id == -1 for id in g.input_ids))
            g.input_ids = []
            g.inputs = []
            g.lut = [None]
            input_gates.append(g)
        elif g.input_ids[1] == -2:
            # an output
            assert g.input_ids[0] >= 0
            assert all((id == -2 for id in g.input_ids[1:]))
            g.inputs = [gates[g.input_ids[0]]]
            g.lut = [False, True]
            output_gates.append(g)
        else:
            assert all((id >= 0 for id in g.input_ids))
            for i in g.input_ids:
                if i >= len(gates):
                    print(f"out of range: {i} on gate {n}")
            g.inputs = [gates[i] for i in g.input_ids]

    logger.info(
        f"interpreted {len(gates)} gates: {len(input_gates)} inputs, {len(output_gates)} outputs"
    )

    return gates, input_gates, output_gates


def compute(
    gates: list[Gate],
    input_gates: list[Gate],
    output_gates: list[Gate],
    input: list[bool],
):
    for g in gates:
        g.reset()

    assert len(input) == len(input_gates) - 2

    for i, g in zip([False, True] + input, input_gates):
        g.memo_result = i
    output: list[bool] = []
    rlim = sys.getrecursionlimit()
    sys.setrecursionlimit(len(gates))
    for g in output_gates:
        output.append(g.compute())
    sys.setrecursionlimit(rlim)

    return output


def test_adder(
    gates: list[Gate], input_gates: list[Gate], output_gates: list[Gate]
) -> None:
    for x, y in [(1, 1), (1030, 0), (0, 1030), (7, 1), (9387, 16384 - 9387)]:
        input = unpack_bits(x, 128) + unpack_bits(y, 128)
        output = compute(gates, input_gates, output_gates, input)
        q = pack_bits(output)
        print(f"{x} + {y} = {q}")

    t_start = time.time()
    t_notify_next = t_start + 1
    i = 0
    while time.time() - t_start < 30:
        x = random.randrange(0, 1 << 128)
        y = random.randrange(0, 1 << 128)
        input = unpack_bits(x, 128) + unpack_bits(y, 128)
        output = compute(gates, input_gates, output_gates, input)
        q = pack_bits(output)
        if q != x + y:
            print(f"\nERROR: {x} + {y} = {q}, expected {x+y}")
            break
        if time.time() > t_notify_next:
            t_notify_next += 1
            print(f"example {i}: {x} + {y} = {q}, expected {x+y}")
        i += 1

    print("\nsuccess!")


def make_test_vectors(
    gates: list[Gate],
    input_gates: list[Gate],
    output_gates: list[Gate],
    count: int,
    seed: any = None,
) -> Iterator[tuple[list[bool], list[bool]]]:

    if seed is None:
        seed = bytes(
            (
                (x & 0xFFFFFFFF) % 255
                for x in itertools.chain(*(g.input_ids for g in gates))
            )
        )

    r = random.Random(seed)

    bias_choices = [
        [0, 1],
        [0, 0, 1],
        [0, 1],
        [0, 1, 1],
        [0, 1],
        [0, 0, 0, 1],
        [0, 1],
        [0, 1, 1, 1],
    ]
    for i in range(count):
        input = [
            bool(r.choice(bias_choices[i % len(bias_choices)])) for _ in input_gates[2:]
        ]
        output = compute(gates, input_gates, output_gates, input)
        yield (input, output)


@dataclass
class XagNode:
    nid: int
    def eval(self, vals: dict[int, bool]):
        raise NotImplementedError(f"{__class__.__name__}.eval")

    def xagb(self):
        raise NotImplementedError(f"{__class__.__name__}.xagb")

@dataclass
class Const(XagNode):
    value: bool
    def eval(self, vals: dict[int, bool]):
        assert self.nid not in vals
        vals[self.nid] = self.value
    def xagb(self):
        return f"Const {{nodeId = {self.nid}, value = {self.value}}}"

@dataclass
class Not(XagNode):
    x: int
    def eval(self, vals: dict[int, bool]):
        assert self.nid not in vals
        vals[self.nid] = not vals[self.x]
    def xagb(self):
        return f"Not {{nodeId = {self.nid}, xIn = {self.x}}}"

@dataclass
class Xor(XagNode):
    x: int
    y: int
    def eval(self, vals: dict[int, bool]):
        assert self.nid not in vals
        vals[self.nid] = vals[self.x] ^ vals[self.y]
    def xagb(self):
        return f"Xor {{nodeId = {self.nid}, xIn = {self.x}, yIn = {self.y}}}"

@dataclass
class And(XagNode):
    x: int
    y: int
    def eval(self, vals: dict[int, bool]):
        assert self.nid not in vals
        vals[self.nid] = vals[self.x] & vals[self.y]
    def xagb(self):
        return f"And {{nodeId = {self.nid}, xIn = {self.x}, yIn = {self.y}}}"

def gen_xag(
    gates: list[Gate], input_gates: list[Gate], output_gates: list[Gate]
) -> tuple[list[XagNode], list[int]]:
    # idf = lambda x: int(x)
    # notf = lambda x: int(not bool(x))
    # xorf = lambda x,y: int(bool(x) ^ bool(y))
    # andf = lambda x,y: int(bool(x) & bool(y))

    # for nc, fc in [("&", andf), ("^", xorf)]:
    #     for nk, fk in [(" ", idf), ("~", notf)]:
    #         for nj, fj in [(" y", idf), ("~y", notf)]:
    #             for ni, fi in [(" x", idf), ("~x", notf)]:
    #                 print(f"{nk}({ni}{nc}{nj}) -> {[fk(fc(fi(x), fj(y))) for x, y in [(0, 0), (0, 1), (1, 0), (1, 1)]]}")

    # produces:
    #  ( x& y) -> [0, 0, 0, 1]
    #  (~x& y) -> [0, 1, 0, 0]
    #  ( x&~y) -> [0, 0, 1, 0]
    #  (~x&~y) -> [1, 0, 0, 0]
    # ~( x& y) -> [1, 1, 1, 0]
    # ~(~x& y) -> [1, 0, 1, 1]
    # ~( x&~y) -> [1, 1, 0, 1]
    # ~(~x&~y) -> [0, 1, 1, 1]
    #  ( x^ y) -> [0, 1, 1, 0]
    #  (~x^ y) -> [1, 0, 0, 1]
    #  ( x^~y) -> [1, 0, 0, 1]
    #  (~x^~y) -> [0, 1, 1, 0]
    # ~( x^ y) -> [1, 0, 0, 1]
    # ~(~x^ y) -> [0, 1, 1, 0]
    # ~( x^~y) -> [0, 1, 1, 0]
    # ~(~x^~y) -> [1, 0, 0, 1]


    subcircuits = [
        # 0 1 0 1 (x)
        # 0 0 1 1 (y)
        # -------
        # 0 0 0 0
        lambda x, y, n: [Const(n+0, False)],  # PATHOLOGICAL
        # 1 0 0 0
        lambda x, y, n: [
            Not(n+0, x),
            Not(n+1, y),
            And(n+2, n+0, n+1),
        ],
        # 0 1 0 0
        lambda x, y, n: [
            Not(n+0, x),
            And(n+1, n+0, y),
        ],
        # 1 1 0 0
        lambda x, y, n: [Not(n+0, y)],  # PATHOLOGICAL
        # 0 0 1 0
        lambda x, y, n: [
            Not(n+0, y),
            And(n+1, x, n+0),
        ],
        # 1 0 1 0
        lambda x, y, n: [Not(n+0, x)],  # PATHOLOGICAL
        # 0 1 1 0
        lambda x, y, n: [Xor(n+0, x, y)],
        # 1 1 1 0
        lambda x, y, n: [
            And(n+0, x, y),
            Not(n+1, n+0),
        ],
        # 0 0 0 1
        lambda x, y, n: [And(n+0, x, y)],
        # 1 0 0 1
        lambda x, y, n: [
            Not(n+0, x),
            Xor(n+1, n+0, y),
        ],
        # 0 1 0 1
        lambda x, y, n: [
            Not(n+0, x),
            Not(n+1, n+0),
        ],  # PATHOLOGICAL
        # 1 1 0 1
        lambda x, y, n: [
            Not(n+0, y),
            And(n+1, x, n+0),
            Not(n+2, n+1),
        ],
        # 0 0 1 1
        lambda x, y, n: [
            Not(n+0, y),
            Not(n+1, n+0),
        ],  # PATHOLOGICAL
        # 1 0 1 1
        lambda x, y, n: [
            Not(n+0, x),
            And(n+1, n+0, y),
            Not(n+2, n+1),
        ],
        # 0 1 1 1
        lambda x, y, n: [
            Not(n+0, x),
            Not(n+1, y),
            And(n+2, n+0, n+1),
            Not(n+3, n+2),
        ],
        # 1 1 1 1
        lambda x, y, n: [Const(n+0, True)],  # PATHOLOGICAL
    ]

    lut_questionable = [
        # 0 1 0 1 (x)
        # 0 0 1 1 (y)
        # -------
        True,  # 0 0 0 0
        False,  # 1 0 0 0
        False,  # 0 1 0 0
        True,  # 1 1 0 0
        False,  # 0 0 1 0
        True,  # 1 0 1 0
        False,  # 0 1 1 0
        False,  # 1 1 1 0
        False,  # 0 0 0 1
        False,  # 1 0 0 1
        True,  # 0 1 0 1
        False,  # 1 1 0 1
        True,  # 0 0 1 1
        False,  # 1 0 1 1
        False,  # 0 1 1 1
        True,  # 1 1 1 1
    ]

    gen_index = {i: i for i in range(len(input_gates))}
    xag_nodes = [Const(0, False), Const(1, True)]
    n = len(input_gates)
    for i, g in enumerate(gates[len(input_gates) : -len(output_gates)]):
        assert len(g.input_ids) == 2
        packed_lut = pack_bits(g.lut)
        if lut_questionable[packed_lut]:
            logger.warning(f"Gate {i} has questionable LUT: {g.lut}")
        xid, yid = g.input_ids
        xn = gen_index[xid]
        yn = gen_index[yid]
        sub = subcircuits[packed_lut](xn, yn, n)
        assert len(sub) > 0
        n += len(sub)
        xag_nodes += sub
        assert (i + len(input_gates)) not in gen_index
        gen_index[i + len(input_gates)] = n - 1
    
    return xag_nodes, [gen_index[g.input_ids[0]] for g in output_gates]


if __name__ == "__main__":
    for f in sys.argv[1:]:
        id_str = os.path.splitext(os.path.basename(f))[0]
        # id_str = (
        #     os.path.splitext(os.path.basename(f))[0]
        #     .translate({c: (c if chr(c).isalnum else " ") for c in range(0, 128)})
        #     .title()
        #     .translate({ord(" "): None})
        # )
        # print(f"module Xag.Benchmarks.{id_str} where")
        # print()
        # print("import Xag.Graph")
        # print()

        print("BenchmarkInput")
        gates, input_gates, output_gates = read_mlut(f)
        xag_nodes, output_order = gen_xag(gates, input_gates, output_gates)
        print("  { xag =")
        print("      Graph")
        xag_array_str = ",\n          ".join((n.xagb() for n in xag_nodes))
        print(f"        [ {xag_array_str}\n        ],")
        print()

        print(f"    inputOrder = [ {', '.join(map(str, range(2, len(input_gates))))} ],")
        print()

        output_order_str = ", ".join((str(g.input_ids[0]) for g in output_gates))
        print(f"    outputOrder = [ {output_order_str} ],")
        print()

        test_vectors = list(
            make_test_vectors(gates, input_gates, output_gates, 100, id_str)
        )
        test_vectors_str = ",\n        ".join(
            (f"({inp}, {outp})" for inp, outp in test_vectors)
        )
        print(f"    testVectors =\n      [ {test_vectors_str}\n      ]")
        print("  }")

        for i, (ti, to) in enumerate(test_vectors):
            xag = xag_nodes[:2] + [Const(nid, val) for nid, val in zip(range(2, len(input_gates)), ti)] + xag_nodes[2:]
            vals = {}
            for node in xag:
                node.eval(vals)
            out = [vals[g.input_ids[0]] for g in output_gates]
            if out != to:
                logger.warning(f"Output does not match for test vector {i} = {[int(x) for x in ti]}")
                logger.warning(f"Expected: {[int(x) for x in to]}")
                logger.warning(f"     Got: {[int(x) for x in out]}")
                
