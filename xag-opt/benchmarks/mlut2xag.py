import struct
from dataclasses import dataclass, field
import os
import random
import sys
import time
from typing import Iterator


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


def read_mlut(path: str, prefix: str) -> tuple[list[Gate], list[Gate], list[Gate]]:
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

    print(f"{prefix}read '{path}': size={size}, regs={regs}, lut_size={lut_size}")
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

    print(
        f"{prefix}  interpreted {len(gates)} gates: {len(input_gates)} inputs, {len(output_gates)} outputs"
    )
    print()

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
    for g in output_gates:
        output.append(g.compute())

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
    gates: list[Gate], input_gates: list[Gate], output_gates: list[Gate], count: int
) -> Iterator[tuple[list[bool], list[bool]]]:
    bias_choices = [[0, 1], [0, 1], [0, 0, 1], [1, 1, 0], [0, 0, 0, 1], [1, 1, 1, 0]]
    for i in range(count):
        input = [
            random.choice(bias_choices[i % len(bias_choices)]) for _ in input_gates
        ]
        output = compute(gates, input_gates, output_gates, input)
        yield (input, output)



def gen_xag(
    gates: list[Gate], input_gates: list[Gate], output_gates: list[Gate]
) -> tuple[list[str], list[int]]:
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
        lambda x, y, n: [f"Const {n+0} False"],  # PATHOLOGICAL
        # 1 0 0 0
        lambda x, y, n: [
            f"Not {n+0} [{x}]",
            f"Not {n+1} [{y}]",
            f"And {n+2} [{n+0}, {n+1}]",
        ],
        # 0 1 0 0
        lambda x, y, n: [f"Not {n+0} [{x}]", f"And {n+1} [{n+0}, {y}]"],
        # 1 1 0 0
        lambda x, y, n: [f"Not {n+0} [{y}]"],  # PATHOLOGICAL
        # 0 0 1 0
        lambda x, y, n: [f"Not {n+0} [{y}]", f"And {n+1} [{x}, {n+0}]"],
        # 1 0 1 0
        lambda x, y, n: [f"Not {n+0} [{x}]"],  # PATHOLOGICAL
        # 0 1 1 0
        lambda x, y, n: [f"Xor {n+0} [{x}, {y}]"],
        # 1 1 1 0
        lambda x, y, n: [
            f"And {n+0} [{x}, {y}]",
            f"Not {n+1} [{n+0}]",
        ],
        # 0 0 0 1
        lambda x, y, n: [f"And {n+0} [{x}, {y}]"],
        # 1 0 0 1
        lambda x, y, n: [f"Not {n+0} [{x}]", f"Xor {n+1} [{n+0}, {y}]"],
        # 0 1 0 1
        lambda x, y, n: [f"Not {n+0} [{x}]", f"Not {n+1} [{n+0}]"],  # PATHOLOGICAL
        # 1 1 0 1
        lambda x, y, n: [
            f"Not {n+0} [{y}]",
            f"And {n+1} [{x}, {n+0}]",
            f"Not {n+2} [{n+1}]",
        ],
        # 0 0 1 1
        lambda x, y, n: [f"Not {n+0} [{y}]", f"Not {n+1} [{n+0}]"],  # PATHOLOGICAL
        # 1 0 1 1
        lambda x, y, n: [
            f"Not {n+0} [{x}]",
            f"And {n+1} [{n+0}, {y}]",
            f"Not {n+2} [{n+1}]",
        ],
        # 0 1 1 1
        lambda x, y, n: [
            f"Not {n+0} [{x}]",
            f"Not {n+1} [{y}]",
            f"And {n+2} [{n+0}, {n+1}]",
            f"Not {n+3} [{n+2}]",
        ],
        # 1 1 1 1
        lambda x, y, n: [f"Const {n+0} True"],  # PATHOLOGICAL
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
    xag_strs = ["Const 0 False", "Const 1 True"]
    n = len(input_gates)
    for i, g in enumerate(gates[len(input_gates) : -len(output_gates)]):
        assert len(g.input_ids) == 2
        packed_lut = pack_bits(g.lut)
        if lut_questionable[packed_lut]:
            print(f"Gate {i} has questionable LUT: {g.lut}")
        xid, yid = g.input_ids
        xn = gen_index[xid]
        yn = gen_index[yid]
        sub = subcircuits[packed_lut](xn, yn, n)
        assert len(sub) > 0
        n += len(sub)
        xag_strs += sub
        assert (i + len(input_gates)) not in gen_index
        gen_index[i + len(input_gates)] = n - 1

    return xag_strs, [gen_index[g.input_ids[0]] for g in output_gates]


if __name__ == "__main__":
    for f in sys.argv[1:]:
        id_str = (
            os.path.splitext(os.path.basename(f))[0]
            .translate({c: (c if chr(c).isalnum else " ") for c in range(0, 128)})
            .title()
            .translate({ord(" "): None})
        )
        print(f"module Xag.Benchmarks.{id_str} where")
        print()
        print("import Xag.Graph")
        print()

        gates, input_gates, output_gates = read_mlut(f, "-- ")
        xag_strs, output_order = gen_xag(gates, input_gates, output_gates)
        xag_array_str = "\n      , ".join(xag_strs)
        test_vectors = make_test_vectors(gates, input_gates, output_gates, 100)

        print(f"xag = [ {xag_array_str}\n]")
        print()
        print(f"input_order = [ 2 .. {len(input_gates) - 1} ]")
        print()
        output_order_str = ", ".join((str(g.input_ids[0]) for g in output_gates))
        print(f"output_order = [ {output_order_str} ]")
        print()
        print("test_vectors = []")


