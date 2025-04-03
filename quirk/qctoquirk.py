import argparse, json, parsyqc, re, sys, urllib.parse
from dataclasses import dataclass


@dataclass
class Args:
    verbose: bool = False
    input_path: str = None


def make_arg_parser():
    parser = argparse.ArgumentParser(
        description="Convert a .qc circuit to a Quirk URL for nice viewing"
    )
    parser.add_argument(
        "-v", "--verbose", action="store_true", help="verbose message output"
    )
    parser.add_argument(
        "input_path",
        metavar="INPUT",
        nargs=1,
        help="path to .qc, or STDIN if unspecified",
    )

    return parser


arg_parser: argparse.ArgumentParser = make_arg_parser()


def simple_gate(gate_name: str, root_exp: int = 0):
    g = gate_name
    if root_exp == 0:
        pass
    elif root_exp == 1:
        g += "^½"
    elif root_exp == -1:
        g += "^-½"
    elif root_exp == 2:
        g += "^¼"
    elif root_exp == -2:
        g += "^-¼"
    else:
        raise ValueError("root_exp should be in the range -2..2")

    def gate_func(n_rows: int, argument_rows: dict[int, bool], decorated_gate=g):
        print(f"generating {n_rows} for {argument_rows}")

        def row_func(i: int):
            for k, (j, inv) in enumerate(argument_rows):
                if i != j:
                    continue
                print(f"matched {k} of {len(argument_rows) - 1}")
                if k == len(argument_rows) - 1:
                    return decorated_gate
                return "◦" if inv else "•"
            return 1

        return list(map(row_func, range(n_rows)))

    return gate_func


def swap_gate(n_rows: int, argument_rows: dict[int, bool]):
    def row_func(i: int):
        for j, inv in argument_rows:
            if i != j:
                continue
            if j >= len(argument_rows) - 2:
                return "Swap"
            return "◦" if inv else "•"
        return 1

    return list(map(row_func, range(n_rows)))


static_gate_names = {
    "H": simple_gate("H"),
    "P": simple_gate("Z", 1),
    "P*": simple_gate("Z", -1),
    "S": simple_gate("Z", 1),
    "S*": simple_gate("Z", -1),
    "T": simple_gate("Z", 2),
    "T*": simple_gate("Z", -2),
    "X": simple_gate("X"),
    "CNOT": simple_gate("X"),
    "TOF": simple_gate("X"),
    "Y": simple_gate("Y"),
    "Z": simple_gate("Z"),
    "ZD": simple_gate("Z"),
    "CZ": simple_gate("Z"),
    "F": swap_gate,
    "FRE": swap_gate,
    "SWAP": swap_gate,
}

t_re = re.compile(r"T[0-9]+")
f_re = re.compile(r"T[0-9]+")


def to_gate_func(gate_name: str):
    norm_gate_name = gate_name.upper()
    if norm_gate_name in static_gate_names:
        return static_gate_names[norm_gate_name]
    elif t_re.match(gate_name):
        return simple_gate("X")
    elif f_re.match(gate_name):
        return swap_gate
    raise ValueError(f"Bad gate name '{gate_name}'")


if __name__ == "__main__":
    args: Args = arg_parser.parse_args(namespace=Args())
    if args.input_path:
        f = open(args.input_path[0], "r")
    else:
        f = sys.stdin
    qcf: parsyqc.QCFile = parsyqc.qc_file.parse(f.read())
    print(repr(qcf))
    for g in qcf.main.gates:
        # %E2%80%A2 = •
        # %E2%97%A6 = ◦
        # %E2%80%A6 = …
        # %C2%BC = ¼
        # %C2%BD = ½
        row_assign = {v: i for i, v in enumerate(qcf.variables)}
        i = len(row_assign)

        def assign_if_missing(v):
            if (v is not None) and (v not in row_assign):
                row_assign[v] = i
                i += 1

        for v in qcf.inputs + qcf.outputs:
            assign_if_missing(v)
        for g in qcf.main.gates:
            for v, _ in g.arguments:
                assign_if_missing(v)
        n_rows = i
        cols = []
        for g in qcf.main.gates:
            cols.append(
                to_gate_func(g.gate)(
                    n_rows, [(row_assign[a], inv) for a, inv in g.arguments]
                )
            )
        print(f"n_rows = {n_rows}\ncols = {cols}")

        res = {
            "cols": cols,
            "init": [0] * n_rows,
        }
        print(
            f"https://algassert.com/quirk#circuit={urllib.parse.quote(json.dumps(res))}"
        )
