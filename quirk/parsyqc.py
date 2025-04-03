from parsy import eof, fail, generate, regex, success
from dataclasses import dataclass


@dataclass
class QCGate:
    gate: str
    param_expr: str | None
    arguments: list[tuple[str, bool]]


@dataclass
class QCSubcircuit:
    name: str | None
    arguments: list[str]
    gates: list[QCGate]


@dataclass
class QCFile:
    variables: list[str]
    inputs: list[str]
    outputs: list[str]
    subcircuits: list[QCSubcircuit]
    main: QCSubcircuit


space = regex(r"[\a\b\f\r\t\v ]")
comment_eol = space.optional() >> regex(r"(#[^\n]*)?\n")
variables_kw = regex(r"\.[vV]")
inputs_kw = regex(r"\.[iI]")
outputs_kw = regex(r"\.[oO]")
begin_kw = regex(r"[bB][eE][gG][iI][nN]")
end_kw = regex(r"[eE][nN][dD]")
ident = regex(r"[0-9A-Za-z_]+")
gate_ident = regex(r"[0-9A-Za-z_]+\*?")
oparen = regex(r"\(")
cparen = regex(r"\)")
tick = regex(r"'")
star = regex(r"\*")
semi = regex(r";")
comma = regex(r",")

uninterp_expr_chunk = regex(r"[^()#]+")


@generate
def paren_expr():
    res = yield oparen
    while True:
        e = yield paren_expr.optional()
        if e is not None:
            res += e
            continue
        ch = yield uninterp_expr_chunk.optional()
        if ch is not None:
            res += ch
            continue
        # else
        break
    res += yield cparen
    return res


@generate
def argument_list():
    l = []
    while True:
        yield space.optional()
        id = yield ident.optional()
        if id is None:
            return l
        inv = (yield tick.optional()) is not None
        l.append((id, inv))
        yield space.optional()
        yield comma.optional()


@generate
def gate():
    yield space.optional()
    g = yield gate_ident
    yield space.optional()
    p = yield paren_expr.optional()
    args = yield argument_list
    return QCGate(gate=g, param_expr=p, arguments=args)


@generate
def qubit_list():
    l = []
    while True:
        yield space.optional()
        id = yield ident.optional()
        if id is None:
            return l
        l.append(id)


@generate
def qc_subcircuit():
    yield space.optional()
    yield begin_kw
    n = None
    args = []
    if (yield space.optional()) is not None:
        n = yield ident.optional()
        if n is not None:
            yield space.optional()
            if (yield oparen.optional()) is not None:
                args = yield qubit_list
                yield cparen
    yield comment_eol
    gs = []
    while True:
        yield space.optional()
        if (yield end_kw.optional()) is not None:
            yield comment_eol
            break
        if (yield comment_eol.optional()) is not None:
            continue
        gs.append((yield gate))
        yield comment_eol.optional()

    return QCSubcircuit(name=n, arguments=args, gates=gs)


@generate
def qc_file():
    vs = None
    ins = None
    outs = None

    # parse header
    while True:
        yield space.optional()
        if (yield comment_eol.optional()) is not None:
            # skip the extra default comment_eol at end of loop
            continue
        elif (yield variables_kw.optional()) is not None:
            if vs is not None:
                return fail("only one .v declaration per file")
            vs = yield (space >> qubit_list)
        elif (yield inputs_kw.optional()) is not None:
            if ins is not None:
                return fail("only one .i declaration per file")
            ins = yield (space >> qubit_list).optional()
        elif (yield outputs_kw.optional()) is not None:
            if outs is not None:
                return fail("only one .o declaration per file")
            outs = yield (space >> qubit_list).optional()
        else:
            # move on to the subcircuits
            break
        # a handy default for all the .v, .i, .o declarations
        yield comment_eol

    sub_names = set()
    subs = []
    main = None
    while True:
        yield space.optional()
        if (yield comment_eol.optional()) is not None:
            continue
        elif (yield (eof >> success(True)).optional()) is not None:
            break
        else:
            sub = yield qc_subcircuit
            if sub.name is None:
                if main is not None:
                    return fail("only one main (unnamed) subcircuit per file")
                main = sub
            else:
                if sub.name in sub_names:
                    return fail(f"duplicate subcircuit named {sub.name}")
                subs.append(sub)
                sub_names.add(sub.name)

    return QCFile(
        variables=vs or [],
        inputs=ins or [],
        outputs=outs or [],
        subcircuits=subs,
        main=main,
    )
