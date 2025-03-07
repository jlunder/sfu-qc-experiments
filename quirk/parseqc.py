import re

noncomment_re = re.compile("^([^#]*)(#.*)?$")
vars_re = re.compile("^\\.[vV] (.*)$")
inputs_re = re.compile("^\\.[iI] (.*)$")
outputs_re = re.compile("^\\.[oO] (.*)$")
begin_re = re.compile("^BEGIN\\s?.*$")
end_re = re.compile("^END\\s?.*$")
gate_re = re.compile("(\\S+)\\s+(.+)")
decl_re = re.compile("[A-Za-z][0-9A-Za-z]*")

split_ws_re = re.compile("\\s+")

#circ.measure_all()    for line_no, l, raw_line in lines:

class QiskitInterp:
    def H_gate(self, circ, _, ctl, tgt):
        if len(ctl) > 0:
            return None
        return circ.h(tgt)
    
    def S_gate(self, circ, _, ctl, tgt):
        if len(ctl) > 0:
            return None
        return circ.s(tgt)
    
    def Sdg_gate(self, circ, _, ctl, tgt):
        if len(ctl) > 0:
            return None
        return circ.sdg(tgt)
    
    def T_gate(self, circ, _, ctl, tgt):
        if len(ctl) > 0:
            return None
        return circ.t(tgt)
    
    def Tdg_gate(self, circ, _, ctl, tgt):
        if len(ctl) > 0:
            return None
        return circ.tdg(tgt)
    
    def X_gate(self, circ, _, ctl, tgt):
        if len(ctl) == 0:
            return circ.x(tgt)
        elif len(ctl) == 1:
            return circ.cx(ctl[0], tgt)
        elif len(ctl) == 2:
            return circ.ccx(ctl[0], ctl[1], tgt)
        else:
            return circ.mcx(ctl, tgt)
    
    def Y_gate(self, circ, _, ctl, tgt):
        if len(ctl) > 0:
            return None
        return circ.y(tgt)
    
    def Z_gate(circ, _, ctl, tgt):
        if len(ctl) == 0:
            return circ.z(tgt)
        elif len(ctl) == 1:
            return circ.cz(ctl[0], tgt)
        elif len(ctl) == 2:
            return circ.ccz(ctl[0], ctl[1], tgt)
        else:
            return None
    
    def Zd_gate(circ, _, ctl, tgt):
        if len(ctl) != 2:
            return None
        circ.tdg(ctl[0])
        circ.tdg(ctl[1])
        circ.cx(tgt, ctl[0])
        circ.t(ctl[0])
        circ.cx(ctl[1], tgt)
        circ.t(tgt)
        circ.cx(ctl[1], ctl[0])
        circ.tdg(ctl[0])
        circ.cx(ctl[1], tgt)
        circ.cx(tgt, ctl[0])
        circ.t(ctl[0])
        circ.tdg(tgt)
        return circ.cx(ctl[1], ctl[0])
    
    def swap_gate(circ, _, ctl, tgt):
        if len(ctl) != 1:
            return None
        return circ.swap(ctl[0], tgt)

accepted_gates = {
    "H": H_gate,
    "P": S_gate,
    "P*": Sdg_gate,
    "S": S_gate,
    "S*": Sdg_gate,
    "T": T_gate,
    "T*": Tdg_gate,
    "X": X_gate,
    "cnot": X_gate,
    "tof": X_gate,
    "Y": Y_gate,
    "Z": Z_gate,
    "tof": X_gate,
    "cnot": X_gate,
    "Z": Z_gate,
    "cz": Z_gate,
    "Zd": Zd_gate,
    "swap": swap_gate,
}

def parse_qc(qc_src):
    lines = [(n, m.group(1).strip(), l) for n, m, l in
                 ((n, noncomment_re.match(l), l) for n, l in
                     enumerate(qc_src.split("\n")))
                 if m and m.group(1).strip() != ""]
    var_decls = None
    input_decls = None
    output_decls = None
    all_decls = None
    decl_ids = {}
    circ = None
    qregs = []
    cregs = []
    state = "decls"
    for line_no, l, raw_line in lines:
        if state == "decls":
            if begin_re.match(l):
                all_decls = set()
                if var_decls is not None:
                    all_decls.update(var_decls)
                if input_decls is not None:
                    all_decls.update(input_decls)
                if output_decls is not None:
                    all_decls.update(output_decls)
                if not all_decls:
                    print(f"Invalid .qc: {line_no}: no variables declared")
                    return None
                for decl in sorted(all_decls):
                    if not decl_re.match(decl):
                        print(f"Invalid .qc: {line_no}: bad decl name '{decl}'")
                        return None
                    decl_ids[decl] = len(decl_ids)
                    qregs.append(qiskit.QuantumRegister(1, decl))
                cregs += [qiskit.ClassicalRegister(1, decl + "_meas") for decl in output_decls]
                circ = qiskit.QuantumCircuit(*(qregs + cregs))
                state = "gates"
                continue
            if vars_re.match(l):
                if var_decls is not None:
                    print(f"Invalid .qc: {line_no}: multiple .v lines")
                    return None
                var_decls = split_ws_re.split(l)[1:]
                continue
            if inputs_re.match(l):
                if input_decls is not None:
                    print(f"Invalid .qc: {line_no}: multiple .i lines")
                    return None
                input_decls = split_ws_re.split(l)[1:]
                continue
            if outputs_re.match(l):
                if output_decls is not None:
                    print(f"Invalid .qc: {line_no}: multiple .o lines")
                    return None
                output_decls = split_ws_re.split(l)[1:]
                continue
            print(f"Invalid .qc: {line_no}: expecting declarations or BEGIN, couldn't parse '{raw_line}'")
            return None
        elif state == "gates":
            if end_re.match(l):
                state = "end"
                continue
            m = gate_re.match(l)
            if not m:
                print(f"Invalid .qc: {line_no}: expecting gate, couldn't parse '{raw_line}'")
                return None
            gate_name = m.group(1)
            if gate_name not in accepted_gates:
                print(f"Invalid .qc: {line_no}: unrecognized gate '{gate_name}'")
                return None
            if not m.group(2):
                print(f"Invalid .qc: {line_no}: no target for gate '{gate_name}'")
                return None
            gate_refs = split_ws_re.split(m.group(2))
            ref_ids = []
            for r in gate_refs:
                if r not in all_decls:
                    print(f"Invalid .qc: {line_no}: '{gate_name}' references undeclared qubit '{r}'")
                    return None
                ref_ids.append(decl_ids[r])
            accepted_gates[gate_name](circ, gate_name, ref_ids[:-1], ref_ids[-1])
        elif state == "end":
            print(f"Invalid .qc: {line_no}: statements after end")
        else:
            print("wtf")
            return None
    if state != "end":
        print(f"Invalid .qc: {line_no + 1}: expecting END, source truncated")
    circ.barrier()
    circ.measure([decl_ids[d] for d in output_decls], list(range(len(cregs))))
    return circ

