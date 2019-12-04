export interface CPU {
  reg: number[];
}

export type OpCode = (cpu: CPU, a: number, b: number, c: number) => void;

export const Instructions: { [name: string]: OpCode } = {
  addr: (cpu, a, b, c) => (cpu.reg[c] = cpu.reg[a] + cpu.reg[b]),
  addi: (cpu, a, b, c) => (cpu.reg[c] = cpu.reg[a] + b),

  mulr: (cpu, a, b, c) => (cpu.reg[c] = cpu.reg[a] * cpu.reg[b]),
  muli: (cpu, a, b, c) => (cpu.reg[c] = cpu.reg[a] * b),

  banr: (cpu, a, b, c) => (cpu.reg[c] = cpu.reg[a] & cpu.reg[b]),
  bani: (cpu, a, b, c) => (cpu.reg[c] = cpu.reg[a] & b),

  borr: (cpu, a, b, c) => (cpu.reg[c] = cpu.reg[a] | cpu.reg[b]),
  bori: (cpu, a, b, c) => (cpu.reg[c] = cpu.reg[a] | b),

  setr: (cpu, a, b, c) => (cpu.reg[c] = cpu.reg[a]),
  seti: (cpu, a, b, c) => (cpu.reg[c] = a),

  gtir: (cpu, a, b, c) => (cpu.reg[c] = a > cpu.reg[b] ? 1 : 0),
  gtri: (cpu, a, b, c) => (cpu.reg[c] = cpu.reg[a] > b ? 1 : 0),
  gtrr: (cpu, a, b, c) => (cpu.reg[c] = cpu.reg[a] > cpu.reg[b] ? 1 : 0),

  eqir: (cpu, a, b, c) => (cpu.reg[c] = a === cpu.reg[b] ? 1 : 0),
  eqri: (cpu, a, b, c) => (cpu.reg[c] = cpu.reg[a] === b ? 1 : 0),
  eqrr: (cpu, a, b, c) => (cpu.reg[c] = cpu.reg[a] === cpu.reg[b] ? 1 : 0)
};
