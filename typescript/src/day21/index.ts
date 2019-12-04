import { Instructions } from '../day16/Device';
import { CPU } from '../day16/Device';
import { readFileSync } from 'fs';

type Line = [string, number, number, number];
type Program = { cpuReg: number; lines: Line[] };

function parseData(s: string): Program {
  const rawLines = s.split('\n').filter(Boolean);
  const cpuReg = rawLines
    .shift()
    .match(/\d+/g)
    .map(Number)[0];
  const lines: Line[] = rawLines.map(line => {
    const [op, a, b, c] = line.match(/\S+/g);
    return [op, +a, +b, +c];
  });
  return { cpuReg, lines };
}

function execute(
  cpu: CPU,
  program: Program,
  shouldStop: (line: number, cpu: CPU) => boolean = (): boolean => false
): number[] {
  const { cpuReg, lines } = program;
  let ip = cpu.reg[cpuReg];
  while (ip >= 0 && ip < lines.length) {
    const [op, a, b, c] = lines[ip];
    Instructions[op](cpu, a, b, c);
    if (shouldStop(ip, cpu)) return;
    cpu.reg[cpuReg]++;
    ip = cpu.reg[cpuReg];
  }
  return cpu.reg;
}

// This is the manually transpiled program.
export function theProgram(value: number, r0: number, inspect: (r3: number) => boolean): void {
  let r3 = 0;
  do {
    let r1 = r3 | 65536;
    r3 = value;
    do {
      const r4 = r1 & 255;
      r3 = (r3 + r4) & 0xffffff;
      r3 *= 65899;
      r3 &= 0xffffff;
      r1 = r1 >> 8;
    } while (r1 > 0);
    if (inspect(r3)) return;
  } while (r3 !== r0);
}

function part1(program: Program): number {
  let target = 0;
  const cpu = { reg: [NaN, 0, 0, 0, 0, 0] };
  execute(cpu, program, (line, cpu) => {
    if (line === 28) {
      target = cpu.reg[3];
      return true;
    }
    return false;
  });
  return target;
}

function part2(program: Program): number {
  const value = program.lines[7][1];
  const found = new Set<number>();
  let last = 0;
  theProgram(value, NaN, r3 => {
    if (found.has(r3)) {
      return true;
    }
    found.add(r3);
    last = r3;
    return false;
  });
  return last;
}

export default function run(): void {
  const program = parseData(readFileSync('./data/day21.txt', 'utf-8'));
  const answer1 = part1(program);
  const answer2 = part2(program);

  console.log('-- Day 21');
  console.log('The value for shortest execution is', answer1);
  console.log('The value for longest execution is', answer2);
}
