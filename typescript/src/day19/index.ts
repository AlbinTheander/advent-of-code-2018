import { Instructions, CPU } from '../day16/Device';
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

function execute(cpu: CPU, program: Program, stopAtLine = Infinity): number[] {
  const { cpuReg, lines } = program;
  let ip = cpu.reg[cpuReg];
  while (ip >= 0 && ip < lines.length && ip !== stopAtLine) {
    const [op, a, b, c] = lines[ip];
    Instructions[op](cpu, a, b, c);
    cpu.reg[cpuReg]++;
    ip = cpu.reg[cpuReg];
  }
  return cpu.reg;
}

function factorSum(n: number): number {
  let result = 0;
  for (let i = 1; i <= n; i++) if (n % i === 0) result += i;
  return result;
}

function part1(program: Program): number {
  const cpu = { reg: [0, 0, 0, 0, 0, 0] };
  const regs = execute(cpu, program);
  return regs[0];
}

function part2(program: Program): number {
  /*
  Decoding the program, it looks something like this (line number in
  parenthesis):

  CALL INIT                 (line 0)
  FOR reg3 = 1 TO reg5      (line 1 and line 12-15)
    FOR reg1 = 1 TO reg5    (line 2 and line 8-11)
      reg2 = reg3 * reg1    (line 3)
      IF (reg2 === reg5)    (line 4-6)
        reg0 = reg0 + reg3  (line 7)

  INIT:
  reg5 = 2*2*19*11          (line 17-20)
  reg2 = 5*22 + 18          (line 21-23)
  reg5 = reg5 + reg2        (line 24) // Here reg5 == 964
  IF (reg0 === 0)           (line 25) // This is true for part 1
    RETURN                  (line 26)
  reg2 = (27 * 28 + 29)     (line 27-32)
         * 30 * 14 * 32
  reg5 = reg5 + reg2        (line 33) // Now reg5 == 10551364
  reg0 = 0                  (line 34)
  RETURN                    (line 35)

  So, basically, it initalizes reg5 to a value, then it sums
  all the factors of that value.

  However, which register is used to what varies between indata.
  We can look at line 33, the last assignment to reg5 above, to
  see which register really is used to hold the value.
  */

  const cpu = { reg: [1, 0, 0, 0, 0, 0] };
  // Let's run the program until line 1 to see what the number is
  const regs = execute(cpu, program, 1);
  const dataReg = program.lines[33][3];
  return factorSum(regs[dataReg]);
}

export default function run(): void {
  const program = parseData(readFileSync('./data/day19.txt', 'utf-8'));
  const answer1 = part1(program);
  const answer2 = part2(program);

  console.log('-- Day 19');
  console.log('The initial run of the program gives', answer1);
  console.log('The second run of the program gives', answer2);
}
