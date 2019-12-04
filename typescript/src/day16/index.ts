import { readFileSync } from 'fs';
import { OpCode, Instructions } from './Device';

interface TestCase {
  before: number[];
  code: number[];
  after: number[];
}

function parseData(s: string): { testCases: TestCase[]; program: number[][] } {
  const testCases = [];
  const lines = s.split('\n').filter(line => line.trim() !== '');
  let lineNr = 0;
  while (lines[lineNr].startsWith('Before')) {
    const before = lines[lineNr].match(/\d+/g).map(Number);
    lineNr++;
    const code = lines[lineNr].match(/\d+/g).map(Number);
    lineNr++;
    const after = lines[lineNr].match(/\d+/g).map(Number);
    lineNr++;
    testCases.push({ before, code, after });
  }
  const program = lines.slice(lineNr).map(line => line.match(/\d+/g).map(Number));
  return { testCases, program };
}

export function runTestCase(testCase: TestCase): OpCode[] {
  return Object.values(Instructions).filter(opcode => {
    const cpu = { reg: testCase.before.slice() };
    const [, a, b, c] = testCase.code;
    opcode(cpu, a, b, c);
    return cpu.reg.toString() === testCase.after.toString();
  });
}

function part1(testCases: TestCase[]): number {
  const threeOrMore = testCases.filter(testCase => {
    const matchingOpCodes = runTestCase(testCase);
    return matchingOpCodes.length >= 3;
  });
  return threeOrMore.length;
}

function createOpCodeMap(testCases: TestCase[]): string[] {
  let opCodeMap = [];
  testCases.forEach(testCase => {
    const matchingOpCodes = runTestCase(testCase).map(opCode => opCode.name);
    const code = testCase.code[0];
    if (opCodeMap[code]) {
      opCodeMap[code] = opCodeMap[code].filter(op => matchingOpCodes.includes(op));
    } else {
      opCodeMap[code] = matchingOpCodes;
    }
  });
  while (opCodeMap.some(code => code.length > 1)) {
    const singles = [].concat(...opCodeMap.filter(code => code.length === 1));
    opCodeMap = opCodeMap.map(code =>
      code.length === 1 ? code : code.filter(op => !singles.includes(op))
    );
  }
  return opCodeMap.map(code => code[0]);
}

// eslint-disable-next-line @typescript-eslint/no-unused-vars
function verifyTestCases(testCases: TestCase[], opMap: string[]): void {
  const i = 1;
  for (const testCase of testCases) {
    const cpu = { reg: testCase.before.slice() };
    const [op, a, b, c] = testCase.code;
    const opcode = opMap[op];
    Instructions[opcode](cpu, a, b, c);
    if (cpu.reg.toString() !== testCase.after.toString()) throw Error('Testcase is weird');
  }
}

function part2(testCases: TestCase[], program: number[][]): number {
  const opCodeMap = createOpCodeMap(testCases);
  // verifyTestCases(testCases, opCodeMap);
  const cpu = { reg: [0, 0, 0, 0] };
  for (const instruction of program) {
    // const oldRegs = cpu.reg.slice();
    const [op, a, b, c] = instruction;
    const opcode = opCodeMap[op];
    Instructions[opcode](cpu, a, b, c);
    // console.log(opcode, a, b, c, oldRegs, cpu.reg);
  }
  return cpu.reg[0];
}

export default function run(): void {
  const { testCases, program } = parseData(readFileSync('./data/day16.txt', 'utf-8'));
  const answer1 = part1(testCases);
  const answer2 = part2(testCases, program);

  console.log('-- Day 16');
  console.log('Number of very ambiguous test cases', answer1); // > 521
  console.log('Register 0 after execution', answer2);
}
