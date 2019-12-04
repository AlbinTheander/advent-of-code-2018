import { readFileSync } from 'fs';

export function parseData(s: string): number[] {
  return s.split('\n').map(Number);
}

export function part1(ns: number[]): number {
  return ns.reduce((a, b) => a + b, 0);
}

function* ciruclar(ns: number[]): Generator<number, number, number> {
  let i = 0;
  while (true) {
    yield ns[i];
    i = (i + 1) % ns.length;
  }
}
export function part2(ns: number[]): number {
  const numbers = ciruclar(ns);
  const found = new Set<number>();
  let sum = 0;
  while (true) {
    const next = numbers.next().value;
    sum += next;
    if (found.has(sum)) return sum;
    found.add(sum);
  }
  return 0;
}

export default function run(): void {
  const data = parseData(readFileSync('data/day1.txt', 'utf-8'));
  const answer1 = part1(data);
  const answer2 = part2(data);
  console.log('-- Day 1');
  console.log('The answer is', answer1);
  console.log('The answer is', answer2);
}
