import { readFileSync } from 'fs';

export function parseData(s: string): string[] {
  return s.split('\n');
}

export function hasExactly(n: number, s: string): boolean {
  const countsPerChar = s
    .split('')
    .reduce((o: { [key: string]: number }, ch: string) => {
      o[ch] = (o[ch] || 0) + 1;
      return o;
    }, {});
  return Object.values(countsPerChar).includes(n);
}

export function commonChars(s1: string, s2: string): string {
  return s1.replace(/./g, (ch: string, i: number) => (s2[i] === ch ? ch : ''));
}

function part1(data: string[]): number {
  const hasExactly2 = data.filter(s => hasExactly(2, s));
  const hasExactly3 = data.filter(s => hasExactly(3, s));
  return hasExactly2.length * hasExactly3.length;
}

function part2(data: string[]): string {
  for (const s1 of data) {
    for (const s2 of data) {
      const common = commonChars(s1, s2);
      if (s1.length === common.length + 1) return common;
    }
  }
  return '';
}

export default function run(): void {
  const data = parseData(readFileSync('./data/day2.txt', 'utf-8'));
  const answer1 = part1(data);
  const answer2 = part2(data);

  console.log('-- Day 2');
  console.log('The checksum is', answer1);
  console.log('The common chars are', answer2);
}
