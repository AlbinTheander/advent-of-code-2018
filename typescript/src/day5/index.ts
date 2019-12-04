import { readFileSync } from 'fs';

export function parseData(s: string): number[] {
  const isLowercase = (ch: string): boolean => ch === ch.toLowerCase();
  return s
    .split('')
    .map(ch =>
      isLowercase(ch) ? -ch.charCodeAt(0) : ch.toLowerCase().charCodeAt(0)
    );
}

export function reducePolymer(p: number[]): number[] {
  const result = Array<number>();
  let i = 0;
  while (i < p.length) {
    if (p[i] + p[i + 1] === 0) {
      i += 2;
    } else {
      result.push(p[i]);
      i += 1;
    }
  }
  return result;
}

function reducePolymerFully(p: number[]): number[] {
  let oldP = [];
  let newP = p;
  while (oldP.length !== newP.length) {
    oldP = newP;
    newP = reducePolymer(newP);
  }
  return newP;
}

function part1(polymer: number[]): number {
  return reducePolymerFully(polymer).length;
}

function part2(polymer: number[]): number {
  let shortest = polymer.length;
  for (let c = 'a'.charCodeAt(0); c <= 'z'.charCodeAt(0); c++) {
    const moddedPolymer = polymer.filter(x => x !== c && x !== -c);
    const length = reducePolymerFully(moddedPolymer).length;
    shortest = Math.min(shortest, length);
  }
  return shortest;
}

export default function run(): void {
  const data = parseData(readFileSync('./data/day5.txt', 'utf-8'));
  const answer1 = part1(data);
  const answer2 = part2(data);

  console.log('-- Day 5');
  console.log('Length of reduced polymer:', answer1);
  console.log('Length of shortest modded polymer', answer2);
}
