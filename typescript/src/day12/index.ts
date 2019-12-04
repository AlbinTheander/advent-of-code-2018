import { readFileSync } from 'fs';

class Pots {
  pots: Set<number> = new Set();
  smallestPot: number;

  constructor(initialPots = '') {
    [...initialPots].forEach((pot, pos) => {
      if (pot === '#') this.pots.add(pos);
    });
  }

  hasPlant(pos: number): boolean {
    return this.pots.has(pos);
  }

  setPlant(pos: number, exists = true): void {
    if (exists) {
      this.pots.add(pos);
    } else {
      this.pots.delete(pos);
    }
  }

  plantRange(): { min: number; max: number } {
    const min = Math.min(...this.pots);
    const max = Math.max(...this.pots);
    return { min, max };
  }

  sum(): number {
    return [...this.pots].reduce((a, b) => a + b, 0);
  }
}

type Rule = string;

export function parseData(s: string): { pots: Pots; rules: Set<Rule> } {
  const lines = s.split('\n');
  const [, initalState] = lines.shift().split(': ');
  lines.shift();
  const rules = new Set<Rule>();
  lines.forEach(line => {
    const [from, to] = line.split(' => ');
    if (to === '#') rules.add(from);
  });

  return { pots: new Pots(initalState), rules };
}

export function nextGeneration(pots: Pots, rules: Set<Rule>): Pots {
  const newPots = new Pots();
  const { min, max } = pots.plantRange();
  let potsToCheck = '.....';

  for (let p = min - 3; p <= max + 3; p++) {
    potsToCheck = potsToCheck.slice(1) + (pots.hasPlant(p + 2) ? '#' : '.');
    if (rules.has(potsToCheck)) newPots.setPlant(p);
  }
  return newPots;
}

function part1(pots: Pots, rules: Set<Rule>): number {
  let next = pots;
  for (let i = 0; i < 20; i++) next = nextGeneration(next, rules);
  return next.sum();
}

function part2(pots: Pots, rules: Set<Rule>): number {
  let next = pots;
  for (let i = 0; i < 5000; i++) {
    next = nextGeneration(next, rules);
  }
  const sum = next.sum();
  /*
  Looking at several runs, there is a very specific pattern:
  Generations   Sum
  5000          110475
  50000         1100475
  500000        11000475
  etc.

  Following that pattern we get:
  */
  const tail = sum % 10000;
  const head = sum - tail;
  const bigSum = (50000000000 / 5000) * head + tail;
  return bigSum;
}

export default function run(): void {
  const { pots, rules } = parseData(readFileSync('./data/day12.txt', 'utf-8'));
  const answer1 = part1(pots, rules);
  const answer2 = part2(pots, rules);

  console.log('-- Day 12');
  console.log('The sum of the planted pots is', answer1);
  console.log('The sum after 50 billion years is', answer2);
}
