import Grid from './Grid';
import { readFileSync } from 'fs';

function parseData(s: string): Grid<string> {
  const chs = s.split('\n').map(line => line.split(''));
  return new Grid(chs);
}

function changeAcre(ch: string, neighbors: string[]): string {
  const trees = neighbors.filter(c => c === '|').length;
  const lumberyards = neighbors.filter(c => c === '#').length;
  if (ch === '.' && trees >= 3) return '|';
  if (ch === '|' && lumberyards >= 3) return '#';
  if (ch === '#' && (trees === 0 || lumberyards === 0)) return '.';
  return ch;
}

function evolve(grid: Grid<string>): Grid<string> {
  const newGrid = grid.cloneAndClear('.');
  for (let y = 0; y < grid.height; y++)
    for (let x = 0; x < grid.width; x++) {
      const ch = grid.get(x, y);
      const neighbors = grid.neighbors(x, y);
      const newCh = changeAcre(ch, neighbors);
      newGrid.set(x, y, newCh);
    }
  return newGrid;
}

function resourceValue(grid: Grid<string>): number {
  const trees = grid
    .toString()
    .split('')
    .filter(c => c === '|').length;
  const lumberyards = grid
    .toString()
    .split('')
    .filter(c => c === '#').length;
  return trees * lumberyards;
}

function part1(grid: Grid<string>): number {
  for (let i = 0; i < 10; i++) grid = evolve(grid);
  return resourceValue(grid);
}

export function cycleCounter<T>(start: T, next: (t: T) => T): (n: number) => T {
  const prevs = [];
  let t = start;
  while (!prevs.includes(t.toString())) {
    prevs.push(t.toString());
    t = next(t);
  }
  const first = prevs.findIndex(x => x === t.toString());
  const cycle = prevs.length - first;
  return (n: number): T => {
    const iterations = n <= first ? n : first + ((n - first) % cycle);
    let result = start;
    for (let i = 0; i < iterations; i++) result = next(result);
    return result;
  };
}

function part2(grid: Grid<string>): number {
  const f = cycleCounter(grid, evolve);
  const resultingGrid = f(1000000000);
  return resourceValue(resultingGrid);
}

export default function run(): void {
  const grid = parseData(readFileSync('./data/day18.txt', 'utf-8'));
  const answer1 = part1(grid);
  const answer2 = part2(grid);

  console.log('-- Day 18');
  console.log('Resource value after 10 minutes: ', answer1);
  console.log('Resource value after 1000000000 minutes:', answer2);
}
