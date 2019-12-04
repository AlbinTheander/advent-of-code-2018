import { readFileSync } from 'fs';

export function parseData(s: string): number[][] {
  const serial = Number(s);
  const powerLevel = (x, y) => {
    const rackId = x + 11;
    const rawPowerLevel = (rackId * (y + 1) + serial) * rackId;
    const digit = Math.floor(rawPowerLevel / 100) % 10;
    return digit - 5;
  };
  const grid: number[][] = Array(300)
    .fill(0)
    .map((_, y) =>
      Array(300)
        .fill(0)
        .map((_, x) => powerLevel(x, y))
    );

  return grid;
}

export function sumGrid(grid: number[][]): number[][] {
  for (let y = grid.length - 1; y >= 0; y--) {
    for (let x = grid[0].length - 1; x >= 0; x--) {
      const me = grid[y][x];
      const x1 = grid[y][x + 1] || 0;
      const y1 = grid[y + 1] ? grid[y + 1][x] : 0;
      const xy1 = grid[y + 1] ? grid[y + 1][x + 1] || 0 : 0;
      grid[y][x] = me + x1 + y1 - xy1;
    }
  }
  return grid;
}

function getSquareSum(
  summedGrid: number[][],
  x: number,
  y: number,
  size: number
): number {
  return (
    summedGrid[y][x] -
    summedGrid[y][x + size] -
    summedGrid[y + size][x] +
    summedGrid[y + size][x + size]
  );
}

function getMaxSquareSum(summedGrid: number[][], size: number): number[] {
  const height = summedGrid.length;
  const width = summedGrid[0].length;
  let best = -Infinity;
  let bestX = 0;
  let bestY = 0;
  for (let y = 0; y < height - size; y++)
    for (let x = 0; x < width - size; x++) {
      const sqSum = getSquareSum(summedGrid, x, y, size);
      if (sqSum > best) {
        best = sqSum;
        bestX = x;
        bestY = y;
      }
    }
  return [bestX + 1, bestY + 1, best];
}

function part1(summedGrid: number[][]): { x: number; y: number } {
  const [x, y] = getMaxSquareSum(summedGrid, 3);
  return { x, y };
}

function part2(summedGrid: number[][]): { x: number; y: number; size: number } {
  let bestScore = -Infinity;
  let best = null;
  for (let size = 1; size < 299; size++) {
    const [x, y, score] = getMaxSquareSum(summedGrid, size);
    if (score > bestScore) {
      bestScore = score;
      best = { x, y, size };
    }
  }
  return best;
}

export default function run(): void {
  const data = parseData(readFileSync('./data/day11.txt', 'utf-8'));
  const summedGrid = sumGrid(data);
  const answer1 = part1(summedGrid);
  const answer2 = part2(summedGrid);

  console.log('-- Day 11');
  console.log('The biggest 3x3 cell is', answer1);
  console.log('The biggest cell is', answer2);
}
