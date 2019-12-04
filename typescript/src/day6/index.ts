import { readFileSync } from 'fs';

interface Point {
  id: number;
  x: number;
  y: number;
  count: number;
}

type Grid = number[][];

export function parseData(s: string): Point[] {
  let id = 1;
  return s.split('\n').map(line => {
    const [x, y] = line.match(/\d+/g).map(Number);
    return { id: id++, x, y, count: 0 };
  });
}

function distance(p1: Point, p2: Point): number {
  return Math.abs(p1.x - p2.x) + Math.abs(p1.y - p2.y);
}

function findClosestPoint(target: Point, points: Point[]): Point | null {
  let bestPoint = null;
  let bestDist = Infinity;
  for (const p of points) {
    const dist = distance(p, target);
    if (dist === bestDist) {
      bestPoint = null;
    } else if (dist < bestDist) {
      bestPoint = p;
      bestDist = dist;
    }
  }
  return bestPoint;
}

function paintGrid(points: Point[]): Grid {
  const maxX = Math.max(...points.map(p => p.x));
  const maxY = Math.max(...points.map(p => p.y));
  const grid: Grid = Array(maxY + 1)
    .fill(0)
    .map(() => Array(maxX + 1).fill(0));
  for (let x = 0; x <= maxX; x++)
    for (let y = 0; y <= maxY; y++) {
      const p = findClosestPoint({ id: -1, x, y, count: 0 }, points);
      if (p) grid[y][x] = p.id;
    }
  return grid;
}

function isInfinite(grid: Grid, id: number): boolean {
  const maxY = grid.length;
  const maxX = grid[0].length;
  for (let y = 0; y < maxY; y++)
    if (grid[y][0] === id || grid[y][maxX - 1] === id) return true;
  for (let x = 0; x < maxY; x++)
    if (grid[0][x] === id || grid[maxY - 1][x] === id) return true;
  return false;
}

export function part1(points: Point[]): number {
  const grid = paintGrid(points);
  grid.forEach(row => row.forEach(id => id && points[id - 1].count++));
  const sortedPoints = points
    .filter(p => !isInfinite(grid, p.id))
    .sort((p1, p2) => p2.count - p1.count);
  return sortedPoints[0].count;
}

function part2(points: Point[]): number {
  const maxOutside = Math.ceil(points.length / 1000);
  const minX = -maxOutside;
  const minY = -maxOutside;
  const maxX = Math.max(...points.map(p => p.x)) + maxOutside;
  const maxY = Math.max(...points.map(p => p.y)) + maxOutside;

  let count = 0;
  const target = { id: -1, x: 0, y: 0, count: 0 };
  for (let y = minY; y <= maxY; y++)
    for (let x = minX; x <= maxX; x++) {
      target.x = x;
      target.y = y;
      const dist = points.reduce((sum, p) => sum + distance(target, p), 0);
      if (dist < 10000) count++;
    }

  return count;
}

export default function run(): void {
  const points = parseData(readFileSync('./data/day6.txt', 'utf-8'));
  const answer1 = part1(points);
  const answer2 = part2(points);

  console.log('-- Day 6');
  console.log('The biggest area is', answer1);
  console.log('Number of points closer than 10000', answer2);
}
