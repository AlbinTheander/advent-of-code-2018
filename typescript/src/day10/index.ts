import { readFileSync } from 'fs';

interface Point {
  x: number;
  y: number;
  dx: number;
  dy: number;
}

function parseData(s: string): Point[] {
  return s.split('\n').map(line => {
    const [x, y, dx, dy] = line.match(/[\-0-9]+/g).map(Number);
    return { x, y, dx, dy };
  });
}

function move({ x, y, dx, dy }: Point): Point {
  return { x: x + dx, y: y + dy, dx, dy };
}

function getHeight(points: Point[]): number {
  const ys = points.map(p => p.y);
  return Math.max(...ys) - Math.min(...ys);
}

function paintPoints(points: Point[]): string {
  const xs = points.map(p => p.x);
  const ys = points.map(p => p.y);
  const minX = Math.min(...xs);
  const maxX = Math.max(...xs);
  const minY = Math.min(...ys);
  const maxY = Math.max(...ys);
  const lines = Array(maxY - minY + 1)
    .fill(0)
    .map(() => Array(maxX - minX + 1).fill(' '));

  points.forEach(p => (lines[p.y - minY][p.x - minX] = '#'));
  return lines.map(line => line.join('')).join('\n');
}

function part1(originalPoints: Point[]): { msg: string; ticks: number } {
  let points = originalPoints;
  let ticks = 0;
  while (true) {
    const height = getHeight(points);
    const newPoints = points.map(move);
    const newHeight = getHeight(newPoints);
    if (newHeight > height) return { msg: paintPoints(points), ticks };
    points = newPoints;
    ticks++;
  }
}

export default function run(): void {
  const data = parseData(readFileSync('./data/day10.txt', 'utf-8'));
  const { msg, ticks } = part1(data);

  console.log('-- Day 10');
  console.log('The message is:');
  console.log(msg);
  console.log('Time needed', ticks);
}
