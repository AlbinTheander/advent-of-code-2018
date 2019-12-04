import { readFileSync } from 'fs';

interface Segment {
  constant: string;
  value: number;
  from: number;
  to: number;
}

function parseData(s: string): Segment[] {
  return s
    .split('\n')
    .filter(line => line.trim())
    .map(line => {
      const [constant, value, , from, to] = line.match(/x|y|\d+/g);
      return { constant, value: +value, from: +from, to: +to };
    });
}

function getBounds(
  segments: Segment[]
): { minX: number; maxX: number; minY: number; maxY: number } {
  let minX = Infinity,
    maxX = -Infinity,
    minY = Infinity,
    maxY = -Infinity;
  segments.forEach(({ constant, value, from, to }) => {
    if (constant === 'x') {
      minX = Math.min(minX, value);
      maxX = Math.max(maxX, value);
      minY = Math.min(minY, from);
      maxY = Math.max(maxY, to);
    } else {
      minY = Math.min(minY, value);
      maxY = Math.max(maxY, value);
      minX = Math.min(minX, from);
      maxX = Math.max(maxX, to);
    }
  });
  return { minX, maxX, minY, maxY };
}

function paintGround(segments: Segment[]): string[][] {
  const { minX, maxX, maxY } = getBounds(segments);
  const ground = Array(maxY + 1)
    .fill(0)
    .map(() => Array(maxX - minX + 3).fill('.'));

  ground[0][500 - minX + 1] = '+';
  segments.forEach(({ constant, value, from, to }) => {
    if (constant === 'x') {
      for (let y = from; y <= to; y++) ground[y][value - minX + 1] = '#';
    } else {
      for (let x = from; x <= to; x++) ground[value][x - minX + 1] = '#';
    }
  });
  return ground;
}

function logGround(ground: string[][]): void {
  console.log(ground.map(line => line.join('')).join('\n'));
}

function doTheDripping(ground: string[][]): void {
  const x = ground[0].findIndex(c => c === '+');
  let drippers = [{ x, y: 0 }];
  let count = 0;
  while (drippers.length > 0 && count++ < 30000) {
    let { x, y } = drippers.shift();
    y++;
    while (y < ground.length && ground[y][x] !== '#') {
      ground[y][x] = '|';
      y++;
    }
    if (y === ground.length) continue;
    let done = false;
    while (!done) {
      y--;
      let xl = x;
      ground[y][xl] = '.';
      while (ground[y][xl] !== '#') {
        ground[y][xl] = '~';
        if (ground[y + 1][xl] === '.' || ground[y + 1][xl] === '|') {
          ground[y][xl] = '+';
          drippers.push({ x: xl, y });
          done = true;
          break;
        }
        xl--;
      }
      let xr = x + 1;
      while (ground[y][xr] !== '#') {
        ground[y][xr] = '~';
        if (ground[y + 1][xr] === '.' || ground[y + 1][xr] === '|') {
          ground[y][xr] = '+';
          drippers.push({ x: xr, y });
          done = true;
          break;
        }
        xr++;
      }
    }
    drippers = drippers.filter(
      (d, i) => i === drippers.findIndex(d1 => d1.x === d.x && d1.y === d.y)
    );
  }
  for (let y = 0; y < ground.length; y++)
    for (let x = 0; x < ground[y].length; x++) {
      if (ground[y][x] === '+') {
        let x0 = x - 1;
        while (ground[y][x0] === '~') {
          ground[y][x0] = '|';
          x0--;
        }
        x0 = x + 1;
        while (ground[y][x0] === '~') {
          ground[y][x0] = '|';
          x0++;
        }
      }
    }
}

function part1(segments: Segment[]): number {
  const ground = paintGround(segments);
  doTheDripping(ground);
  const { minY, maxY } = getBounds(segments);
  let waterCount = 0;
  ground.slice(minY, maxY + 1).forEach(level =>
    level.forEach(c => {
      if ('+|~'.includes(c)) waterCount++;
    })
  );
  return waterCount;
}

function part2(segments: Segment[]): number {
  const ground = paintGround(segments);
  doTheDripping(ground);
  const { minY, maxY } = getBounds(segments);
  let waterCount = 0;
  ground.slice(minY, maxY + 1).forEach(level =>
    level.forEach(c => {
      if (c === '~') waterCount++;
    })
  );
  return waterCount;
}

export default function run(): void {
  const segments = parseData(readFileSync('./data/day17.txt', 'utf-8'));
  const answer1 = part1(segments);
  const answer2 = part2(segments);

  console.log('--Day 17');
  console.log('Total water in ground:', answer1); // 24427 too low
  console.log('Retained water:', answer2);
}
