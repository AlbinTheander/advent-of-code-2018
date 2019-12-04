import { readFileSync } from 'fs';

interface Patch {
  id: number;
  x: number;
  y: number;
  width: number;
  height: number;
}

interface Fabric {
  squares: number[];
  singleId: number;
}

function parseData(s: string): Patch[] {
  return s.split('\n').map(line => {
    const [id, x, y, width, height] = line.match(/\d+/g).map(Number);
    return { id, x, y, width, height };
  });
}

function paintPatches(patches: Patch[]): Fabric {
  const squares = Array(1000 * 1000).fill(0);
  const singles = new Set<number>();
  for (const p of patches) {
    singles.add(p.id);
    for (let x = p.x; x < p.x + p.width; x++)
      for (let y = p.y; y < p.y + p.height; y++) {
        const pos = y * 1000 + x;
        if (squares[pos] === 0) {
          squares[pos] = p.id;
        } else {
          singles.delete(squares[pos]);
          singles.delete(p.id);
          squares[pos] = -1;
        }
      }
  }
  const singleId = singles.values().next().value;
  return { squares, singleId };
}

function part1(fabric: Fabric): number {
  return fabric.squares.filter(x => x === -1).length;
}

function part2(fabric: Fabric): number {
  return fabric.singleId;
}

export default function run(): void {
  const data = parseData(readFileSync('./data/day3.txt', 'utf-8'));
  const fabric = paintPatches(data);
  const answer1 = part1(fabric);
  const answer2 = part2(fabric);

  console.log('-- Day3');
  console.log('The number of double-painted squares are', answer1);
  console.log('The id of the single-painted patch is', answer2);
}
