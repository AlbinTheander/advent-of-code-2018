import { readFileSync } from 'fs';
import { PriorityQueue } from '../util/PriorityQueue';

type Pos3 = [number, number, number];
type Nanobot = { pos: Pos3; radius: number };

function parseData(s: string): Nanobot[] {
  const lines = s.split('\n');
  return lines.map(line => {
    const [x, y, z, radius] = line.match(/[-\d]+/g).map(Number);
    return { pos: [x, y, z], radius };
  });
}

function maxBy<T>(ts: T[], val: (t: T) => number): T {
  let best = null;
  let bestVal = -Infinity;
  for (const t of ts) {
    const v = val(t);
    if (v > bestVal) {
      best = t;
      bestVal = v;
    }
  }
  return best;
}

function dist(p1: Pos3, p2: Pos3): number {
  const result = p1.reduce((sum, _, c) => sum + Math.abs(p1[c] - p2[c]), 0);
  return result;
}

function part1(bots: Nanobot[]): number {
  const strongest = maxBy(bots, bot => bot.radius);
  console.log(strongest);
  const inRange = bots.filter(b => dist(strongest.pos, b.pos) <= strongest.radius);
  return inRange.length;
}

// This will, hopefully, find a point that is common to the most amount of bots.
function getPointCommonToMostBots(bots: Nanobot[]): Pos3 {
  type Box = { center: Pos3; size: Pos3; botsInRange: number };
  const botsInRange = (pos: Pos3): number => bots.filter(b => dist(b.pos, pos) <= b.radius).length;
  const isZero = (pos: Pos3): boolean => pos.every(c => c === 0);

  // Create 4x4x4 sectors with half the side lengths for the given box.
  const sectors = (box: Box): Box[] => {
    const halfSize = box.size.map(c => c >> 1);
    const [[minX, maxX], [minY, maxY], [minZ, maxZ]] = box.center.map((c, i) => [
      c - halfSize[i],
      c + halfSize[i] + 1
    ]);
    const [stepX, stepY, stepZ] = [(maxX - minX) / 3, (maxY - minY) / 3, (maxZ - minZ) / 3];
    const boxes = [];
    for (let x = minX; x <= maxX; x += stepX)
      for (let y = minY; y <= maxY; y += stepY)
        for (let z = minZ; z <= maxZ; z += stepZ) {
          const p = [x, y, z].map(c => Math.floor(c)) as Pos3;
          const newBox = {
            center: p,
            size: halfSize,
            botsInRange: botsInRange(p)
          };
          boxes.push(newBox);
        }
    return boxes;
  };

  // First we create a box around all bots.
  const xs = bots.map(b => b.pos[0]);
  const ys = bots.map(b => b.pos[1]);
  const zs = bots.map(b => b.pos[2]);
  const min = [Math.min(...xs), Math.min(...ys), Math.min(...zs)];
  const max = [Math.max(...xs), Math.max(...ys), Math.max(...zs)];
  const size = min.map((_, i) => max[i] - min[i]) as Pos3;
  const mid = min.map((_, i) => min[i] + Math.floor(size[i] / 2)) as Pos3;
  let box: Box = { center: mid, size, botsInRange: botsInRange(mid) };

  // Now add it as the starting box to check.
  const boxesToCheck = new PriorityQueue<Box>();
  boxesToCheck.add(box, -box.botsInRange);
  // We'll never run out of boxes to check. If the box size doesn't converge to zero,
  // we'll run out of memory.
  while (!boxesToCheck.empty()) {
    // Take the best box.
    box = boxesToCheck.take();
    // If we're down to a point, we'll use that.
    if (isZero(box.size)) {
      return box.center;
    }
    // Create new smaller boxes inside this one and add them to the queue
    sectors(box).forEach(sector => boxesToCheck.add(sector, -sector.botsInRange));
  }
  return null;
}

function part2(bots: Nanobot[]): number {
  const p = getPointCommonToMostBots(bots);
  const botsInRange = bots.filter(b => dist(b.pos, p) <= b.radius);

  // Find the distance to the closest point that is common to all of them.
  // It looks a bit weird, with Math.max and all, but think! It works.
  // Also funny that we don't even know which point that is. :-)
  const result = Math.max(...botsInRange.map(b => dist(b.pos, [0, 0, 0]) - b.radius));
  return result;
}

export default function run(): void {
  const bots = parseData(readFileSync('./data/day23.txt', 'utf-8'));
  const answer1 = part1(bots);
  const answer2 = part2(bots);

  console.log('--Day 23');
  console.log('Amount of bots in range of the most powerful one:', answer1);
  console.log('Distance to the optimal teleporation point:', answer2);
}
