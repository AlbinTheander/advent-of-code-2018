import { readFileSync } from 'fs';

type Pos4 = [number, number, number, number];
type Constellation = Pos4[];

function parseData(s: string): Pos4[] {
  return s.split('\n').map(line => line.match(/[-\d]+/g).map(Number)) as Pos4[];
}

function dist(p1: Pos4, p2: Pos4): number {
  return p1.reduce((sum, _, i) => sum + Math.abs(p1[i] - p2[i]), 0);
}

function belongsTo(p: Pos4, con: Constellation): boolean {
  return con.some(cp => dist(p, cp) <= 3);
}

function buildConstellations(points: Pos4[]): Constellation[] {
  let constellations = [];
  for (const p of points) {
    const matching = constellations.filter(c => belongsTo(p, c));
    if (matching.length === 0) {
      constellations.push([p]);
    } else {
      const join = [].concat(...matching, [p]);
      constellations = constellations.filter(c => !matching.includes(c));
      constellations.push(join);
    }
  }
  return constellations;
}

export default function run(): void {
  const points = parseData(readFileSync('./data/day25.txt', 'utf-8'));
  const answer1 = buildConstellations(points).length;

  console.log('-- Day 25');
  console.log('Number of constellations:', answer1);
}
