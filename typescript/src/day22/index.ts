import { KeyedMap } from '../util/KeyedMap';
import { Pos } from '../util/Pos';
import { FPriorityQueue } from '../util/PriorityQueue';
import { readFileSync } from 'fs';

function parseData(s: string): number[] {
  return s.match(/\d+/g).map(Number);
}

const ROCK = 0;
const WET = 1;
const NARROW = 2;

export class Cave {
  geoIndexMap = new KeyedMap<Pos, number>(p => p.toString());
  depth: number;
  target: Pos;

  constructor(depth: number, targetX: number, targetY: number) {
    this.depth = depth;
    this.target = [targetX, targetY];
    this.geoIndexMap.set([0, 0], 0);
    this.geoIndexMap.set(this.target, 0);
  }

  geoIndex(x: number, y: number): number {
    const pos: Pos = [x, y];
    if (this.geoIndexMap.has(pos)) return this.geoIndexMap.get(pos);
    const geo =
      y === 0
        ? x * 16807
        : x === 0
        ? y * 48271
        : this.erosionLevel(x, y - 1) * this.erosionLevel(x - 1, y);
    this.geoIndexMap.set(pos, geo);
    return geo;
  }

  erosionLevel(x: number, y: number): number {
    return (this.geoIndex(x, y) + this.depth) % 20183;
  }

  terrain(x: number, y: number): number {
    return this.erosionLevel(x, y) % 3;
  }
}

function caveToString(cave: Cave): string {
  const lines = [];
  for (let y = 0; y <= cave.target[1]; y++) {
    let line = '';
    for (let x = 0; x <= cave.target[0]; x++) line += '.=|'[cave.terrain(x, y)];
    lines.push(line);
  }
  return lines.join('\n');
}

function part2(cave: Cave): number {
  type Tool = 'torch' | 'gear' | 'neither';

  interface ToolPos {
    pos: Pos;
    tool: Tool;
  }

  interface ToCheck extends ToolPos {
    distance: number;
  }

  const toCheck = new FPriorityQueue<ToCheck>(t => t.distance);
  const map = new KeyedMap<ToolPos, ToCheck>(
    tp => tp.tool + tp.pos,
    tp => ({ ...tp, distance: Infinity })
  );
  const validState = (tp: ToolPos): boolean => {
    if (tp.pos[0] < 0) return false;
    if (tp.pos[1] < 0) return false;
    const terrain = cave.terrain(tp.pos[0], tp.pos[1]);
    if (terrain === ROCK && tp.tool === 'neither') return false;
    if (terrain === WET && tp.tool === 'torch') return false;
    if (terrain === NARROW && tp.tool === 'gear') return false;
    return true;
  };

  const up = (p: Pos): Pos => [p[0], p[1] - 1];
  const right = (p: Pos): Pos => [p[0] + 1, p[1]];
  const down = (p: Pos): Pos => [p[0], p[1] + 1];
  const left = (p: Pos): Pos => [p[0] - 1, p[1]];

  const startState: ToCheck = { pos: [0, 0], tool: 'torch', distance: 0 };
  toCheck.add(startState);

  while (!toCheck.empty()) {
    const state = toCheck.take();
    if (
      state.pos[0] === cave.target[0] &&
      state.pos[1] === cave.target[1] &&
      state.tool === 'torch'
    ) {
      return state.distance;
    }
    if (!validState(state)) continue;
    const prevState = map.get(state);
    if (prevState.distance <= state.distance) continue;
    map.set(state, state);
    toCheck.add({ ...state, pos: up(state.pos), distance: state.distance + 1 });
    toCheck.add({ ...state, pos: right(state.pos), distance: state.distance + 1 });
    toCheck.add({ ...state, pos: down(state.pos), distance: state.distance + 1 });
    toCheck.add({ ...state, pos: left(state.pos), distance: state.distance + 1 });
    toCheck.add({ ...state, tool: 'gear', distance: state.distance + 7 });
    toCheck.add({ ...state, tool: 'torch', distance: state.distance + 7 });
    toCheck.add({ ...state, tool: 'neither', distance: state.distance + 7 });
  }
}

function part1(cave: Cave): number {
  let sum = 0;
  for (let y = 0; y <= cave.target[1]; y++)
    for (let x = 0; x <= cave.target[0]; x++) sum += cave.terrain(x, y);
  return sum;
}

export default function run(): void {
  const [depth, x, y] = parseData(readFileSync('./data/day22.txt', 'utf-8'));
  const cave = new Cave(depth, x, y);
  const answer1 = part1(cave);
  const answer2 = part2(cave);

  console.log('--Day 22');
  console.log('The total risk level is', answer1);
  console.log('The fastest way to the target is', answer2, 'minutes');
}
