/* eslint-disable @typescript-eslint/no-use-before-define */
import { Pos } from '../util/Pos';
import { readFileSync } from 'fs';
import { KeyedMap } from '../util/KeyedMap';

function buildRooms(s: string): KeyedMap<Pos, number> {
  const rooms = new KeyedMap<Pos, number>(
    p => p.toString(),
    () => Infinity
  );

  let currentPos: Pos = [0, 0];
  const stack = [currentPos];
  rooms.set([0, 0], 0);

  function walk(dx: number, dy: number): void {
    const currentDist = rooms.get(currentPos);
    currentPos = [currentPos[0] + dx, currentPos[1] + dy];
    const newDist = rooms.get(currentPos);
    const dist = Math.min(newDist, currentDist + 1);
    rooms.set(currentPos, dist);
  }

  for (const ch of s) {
    switch (ch) {
      case 'N':
        walk(0, -1);
        break;
      case 'E':
        walk(1, 0);
        break;
      case 'S':
        walk(0, 1);
        break;
      case 'W':
        walk(-1, 0);
        break;
      case '(':
        stack.push(currentPos);
        break;
      case ')':
        currentPos = stack.pop();
        break;
      case '|':
        currentPos = stack[stack.length - 1];
        break;
    }
  }
  return rooms;
}

function part1(rooms: KeyedMap<Pos, number>): number {
  const distances = [...rooms.values()];
  return Math.max(...distances);
}

function part2(rooms: KeyedMap<Pos, number>): number {
  const distances = [...rooms.values()];
  return distances.filter(d => d >= 1000).length;
}

export default function run(): void {
  const data = readFileSync('./data/day20.txt', 'utf-8');
  const rooms = buildRooms(data);
  const answer1 = part1(rooms);
  const answer2 = part2(rooms);

  console.log('-- Day 20');
  console.log('The shortest path to the furthest room is', answer1);
  console.log('Number of rooms at least 1000 doors away:', answer2);
}
