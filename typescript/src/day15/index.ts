import { Creature, Elf, Goblin } from './creatures';
import { sortByPos } from './pos';
import { printCave } from './cave';
import { readFileSync } from 'fs';
import { log, setEnabled } from './log';

type Cave = string[][];

let elfPower = 3;

export function parseData(s: string): { cave: Cave; creatures: Creature[] } {
  const cave = s.split('\n').map(line => line.split(''));
  const creatures = [];
  for (let y = 0; y < cave.length; y++)
    for (let x = 0; x < cave.length; x++)
      switch (cave[y][x]) {
        case 'G':
          creatures.push(new Goblin(x, y));
          break;
        case 'E':
          creatures.push(new Elf(x, y, elfPower));
          break;
      }
  return { cave, creatures };
}

function bothSpeicesAreAlive(creatures: Creature[]): boolean {
  return (
    creatures.some(c => c.isAlive() && c instanceof Elf) &&
    creatures.some(c => c.isAlive() && c instanceof Goblin)
  );
}

export function runRound(cave: Cave, creatures: Creature[]): boolean {
  sortByPos(creatures);
  let ci = 0;
  while (ci < creatures.length) {
    if (!bothSpeicesAreAlive(creatures)) return false;
    const creature = creatures[ci];
    if (creature.isAlive()) creature.takeTurn(cave, creatures);
    ci++;
  }
  return true;
}

function battle(cave: Cave, creatures: Creature[]): number {
  let round = 0;
  let lastRoundWasComplete = true;
  while (bothSpeicesAreAlive(creatures) && round < 10000) {
    round++;
    log('Round', round);
    lastRoundWasComplete = runRound(cave, creatures);
  }
  if (!lastRoundWasComplete) round--;
  const totalHp = creatures.filter(c => c.isAlive()).reduce((sum, c) => sum + c.hp, 0);
  return round * totalHp;
}

function part1() {
  elfPower = 3;
  const { cave, creatures } = parseData(readFileSync('./data/day15.txt', 'utf-8'));
  return battle(cave, creatures);
}

function part2() {
  let losingPower = 3;
  let winningPower = 100;
  while (losingPower + 1 < winningPower) {
    elfPower = Math.floor((losingPower + winningPower) / 2);
    const { cave, creatures } = parseData(readFileSync('./data/day15.txt', 'utf-8'));
    battle(cave, creatures);
    const elvesSurvived = creatures.filter(c => c instanceof Elf).every(c => c.isAlive());
    if (elvesSurvived) winningPower = elfPower;
    else losingPower = elfPower;
  }
  elfPower = winningPower;
  const { cave, creatures } = parseData(readFileSync('./data/day15.txt', 'utf-8'));
  return battle(cave, creatures);
}

export default function run(): void {
  setEnabled(false);
  const answer1 = part1();
  const answer2 = part2();

  console.log('-- Day 15');
  console.log('Rounds * remaining hitpoints: ', answer1);
  console.log('Rounds * remaining hitpoints with stronger elves:', answer2);
}
