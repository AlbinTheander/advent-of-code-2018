import { readFileSync } from 'fs';

interface Marble {
  value: number;
  next: Marble;
  prev: Marble;
}
function clockwise(n: number, current: Marble): Marble {
  if (n === 0) return current;
  if (n < 0) throw Error('Can only walk positive steps');
  return clockwise(n - 1, current.next);
}

function counterClockwise(n: number, current: Marble): Marble {
  if (n === 0) return current;
  if (n < 0) throw Error('Can only walk positive steps');
  return counterClockwise(n - 1, current.prev);
}

function insertAfter(marble: Marble, value: number): Marble {
  const newMarble = {
    value,
    next: marble.next,
    prev: marble
  };
  marble.next = newMarble;
  newMarble.next.prev = newMarble;
  return newMarble;
}

function deleteCurrent(marble: Marble): Marble {
  marble.next.prev = marble.prev;
  marble.prev.next = marble.next;
  return marble.next;
}

function createMarble(value: number): Marble {
  const marble: Partial<Marble> = { value };
  marble.prev = marble as Marble;
  marble.next = marble as Marble;
  return marble as Marble;
}

function getMarbles(marble: Marble): number[] {
  const values = [marble.value];
  let m = marble.next;
  while (m !== marble) {
    values.push(m.value);
    m = m.next;
  }
  return values;
}

function parseData(s: string): { elves: number; marbles: number } {
  const [elves, marbles] = s.match(/\d+/g).map(Number);
  return { elves, marbles };
}

export function part1(elfCount: number, maxMarble: number): number {
  const elves = Array(elfCount).fill(0);
  let currentMarble = createMarble(0);
  for (let m = 1, elf = 0; m <= maxMarble; m++, elf = (elf + 1) % elfCount) {
    if (m % 23 === 0) {
      elves[elf] += m;
      currentMarble = counterClockwise(7, currentMarble);
      elves[elf] += currentMarble.value;
      currentMarble = deleteCurrent(currentMarble);
    } else {
      currentMarble = clockwise(1, currentMarble);
      currentMarble = insertAfter(currentMarble, m);
    }
  }
  return Math.max(...elves);
}

export default function run(): void {
  const { elves, marbles } = parseData(
    readFileSync('./data/day9.txt', 'utf-8')
  );
  const answer1 = part1(elves, marbles);
  const answer2 = part1(elves, marbles * 100);

  console.log('-- Day 9');
  console.log('The winning elf had', answer1, 'points');
  console.log('The next winning elf had', answer2, 'points');
}
