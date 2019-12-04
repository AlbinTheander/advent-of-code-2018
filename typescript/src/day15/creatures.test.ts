import { printCave } from './cave';
import { parseData, runRound } from '.';
import { setEnabled } from './log';

const cave1 = `
#######   
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######`.slice(1);

test('Creatures can mark targets', () => {
  const { cave, creatures } = parseData(cave1);
  setEnabled(false);
  for (let i = 0; i < 28; i++) runRound(cave, creatures);
  setEnabled(true);
  printCave(cave);
  console.log(creatures);
});
test('hello', () => {});
