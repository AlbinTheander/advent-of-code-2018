import { parseData, nextGeneration } from '.';

const { pots, rules } = parseData(`initial state: #..#.#..##......###...###

...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #`);
test('can generate next generation', () => {
  let next = pots;
  for (let i = 0; i < 20; i++) next = nextGeneration(next, rules);
  expect(next.sum()).toEqual(325);
});
