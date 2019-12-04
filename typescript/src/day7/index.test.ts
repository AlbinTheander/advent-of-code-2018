import { parseData, part1 } from '.';

test('Day 7 parseData can split lines', () => {
  const data = `Step B must be finished before step K can begin.
    Step F must be finished before step I can begin.`;
  const edges = parseData(data);
  expect(edges).toEqual([
    { from: 'B', to: 'K' },
    { from: 'F', to: 'I' }
  ]);
});

test('Day 7 part 1 works', () => {
  const data = `Step C must be finished before step A can begin.
  Step C must be finished before step F can begin.
  Step A must be finished before step B can begin.
  Step A must be finished before step D can begin.
  Step B must be finished before step E can begin.
  Step D must be finished before step E can begin.
  Step F must be finished before step E can begin.`;
  const edges = parseData(data);
  expect(part1(edges)).toEqual('CABDFE');
});
