const fs = require('fs');

const input = `initial state: #..#.#..##......###...###

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
####. => #`;

function readInput() {
  return fs.readFileSync('../input/day12.txt', 'utf-8');
  // return input;
}

function parseInput(input) {
  const [initialInfo, , ...transformationInfo] = input.split('\n');

  const pots = initialInfo.replace('initial state: ', '').trim();
  const transformations = transformationInfo.map((line) => {
    const [from, to] = line.trim().match(/[#.]+/g);
    return { from, to };
  });
  const state = { pots, index: 0, generation: 0 };
  return { state, transformations };
}

function *quintuples(s) {
  const data = '.....' + s + '.....';
  for (let i = 0; i < data.length - 5; i++) {
    yield data.slice(i, i+5);
  }
}

function runTranformations(state, transformations) {
  const { pots, index, generation } = state;
  const groups = quintuples(pots);
  let newPots = '';
  for (let group of groups) {
    const transformation = transformations.find(t => t.from === group);
    const newPot = transformation ? transformation.to : '.';
    newPots = newPots + newPot;
  }
  const firstHash = newPots.indexOf('#');
  const lastHash = newPots.lastIndexOf('#');
  const newIndex = index - 3 + firstHash;
  newPots = newPots.slice(firstHash, lastHash + 1);
  return { pots: newPots, index: newIndex, generation: generation + 1 };
}

function countPotSum(state) {
  let pos = state.index;
  let score = 0;
  for(let pot of state.pots) {
    if (pot === '#') score += pos;
    pos++;
  }
  return score;
}

function findRecurringState(state, transformations) {
  let states = [state];
  while (true) {
    state = runTranformations(state, transformations);
    const oldState = states.find(s => s.pots === state.pots);
    if (oldState) {
      return { oldState, newState: state };
    }
    states.push(state);
  }
}


function part1() {
  let { state, transformations } = parseInput(readInput());
  for (let i = 0; i < 20; i++) {
    state = runTranformations(state, transformations);
  }
  const score = countPotSum(state);
  console.log('After 20 iterations, the pot score is', score)
}

function part2() {
  let { state, transformations } = parseInput(readInput());
  const { oldState, newState } = findRecurringState(state, transformations);
  const oldScore = countPotSum(oldState);
  const newScore = countPotSum(newState);
  const score = (50e9 - oldState.generation) * (newScore - oldScore) + oldScore;

  console.log('After 50e9 iterations the score is', score);
}

function solve() {
  console.log();
  console.log('Day 12');
  part1();
  part2();
}

module.exports = solve;