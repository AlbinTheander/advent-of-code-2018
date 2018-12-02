const fs = require('fs');
const { curry, groupBy, any, identity, values, filter } = require('ramda');

function getInput() {
  return fs.readFileSync('../input/day02.txt', 'utf-8').split('\n');
}

function hasExactly(n, s) {
  const charGroups = groupBy(identity, s.split(''));
  return any((cs) => cs.length === n, values(charGroups));
}

function part1() {
  const ids = getInput();
  const doubles = filter(curry(hasExactly)(2))(ids);
  const triples = filter(curry(hasExactly)(3))(ids);
  const answer = doubles.length * triples.length;
  console.log('The box checksum is', answer);
}

function *allPairs(stringArray) {
  for(let i = 0; i < stringArray.length; i++)
    for (let j = i+1; j < stringArray.length; j++)
      yield([stringArray[i], stringArray[j]]);
}

function commonChars(s1, s2) {
  return s1.split('').filter((c, i) => c === s2[i]).join('');
}

function findMatchingBoxes(boxes) {
  const pairs = allPairs(boxes);
  for (let [b1, b2] of pairs) {
    const common = commonChars(b1, b2);
    if (common.length === b1.length - 1) return common;
  }
}

function part2() {
  const boxes = getInput();
  const result = findMatchingBoxes(boxes);
  console.log('The common part of the two boxes is', result);
}

module.exports = function() {
  console.log();
  console.log('Day 2');
  part1();
  part2();
}