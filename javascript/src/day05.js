const fs = require('fs');

const ABC = 'abcdefghijklmnopqrstuvwxyz'.split('');

function getInput() {
  return fs.readFileSync('../input/day05.txt', 'utf-8');
}

// Generates a long RegExp on the form /aA|Aa|bB|Bb|.../g
// that can match all "reactive" parts of the string
function getReactionRegEx() {
  const pairs = [];
  ABC.forEach((ch) => {
    pairs.push(ch + ch.toUpperCase());
    pairs.push(ch.toUpperCase() + ch);
  });
  const regEx = pairs.join('|');
  return new RegExp(regEx, 'g');
}

function reactAll(s) {
  const regExp = getReactionRegEx();
  let prev;
  let current = s;
  do {
    prev = current;
    current = current.replace(regExp, '');
  } while (current !== prev);
  return current;
}

function *oneCharRemoved(s) {
  for(let ch of ABC) {
    yield s.replace(new RegExp(ch, 'ig'), '');
  }
}

function getShortestWithOneCharRemoved(s) {
  let best = s;
  for(let s1 of oneCharRemoved(s)) {
    const shrunk = reactAll(s1);
    if (shrunk.length < best.length) best = shrunk;
  }
  return best;
}

function part1() {
  const input = getInput();
  const shrunk = reactAll(input);
  console.log('The length of the shrunk polymer is', shrunk.length);
}

function part2() {
  const input = getInput();
  const shrunk = getShortestWithOneCharRemoved(input);
  console.log('By removing one letter we can get it down to', shrunk.length);
}


function solve() {
  console.log();
  console.log('Day 5');
  part1();
  part2();
}

module.exports = solve;