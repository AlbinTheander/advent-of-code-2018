const fs = require('fs');

function part1() {
  const data = fs.readFileSync('../input/day01.txt', 'utf-8');
  const result = eval(data.replace('\n', ' + '));
  console.log('The final frequency is', result);
}

function part2() {
  const data = fs.readFileSync('../input/day01.txt', 'utf-8');
  const nums = data.split('\n').map(Number);
  const result = findFirstRepeatedFreq(nums);
  console.log('The first repeated frequency is', result);
}

function findFirstRepeatedFreq(nums) {
  const found = new Set();
  let current = 0;
  for(let i = 0; !found.has(current); i = (i + 1) % nums.length) {
    found.add(current);
    current += nums[i];
  }
  return current;
}

module.exports = function() {
  console.log('Day 01');
  part1();
  part2();
}