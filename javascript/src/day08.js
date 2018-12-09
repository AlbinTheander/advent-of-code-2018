const fs = require('fs');

function readInput() {
  return fs.readFileSync('../input/day08.txt', 'utf-8');
}

function parseInput(input) {
  return input.split(' ').map(Number);
}

// readNode will consume the array while reading from it.
function readNode(data) {
  const childCount = data.shift();
  const metaDataCount = data.shift();
  const children = [];
  for(let i = 0; i < childCount; i++) {
    const child = readNode(data);
    children.push(child);
  }
  const metaData = data.splice(0, metaDataCount);
  return { meta: metaData, children };
}

function forEachNode(root, fn) {
  fn(root);
  root.children.forEach((child) => forEachNode(child, fn));
}

function sumMeta(root) {
  let sum = 0;
  forEachNode(root, (node) => node.meta.forEach(m => sum += m));
  return sum;
}

function evaluateNode(node) {
  // No node - no score
  if (!node) return 0;

  // No children - sum of meta
  if (node.children.length === 0)
    return node.meta.reduce((a, b) => a+b, 0);

  // Otherwise, sum the value of the children, that meta is pointing at.
  // Note that the meta is indexing the children as 1..., not zero-based.
  let sum = 0;
  node.meta.forEach((meta) => sum += evaluateNode(node.children[meta-1]));

  return sum;
}

function part1() {
  const data = parseInput(readInput());
  const root = readNode(data);
  const totalMeta = sumMeta(root);
  console.log('The total meta sum is', totalMeta);
}

function part2() {
  const data = parseInput(readInput());
  const root = readNode(data);
  const rootValue = evaluateNode(root);
  console.log('The value of the root node is', rootValue);
}

function solve() {
  console.log();
  console.log('Day 8');
  part1();
  part2();
}

module.exports = solve;
