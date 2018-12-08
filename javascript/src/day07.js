const fs = require('fs');
const R = require('ramda');

function getInput() {
  const input = fs.readFileSync('../input/day07.txt', 'utf-8');
  return input;
}

function parseInput(input) {
  const lineToEdge = (line) => ({ from: line[5], to: line[36] });
  const lines = input.split('\n');
  const edges = lines.map(lineToEdge);
  const froms = edges.map(e => e.from);
  const tos = edges.map(e => e.to);
  const nodes = R.uniq([...froms, ...tos]);
  return { nodes, edges };
}

function removeNode(graph, node) {
  const { nodes, edges } = graph;
  const newNodes = nodes.filter((n) => n !== node);
  const newEdges = edges.filter(e => e.from !== node && e.to !== node);
  return { nodes: newNodes, edges: newEdges };
}

function findStartNodes(graph) {
  const { nodes, edges } = graph;
  // Find all nodes with no edge leading to them.
  const startNodes = nodes.filter(n => !edges.some(e => e.to === n));
  return startNodes;
}

function getWorkOrder(originalGraph) {
  let graph = originalGraph;
  const order = [];
  while(graph.nodes.length > 0) {
    const [node] = findStartNodes(graph).sort();
    order.push(node);
    graph = removeNode(graph, node);
  }
  return order.join('');
}

class Worker {
  constructor() {
    this.remainingTime = 0;
  }

  isFinished() {
    return this.remainingTime === 0;
  }

  addJob(node) {
    if (!node) return;
    this.node = node;
    this.remainingTime = node.charCodeAt(0) - 'A'.charCodeAt(0) + 61;
  }

  work() {
    if (this.remainingTime > 0) this.remainingTime--;
  }
}

function getNextNode(graph, workers) {
  const allStartNodes = findStartNodes(graph).sort();
  const availableStartNodes = allStartNodes.filter((n) => workers.every(w => w.node !== n));
  return availableStartNodes[0];
}

function runWithHelp(originalGraph, workerCount) {
  let graph = originalGraph;
  const workers = Array(workerCount).fill(0).map(() => new Worker());
  let tick = 0;
  do {
    // Let'em work
    workers.forEach(worker => worker.work());

    // Remove finished work
    workers.filter(w => w.isFinished()).forEach(w => graph = removeNode(graph, w.node));

    // Give idle workers more work
    workers.filter(w => w.isFinished()).forEach(w => w.addJob(getNextNode(graph, workers)));

    tick++;
  } while (!workers.every(worker => worker.isFinished()));
  return tick - 1;
}

function part1() {
  const graph = parseInput(getInput());
  const order = getWorkOrder(graph);
  console.log('The work order is', order);
}

function part2() {
  const graph = parseInput(getInput());
  const workTime = runWithHelp(graph, 5);
  console.log('The total work time is', workTime);
}

function solve() {
  console.log();
  console.log('Day 7');
  part1();
  part2();
}

module.exports = solve;


// const edges = data.split('\n').map(parseLine);
// console.log(getOrder(edges));

// console.log('1322 is too high');
// runWithHelp(edges);