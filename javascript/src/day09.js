
function createChain() {
  const node = { marble: 0 };
  node.next = node;
  node.prev = node;
  return node;
}

function chainToArray(chain) {
  let node = chain;
  while (node.marble !== 0) node = stepRight(node);
  let array = [];
  do {
    array.push(node.marble);
    node = stepRight(node);
  } while (node.marble !== 0);
  return array;
}

function stepRight(chain, steps = 1) {
  for (let i = 0; i < steps; i++) chain = chain.next;
  return chain;
}

function stepLeft(chain, steps = 1) {
  for (let i = 0; i < steps; i++) chain = chain.prev;
  return chain;
}

function addMarble(marble, chain) {
  const prevNode = chain.next;
  const nextNode = chain.next.next;
  const node = { marble, next: nextNode, prev: prevNode };
  nextNode.prev = node;
  prevNode.next = node;
  return node;
}

function removeCurrentMarble(chain) {
  const nextNode = chain.next;
  const prevNode = chain.prev;

  nextNode.prev = prevNode;
  prevNode.next = nextNode;
  return nextNode;
}

function playGame(marbleCount, playerCount) {
  const scores = Array(playerCount).fill(0);
  let player = 0;
  let chain = createChain();
  for (let marble = 1; marble <= marbleCount; marble++) {
    if (marble % 23 === 0) {
      chain = stepLeft(chain, 7);
      scores[player] += marble + chain.marble;
      chain = removeCurrentMarble(chain);
    } else {
      chain = addMarble(marble, chain);
    }
    // console.log(player, marble, chainToArray(chain).join(' '));
    player = (player + 1) % scores.length;
  }
  return scores;
}

function part1() {
  const scores = playGame(71019, 432);
  const winner = Math.max(...scores);
  console.log('The winner of the game scores', winner);
}

function part2() {
  const scores = playGame(7101900, 432);
  const winner = Math.max(...scores);
  console.log('The winner of the extended game scores', winner);
}

function solve() {
  console.log();
  console.log('Day 9');
  part1();
  part2();
}

module.exports = solve;