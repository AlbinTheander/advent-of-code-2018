import { readFileSync } from 'fs';

function parseData(s: string): number[] {
  return s.split(' ').map(Number);
}

interface Node {
  children: Node[];
  metadata: number[];
}

function buildTree(ns: number[]): Node {
  let i = 0;
  const read = (): number => ns[i++];

  function parseNode(): Node {
    const childCount = read();
    const metaCount = read();
    const children = Array(childCount)
      .fill(0)
      .map(() => parseNode());
    const metadata = Array(metaCount)
      .fill(0)
      .map(read);
    return { children, metadata };
  }

  return parseNode();
}

function traverse(node: Node, f: (Node) => void): void {
  f(node);
  node.children.forEach(child => traverse(child, f));
}

function sum(a: number, b: number): number {
  return a + b;
}

function part1(ns: number[]): number {
  const tree = buildTree(ns);
  let total = 0;
  traverse(tree, node => (total += node.metadata.reduce(sum, 0)));
  return total;
}

function part2(ns: number[]): number {
  const tree = buildTree(ns);

  function evaluateNode(node: Node): number {
    if (node.children.length === 0) {
      return node.metadata.reduce(sum, 0);
    } else {
      return node.metadata.reduce((total, d) => {
        const child = node.children[d - 1];
        return total + (child ? evaluateNode(child) : 0);
      }, 0);
    }
  }

  return evaluateNode(tree);
}

export default function run(): void {
  const data = parseData(readFileSync('./data/day8.txt', 'utf-8'));
  const answer1 = part1(data);
  const answer2 = part2(data);

  console.log('-- Day 8');
  console.log('The sum is', answer1);
  console.log('The root value is', answer2);
}
