import { readFileSync } from 'fs';

interface Edge {
  from: string;
  to: string;
}

export function parseData(s: string): Edge[] {
  return s.split('\n').map(line => {
    const [from, to] = line.match(/[A-Z](?= )/g);
    return { from, to };
  });
}

function getTasks(edges: Edge[]): string[] {
  const nodeSet = edges.reduce((ns: Set<string>, edge: Edge): Set<string> => {
    ns.add(edge.to);
    ns.add(edge.from);
    return ns;
  }, new Set<string>());

  return [...nodeSet].sort();
}

export function part1(edges: Edge[]): string {
  let tasks = getTasks(edges);
  let edgesLeft = edges;
  let result = '';
  while (tasks.length > 0) {
    const nextNode = tasks.find(n => edgesLeft.every(e => e.to !== n));
    result += nextNode;
    tasks = tasks.filter(n => n !== nextNode);
    edgesLeft = edgesLeft.filter(e => e.from !== nextNode);
  }
  return result;
}

class Worker {
  id: number;
  task: string | null = null;
  timeLeft = 0;

  constructor(id: number) {
    this.id = id;
  }

  isIdle(): boolean {
    return this.task === null;
  }

  isDone(): boolean {
    return this.task && this.timeLeft === 0;
  }

  assign(task: string): void {
    this.task = task;
    this.timeLeft = 61 + task.charCodeAt(0) - 'A'.charCodeAt(0);
  }

  work(): void {
    if (this.task) this.timeLeft--;
  }
}
function part2(edges: Edge[]): number {
  let tasks = getTasks(edges);
  let edgesLeft = edges;
  const workers = Array(6)
    .fill(0)
    .map((_, id) => new Worker(id));

  let ticks = 0;

  while (tasks.length > 0 || workers.some(w => !w.isIdle())) {
    // Assign tasks to idle workers
    workers
      .filter(w => w.isIdle())
      .forEach(w => {
        const nextTask = tasks.find(t =>
          edgesLeft.every(edge => edge.to !== t)
        );
        if (nextTask) {
          // console.log(ticks, ': Worker', w.id, 'starts working with', nextTask);
          w.assign(nextTask);
          tasks = tasks.filter(t => t !== w.task);
        }
      });
    // Let them work
    workers.forEach(w => w.work());
    // Clean up finished work
    workers.forEach(w => {
      if (w.isDone()) {
        // console.log(ticks, ': Worker', w.id, 'is done with', w.task);
        edgesLeft = edgesLeft.filter(e => e.from !== w.task);
        w.task = null;
      }
    });
    ticks++;
  }
  return ticks;
}

export default function run(): void {
  const edges = parseData(readFileSync('./data/day7.txt', 'utf-8'));
  const answer1 = part1(edges);
  const answer2 = part2(edges);

  console.log('-- Day 7');
  console.log('The correct order is', answer1);
  console.log('The total time is', answer2);
}
