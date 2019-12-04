import { KeyedMap } from './KeyedMap';
import { BetterSet } from './BetterSet';
import { Pos } from './Pos';

export class Graph2D {
  nodes = new BetterSet<Pos>(pos => pos.toString());
  edgesPerNode = new KeyedMap<Pos, BetterSet<Pos>>(
    c => c.toString(),
    () => new BetterSet<Pos>(pos => pos.toString())
  );

  addEdge(p1: Pos, p2: Pos): void {
    this.nodes.add(p1);
    const p1Edges = this.edgesPerNode.get(p1);
    p1Edges.add(p2);
    this.edgesPerNode.set(p1, p1Edges);

    this.nodes.add(p2);
    const p2Edges = this.edgesPerNode.get(p2);
    p2Edges.add(p1);
    this.edgesPerNode.set(p2, p2Edges);
  }

  getNodesFrom(x: number, y: number): Pos[] {
    return [...this.edgesPerNode.get([x, y]).values()];
  }

  getNodes(): Pos[] {
    return [...this.nodes.values()];
  }

  hasNode(p: Pos): boolean {
    return this.nodes.has(p);
  }

  toString(): string {
    const lines = [];
    const xs = this.getNodes().map(p => p[0]);
    const ys = this.getNodes().map(p => p[1]);
    const minX = Math.min(...xs);
    const maxX = Math.max(...xs);
    const minY = Math.min(...ys);
    const maxY = Math.max(...ys);
    console.log(minX, maxX, minY, maxY);

    for (let y = minY; y <= maxY; y++) {
      let line1 = '';
      let line2 = '';
      for (let x = minX; x <= maxX; x++) {
        if (x === 0 && y === 0) line1 += 'O';
        else line1 += this.nodes.has([x, y]) ? 'X' : ' ';
        line2 += this.edgesPerNode.get([x, y]).has([x, y + 1]) ? '|' : ' ';

        line1 += this.edgesPerNode.get([x, y]).has([x + 1, y]) ? '-' : ' ';
        line2 += ' ';
      }
      lines.push(line1, line2);
    }
    return lines.join('\n');
  }
}
