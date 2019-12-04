import { Graph2D } from './Graph2D';

test('Graph2D works', () => {
  const graph2D = new Graph2D();
  graph2D.addEdge([0, 0], [0, -1]);
  expect(graph2D.getNodesFrom(0, 0)).toEqual([[0, -1]]);
  expect(graph2D.getNodesFrom(0, -1)).toEqual([[0, 0]]);
});

test('Graph2D works with some more doors', () => {
  const graph2D = new Graph2D();
  graph2D.addEdge([0, 0], [0, 1]);
  graph2D.addEdge([0, 1], [0, 0]);
  graph2D.addEdge([0, 0], [-1, 0]);
  graph2D.addEdge([0, 0], [0, -1]);
  graph2D.addEdge([0, 0], [1, 0]);
  graph2D.addEdge([1, 0], [2, 0]);
  expect(graph2D.getNodesFrom(0, 0).sort()).toEqual([
    [-1, 0],
    [0, -1],
    [0, 1],
    [1, 0]
  ]);
  expect(graph2D.getNodesFrom(1, 0).sort()).toEqual([
    [0, 0],
    [2, 0]
  ]);
});
