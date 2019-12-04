interface Pos {
  x: number;
  y: number;
}

export function sortByPos(ps: Pos[]): Pos[] {
  return ps.sort((p1: Pos, p2: Pos) => p1.y - p2.y || p1.x - p2.x);
}
