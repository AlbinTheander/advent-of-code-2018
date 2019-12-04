import { log } from './log';

export function printCave(cave: (string | number)[][]): void {
  log(cave.map(line => line.join('')).join('\n'));
}
