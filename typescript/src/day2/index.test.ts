import { hasExactly, commonChars } from './index';

test('hasExactly works', () => {
  expect(hasExactly(2, 'abcde')).toBeFalsy();
  expect(hasExactly(2, 'abcdafg')).toBeTruthy();
  expect(hasExactly(2, 'abcdafga')).toBeFalsy();
  expect(hasExactly(2, 'aabbb')).toBeTruthy();
});

test('common works', () => {
  expect(commonChars('abcd', 'abcd')).toEqual('abcd');
  expect(commonChars('abxd', 'abcd')).toEqual('abd');
});
