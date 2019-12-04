import { runTestCase } from '.';

test('Can run a test case', () => {
  const testCase = {
    before: [3, 2, 1, 1],
    code: [9, 2, 1, 2],
    after: [3, 2, 2, 1]
  };
  const result = runTestCase(testCase);
  console.log(result);
});
