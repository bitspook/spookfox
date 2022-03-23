export const sleep = (ms: number) => new Promise((r) => setTimeout(r, ms));

export const gobbleErrorsOf =
  (fn: (...args: any[]) => Promise<any>) =>
  async (...args) => {
    try {
      return await fn(...args);
    } catch (error) {
      console.error('Error while performing', { fn, error });
    }
  };
