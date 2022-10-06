declare module '*.svg' {
  const content: any;
  export default content;
}

declare module 'uuid' {
  export function v4(): string;
}
