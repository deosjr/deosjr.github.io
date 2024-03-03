function test(tokens) {
    const e = JSON.stringify(tokens[0]);
    return `foo${e}`;
}
export { test };
