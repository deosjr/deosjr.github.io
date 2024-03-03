interface token {
    type: string;
    content: string;
    alias: string;
    matchedStr: string;
}

function test(tokens:(string|token)[]):string {
    const e = JSON.stringify(tokens[0]);
    return `foo${e}`;
}

export { test };
