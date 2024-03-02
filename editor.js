import CodeFlask from "https://cdn.jsdelivr.net/npm/codeflask@1.4.1/+esm";

const editor = document.getElementById('editor');
const output = document.getElementById("output");

const flask = new CodeFlask(editor, {
    language: 'js',
    lineNumbers: true
});

flask.updateCode("TODO: lispmachine")

flask.onUpdate( e => {
    console.log(e);
    while (output.firstChild) {
        output.removeChild(output.firstChild);
    }
    output.insertAdjacentHTML('beforeend', e)
})
