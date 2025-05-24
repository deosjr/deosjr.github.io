window.addEventListener("load", async () => {
  try {
    await Scheme.load_main("minikanren.wasm", {
      reflect_wasm_dir: ".",
      user_imports: {
        document: {
          body() { return document.body; },
          createTextNode: Document.prototype.createTextNode.bind(document)
        },
        element: {
          appendChild(parent, child) { return parent.appendChild(child); }
        }
      }});
  } catch(e) {
    if(e instanceof WebAssembly.CompileError) {
	console.log(e);
      document.getElementById("wasm-error").hidden = false;
    }
  }
});
