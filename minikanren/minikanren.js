window.addEventListener("load", async () => {
  try {
    await Scheme.load_main("minikanren.wasm", {
      reflect_wasm_dir: ".",
      user_imports: {
        document: {
          body() { return document.body; },
          getElementById: Document.prototype.getElementById.bind(document),
          createTextNode: Document.prototype.createTextNode.bind(document),
          createElement: Document.prototype.createElement.bind(document)
        },
        element: {
          setAttribute(elem, name, value) { elem.setAttribute(name, value); },
          setBackground(elem, value) { elem.style.background = value },
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
