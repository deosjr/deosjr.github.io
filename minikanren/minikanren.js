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
          setZIndex(elem, value) { elem.style.zIndex = value },
          setLeft(elem, value) { elem.style.left = value },
          setTop(elem, value) { elem.style.top = value },
          addEventListener(elem, name, f) { elem.addEventListener(name, f, true); },
          appendChild(parent, child) { return parent.appendChild(child); },
          offsetLeft(elem) { return elem.offsetLeft; },
          offsetTop(elem) { return elem.offsetTop; }
        },
        event: {
	  preventDefault(e) { e.preventDefault() },
          mouseX(e) { return e.clientX },
          mouseY(e) { return e.clientY }
        }
      }});
  } catch(e) {
    if(e instanceof WebAssembly.CompileError) {
	console.log(e);
      document.getElementById("wasm-error").hidden = false;
    }
  }
});
