window.addEventListener("load", async () => {
  try {
    await Scheme.load_main("dynamicland.wasm", {
      reflect_wasm_dir: ".",
      user_imports: {
        document: {
          body() { return document.body; },
          getElementById: Document.prototype.getElementById.bind(document),
          createTextNode: Document.prototype.createTextNode.bind(document),
          createElement: Document.prototype.createElement.bind(document)
        },
        element: {
          removeElement(elem) { elem.remove(); },
          setAttribute(elem, name, value) { elem.setAttribute(name, value); },
          setStyle(elem, value) { elem.style = value },
          setBackground(elem, value) { elem.style.background = value },
          setZIndex(elem, value) { elem.style.zIndex = value },
          setLeft(elem, value) { elem.style.left = value },
          setTop(elem, value) { elem.style.top = value },
          setPosition(elem, value) { elem.style.position = value },
          getZIndex(elem) { return elem.style.zIndex },
          getLeft(elem) { return elem.style.left },
          getTop(elem) { return elem.style.top },
          getPosition(elem) { return elem.style.position },
          addEventListener(elem, name, f) { elem.addEventListener(name, f, true); },
          appendChild(parent, child) { return parent.appendChild(child); },
          offsetLeft(elem) { return elem.offsetLeft; },
          offsetTop(elem) { return elem.offsetTop; },
          getBoundingClientRect(elem) { return elem.getBoundingClientRect() },
          getX(elem) { return elem.x },
          getY(elem) { return elem.y },
          getWidth(elem) { return elem.width },
          getHeight(elem) { return elem.height }
        },
        event: {
	  preventDefault(e) { e.preventDefault() },
          mouseX(e) { return e.clientX },
          mouseY(e) { return e.clientY },
	  firstTouch(e) { return e.changedTouches[0] }
        }
      }});
  } catch(e) {
    if(e instanceof WebAssembly.CompileError) {
	console.log(e);
      document.getElementById("wasm-error").hidden = false;
    }
  }
});
