window.addEventListener("load", async () => {
  try {
    await Scheme.load_main("datalog.wasm", {
      reflect_wasm_dir: ".",
      user_imports: {
        window: {
          window() { return window; }
        },
        console: {
          log(str) { console.log(str); }
        },
        document: {
          body() { return document.body; },
          getElementById: Document.prototype.getElementById.bind(document),
          createTextNode: Document.prototype.createTextNode.bind(document),
          createElement: Document.prototype.createElement.bind(document),
          createSVGElement(name) { return document.createElementNS("http://www.w3.org/2000/svg", name)}
        },
        element: {
          removeElement(elem) { elem.remove(); },
          setAttribute(elem, name, value) { elem.setAttribute(name, value); },
          setStyle(elem, value) { elem.style = value },
          setBackground(elem, value) { elem.style.background = value },
          addClass(elem, className) { elem.classList.add(className); },
          setZIndex(elem, value) { elem.style.zIndex = value },
          setLeft(elem, value) { elem.style.left = value },
          setTop(elem, value) { elem.style.top = value },
          setTransform(elem, value) { elem.style.transform = value },
          setPosition(elem, value) { elem.style.position = value },
          setInnerHTML(elem, value) { elem.innerHTML = value },
          getZIndex(elem) { return elem.style.zIndex },
          getLeft(elem) { return elem.style.left },
          getTop(elem) { return elem.style.top },
          getTransform(elem) { return elem.style.transform },
          getPosition(elem) { return elem.style.position },
          addEventListener(elem, name, f) { elem.addEventListener(name, f, true); },
          appendChild(parent, child) { return parent.appendChild(child); },
          offsetLeft(elem) { return elem.offsetLeft; },
          offsetTop(elem) { return elem.offsetTop; },
          getBoundingClientRect(elem) { return elem.getBoundingClientRect() },
          getX(elem) { return elem.x },
          getY(elem) { return elem.y },
          getWidth(elem) { return elem.width },
          getHeight(elem) { return elem.height },
          focus(elem) { elem.focus(); },
          querySelector(elem, string) { return elem.querySelector(string); }
        },
        event: {
          preventDefault(e) { e.preventDefault() },
          mouseX(e) { return e.clientX },
          mouseY(e) { return e.clientY },
          firstTouch(e) { return e.changedTouches[0] },
          getKey(e) { return e.key }
        }
      }});
  } catch(e) {
    if(e instanceof WebAssembly.CompileError) {
	console.log(e);
      document.getElementById("wasm-error").hidden = false;
    }
  }
});
