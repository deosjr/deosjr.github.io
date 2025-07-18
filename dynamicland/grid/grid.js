setInterval(() => {
    const event = new Event("update-realtalk");
    window.dispatchEvent(event);
}, 100)

window.addEventListener("load", async () => {
  try {
    await Scheme.load_main("grid.wasm", {
      reflect_wasm_dir: "../.",
      user_imports: {
        window: {
          window() { return window; }
        },
        console: {
          log(str) { console.log(str); }
        },
        date: {
          now() { return Date.now(); }
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
        },
        array: {
          make(len) { return new Array(len); },
          set(arr, i, v) { arr[i] = v; }
        },
        d3: {
          // points is an array of arrays: [ [x, y], ... ]
          catmullRom(points) {
            const svg = d3.select("#table").select("svg");
            const line = d3.line().curve(d3.curveCatmullRom)
              .x(d => d[0])
              .y(d => d[1]);
            return svg.append("path")
              .attr("d", line(points))
              .attr("style", "stroke: steelblue; fill: none; stroke-width: 2;")
              .attr("class", "line")
              .node();
          }
        }
      }});
  } catch(e) {
    if(e instanceof WebAssembly.CompileError) {
	console.log(e);
      document.getElementById("wasm-error").hidden = false;
    }
  }
});
