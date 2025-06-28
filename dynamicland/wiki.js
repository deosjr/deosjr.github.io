const cache = new Map();

function getCachedUrl(url) {
  return cache.get(url) || null;
}

function getUrl(url) {
  console.log("getting...")
  const ans = getCachedUrl(url)
  if (ans === null) {
    fetchAndCacheUrl(url)
    return ""
  }
  return ans
}

// somehow callback and recalculate-pages?
// or use an event with a listener registered in wasm?
async function fetchAndCacheUrl(url) {
  console.log("fetching...")
  if (cache.has(url)) return; // already fetched or in progress

  try {
    const response = await fetch(url);
    if (!response.ok) throw new Error('Failed to fetch url');
    const data = await response.text();
    cache.set(url, data);
    const event = new Event("urlfetched");
    window.dispatchEvent(event);
  } catch (err) {
    console.error('Fetch error:', err);
    //cache.set(title, null); // Optional: mark as failed
  }
}

window.addEventListener("load", async () => {
  try {
    await Scheme.load_main("wiki.wasm", {
      reflect_wasm_dir: ".",
      user_imports: {
        wiki: {
          html(url) { return getUrl(url); }
        },
        window: {
          window() { return window; }
        },
        document: {
          body() { return document.body; },
          getElementById: Document.prototype.getElementById.bind(document),
          createTextNode: Document.prototype.createTextNode.bind(document),
          createElement: Document.prototype.createElement.bind(document),
          parseDOM(html) { 
            const parser = new DOMParser();
            return parser.parseFromString(html, "text/html");
          }
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
          getProperty(elem, key) { return elem[key]; },
          setProperty(elem, key, value) { elem[key] = value; },
          focus(elem) { elem.focus(); },
          querySelector(elem, string) { return elem.querySelector(string); },
          querySelectorAll(elem, string) { return Array.from(elem.querySelectorAll(string)); }
        },
        array: {
          length(arr) { return arr.length },
          ref(arr, i) { return arr[i] }
        },
        console: {
          log(str) { console.log(str); }
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
