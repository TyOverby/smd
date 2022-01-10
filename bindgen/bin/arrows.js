function box(e) {
  let x = e.offsetLeft;
  let y = e.offsetTop;
  let width = e.offsetWidth;
  let height = e.offsetHeight;
  let midx = x + (width / 2);
  let midy = y + (height / 2);
  return { x, y, width, height, midx, midy }
}

function linep(from, to, width) {
  width = width || 1;
  return `
      <line x1="${from.x}" 
            y1="${from.y}" 
            x2="${to.x}" 
            y2="${to.y}" 
            stroke-width="${width}px"
            stroke="rgb(50,50,50)"
            stroke-linecap="round"
            />
      `
// stroke="rgb(${Math.random() * 255},${Math.random() * 255},${Math.random() * 255})"
}

function line(from, to, width) {
  return linep({x:from.midx, y:from.midy}, {x:to.midx, y:to.midy}, width);
}

function length({x,y}) {
  return Math.sqrt(x*x + y*y);
}
function norm({x, y}) {
  let l = Math.sqrt(x*x + y*y);
  return { x : x / l, y : y / l };
}
function perp({x, y}) {
  return [{ x:y, y:-x}, {x:-y, y:x}];
}
function scale({x, y}, s) {
  return { x: x * s, y: y * s};
}
function midpoint({ x:x1, y:y1 }, { x:x2, y:y2 }) {
  return { x: (x1 + x2) / 2, y: (y1 + y2) / 2};
}
function sub({x:x1, y:y1}, {x:x2, y:y2}) {
  return { x: x1 - x2, y: y1 - y2};
}
function add({x:x1, y:y1}, {x:x2, y:y2}) {
  return { x: x1 + x2, y: y1 + y2};
}
function cross({x:x1, y:y1}, {x:x2, y:y2}) {
  return (x1*y2) - (y1*x2);
}

function angle({x,y}) {
  return Math.atan2(x, y) ||0;
}

start_squeeze = 0.9;
prog_squeeze = 0.9;

function sqwoosh(from, to) {
  let out = '';
  out += line(from, to, 2);

  let start = {x: from.midx, y:from.midy };
  let end = {x: to.midx, y:to.midy };
  let v = norm(sub(end, start));
  let [a, b] = perp(v);
  start = add(start, scale(v, to.width/2));
  end = add(start, scale(v, 10*Math.log10(length(end, start))));
  let radius = to.width / 2.0;
  for (var dist = 1; dist < radius; dist += 1) { 
    let rat = dist / (to.width / 2);
    let p = add(start, scale(a, dist));
    let fac = (rat * rat * rat);
    p = add(p, scale(v, -fac * radius));
    let v2 = norm(sub(p, end));
    p = add(p, scale(v2, 2 * (1-rat)));
    let q = add(start, scale(b, dist));
    q = add(q, scale(v, -fac * radius));
    let v3 = norm(sub(q, end));
    q = add(q, scale(v3, 2 * (1-rat)));
    out += linep(p, end);
    out += linep(q, end);
    end = add(start, scale(sub(end, start), prog_squeeze));
  }

  return out;
}

function run(node) {
  let content = node.children[1];
  let canvas = node.children[0];
  let providers = Array.from(content.querySelectorAll("[data-src-name]"));
  let arrows = "";

  for (let provider of providers) {
    let provider_box = box(provider);
    let name = provider.getAttribute('data-src-name');
    let subscribers = Array.from(content.querySelectorAll(`.dest-class-${name}`));
    for (let subscriber of subscribers) {
      let subscriber_box = box(subscriber);
      arrows += sqwoosh(provider_box, subscriber_box);
    }
  }
  canvas.innerHTML = arrows;

  var scale = 1.0;
  var tx = 0.0;
  var ty = 0.0;
  content.addEventListener('wheel', function(event) {
    event.preventDefault();
    scale += event.deltaY * -0.01;

    scale = Math.min(Math.max(.125, scale), 4);
    node.style.transform = `scale(${scale}) translate(${tx}px,${ty}px)`;
  });
  content.addEventListener('mousedown', function() {
    down = true;
  });

  function onmove(event) {
    tx += event.movementX;
    ty += event.movementY;
    let w = content.clientWidth;
    let h = content.clientHeight;
    node.style.transformOrigin = `${tx + w/2}px ${ty + h/2}px`;
    node.style.transform = `scale(${scale}) translate(${tx}px,${ty}px)`;
    console.log({w,h});
  }

  function ondown() {
    content.addEventListener('mousemove',onmove);
    document.addEventListener('mouseup', function() {
      content.removeEventListener('mousemove', onmove);
    }, { once: true });
  }

  content.addEventListener('mousedown', ondown);
}


function go() {
  requestAnimationFrame(function () {
    let maps = Array.from(document.querySelectorAll(".map"));
    for (let map of maps) {
      run(map);
    }
  });
}

go();
