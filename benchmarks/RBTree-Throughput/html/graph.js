// The goal here is to always force a fresh load if the page has been reloaded.
var firstForceMap = {};
function getDataIndex(url) {
    var opts = {};
    if (!firstForceMap[url]) {
        opts = {cache: "reload"}; // https://developer.mozilla.org/en-US/docs/Web/API/Request/cache
        firstForceMap[url] = true;
    }
    return fetch(url, opts).then(r => r.text().then(t => parseDataIndex(t)));
}

function parseDataIndex(t) {
    let a = JSON.parse("[" + t + " {}]");
    a.pop();
    return a;
}

function getData(url) {
    var opts = {};
    if (!firstForceMap[url]) {
        opts = {cache: "reload"}; // https://developer.mozilla.org/en-US/docs/Web/API/Request/cache
        firstForceMap[url] = true;
    }
    return fetch(url, opts).then(r => r.text().then(t => parseData(t)));
}

function parseData(d) {
    var headers = null;
    var rows = [];
    d.split("\n").forEach(l => {
        var t = l.trim();
        if (t.length !== 0) {
            var row = t.split(/\s+/);
            if (headers) {
                rows.push(row.map(v => parseFloat(v)));
            } else {
                headers = row;
            }
        }
    });

    headers.shift();
    xs = rows.map(a => { var x = a[0]; a.shift(); return x; });
    cols = rows[0].map((col, i) => rows.map(row => row[i]));
    return { headers: headers, cols: cols, xs: xs };
}

function combine(pa, da, ias, pb, db, ibs) {
    var xs = da.xs;
    var hs = ias.map(i => pa + da.headers[i]).concat(ibs.map(i => pb + db.headers[i]));
    var cols = ias.map(i => da.cols[i]).concat(ibs.map(i => db.cols[i]));
    return { headers: hs, cols: cols, xs: xs };
}

function rotateText(c, s, a, x, y, h = 0) {
    c.save();
    c.translate(x,y);
    c.rotate(a);
    c.textAlign = "center";
    c.fillText(s, 0, h);
    c.restore();
}

var strokes = [
    "blue",
    "blue",
    "red",
    "red",
    "orange",
    "orange",
    "black",
    "black"
];

var dashes = [
    [],
    [3,3],
];

var axisStroke = "rgba(0,0,0,0.4)"
var axisDash = [1,1]

function drawAxis(c, y0, w, h, tx, ty) {
    l = (xl,yl,xh,yh) => {
        c.beginPath();
        c.strokeStyle = axisStroke;
        c.setLineDash(axisDash);
        c.moveTo(tx(xl),ty(yl));
        c.lineTo(tx(xh),ty(yh));
        c.stroke();
    };
    
    hl = y => l(0,y,w,y);
    vl = x => l(x,0,x,h);
    
    vl(1);
    vl(18);
    vl(36);
    vl(36+18);
    vl(72);

    e = Math.floor(Math.log10(h/4));
    for (let y = 0; y < h; y += y0) {
        hl(y);
    }

    l(0,0,h/y0,h);
    
    c.textAlign = "center";
    c.font = "20px Helvetica";
    c.fillStyle = axisStroke;
    vt = (x,s) => c.fillText(s, tx(x), ty(h/80));

    rotateText(c, "linear", Math.atan2(ty(y0) - ty(0), tx(1) - tx(0)), tx(h/2/y0), ty(h/2), -5);
    
    vt((18)/2, "1 Core");
    vt((18+36)/2, "2 Cores");
    vt((36 + 36 + 18)/2, "hyper-threads");
    vt((36+18+72)/2, "hyper-threads");

    c.textAlign = "right";
    c.font = "16px Helvetica";
    ht = (y,s) => c.fillText(s, tx(0), ty(y)+5);
    var i = 1;
    for (let y = y0; y < h; y += y0) {
        ht(y,i++);
    }

    c.textAlign = "center";
    c.font = "20px Helvetica";
    c.fillStyle = "black"
    rotateText(c, "Throughput Speedup", -Math.PI/2, 25, ty(h/2));
    rotateText(c, "Threads", 0, tx(w/2), ty(0)+25);
}

function drawGraph(c, data, y0, maxY, w, h) {
    var max = as => as.reduce((a,b)=>Math.max(a,b));
    var maxX = max(data.xs);
    if (!maxY)
        maxY = max(data.cols.map(max));
    if (!y0)
        y0 = max(data.cols.map(c => c[0]));

    var pl = 55;
    var pr = 10;
    var pt = 5;
    var pb = 30;

    var tx = x => pl + (w-pl-pr) * x / maxX;
    var ty = y => pt + (h-pt-pb) - (h-pt-pb) * y / maxY;

    c.globalAlpha = 1;
    drawAxis(c, y0, maxX, maxY, tx, ty);
    
    //c.globalAlpha = 0.8;
    data.cols.forEach((rs,i) => {
        c.beginPath();
        c.strokeStyle = strokes[i % strokes.length];
        c.setLineDash(dashes[i % dashes.length]);
        c.moveTo(tx(data.xs[0]), ty(rs[0]));
        data.xs.forEach((x,j) => {    
            c.lineTo(tx(x),ty(rs[j]));
        });
        c.stroke(); 
    });
}

function drawKeyItem(c,i) {
    var y = 16;
    c.beginPath();
    c.strokeStyle = strokes[i % strokes.length];
    c.setLineDash(dashes[i % dashes.length]);
    c.moveTo(0, y);
    c.lineTo(35,y);
    c.stroke();
}

function drawKey(c,data) {
    l = (y,i) => {
        c.beginPath();
        c.strokeStyle = strokes[i % strokes.length];
        c.setLineDash(dashes[i % dashes.length]);
        c.moveTo(0, y);
        c.lineTo(35,y);
        c.stroke();
    };

    c.textAlign = "left";
    c.font = "20px Helvetica";
    c.fillStyle = "black"
    var y = 0;
    data.headers.forEach((h,i) => {
        l(y+18, i);
        y += 25;

        c.fillText(h, 40, y);
    });
}