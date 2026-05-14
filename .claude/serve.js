const http = require('http');
const fs = require('fs');
const path = require('path');
const port = process.env.PORT || 9000;
const root = path.join(__dirname, '..');
const mime = { '.html':'text/html', '.css':'text/css', '.js':'application/javascript',
  '.png':'image/png', '.jpg':'image/jpeg', '.jpeg':'image/jpeg', '.svg':'image/svg+xml',
  '.pdf':'application/pdf', '.ico':'image/x-icon', '.woff2':'font/woff2' };
http.createServer((req, res) => {
  let fp = path.join(root, req.url === '/' ? '/index.html' : req.url);
  if (!fp.startsWith(root)) { res.writeHead(403); return res.end(); }
  fs.readFile(fp, (err, data) => {
    if (err) { res.writeHead(404); return res.end('Not found'); }
    res.writeHead(200, { 'Content-Type': mime[path.extname(fp)] || 'application/octet-stream' });
    res.end(data);
  });
}).listen(port, () => console.log(`Serving on ${port}`));
