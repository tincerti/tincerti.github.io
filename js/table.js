function initResearchTable(tableId, searchId) {
  const table = document.getElementById(tableId);
  const searchInput = document.getElementById(searchId);
  if (!table || !searchInput) return;

  const tbody = table.querySelector('tbody');
  const headers = Array.from(table.querySelectorAll('thead th'));

  let sortCol = -1;
  let sortDir = 1;

  // ── Sort ────────────────────────────────────────────────────────
  headers.forEach((th, i) => {
    th.classList.add('sortable');
    th.addEventListener('click', () => {
      if (sortCol === i) {
        sortDir *= -1;
      } else {
        sortCol = i;
        sortDir = 1;
      }
      headers.forEach(h => h.classList.remove('sort-asc', 'sort-desc'));
      th.classList.add(sortDir === 1 ? 'sort-asc' : 'sort-desc');
      sortRows();
    });
  });

  // ── Filter bars (topic + region), read from <tr data-*> ──────────
  const rows = Array.from(tbody.rows);
  const parse = (row, key) =>
    (row.dataset[key] || '').split('|').map(t => t.trim()).filter(Boolean);

  const active = { topics: null, regions: null };

  function buildBar(key, label) {
    const values = new Set();
    rows.forEach(r => parse(r, key).forEach(v => values.add(v)));
    if (!values.size) return null;
    const bar = document.createElement('div');
    bar.className = 'topic-filter-bar';
    bar.innerHTML =
      `<span class="topic-filter-label">${label}</span>` +
      Array.from(values).sort().map(v =>
        `<button class="topic-filter-btn" data-value="${v}">${v}</button>`
      ).join('');
    bar.addEventListener('click', e => {
      const btn = e.target.closest('.topic-filter-btn');
      if (!btn) return;
      active[key] = active[key] === btn.dataset.value ? null : btn.dataset.value;
      bar.querySelectorAll('.topic-filter-btn').forEach(b =>
        b.classList.toggle('active', b.dataset.value === active[key])
      );
      filterRows();
    });
    return bar;
  }

  const topicBar = buildBar('topics', 'Filter by topic:');
  const regionBar = buildBar('regions', 'Filter by region:');
  let anchor = searchInput;
  [topicBar, regionBar].forEach(bar => {
    if (!bar) return;
    anchor.insertAdjacentElement('afterend', bar);
    anchor = bar;
  });

  // ── Search + filter ──────────────────────────────────────────────
  searchInput.addEventListener('input', filterRows);

  function rowMatches(row, key) {
    return !active[key] || parse(row, key).includes(active[key]);
  }

  function filterRows() {
    const q = searchInput.value.toLowerCase();
    Array.from(tbody.rows).forEach(row => {
      const matchesSearch = !q || row.textContent.toLowerCase().includes(q);
      row.style.display =
        matchesSearch && rowMatches(row, 'topics') && rowMatches(row, 'regions') ? '' : 'none';
    });
  }

  // ── Sort rows ────────────────────────────────────────────────────
  function getCellText(row, col) {
    const cell = row.cells[col];
    return cell ? cell.textContent.trim().toLowerCase() : '';
  }

  function sortRows() {
    const rows = Array.from(tbody.rows);
    rows.sort((a, b) => {
      const ta = getCellText(a, sortCol);
      const tb = getCellText(b, sortCol);
      return ta.localeCompare(tb, undefined, { numeric: true }) * sortDir;
    });
    rows.forEach(r => tbody.appendChild(r));
    filterRows();
  }
}
