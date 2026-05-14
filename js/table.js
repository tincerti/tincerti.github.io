function initResearchTable(tableId, searchId) {
  const table = document.getElementById(tableId);
  const searchInput = document.getElementById(searchId);
  if (!table || !searchInput) return;

  const tbody = table.querySelector('tbody');
  const headers = Array.from(table.querySelectorAll('thead th'));

  let sortCol = -1;
  let sortDir = 1;
  let activeTopic = null;

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

  // ── Parse topics cells into clickable pill spans ─────────────────
  const topicSet = new Set();

  Array.from(tbody.rows).forEach(row => {
    const cell = row.querySelector('.topics-cell');
    if (!cell) return;
    const topics = cell.textContent.split(';').map(t => t.trim()).filter(Boolean);
    topics.forEach(t => topicSet.add(t));
    cell.innerHTML = topics.map(t =>
      `<span class="topic-tag" data-topic="${t}">${t}</span>`
    ).join('');
  });

  // ── Topic filter bar ─────────────────────────────────────────────
  const filterBar = document.createElement('div');
  filterBar.className = 'topic-filter-bar';
  filterBar.innerHTML =
    '<span class="topic-filter-label">Filter by topic:</span>' +
    Array.from(topicSet).sort().map(t =>
      `<button class="topic-filter-btn" data-topic="${t}">${t}</button>`
    ).join('');
  searchInput.insertAdjacentElement('afterend', filterBar);

  // ── Topic click handlers ─────────────────────────────────────────
  filterBar.addEventListener('click', e => {
    const btn = e.target.closest('.topic-filter-btn');
    if (btn) toggleTopic(btn.dataset.topic);
  });

  tbody.addEventListener('click', e => {
    const tag = e.target.closest('.topic-tag');
    if (tag) toggleTopic(tag.dataset.topic);
  });

  function toggleTopic(topic) {
    activeTopic = activeTopic === topic ? null : topic;
    filterBar.querySelectorAll('.topic-filter-btn').forEach(btn =>
      btn.classList.toggle('active', btn.dataset.topic === activeTopic)
    );
    tbody.querySelectorAll('.topic-tag').forEach(tag =>
      tag.classList.toggle('active', tag.dataset.topic === activeTopic)
    );
    filterRows();
  }

  // ── Search ───────────────────────────────────────────────────────
  searchInput.addEventListener('input', filterRows);

  function rowMatchesTopic(row) {
    if (!activeTopic) return true;
    return Array.from(row.querySelectorAll('.topic-tag'))
      .some(tag => tag.dataset.topic === activeTopic);
  }

  function filterRows() {
    const q = searchInput.value.toLowerCase();
    Array.from(tbody.rows).forEach(row => {
      const matchesSearch = !q || row.textContent.toLowerCase().includes(q);
      row.style.display = matchesSearch && rowMatchesTopic(row) ? '' : 'none';
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
