// ═══════════════════════════════════════════════
// FULL CLADOGRAM
// ═══════════════════════════════════════════════
// The whole catalogue drawn as a branching tree, root on the left. Clades open
// and close in place. Players see only the genera they have discovered (the
// rest are "???" tips); the analytics administrator sees every name plus the
// lineage audit flags.
const CLADOGRAM_DEFAULT_OPEN_DEPTH = 4;
const CLADOGRAM_SHORT_LINEAGE_DEPTH = 4;
const CLADOGRAM_CROWDED_NODE_GENERA = 8;
const CLADOGRAM_COLUMN = 214;
const CLADOGRAM_ROW = 34;
const CLADOGRAM_CLADE_WIDTH = 172;
const CLADOGRAM_CLADE_HEIGHT = 26;
const CLADOGRAM_LEAF_WIDTH = 158;
const CLADOGRAM_LEAF_HEIGHT = 22;
const CLADOGRAM_PADDING = 28;
const CLADOGRAM_ZOOM_STEPS = [0.4, 0.55, 0.7, 0.85, 1, 1.2, 1.45];

let cladogramExpanded = null;
let cladogramFocusName = '';
let cladogramView = null;
let cladogramRoot = null;
let cladogramZoom = 1;
let cladogramLayout = null;
let cladogramHighlightKey = '';

function buildCladogramTree(dinosaurs, unlockedSet) {
    const createNode = (name, depth, parent) => ({
        name, depth, parent, children: new Map(), genera: [], total: 0, unlocked: 0
    });
    const root = createNode('Dinosauria', 0, null);
    const nodesByName = new Map([['Dinosauria', root]]);
    const leafParents = new Map();

    for (const dino of dinosaurs) {
        const lineage = Array.isArray(dino.linhagem) ? dino.linhagem : [];
        let node = root;
        lineage.slice(1).forEach((clade, index) => {
            if (!node.children.has(clade)) {
                const child = createNode(clade, index + 1, node);
                node.children.set(clade, child);
                nodesByName.set(clade, child);
            }
            node = node.children.get(clade);
        });
        node.genera.push(dino);
        leafParents.set(dino.nome.toLowerCase(), node);
    }

    const count = node => {
        node.total = node.genera.length;
        node.unlocked = node.genera.filter(dino => unlockedSet.has(dino.nome.toLowerCase())).length;
        for (const child of node.children.values()) {
            count(child);
            node.total += child.total;
            node.unlocked += child.unlocked;
        }
    };
    count(root);
    root.nodesByName = nodesByName;
    root.leafParents = leafParents;
    return root;
}

function collectCladogramAudit(root) {
    const shortLineages = [];
    const crowdedNodes = [];
    const visit = node => {
        if (node.genera.length >= CLADOGRAM_CROWDED_NODE_GENERA) crowdedNodes.push(node);
        for (const dino of node.genera) {
            if ((dino.linhagem?.length || 0) <= CLADOGRAM_SHORT_LINEAGE_DEPTH) shortLineages.push(dino);
        }
        node.children.forEach(visit);
    };
    visit(root);
    return {
        shortLineages: shortLineages.sort((a, b) => a.nome.localeCompare(b.nome)),
        crowdedNodes: crowdedNodes.sort((a, b) => b.genera.length - a.genera.length)
    };
}

function isCladogramCladeOpen(node) {
    if (node.depth === 0) return true;
    return cladogramExpanded
        ? cladogramExpanded.has(node.name)
        : node.depth < CLADOGRAM_DEFAULT_OPEN_DEPTH;
}

function isCladogramGenusNamed(dino, view) {
    return view.isAdmin || view.unlockedSet.has(dino.nome.toLowerCase());
}

// Ladderized layout: smaller branches above larger ones, tips stacked in rows,
// each clade centred on the span of its children.
function layoutCladogram(root, view) {
    const clades = [];
    const leaves = [];
    const edges = [];
    let row = 0;
    let maxX = 0;

    const place = node => {
        const x = node.depth * CLADOGRAM_COLUMN;
        const open = isCladogramCladeOpen(node);
        if (!open) {
            const y = row++ * CLADOGRAM_ROW;
            clades.push({ node, x, y, open: false });
            maxX = Math.max(maxX, x + CLADOGRAM_CLADE_WIDTH + 120);
            return y;
        }

        const named = node.genera.filter(dino => isCladogramGenusNamed(dino, view))
            .sort((a, b) => a.nome.localeCompare(b.nome));
        const hidden = node.genera.filter(dino => !isCladogramGenusNamed(dino, view));
        const entries = [
            ...named.map(dino => ({ dino, size: 1 })),
            ...hidden.map(dino => ({ dino, size: 1, hidden: true })),
            ...[...node.children.values()].map(child => ({ child, size: child.total }))
        ].sort((a, b) => a.size - b.size
            || (a.child ? 1 : 0) - (b.child ? 1 : 0)
            || (a.hidden ? 1 : 0) - (b.hidden ? 1 : 0)
            || String(a.child?.name || a.dino.nome).localeCompare(String(b.child?.name || b.dino.nome)));

        const childX = x + CLADOGRAM_COLUMN;
        const childEdges = entries.map(entry => {
            if (entry.child) {
                return { key: `clade:${entry.child.name}`, x2: childX, y2: place(entry.child) };
            }
            const y = row++ * CLADOGRAM_ROW;
            const key = entry.hidden
                ? `hidden:${node.name}:${hidden.indexOf(entry.dino)}`
                : `genus:${entry.dino.nome.toLowerCase()}`;
            leaves.push({ key, dino: entry.dino, hidden: Boolean(entry.hidden), x: childX, y });
            maxX = Math.max(maxX, childX + CLADOGRAM_LEAF_WIDTH + 40);
            return { key, x2: childX, y2: y };
        });

        const childYs = childEdges.map(edge => edge.y2);
        const y = childYs.length ? (Math.min(...childYs) + Math.max(...childYs)) / 2 : row++ * CLADOGRAM_ROW;
        childEdges.forEach(edge => edges.push({ ...edge, x1: x + CLADOGRAM_CLADE_WIDTH, y1: y }));
        clades.push({ node, x, y, open: true });
        return y;
    };

    place(root);
    return {
        clades, leaves, edges,
        width: maxX + CLADOGRAM_PADDING * 2,
        height: Math.max(1, row) * CLADOGRAM_ROW + CLADOGRAM_PADDING * 2
    };
}

function fitCladogramFont(text, width, maxSize) {
    return Math.max(9, Math.min(maxSize, Math.floor(width / (String(text).length * 0.56))));
}

function renderCladogramSvgMarkup(layout, view, previousKeys) {
    const pad = CLADOGRAM_PADDING;
    const midClade = CLADOGRAM_CLADE_HEIGHT / 2;
    const isNew = key => previousKeys && !previousKeys.has(key) ? ' is-new' : '';
    const markup = [];

    // Right-angle cladogram connectors with rounded elbows.
    for (const edge of layout.edges) {
        const x1 = edge.x1 + pad;
        const y1 = edge.y1 + pad;
        const x2 = edge.x2 + pad;
        const y2 = edge.y2 + pad;
        const elbow = x1 + 18;
        const direction = Math.sign(y2 - y1);
        const radius = Math.min(8, Math.abs(y2 - y1) / 2);
        const d = direction === 0
            ? `M ${x1} ${y1} H ${x2}`
            : `M ${x1} ${y1} H ${elbow - radius} Q ${elbow} ${y1} ${elbow} ${y1 + direction * radius}`
                + ` V ${y2 - direction * radius} Q ${elbow} ${y2} ${elbow + radius} ${y2} H ${x2}`;
        markup.push(`<path class="cladogram-edge${isNew(edge.key)}" d="${d}"/>`);
    }

    for (const { node, x, y, open } of layout.clades) {
        const left = x + pad;
        const top = y + pad - midClade;
        const key = `clade:${node.name}`;
        const safeClade = escapeChallengeHtml(node.name);
        const flagged = view.isAdmin && node.genera.length >= CLADOGRAM_CROWDED_NODE_GENERA;
        const countTitle = view.isAdmin
            ? t('cladogram.countAllTitle', { total: node.total })
            : t('cladogram.countTitle', { unlocked: node.unlocked, total: node.total });
        const countText = view.isAdmin ? `${node.total}` : `${node.unlocked}/${node.total}`;
        const isRoot = node.depth === 0;
        const toggleLabel = t(open ? 'cladogram.collapseClade' : 'cladogram.expandClade', { clade: safeClade });
        const fontSize = fitCladogramFont(node.name, CLADOGRAM_CLADE_WIDTH - (isRoot ? 20 : 44), 13);

        markup.push(`
            <g class="cladogram-clade${open ? ' is-open' : ''}${flagged ? ' is-flagged' : ''}${isNew(key)}${cladogramHighlightKey === key ? ' is-highlighted' : ''}"
               data-cladogram-key="${escapeChallengeHtml(key)}" ${isRoot ? '' : `data-cladogram-toggle="${safeClade}" tabindex="0" role="button" aria-expanded="${open}" aria-label="${toggleLabel}"`}>
                <title>${safeClade} · ${countTitle}</title>
                <rect class="cladogram-clade-box" x="${left}" y="${top}" width="${CLADOGRAM_CLADE_WIDTH}" height="${CLADOGRAM_CLADE_HEIGHT}" rx="7"/>
                <text class="cladogram-clade-label" x="${left + (isRoot ? CLADOGRAM_CLADE_WIDTH / 2 : (CLADOGRAM_CLADE_WIDTH - 24) / 2)}" y="${y + pad}" style="font-size:${fontSize}px">${safeClade}</text>
                ${isRoot ? '' : `
                    <line class="cladogram-clade-divider" x1="${left + CLADOGRAM_CLADE_WIDTH - 24}" y1="${top + 4}" x2="${left + CLADOGRAM_CLADE_WIDTH - 24}" y2="${top + CLADOGRAM_CLADE_HEIGHT - 4}"/>
                    <text class="cladogram-clade-toggle" x="${left + CLADOGRAM_CLADE_WIDTH - 12}" y="${y + pad}">${open ? '−' : '+'}</text>`}
                ${open ? '' : `
                    <path class="cladogram-wedge" d="M ${left + CLADOGRAM_CLADE_WIDTH + 6} ${y + pad} L ${left + CLADOGRAM_CLADE_WIDTH + 52} ${y + pad - 11} L ${left + CLADOGRAM_CLADE_WIDTH + 52} ${y + pad + 11} Z"/>
                    <text class="cladogram-wedge-count" x="${left + CLADOGRAM_CLADE_WIDTH + 60}" y="${y + pad}">${countText}</text>`}
                ${flagged ? `<circle class="cladogram-flag-dot" cx="${left + 4}" cy="${top + 4}" r="5"/>` : ''}
            </g>`);
    }

    for (const leaf of layout.leaves) {
        const left = leaf.x + pad;
        const top = leaf.y + pad - CLADOGRAM_LEAF_HEIGHT / 2;
        const level = DIFFICULTY_MAP[leaf.dino.dificuldade];
        const highlighted = cladogramHighlightKey === leaf.key ? ' is-highlighted' : '';
        if (leaf.hidden) {
            markup.push(`
                <g class="cladogram-leaf is-hidden${isNew(leaf.key)}" aria-label="${t('cladogram.lockedOne')}">
                    <rect class="cladogram-leaf-box" x="${left}" y="${top}" width="${CLADOGRAM_LEAF_WIDTH}" height="${CLADOGRAM_LEAF_HEIGHT}" rx="11"/>
                    <circle class="cladogram-level-dot difficulty-${level}" cx="${left + 12}" cy="${leaf.y + pad}" r="4"/>
                    <text class="cladogram-leaf-label" x="${left + CLADOGRAM_LEAF_WIDTH / 2 + 6}" y="${leaf.y + pad}">???</text>
                </g>`);
            continue;
        }
        const name = leaf.dino.nome;
        const safeName = escapeChallengeHtml(name);
        const undiscovered = !cladogramView.unlockedSet.has(name.toLowerCase());
        const flagged = cladogramView.isAdmin && (leaf.dino.linhagem?.length || 0) <= CLADOGRAM_SHORT_LINEAGE_DEPTH;
        markup.push(`
            <g class="cladogram-leaf${undiscovered ? ' is-undiscovered' : ''}${flagged ? ' is-flagged' : ''}${isNew(leaf.key)}${highlighted}"
               data-cladogram-key="${escapeChallengeHtml(leaf.key)}" data-cladogram-entry="${safeName}"
               tabindex="0" role="button" aria-label="${t('cladogram.openEntry', { name: safeName })}">
                <title>${safeName}</title>
                <rect class="cladogram-leaf-box" x="${left}" y="${top}" width="${CLADOGRAM_LEAF_WIDTH}" height="${CLADOGRAM_LEAF_HEIGHT}" rx="11"/>
                <circle class="cladogram-level-dot difficulty-${level}" cx="${left + 12}" cy="${leaf.y + pad}" r="4"/>
                <text class="cladogram-leaf-label" x="${left + CLADOGRAM_LEAF_WIDTH / 2 + 6}" y="${leaf.y + pad}" style="font-size:${fitCladogramFont(name, CLADOGRAM_LEAF_WIDTH - 34, 13)}px">${safeName}</text>
                ${flagged ? `<circle class="cladogram-flag-dot" cx="${left + CLADOGRAM_LEAF_WIDTH - 4}" cy="${top + 4}" r="5"/>` : ''}
            </g>`);
    }

    return markup.join('');
}

function getCladogramItemY(layout, key) {
    const clade = layout?.clades.find(item => `clade:${item.node.name}` === key);
    if (clade) return clade.y;
    return layout?.leaves.find(item => item.key === key)?.y ?? null;
}

// Redraws the tree. With an anchor key, the scroll position is adjusted so that
// element stays where the player clicked it while branches open around it.
function drawCladogram({ anchorKey = '' } = {}) {
    const viewport = document.getElementById('cladogram-viewport');
    const svg = document.getElementById('cladogram-svg');
    if (!viewport || !svg || !cladogramRoot || !cladogramView) return;

    const previous = cladogramLayout;
    const previousKeys = previous
        ? new Set([...previous.clades.map(item => `clade:${item.node.name}`), ...previous.edges.map(edge => edge.key),
            ...previous.leaves.map(item => item.key)])
        : null;
    const anchorBefore = anchorKey ? getCladogramItemY(previous, anchorKey) : null;

    cladogramLayout = layoutCladogram(cladogramRoot, cladogramView);
    const { width, height } = cladogramLayout;
    svg.setAttribute('viewBox', `0 0 ${width} ${height}`);
    svg.setAttribute('width', Math.round(width * cladogramZoom));
    svg.setAttribute('height', Math.round(height * cladogramZoom));
    svg.innerHTML = renderCladogramSvgMarkup(cladogramLayout, cladogramView, previousKeys);

    const anchorAfter = anchorKey ? getCladogramItemY(cladogramLayout, anchorKey) : null;
    if (anchorBefore !== null && anchorAfter !== null) {
        viewport.scrollTop += (anchorAfter - anchorBefore) * cladogramZoom;
    }
    const zoomValue = document.getElementById('cladogram-zoom-value');
    if (zoomValue) zoomValue.textContent = `${Math.round(cladogramZoom * 100)}%`;
}

function scrollCladogramTo(key, behavior = 'smooth') {
    const viewport = document.getElementById('cladogram-viewport');
    const clade = cladogramLayout?.clades.find(item => `clade:${item.node.name}` === key);
    const leaf = cladogramLayout?.leaves.find(item => item.key === key);
    const item = clade || leaf;
    if (!viewport || !item) return;
    viewport.scrollTo({
        left: Math.max(0, (item.x + CLADOGRAM_PADDING) * cladogramZoom - viewport.clientWidth / 3),
        top: Math.max(0, (item.y + CLADOGRAM_PADDING) * cladogramZoom - viewport.clientHeight / 2),
        behavior
    });
}

function ensureCladogramExpandedSet() {
    if (cladogramExpanded) return cladogramExpanded;
    cladogramExpanded = new Set(
        [...cladogramRoot.nodesByName.values()]
            .filter(node => node.depth < CLADOGRAM_DEFAULT_OPEN_DEPTH)
            .map(node => node.name)
    );
    return cladogramExpanded;
}

function toggleCladogramClade(name) {
    const expanded = ensureCladogramExpandedSet();
    if (expanded.has(name)) expanded.delete(name);
    else expanded.add(name);
    cladogramHighlightKey = '';
    drawCladogram({ anchorKey: `clade:${name}` });
    document.querySelector(`#cladogram-svg [data-cladogram-key="${CSS.escape(`clade:${name}`)}"]`)
        ?.focus({ preventScroll: true });
}

function setAllCladogramClades(open) {
    if (!cladogramRoot) return;
    cladogramExpanded = new Set(open ? cladogramRoot.nodesByName.keys() : ['Dinosauria']);
    cladogramHighlightKey = '';
    drawCladogram();
    scrollCladogramTo('clade:Dinosauria', 'auto');
}

function setCladogramZoom(direction) {
    const index = CLADOGRAM_ZOOM_STEPS.indexOf(cladogramZoom);
    const viewport = document.getElementById('cladogram-viewport');
    const next = direction === 0
        ? 1
        : CLADOGRAM_ZOOM_STEPS[Math.max(0, Math.min(CLADOGRAM_ZOOM_STEPS.length - 1, index + direction))];
    if (!viewport || next === cladogramZoom) return;
    const centerX = (viewport.scrollLeft + viewport.clientWidth / 2) / cladogramZoom;
    const centerY = (viewport.scrollTop + viewport.clientHeight / 2) / cladogramZoom;
    cladogramZoom = next;
    drawCladogram();
    viewport.scrollLeft = centerX * cladogramZoom - viewport.clientWidth / 2;
    viewport.scrollTop = centerY * cladogramZoom - viewport.clientHeight / 2;
}

function revealCladogramTarget(query) {
    const status = document.getElementById('cladogram-status');
    const value = String(query || '').trim().toLowerCase();
    if (!cladogramRoot || !value) return false;

    // Only clades and named genera can match, so the search never reveals an
    // undiscovered genus to a player.
    const cladeNames = [...cladogramRoot.nodesByName.keys()];
    const genusNames = [...cladogramRoot.leafParents.keys()].filter(name =>
        cladogramView.isAdmin || cladogramView.unlockedSet.has(name));
    const find = names => names.find(name => name.toLowerCase() === value)
        || names.find(name => name.toLowerCase().startsWith(value));
    const genus = find(genusNames);
    const clade = genus ? null : find(cladeNames);

    if (!genus && !clade) {
        if (status) status.textContent = t('cladogram.notFound', { query: String(query).trim() });
        return false;
    }

    const expanded = ensureCladogramExpandedSet();
    let node = genus ? cladogramRoot.leafParents.get(genus) : cladogramRoot.nodesByName.get(clade)?.parent;
    for (; node; node = node.parent) expanded.add(node.name);
    cladogramHighlightKey = genus ? `genus:${genus}` : `clade:${clade}`;
    drawCladogram();
    scrollCladogramTo(cladogramHighlightKey);
    if (status) status.textContent = '';
    return true;
}

function renderCladogramAudit(root) {
    const audit = collectCladogramAudit(root);
    const revealButton = (value, label) =>
        `<button type="button" class="cladogram-audit-item" data-cladogram-reveal="${escapeChallengeHtml(value)}">${label}</button>`;
    const list = items => items.length
        ? `<div class="cladogram-audit-list">${items.join('')}</div>`
        : `<p class="cladogram-audit-empty">${t('cladogram.auditNone')}</p>`;

    return `
        <section class="cladogram-audit" aria-labelledby="cladogram-audit-title">
            <h3 id="cladogram-audit-title">${t('cladogram.auditTitle')}</h3>
            <details>
                <summary>${t('cladogram.auditShort', { count: audit.shortLineages.length })}</summary>
                <p>${t('cladogram.auditShortCopy', { depth: CLADOGRAM_SHORT_LINEAGE_DEPTH })}</p>
                ${list(audit.shortLineages.map(dino => revealButton(dino.nome,
                    `<em>${escapeChallengeHtml(dino.nome)}</em> · ${escapeChallengeHtml(dino.linhagem.at(-1) || 'Dinosauria')}`)))}
            </details>
            <details>
                <summary>${t('cladogram.auditCrowded', { count: audit.crowdedNodes.length })}</summary>
                <p>${t('cladogram.auditCrowdedCopy', { minimum: CLADOGRAM_CROWDED_NODE_GENERA })}</p>
                ${list(audit.crowdedNodes.map(node => revealButton(node.name,
                    `${escapeChallengeHtml(node.name)} · ${node.genera.length}`)))}
            </details>
        </section>
    `;
}

function activateCladogramItem(target) {
    const toggle = target?.closest?.('[data-cladogram-toggle]');
    if (toggle) {
        toggleCladogramClade(toggle.dataset.cladogramToggle);
        return true;
    }
    const entry = target?.closest?.('[data-cladogram-entry]');
    if (entry) {
        cladogramFocusName = entry.dataset.cladogramEntry;
        showMuseumEntry(entry.dataset.cladogramEntry);
        return true;
    }
    const reveal = target?.closest?.('[data-cladogram-reveal]');
    if (reveal) {
        revealCladogramTarget(reveal.dataset.cladogramReveal);
        return true;
    }
    return false;
}

// Mouse drag pans the viewport; touch keeps native scrolling.
function initCladogramPanning(viewport) {
    let drag = null;
    viewport.addEventListener('pointerdown', event => {
        if (event.pointerType !== 'mouse' || event.button !== 0) return;
        drag = { x: event.clientX, y: event.clientY, left: viewport.scrollLeft, top: viewport.scrollTop, moved: false };
    });
    viewport.addEventListener('pointermove', event => {
        if (!drag) return;
        const dx = event.clientX - drag.x;
        const dy = event.clientY - drag.y;
        if (!drag.moved && Math.hypot(dx, dy) < 5) return;
        if (!drag.moved) {
            drag.moved = true;
            viewport.setPointerCapture?.(event.pointerId);
            viewport.classList.add('is-dragging');
        }
        viewport.scrollLeft = drag.left - dx;
        viewport.scrollTop = drag.top - dy;
    });
    const end = () => {
        if (drag?.moved) viewport.dataset.suppressClick = 'true';
        drag = null;
        viewport.classList.remove('is-dragging');
    };
    viewport.addEventListener('pointerup', end);
    viewport.addEventListener('pointercancel', end);
    viewport.addEventListener('click', event => {
        if (viewport.dataset.suppressClick === 'true') {
            delete viewport.dataset.suppressClick;
            return;
        }
        activateCladogramItem(event.target);
    });
    viewport.addEventListener('keydown', event => {
        if (event.key !== 'Enter' && event.key !== ' ') return;
        if (activateCladogramItem(event.target)) event.preventDefault();
    });
}

function openCladogramFromMuseum(name) {
    closeMuseumEntry();
    showCladogram({ focus: name });
}

// Restoring the route keeps the last focus; entry points pass their own.
async function showCladogram({ focus = null } = {}) {
    setAppRoute('/cladogram');
    setHeaderControls('cladogram');
    if (focus !== null) cladogramFocusName = focus;
    const appContent = document.getElementById('app-content');

    appContent.innerHTML = `<div class="game-card">${renderAppState(t('cladogram.loading'))}</div>`;
    const loadingCard = appContent.firstElementChild;
    const ownerId = currentUserId;
    const isCurrentRequest = () => appContent.firstElementChild === loadingCard
        && currentUserId === ownerId;

    try {
        let catalog = fullDatabase;
        if (!fullDatabase || fullDatabase.length === 0) {
            catalog = (await callGameApi('catalog')).dinosaurs || [];
            if (!isCurrentRequest()) return;
        }
        const [dinosaurs, discoveryRecords, isAdmin] = await Promise.all([
            ensureMuseumCatalogLineages(catalog),
            getDiscoveryRecords(),
            currentUserId ? initializeAnalyticsAccess() : false
        ]);
        if (!isCurrentRequest()) return;
        fullDatabase = dinosaurs;
        museumDiscoveryRecords = discoveryRecords;

        const unlockedSet = new Set(Object.values(discoveryRecords).map(record => record.name.toLowerCase()));
        cladogramView = { unlockedSet, isAdmin: isAdmin === true };
        cladogramRoot = buildCladogramTree(dinosaurs, unlockedSet);
        cladogramLayout = null;
        const searchNames = [
            ...dinosaurs.filter(dino => isCladogramGenusNamed(dino, cladogramView)).map(dino => dino.nome),
            ...cladogramRoot.nodesByName.keys()
        ].sort((a, b) => a.localeCompare(b));

        appContent.innerHTML = `
            <div class="game-card cladogram-screen">
                <h2 class="screen-title">${t('cladogram.title')}</h2>
                <p class="cladogram-intro">${t(cladogramView.isAdmin ? 'cladogram.adminIntro' : 'cladogram.intro')}</p>
                <div class="cladogram-progress">
                    ${t('cladogram.progress', { unlocked: cladogramRoot.unlocked, total: cladogramRoot.total })}
                </div>

                <div class="cladogram-toolbar">
                    <form class="museum-search cladogram-search" onsubmit="event.preventDefault(); revealCladogramTarget(this.elements.query.value)">
                        <label for="cladogram-search-input">${t('cladogram.search')}</label>
                        <div class="cladogram-search-row">
                            <input id="cladogram-search-input" name="query" type="search" list="cladogram-names"
                                   placeholder="${t('cladogram.searchPlaceholder')}" autocomplete="off">
                            <button type="submit" class="btn-hint">${t('cladogram.find')}</button>
                        </div>
                        <datalist id="cladogram-names">
                            ${searchNames.map(name => `<option value="${escapeChallengeHtml(name)}"></option>`).join('')}
                        </datalist>
                    </form>
                    <div class="cladogram-toolbar-actions">
                        <button type="button" class="btn-hint" onclick="setAllCladogramClades(true)">${t('cladogram.expandAll')}</button>
                        <button type="button" class="btn-hint" onclick="setAllCladogramClades(false)">${t('cladogram.collapseAll')}</button>
                    </div>
                </div>
                <div class="cladogram-status" id="cladogram-status" aria-live="polite"></div>

                ${cladogramView.isAdmin ? renderCladogramAudit(cladogramRoot) : ''}

                <div class="cladogram-stage">
                    <div class="cladogram-legend" aria-hidden="true">
                        <span><i class="cladogram-swatch is-clade"></i>${t('cladogram.legendClade')}</span>
                        <span><i class="cladogram-swatch is-genus"></i>${t('cladogram.legendGenus')}</span>
                        <span><i class="cladogram-swatch is-hidden"></i>${t('cladogram.legendLocked')}</span>
                        ${cladogramView.isAdmin ? `<span><i class="cladogram-swatch is-flag"></i>${t('cladogram.legendFlag')}</span>` : ''}
                    </div>
                    <div class="cladogram-zoom" role="group" aria-label="${t('tree.zoom')}">
                        <button type="button" onclick="setCladogramZoom(1)" aria-label="${t('tree.zoomIn')}" title="${t('tree.zoomIn')}">+</button>
                        <button type="button" id="cladogram-zoom-value" onclick="setCladogramZoom(0)" aria-label="${t('tree.zoomReset')}" title="${t('tree.zoomReset')}">100%</button>
                        <button type="button" onclick="setCladogramZoom(-1)" aria-label="${t('tree.zoomOut')}" title="${t('tree.zoomOut')}">&minus;</button>
                    </div>
                    <div class="cladogram-viewport" id="cladogram-viewport">
                        <svg id="cladogram-svg" class="cladogram-svg" xmlns="http://www.w3.org/2000/svg"></svg>
                    </div>
                    <p class="cladogram-hint">${t('cladogram.hint')}</p>
                </div>
            </div>`;

        appContent.querySelector('.cladogram-audit')?.addEventListener('click', event => activateCladogramItem(event.target));
        initCladogramPanning(document.getElementById('cladogram-viewport'));
        drawCladogram();
        focusAppScreenHeading();
        if (!cladogramFocusName || !revealCladogramTarget(cladogramFocusName)) {
            scrollCladogramTo(cladogramHighlightKey || 'clade:Dinosauria', 'auto');
        }
    } catch (err) {
        console.error('Cladogram Error:', err);
        if (!isCurrentRequest()) return;
        appContent.innerHTML = `<div class="game-card">${renderAppState(t('cladogram.loadError'), {
            type: 'error', detail: err.message
        })}</div>`;
    }
}
