// ═══════════════════════════════════════════════════════════════════════
// GLOBAL COLLAPSED STATE (Accessible globally)
// ═══════════════════════════════════════════════════════════════════════
if (typeof window.collapsedClades === 'undefined') {
    window.collapsedClades = new Set();
}

if (typeof window.currentTreeSnapshot === 'undefined') {
    window.currentTreeSnapshot = null;
}

if (typeof window.treeAnimationState === 'undefined') {
    window.treeAnimationState = {
        nodeKeys: new Set(),
        edgeKeys: new Set(),
        hasRendered: false,
        mode: 'restore',
        focusKey: null
    };
}

if (typeof window.treeViewState === 'undefined') {
    window.treeViewState = {
        bestFocusKey: null,
        latestFocusKey: null,
        bestCladeName: 'Dinosauria',
        latestLabel: ''
    };
}

function resetTreeAnimationState() {
    window.treeAnimationState = {
        nodeKeys: new Set(),
        edgeKeys: new Set(),
        hasRendered: false,
        mode: 'restore',
        focusKey: null
    };
    window.treeViewState = {
        bestFocusKey: null,
        latestFocusKey: null,
        bestCladeName: 'Dinosauria',
        latestLabel: ''
    };
}

function setTreeAnimationMode(mode, focusKey = null) {
    window.treeAnimationState.mode = mode || 'default';
    window.treeAnimationState.focusKey = focusKey;
}

function getTreeAnimationMode() {
    return window.treeAnimationState?.mode || 'default';
}

function getTreeElementByKey(key) {
    if (!key) return null;
    const svg = document.getElementById('tree-svg');
    if (!svg) return null;

    return Array.from(svg.querySelectorAll('[data-tree-key]')).find(element => {
        return element.dataset.treeKey === key || `display:${element.dataset.treeDisplay}` === key;
    }) || null;
}

function centerTreeElement(element, behavior = 'smooth') {
    const container = document.getElementById('tree-container');
    if (!container || !element) return false;

    const containerRect = container.getBoundingClientRect();
    const targetRect = element.getBoundingClientRect();
    const left = targetRect.left + targetRect.width / 2 - (containerRect.left + containerRect.width / 2);
    const top = targetRect.top + targetRect.height / 2 - (containerRect.top + containerRect.height / 2);
    const reduceMotion = window.matchMedia?.('(prefers-reduced-motion: reduce)').matches;

    container.scrollBy({
        left,
        top,
        behavior: reduceMotion ? 'auto' : behavior
    });
    return true;
}

function focusTreeKey(key, options = {}) {
    if (!key) return;

    const focusVisibleElement = () => {
        const element = getTreeElementByKey(key);
        if (element) centerTreeElement(element, options.behavior || 'smooth');
    };

    if (getTreeElementByKey(key)) {
        focusVisibleElement();
        return;
    }

    if (options.expand !== false && window.collapsedClades.size && window.currentTreeSnapshot) {
        window.collapsedClades.clear();
        renderTreeSnapshot(window.currentTreeSnapshot);
        requestAnimationFrame(() => requestAnimationFrame(focusVisibleElement));
    }
}

function focusTreeBestTrail() {
    focusTreeKey(window.treeViewState?.bestFocusKey, { expand: true });
}

function focusTreeLatest() {
    focusTreeKey(window.treeViewState?.latestFocusKey, { expand: true });
}

function focusTreeRoot() {
    focusTreeKey('node:Dinosauria', { expand: false });
}

function expandAllTreeClades() {
    if (!window.collapsedClades.size || !window.currentTreeSnapshot) return;
    window.collapsedClades.clear();
    renderTreeSnapshot(window.currentTreeSnapshot);
}

function ensureTreeToolbar(container) {
    let toolbar = container.previousElementSibling;
    if (!toolbar || !toolbar.classList.contains('tree-toolbar')) {
        toolbar = document.createElement('div');
        toolbar.className = 'tree-toolbar';
        toolbar.innerHTML = `
            <div class="tree-toolbar-copy" aria-live="polite">
                <span class="tree-toolbar-kicker">Best known trail</span>
                <strong class="tree-toolbar-clade">Dinosauria</strong>
            </div>
            <div class="tree-toolbar-actions">
                <button type="button" class="tree-toolbar-btn" data-tree-action="best">Best trail</button>
                <button type="button" class="tree-toolbar-btn" data-tree-action="latest">Latest move</button>
                <button type="button" class="tree-toolbar-btn" data-tree-action="root">Root</button>
                <button type="button" class="tree-toolbar-btn" data-tree-action="expand">Expand all</button>
            </div>
        `;
        toolbar.addEventListener('click', event => {
            const action = event.target.closest('[data-tree-action]')?.dataset.treeAction;
            if (action === 'best') focusTreeBestTrail();
            if (action === 'latest') focusTreeLatest();
            if (action === 'root') focusTreeRoot();
            if (action === 'expand') expandAllTreeClades();
        });
        container.parentNode.insertBefore(toolbar, container);
    }

    const clade = toolbar.querySelector('.tree-toolbar-clade');
    const latestButton = toolbar.querySelector('[data-tree-action="latest"]');
    const expandButton = toolbar.querySelector('[data-tree-action="expand"]');
    if (clade) clade.textContent = window.treeViewState.bestCladeName || 'Dinosauria';
    if (latestButton) {
        latestButton.disabled = !window.treeViewState.latestFocusKey;
        latestButton.title = window.treeViewState.latestLabel
            ? `Center ${window.treeViewState.latestLabel}`
            : 'No guess to center yet';
    }
    if (expandButton) expandButton.disabled = window.collapsedClades.size === 0;
}

function toggleCladeCollapse(clade) {
    if (window.collapsedClades.has(clade)) {
        window.collapsedClades.delete(clade);
    } else {
        window.collapsedClades.add(clade);
    }
    if (window.currentTreeSnapshot) {
        renderTreeSnapshot(window.currentTreeSnapshot);
    }
}

function initTreePanning() {
  const container = document.getElementById('tree-container');
  if (!container || container.dataset.panInitialized === 'true') return;

  container.dataset.panInitialized = 'true';

  let isDown = false;
  let startX, startY, scrollLeft, scrollTop;

  container.addEventListener('mousedown', e => {
    if (e.target.closest('.tree-node-group, g')) return;
    isDown = true;
    startX = e.pageX - container.offsetLeft;
    startY = e.pageY - container.offsetTop;
    scrollLeft = container.scrollLeft;
    scrollTop  = container.scrollTop;
  });

  container.addEventListener('mouseleave', () => isDown = false);
  container.addEventListener('mouseup',    () => isDown = false);

  container.addEventListener('mousemove', e => {
    if (!isDown) return;
    e.preventDefault();
    const x = e.pageX - container.offsetLeft;
    const y = e.pageY - container.offsetTop;
    container.scrollLeft = scrollLeft - (x - startX);
    container.scrollTop  = scrollTop  - (y - startY);
  });

  // Touch devices use the container's native momentum scrolling.
}

function renderCurrentGameTree() {
  if (window.currentTreeSnapshot) {
    renderTreeSnapshot(window.currentTreeSnapshot);
  }
}

function renderTreeSnapshot(treeSnapshot) {
  if (!treeSnapshot || !Array.isArray(treeSnapshot.nodes) || !Array.isArray(treeSnapshot.leaves)) {
    const wrapper = document.getElementById('tree-scroll-wrapper');
    if (wrapper) wrapper.innerHTML = '<div class="empty-state">No tree data.</div>';
    return;
  }

  window.currentTreeSnapshot = treeSnapshot;

  const nodes = new Map();
  treeSnapshot.nodes.forEach(node => {
    if (!node || typeof node.name !== 'string') return;
    nodes.set(node.name, {
      depth: Number(node.depth) || 0,
      children: Array.isArray(node.children) ? [...node.children] : [],
      type: node.type === 'root' ? 'root' : 'internal',
      lineageIndex: Number.isFinite(Number(node.lineageIndex))
        ? Number(node.lineageIndex)
        : 0,
      isHinted: node.isHinted === true
    });
  });

  const leaves = treeSnapshot.leaves
    .filter(leaf => leaf && typeof leaf.name === 'string')
    .map(leaf => ({ ...leaf }));

  const animationState = window.treeAnimationState;
  const currentNodeKeys = new Set();
  const currentEdgeKeys = new Set();

  nodes.forEach((data, clade) => {
    currentNodeKeys.add(`node:${clade}`);
    data.children.forEach(child => currentEdgeKeys.add(`edge:${clade}->${child}`));
  });
  leaves.forEach(leaf => {
    currentNodeKeys.add(`leaf:${leaf.name}`);
    currentEdgeKeys.add(`edge:${leaf.parentNode}->${leaf.name}`);
  });

  const animationMode = animationState.mode || 'default';
  const shouldAnimateArrivals = ['guess', 'hint', 'victory', 'reveal'].includes(animationMode);
  const animation = {
    mode: animationMode,
    focusKey: animationState.focusKey || null,
    newNodeKeys: shouldAnimateArrivals
      ? new Set([...currentNodeKeys].filter(key => !animationState.nodeKeys.has(key)))
      : new Set(),
    newEdgeKeys: shouldAnimateArrivals
      ? new Set([...currentEdgeKeys].filter(key => !animationState.edgeKeys.has(key)))
      : new Set()
  };

  renderTreeModel(nodes, leaves, animation);

  animationState.nodeKeys = currentNodeKeys;
  animationState.edgeKeys = currentEdgeKeys;
  animationState.hasRendered = true;
  animationState.mode = 'default';
  animationState.focusKey = null;
}

function renderTreeModel(nodes, leaves, animation = {}) {
  const container = document.getElementById('tree-container');
  const wrapper = document.getElementById('tree-scroll-wrapper');
  if (!container || !wrapper || !nodes.has('Dinosauria')) return;

  const savedScrollLeft = container.scrollLeft;
  const savedScrollTop = container.scrollTop;
  const animationMode = animation.mode || 'default';
  const focusKey = animation.focusKey || null;
  const newNodeKeys = animation.newNodeKeys || new Set();
  const newEdgeKeys = animation.newEdgeKeys || new Set();

  const nodeWidth = 200;
  const nodeHeight = 45;

  // ─────────────────────────────────────────────────────────────────────
  // ── COLLAPSE FILTERING LOGIC
  // ─────────────────────────────────────────────────────────────────────
  const parentMap = new Map();
  nodes.forEach((data, clade) => {
    data.children.forEach(child => parentMap.set(child, clade));
  });
  const hiddenNodeCache = new Map();

  function isNodeHidden(nodeName) {
    if (hiddenNodeCache.has(nodeName)) return hiddenNodeCache.get(nodeName);
    let current = nodeName;
    while (current && current !== 'Dinosauria') {
      let parent = parentMap.get(current);
      if (parent && window.collapsedClades.has(parent)) {
        hiddenNodeCache.set(nodeName, true);
        return true;
      }
      current = parent;
    }
    hiddenNodeCache.set(nodeName, false);
    return false;
  }

  nodes.forEach((data, clade) => {
    if (clade !== 'Dinosauria' && isNodeHidden(clade)) {
      nodes.delete(clade);
    }
  });

  nodes.forEach((data, clade) => {
    if (window.collapsedClades.has(clade)) {
      data.children = [];
    } else {
      data.children = data.children.filter(child => !isNodeHidden(child));
    }
  });

  const visibleLeaves = leaves.filter(leaf => {
    return !isNodeHidden(leaf.parentNode) && !window.collapsedClades.has(leaf.parentNode);
  });
  const visibleLeavesByParent = new Map();
  visibleLeaves.forEach(leaf => {
    if (!visibleLeavesByParent.has(leaf.parentNode)) {
      visibleLeavesByParent.set(leaf.parentNode, []);
    }
    visibleLeavesByParent.get(leaf.parentNode).push(leaf);
  });

  const bestPathNodeKeys = new Set();
  const bestPathEdgeKeys = new Set();
  const victoryNodeKeys = new Set();
  const victoryEdgeKeys = new Set();
  const targetLeaf = leaves.find(leaf => leaf.isTarget);
  const visibleTargetLeaf = visibleLeaves.find(leaf => leaf.isTarget);

  if (targetLeaf) {
    bestPathNodeKeys.add(`leaf:${targetLeaf.name}`);
    bestPathEdgeKeys.add(`edge:${targetLeaf.parentNode}->${targetLeaf.name}`);

    let current = targetLeaf.parentNode;
    while (current) {
      bestPathNodeKeys.add(`node:${current}`);
      const parent = parentMap.get(current);
      if (parent) bestPathEdgeKeys.add(`edge:${parent}->${current}`);
      current = parent;
    }
  }

  const fallbackLatestLeaf = [...leaves].reverse().find(leaf => !leaf.isTarget);
  if (focusKey) {
    window.treeViewState.latestFocusKey = focusKey;
  } else if (!window.treeViewState.latestFocusKey && fallbackLatestLeaf) {
    window.treeViewState.latestFocusKey = `leaf:${fallbackLatestLeaf.name}`;
  }
  const latestKey = window.treeViewState.latestFocusKey;
  const latestLeaf = latestKey?.startsWith('leaf:')
    ? leaves.find(leaf => `leaf:${leaf.name}` === latestKey)
    : null;
  window.treeViewState.latestLabel = latestKey?.startsWith('display:')
    ? latestKey.slice('display:'.length)
    : (latestKey?.startsWith('node:')
      ? latestKey.slice('node:'.length)
      : (latestLeaf?.displayName || fallbackLatestLeaf?.displayName || ''));
  window.treeViewState.bestFocusKey = targetLeaf ? `leaf:${targetLeaf.name}` : 'node:Dinosauria';
  window.treeViewState.bestCladeName = targetLeaf?.parentNode || 'Dinosauria';
  ensureTreeToolbar(container);

  if (animationMode === 'victory' && visibleTargetLeaf && !visibleTargetLeaf.isGiveUp) {
    victoryNodeKeys.add(`leaf:${visibleTargetLeaf.name}`);
    victoryEdgeKeys.add(`edge:${visibleTargetLeaf.parentNode}->${visibleTargetLeaf.name}`);

    let current = visibleTargetLeaf.parentNode;
    while (current) {
      victoryNodeKeys.add(`node:${current}`);
      const parent = parentMap.get(current);
      if (parent) victoryEdgeKeys.add(`edge:${parent}->${current}`);
      current = parent;
    }
  }
  
  const nodePositions = new Map();
  const leafPositions = new Map();
  const startY = 60;
  
  const slotCountCache = new Map();
  function countSlots(clade) {
    if (slotCountCache.has(clade)) return slotCountCache.get(clade);
    const nd = nodes.get(clade);
    if (!nd) return 1;
    
    const dl = (visibleLeavesByParent.get(clade) || []).length;
    let cs = 0;
    nd.children.forEach(ch => { cs += countSlots(ch); });
    const total = Math.max(1, dl + cs);
    slotCountCache.set(clade, total);
    return total;
  }
  
  const totalSlots = countSlots('Dinosauria');
  const dynH = Math.max(nodeWidth + 25, 250 - Math.max(0, (totalSlots - 3) * 8));
  const dynV = Math.max(80, Math.floor(120 * Math.max(0.7, 1 - (totalSlots - 15) * 0.02)));
  
  function positionNode(clade, left, right, depth) {
    const nd = nodes.get(clade);
    if (!nd) return;
    
    const y = startY + depth * dynV;
    const intChildren = nd.children.slice().sort((a, b) => 
      nodes.get(a).lineageIndex - nodes.get(b).lineageIndex
    );
    const dirLeaves = visibleLeavesByParent.get(clade) || [];
    
    const slots =[];
    intChildren.forEach(ch => slots.push({ 
      type: 'node', 
      name: ch, 
      slotCount: countSlots(ch) 
    }));
    dirLeaves.forEach(lf => slots.push({ 
      type: 'leaf', 
      name: lf.name, 
      slotCount: 1, 
      leaf: lf 
    }));
    
    const tot = slots.reduce((s, sl) => s + sl.slotCount, 0);
    const w = right - left;
    
    nodePositions.set(clade, { x: left + w / 2, y });
    
    let cur = left;
    slots.forEach(sl => {
      const sw = (sl.slotCount / tot) * w;
      if (sl.type === 'node') {
        positionNode(sl.name, cur, cur + sw, depth + 1);
      } else {
        leafPositions.set(sl.name, { 
          x: cur + sw / 2, 
          y: startY + (depth + 1) * dynV, 
          ...sl.leaf 
        });
      }
      cur += sw;
    });
  }
  
  positionNode('Dinosauria', 0, totalSlots * dynH, 0);
  const allX =[
    ...Array.from(leafPositions.values()).map(p => p.x), 
    ...Array.from(nodePositions.values()).map(p => p.x)
  ];
  const allY =[
    ...Array.from(leafPositions.values()).map(p => p.y), 
    ...Array.from(nodePositions.values()).map(p => p.y)
  ];
  
  if (!allX.length || !allY.length) {
    wrapper.innerHTML = '<div class="empty-state">No tree data.</div>';
    return;
  }
  
  const minX = Math.min(...allX);
  const maxX = Math.max(...allX);
  const maxY = Math.max(...allY);
  const pad = 115;
  const svgW = maxX - minX + pad * 2;
  const svgH = maxY + pad;
  
  let svg = wrapper.querySelector('#tree-svg');
  if (!svg) {
    wrapper.innerHTML = '<svg id="tree-svg"></svg>';
    svg = wrapper.querySelector('#tree-svg');
  }
  
  svg.setAttribute('viewBox', `${minX - pad} 0 ${svgW} ${svgH}`);
  svg.setAttribute('preserveAspectRatio', 'xMidYMin meet');
  svg.innerHTML = '';
  
  nodes.forEach((data, clade) => {
    const pos = nodePositions.get(clade);
    data.children.forEach(child => {
      const cp = nodePositions.get(child);
      if (!cp) return;
      
      const midY = (pos.y + nodeHeight / 2 + cp.y - nodeHeight / 2) / 2;
      const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
      const edgeKey = `edge:${clade}->${child}`;
      path.setAttribute('d', `M ${pos.x} ${pos.y + nodeHeight / 2} C ${pos.x} ${midY}, ${cp.x} ${midY}, ${cp.x} ${cp.y - nodeHeight / 2}`);
      path.setAttribute('class', [
        'tree-line',
        'tree-line-revealed',
        bestPathEdgeKeys.has(edgeKey) ? 'tree-line-best' : '',
        newEdgeKeys.has(edgeKey) ? 'new-line' : '',
        victoryEdgeKeys.has(edgeKey) ? 'tree-victory-path' : ''
      ].filter(Boolean).join(' '));
      path.setAttribute('stroke', 'var(--color-accent)'); 
      path.setAttribute('stroke-width', '3');
      path.setAttribute('fill', 'none');
      svg.appendChild(path);
    });
  });
  
nodes.forEach((data, clade) => {
    const pos = nodePositions.get(clade);
    const g = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const nodeKey = `node:${clade}`;
    const isNewNode = newNodeKeys.has(nodeKey);
    const isFocusedNode = focusKey === nodeKey;
    const isHintArrival = animationMode === 'hint' && data.isHinted === true && isFocusedNode;
    g.dataset.treeKey = nodeKey;
    g.dataset.treeDisplay = clade;
    g.setAttribute('class', [
      'tree-node-group',
      bestPathNodeKeys.has(nodeKey) ? 'tree-node-best' : '',
      isNewNode ? 'new-node' : '',
      isNewNode || isFocusedNode ? 'tree-new-focus' : '',
      isFocusedNode ? 'tree-primary-focus' : '',
      isHintArrival ? 'tree-hint-arrival' : '',
      victoryNodeKeys.has(nodeKey) ? 'tree-victory-node' : ''
    ].filter(Boolean).join(' '));
    g.style.cursor = 'pointer';
    g.onclick = () => showCladeInfo(clade);
    
    const rect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
    rect.setAttribute('x', pos.x - nodeWidth / 2);
    rect.setAttribute('y', pos.y - nodeHeight / 2);
    rect.setAttribute('width', nodeWidth);
    rect.setAttribute('height', nodeHeight);
    rect.setAttribute('rx', '6');
    const isHinted = data.isHinted === true
      || (typeof hintHistory !== 'undefined'
        && Array.isArray(hintHistory)
        && hintHistory.some(h => h.cladeName === clade));
    
    rect.setAttribute('fill', isHinted ? 'var(--tree-hint-bg)' : 'var(--tree-ancestor-bg)');
    rect.setAttribute('stroke', isHinted ? 'var(--tree-hint-border)' : 'var(--tree-ancestor-border)');
    rect.setAttribute('stroke-width', '2.5');
    g.appendChild(rect);
    
    const label = document.createElementNS('http://www.w3.org/2000/svg', 'text');
    label.setAttribute('x', clade === 'Dinosauria' ? pos.x : pos.x - 12);
    label.setAttribute('y', pos.y);
    label.setAttribute('class', 'tree-node-label tree-node-label-ancestor');
    label.setAttribute('text-anchor', 'middle');
    label.setAttribute('dominant-baseline', 'middle');
    label.setAttribute('font-weight', '600');
    label.textContent = clade;

    const maxTextWidth = clade === 'Dinosauria' ? 185 : 165; 
    const fontSize = Math.max(11, Math.min(15, Math.floor(maxTextWidth / (clade.length * 0.52))));
    
    if (fontSize < 15) {
      label.setAttribute('style', `font-size: ${fontSize}px !important;`);
    } else {
      label.setAttribute('font-size', '15px');
    }
    g.appendChild(label);

    if (clade !== 'Dinosauria') {
      const toggleG = document.createElementNS('http://www.w3.org/2000/svg', 'g');
      toggleG.style.cursor = 'pointer';
      toggleG.onclick = (e) => {
        e.stopPropagation();
        toggleCladeCollapse(clade);
      };
      
      const line = document.createElementNS('http://www.w3.org/2000/svg', 'line');
      line.setAttribute('x1', pos.x + nodeWidth / 2 - 25);
      line.setAttribute('y1', pos.y - nodeHeight / 2);
      line.setAttribute('x2', pos.x + nodeWidth / 2 - 25);
      line.setAttribute('y2', pos.y + nodeHeight / 2);
      line.setAttribute('stroke', isHinted ? 'var(--tree-hint-border)' : 'var(--tree-ancestor-border)');
      line.setAttribute('stroke-width', '1.5');
      toggleG.appendChild(line);

      const sym = document.createElementNS('http://www.w3.org/2000/svg', 'text');
      sym.setAttribute('x', pos.x + nodeWidth / 2 - 12);
      sym.setAttribute('y', pos.y);
      sym.setAttribute('text-anchor', 'middle');
      sym.setAttribute('dominant-baseline', 'middle');
      sym.setAttribute('font-size', '14px');
      sym.setAttribute('font-weight', 'bold');
      sym.setAttribute('fill', '#fff');
      sym.textContent = window.collapsedClades.has(clade) ? '+' : '−';
      toggleG.appendChild(sym);
      
      g.appendChild(toggleG);
    }

    svg.appendChild(g);
  });
  
  leafPositions.forEach((leaf, name) => {
    const pp = nodePositions.get(leaf.parentNode);
    if (!pp) return;
    
    const midY = (pp.y + nodeHeight / 2 + leaf.y - nodeHeight / 2) / 2;
    const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
    const edgeKey = `edge:${leaf.parentNode}->${name}`;
    const lineType = leaf.isTarget
      ? 'tree-line-revealed'
      : (leaf.isHint ? 'tree-line-hint' : 'tree-line-guess');
    path.setAttribute('d', `M ${pp.x} ${pp.y + nodeHeight / 2} C ${pp.x} ${midY}, ${leaf.x} ${midY}, ${leaf.x} ${leaf.y - nodeHeight / 2}`);
    path.setAttribute('class', [
      'tree-line',
      lineType,
      bestPathEdgeKeys.has(edgeKey) ? 'tree-line-best' : '',
      newEdgeKeys.has(edgeKey) ? 'new-line' : '',
      victoryEdgeKeys.has(edgeKey) ? 'tree-victory-path' : ''
    ].filter(Boolean).join(' '));
    path.setAttribute('stroke-width', leaf.isTarget ? '3' : '2');
    path.setAttribute('fill', 'none');
    svg.appendChild(path);
  });
  
leafPositions.forEach((leaf, name) => {
    const g = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const nodeKey = `leaf:${name}`;
    const isNewNode = newNodeKeys.has(nodeKey);
    const isFocusedNode = focusKey === nodeKey || focusKey === `display:${leaf.displayName}`;
    g.dataset.treeKey = nodeKey;
    g.dataset.treeDisplay = leaf.displayName;
    g.setAttribute('class', [
      'tree-leaf-group',
      bestPathNodeKeys.has(nodeKey) ? 'tree-best-destination' : '',
      isNewNode ? 'new-node' : '',
      isNewNode || isFocusedNode ? 'tree-new-focus' : '',
      isFocusedNode ? 'tree-primary-focus' : '',
      animationMode === 'hint' && leaf.isHint && isFocusedNode ? 'tree-hint-arrival' : '',
      victoryNodeKeys.has(nodeKey) ? 'tree-victory-node' : '',
      animationMode === 'reveal' && leaf.isTarget ? 'tree-reveal-target' : ''
    ].filter(Boolean).join(' '));
    g.style.cursor = 'pointer';
    g.onclick = () => {
      if (leaf.displayName === '?') return;
      showCladeInfo(leaf.displayName);
    };
    
    const rect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
    rect.setAttribute('x', leaf.x - nodeWidth / 2);
    rect.setAttribute('y', leaf.y - nodeHeight / 2);
    rect.setAttribute('width', nodeWidth);
    rect.setAttribute('height', nodeHeight);
    rect.setAttribute('rx', '6');
    
    if (leaf.isTarget && !leaf.isGiveUp) {
        rect.setAttribute('fill', 'var(--tree-target-bg)');
        rect.setAttribute('stroke', 'var(--tree-target-border)');
        rect.setAttribute('stroke-width', '3');
    } else if (leaf.isTarget && leaf.isGiveUp) {
        rect.setAttribute('fill', 'var(--tree-gaveup-bg)');
        rect.setAttribute('stroke', 'var(--tree-gaveup-border)');
        rect.setAttribute('stroke-width', '3');
    } else if (leaf.isHint) {
        rect.setAttribute('fill', 'var(--tree-leaf-hint-bg)');
        rect.setAttribute('stroke', 'var(--tree-leaf-hint-border)');
        rect.setAttribute('stroke-width', '2');
        rect.setAttribute('stroke-dasharray', '3,3');
    } else {
        rect.setAttribute('fill', 'var(--tree-leaf-bg)');
        rect.setAttribute('stroke', 'var(--tree-leaf-border)');
        rect.setAttribute('stroke-width', '2');
        rect.setAttribute('stroke-dasharray', '5,5');
    }
    
    g.appendChild(rect);
    
    const label = document.createElementNS('http://www.w3.org/2000/svg', 'text');
    label.setAttribute('x', leaf.x);
    label.setAttribute('y', leaf.y);
    label.setAttribute('class', leaf.isTarget 
      ? 'tree-node-label tree-node-label-revealed' 
      : (leaf.isHint ? 'tree-node-label tree-node-label-hint' : 'tree-node-label tree-node-label-guess')
    );
    label.setAttribute('text-anchor', 'middle');
    label.setAttribute('dominant-baseline', 'middle');
    label.setAttribute('font-weight', leaf.isTarget ? '700' : '500');
    label.setAttribute('font-style', 'italic'); 
    label.textContent = leaf.displayName;

    const maxLeafWidth = 150;
    const defaultSize = leaf.isTarget ? 20 : 15;
    const leafFontSize = Math.max(11, Math.min(defaultSize, Math.floor(maxLeafWidth / (leaf.displayName.length * 0.52))));

    if (leafFontSize < defaultSize) {
      label.setAttribute('style', `font-size: ${leafFontSize}px !important;`);
    } else {
      label.setAttribute('font-size', defaultSize + 'px');
    }

    g.appendChild(label);
    svg.appendChild(g);
  });
  
  requestAnimationFrame(() => {
    const svgB = svg.getBBox();
    const padding = 80;
    const naturalW = svgB.width + padding * 2;
    const naturalH = svgB.height + padding;
    const availW = container.getBoundingClientRect().width - 40;
    const shouldScale = naturalW <= availW * 1.2;

    if (shouldScale) {
      const scale = Math.min(1, availW / naturalW);
      svg.setAttribute('width', naturalW);
      svg.setAttribute('height', naturalH);
      wrapper.style.transform = `scale(${scale})`;
      wrapper.style.transformOrigin = 'top center';
      wrapper.style.width = '100%';
      wrapper.style.height = (naturalH * scale) + 'px';
      container.style.overflowX = 'hidden';
    } else {
      svg.setAttribute('width', naturalW);
      svg.setAttribute('height', naturalH);
      wrapper.style.transform = 'none';
      wrapper.style.width = naturalW + 'px';
      wrapper.style.height = naturalH + 'px';
      container.style.overflowX = 'auto';
    }

    container.scrollLeft = savedScrollLeft;
    container.scrollTop = savedScrollTop;

    if (animationMode === 'guess' || animationMode === 'hint') {
      const focusTargets = svg.querySelectorAll('.tree-new-focus');
      const focusTarget = svg.querySelector('.tree-primary-focus')
        || focusTargets[focusTargets.length - 1];

      if (focusTarget) centerTreeElement(focusTarget);
    }
  });

  initTreePanning();
}
