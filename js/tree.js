// ═══════════════════════════════════════════════
// TREE AND PHYLOGENY LOGIC 
// ═══════════════════════════════════════════════
function findDinosauriaIndex(lineage) {
    return lineage.findIndex(c => c === 'Dinosauria');
}

function findMRCA(lineage1, lineage2) {
    const minLen = Math.min(lineage1.length, lineage2.length);
    let mrcaIndex = -1;
    
    for (let i = 0; i < minLen; i++) {
    if (lineage1[i] === lineage2[i]) {
        mrcaIndex = i;
    } else {
        break;
    }
    }
    
    return mrcaIndex >= 0 ? { clade: lineage1[mrcaIndex], index: mrcaIndex } : null;
}

function initTreePanning() {
    const container = document.getElementById('tree-container');
    if (!container) return;

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

    container.addEventListener('touchstart', e => {
    if (e.touches.length === 1) {
        startX = e.touches[0].pageX;
        startY = e.touches[0].pageY;
        scrollLeft = container.scrollLeft;
        scrollTop  = container.scrollTop;
    }
    }, { passive: true });

    container.addEventListener('touchmove', e => {
    if (e.touches.length !== 1) return;
    const x = e.touches[0].pageX;
    const y = e.touches[0].pageY;
    container.scrollLeft = scrollLeft - (x - startX);
    container.scrollTop  = scrollTop  - (y - startY);
    }, { passive: true });
}

function renderEnhancedTree() {
    const container = document.getElementById('tree-container');
    const wrapper   = document.getElementById('tree-scroll-wrapper');
    
    const nodeWidth  = 160;
    const nodeHeight = 45;
    const targetLineage = targetDino.linhagem;
    const dinoIndex     = findDinosauriaIndex(targetLineage);
    
    let deepestMRCA      = 'Dinosauria';
    let deepestMRCAIndex = dinoIndex;
    
    const guessData = guesses.map(guess => {
    const mrca = findMRCA(guess.dino.linhagem, targetLineage);
    return { 
        guess, 
        mrcaClade: mrca ? mrca.clade : 'Dinosauria', 
        mrcaIndex: mrca ? mrca.index : dinoIndex 
    };
    });
    
    guessData.forEach(d => {
    if (d.mrcaIndex > deepestMRCAIndex) {
        deepestMRCA = d.mrcaClade;
        deepestMRCAIndex = d.mrcaIndex;
    }
    });
    
    const guessGroupingClades = new Set();
    for (let i = 0; i < guesses.length; i++) {
    for (let j = i + 1; j < guesses.length; j++) {
        const mrca = findMRCA(guesses[i].dino.linhagem, guesses[j].dino.linhagem);
        if (mrca && mrca.clade !== 'Dinosauria') {
        guessGroupingClades.add(mrca.clade);
        }
    }
    }
    
    const showClades = new Set(['Dinosauria']);
    revealedClades.forEach(c => {
    if (targetLineage.includes(c)) {
        showClades.add(c);
    }
    });

    if (guesses.length > 0) {
    if (deepestMRCA !== 'Dinosauria') showClades.add(deepestMRCA);
    guessGroupingClades.forEach(c => showClades.add(c));
    }

    let changed = true;
    while (changed) {
    changed = false;
    const cur = Array.from(showClades);
    
    for (let i = 0; i < cur.length; i++) {
        for (let j = i + 1; j < cur.length; j++) {
        if (cur[i] === 'Dinosauria' || cur[j] === 'Dinosauria') continue;
        
        let lin1 = targetLineage.includes(cur[i]) 
            ? targetLineage 
            : guesses.find(g => g.dino.linhagem.includes(cur[i]))?.dino.linhagem;
        
        let lin2 = targetLineage.includes(cur[j]) 
            ? targetLineage 
            : guesses.find(g => g.dino.linhagem.includes(cur[j]))?.dino.linhagem;
        
        if (!lin1 || !lin2) continue;
        
        const idx1 = lin1.indexOf(cur[i]);
        const idx2 = lin2.indexOf(cur[j]);
        
        if ((lin2.includes(cur[i]) && lin2.indexOf(cur[i]) < idx2) ||
            (lin1.includes(cur[j]) && lin1.indexOf(cur[j]) < idx1)) {
            continue;
        }
        
        const mrca = findMRCA(lin1, lin2);
        if (mrca && mrca.clade !== 'Dinosauria' && !showClades.has(mrca.clade)) {
            showClades.add(mrca.clade);
            changed = true;
        }
        }
    }
    }
    
    const nodes = new Map();
    nodes.set('Dinosauria', { 
    depth: 0, 
    children: [], 
    type: 'root', 
    lineageIndex: dinoIndex 
    });
    
    const cladeList = Array.from(showClades)
    .filter(c => c !== 'Dinosauria')
    .sort((a, b) => {
        let aI = -1, bI = -1;
        
        if (targetLineage.includes(a)) {
        aI = targetLineage.indexOf(a);
        } else {
        for (const g of guesses) {
            if (g.dino.linhagem.includes(a)) {
            aI = g.dino.linhagem.indexOf(a);
            break;
            }
        }
        }
        
        if (targetLineage.includes(b)) {
        bI = targetLineage.indexOf(b);
        } else {
        for (const g of guesses) {
            if (g.dino.linhagem.includes(b)) {
            bI = g.dino.linhagem.indexOf(b);
            break;
            }
        }
        }
        
        return aI - bI;
    });
    
    cladeList.forEach(clade => {
    let cladeLineage = null;
    let cladeIndex = -1;
    
    if (targetLineage.includes(clade)) {
        cladeLineage = targetLineage;
        cladeIndex = targetLineage.indexOf(clade);
    } else {
        for (const g of guesses) {
        if (g.dino.linhagem.includes(clade)) {
            cladeLineage = g.dino.linhagem;
            cladeIndex = g.dino.linhagem.indexOf(clade);
            break;
        }
        }
    }
    
    if (!cladeLineage) return;
    
    let parent = 'Dinosauria';
    for (let i = cladeIndex - 1; i >= dinoIndex; i--) {
        if (showClades.has(cladeLineage[i])) {
        parent = cladeLineage[i];
        break;
        }
    }
    
    if (!nodes.has(parent)) {
        console.warn('Parent not found:', parent);
        return;
    }
    
    nodes.set(clade, { 
        depth: 0, 
        children: [], 
        type: 'internal', 
        lineageIndex: cladeIndex 
    });
    
    if (!nodes.get(parent).children.includes(clade)) {
        nodes.get(parent).children.push(clade);
    }
    });
    
    (function recalcDepths(c, d) {
    nodes.get(c).depth = d;
    nodes.get(c).children.forEach(ch => recalcDepths(ch, d + 1));
    })('Dinosauria', 0);
    
    const leaves = [];
    guesses.forEach((guess, idx) => {
    let attachTo = 'Dinosauria';
    for (let i = guess.dino.linhagem.length - 1; i >= 0; i--) {
        if (showClades.has(guess.dino.linhagem[i])) {
        attachTo = guess.dino.linhagem[i];
        break;
        }
    }

    const isWinningGuess = gameWon && !isGiveUpMode && guess.dino.nome === targetDino.nome;

    leaves.push({ 
        name: guess.dino.nome + '__leaf_' + idx, 
        displayName: guess.dino.nome, 
        parentNode: attachTo, 
        isTarget: isWinningGuess,
        isGiveUp: false,
        isHint: guess.isHint, 
        lineage: guess.dino.linhagem 
    });
});

    if (!gameWon || isGiveUpMode) {
    let mysteryParent = 'Dinosauria';
    let maxDepthRevealed = -1;

    if (deepestMRCA !== 'Dinosauria') {
        const deepestMRCAIndex = targetLineage.indexOf(deepestMRCA);
        if (deepestMRCAIndex > maxDepthRevealed) {
        maxDepthRevealed = deepestMRCAIndex;
        mysteryParent = deepestMRCA;
        }
    }

    revealedClades.forEach(clade => {
        if (targetLineage.includes(clade)) {
        const cladeIndex = targetLineage.indexOf(clade);
        if (cladeIndex > maxDepthRevealed) {
            maxDepthRevealed = cladeIndex;
            mysteryParent = clade;
        }
        }
    });

    leaves.push({ 
        name: '?__leaf_target', 
        displayName: isGiveUpMode ? targetDino.nome : '?', 
        parentNode: mysteryParent, 
        isTarget: true,
        isGiveUp: isGiveUpMode,
        isHint: false, 
        lineage: targetLineage 
    });
    }
    
    const nodePositions = new Map();
    const leafPositions = new Map();
    const startY = 60;
    
    function countSlots(clade) {
    const nd = nodes.get(clade);
    if (!nd) return 1;
    
    const dl = leaves.filter(l => l.parentNode === clade).length;
    let cs = 0;
    nd.children.forEach(ch => { cs += countSlots(ch); });
    
    return Math.max(1, dl + cs);
    }
    
    const totalSlots = countSlots('Dinosauria');
    const dynH = Math.max(100, 200 - Math.max(0, (totalSlots - 3) * 8));
    const maxDepth = Math.max(...Array.from(nodes.values()).map(n => n.depth));
    const dynV = Math.max(80, Math.floor(120 * Math.max(0.7, 1 - (totalSlots - 15) * 0.02)));
    
    function positionNode(clade, left, right, depth) {
    const nd = nodes.get(clade);
    if (!nd) return;
    
    const y = startY + depth * dynV;
    const intChildren = nd.children.slice().sort((a, b) => 
        nodes.get(a).lineageIndex - nodes.get(b).lineageIndex
    );
    const dirLeaves = leaves.filter(l => l.parentNode === clade);
    
    const slots = [];
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
    const allX = [
    ...Array.from(leafPositions.values()).map(p => p.x), 
    ...Array.from(nodePositions.values()).map(p => p.x)
    ];
    const allY = [
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
    const pad = 80;
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
        path.setAttribute('d', `M ${pos.x} ${pos.y + nodeHeight / 2} C ${pos.x} ${midY}, ${cp.x} ${midY}, ${cp.x} ${cp.y - nodeHeight / 2}`);
        path.setAttribute('class', 'tree-line tree-line-revealed new-line');
        path.setAttribute('stroke', '#c9a96e');
        path.setAttribute('stroke-width', '3');
        path.setAttribute('fill', 'none');
        svg.appendChild(path);
    });
    });
    
    nodes.forEach((data, clade) => {
    const pos = nodePositions.get(clade);
    const g = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    g.setAttribute('class', 'tree-node-group new-node');
    g.style.cursor = 'pointer';
    g.onclick = () => showCladeInfo(clade);
    
    const rect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
    rect.setAttribute('x', pos.x - nodeWidth / 2);
    rect.setAttribute('y', pos.y - nodeHeight / 2);
    rect.setAttribute('width', nodeWidth);
    rect.setAttribute('height', nodeHeight);
    rect.setAttribute('rx', '6');
    const isHinted = hintHistory.some(h => h.cladeName === clade);
    rect.setAttribute('fill', isHinted ? '#6b4a8b' : '#8b4726');
    rect.setAttribute('stroke', isHinted ? '#a67cc9' : '#a6623d');
    rect.setAttribute('stroke', '#a6623d');
    rect.setAttribute('stroke-width', '2.5');
    g.appendChild(rect);
    
    const label = document.createElementNS('http://www.w3.org/2000/svg', 'text');
    label.setAttribute('x', pos.x);
    label.setAttribute('y', pos.y);
    label.setAttribute('class', 'tree-node-label tree-node-label-ancestor');
    label.setAttribute('text-anchor', 'middle');
    label.setAttribute('dominant-baseline', 'middle');
    label.setAttribute('font-size', '15px');
    label.setAttribute('font-weight', '600');
    label.textContent = clade;
    g.appendChild(label);
    svg.appendChild(g);
    });
    
    leafPositions.forEach((leaf, name) => {
    const pp = nodePositions.get(leaf.parentNode);
    if (!pp) return;
    
    const midY = (pp.y + nodeHeight / 2 + leaf.y - nodeHeight / 2) / 2;
    const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
    path.setAttribute('d', `M ${pp.x} ${pp.y + nodeHeight / 2} C ${pp.x} ${midY}, ${leaf.x} ${midY}, ${leaf.x} ${leaf.y - nodeHeight / 2}`);
    path.setAttribute('class', leaf.isTarget 
        ? 'tree-line tree-line-revealed new-line' 
        : (leaf.isHint ? 'tree-line tree-line-hint new-line' : 'tree-line tree-line-guess new-line')
    );
    path.setAttribute('stroke-width', leaf.isTarget ? '3' : '2');
    path.setAttribute('fill', 'none');
    svg.appendChild(path);
    });
    
    leafPositions.forEach((leaf, name) => {
    const g = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    g.setAttribute('class', 'new-node');
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
    rect.setAttribute('fill', '#4a7c59');
    rect.setAttribute('stroke', '#5a9c6b');
    rect.setAttribute('stroke-width', '3');
    } else if (leaf.isTarget && leaf.isGiveUp) {
    rect.setAttribute('fill', '#8b3a2a');
    rect.setAttribute('stroke', '#c06040');
    rect.setAttribute('stroke-width', '3');
    } else if (leaf.isHint) {
    rect.setAttribute('fill', 'rgba(166,138,90,0.3)');
    rect.setAttribute('stroke', '#a68a5a');
    rect.setAttribute('stroke-width', '2');
    rect.setAttribute('stroke-dasharray', '3,3');
    } else {
    rect.setAttribute('fill', 'rgba(255,255,255,0.1)');
    rect.setAttribute('stroke', '#8b6f47');
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
    label.setAttribute('font-size', leaf.isTarget ? '20px' : '15px');
    label.setAttribute('font-weight', leaf.isTarget ? '700' : '500');
    label.setAttribute('font-style', 'italic'); 
    label.textContent = leaf.displayName;
    g.appendChild(label);
    svg.appendChild(g);
    });
    
    requestAnimationFrame(() => {
    const svgB = svg.getBBox();
    const padding = 80;
    const naturalW = svgB.width + padding * 2;
    const naturalH = svgB.height + padding;
    const availW = container.getBoundingClientRect().width - 40;
    const isMobile = window.innerWidth <= 768;
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
    });

    initTreePanning();
}