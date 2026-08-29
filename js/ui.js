// ═══════════════════════════════════════════════
// UI AND MODALS
// ═══════════════════════════════════════════════
function setHeaderControls(screen) {
    const controls = document.getElementById('header-controls');
    if (!controls) return;

    const statsBtn = currentUser 
      ? `<button class="btn-hint btn-header" onclick="showStatsDashboard()">Stats</button>` 
      : '';

    const analyticsBtn = isAnalyticsAdmin
      ? `<button class="btn-hint btn-header" onclick="showAnalyticsDashboard()">Analytics</button>`
      : '';

    const logoutBtn = currentUser 
      ? `<button class="btn-hint btn-header btn-account" onclick="logout()" title="Sign out: ${currentUser}">${currentUser}</button>` 
      : '';

    const backBtn = `<button class="btn-hint btn-header btn-with-icon" onclick="navigateToAppRoute('/')"><i class="ui-icon ui-icon-arrow-left" aria-hidden="true"></i><span>Levels</span></button>`;

const map = {
      'login':        '',
      'difficulty': `<button class="btn-hint btn-header" onclick="showMuseum()">Museum</button>` + analyticsBtn + (currentUser ? statsBtn + logoutBtn : `<button class="btn-hint btn-header" onclick="showLoginModal()">Sign In</button>`),
      'game': backBtn + (currentUser ? statsBtn : `<button class="btn-hint btn-header" onclick="showLoginModal()">Sign In</button>`),
      'stats':        `<button class="btn-hint btn-header btn-with-icon" onclick="navigateBackOrHome('/')"><i class="ui-icon ui-icon-arrow-left" aria-hidden="true"></i><span>Back</span></button>`,
      'museum':       `<button class="btn-hint btn-header btn-with-icon" onclick="navigateBackOrHome('/')"><i class="ui-icon ui-icon-arrow-left" aria-hidden="true"></i><span>Back</span></button>`,
      'about':        `<button class="btn-hint btn-header btn-with-icon" onclick="navigateBackOrHome('/')"><i class="ui-icon ui-icon-arrow-left" aria-hidden="true"></i><span>Back</span></button>`,
      'practice-menu':`<button class="btn-hint btn-header btn-with-icon" onclick="navigateToAppRoute('/')"><i class="ui-icon ui-icon-arrow-left" aria-hidden="true"></i><span>Back</span></button>`,
      'practice':     `<button class="btn-hint btn-header btn-with-icon" onclick="navigateToAppRoute('/')"><i class="ui-icon ui-icon-arrow-left" aria-hidden="true"></i><span>Back</span></button>`,
      'friends':      `<button class="btn-hint btn-header btn-with-icon" onclick="navigateBackOrHome('/')"><i class="ui-icon ui-icon-arrow-left" aria-hidden="true"></i><span>Back</span></button>`,
      'challenge':    `<button class="btn-hint btn-header btn-with-icon" onclick="navigateBackOrHome('/friends')"><i class="ui-icon ui-icon-arrow-left" aria-hidden="true"></i><span>Friends</span></button>`,
      'analytics':    `<button class="btn-hint btn-header btn-with-icon" onclick="navigateBackOrHome('/')"><i class="ui-icon ui-icon-arrow-left" aria-hidden="true"></i><span>Back</span></button>`,
    };

    controls.innerHTML = map[screen] || '';
}

function toggleTheme() {
        currentTheme = currentTheme === 'dark' ? 'light' : 'dark';
        document.body.classList.toggle('light-mode');
        localStorage.setItem('phylosaur-theme', currentTheme);
        updateThemeToggleState();
    }

function updateThemeToggleState() {
    const toggle = document.getElementById('theme-toggle');
    if (!toggle) return;

    const isLight = currentTheme === 'light';
    toggle.setAttribute('aria-pressed', String(isLight));
    toggle.setAttribute('aria-label', isLight ? 'Switch to dark mode' : 'Switch to light mode');
}

function showModal(options) {
    return new Promise((resolve) => {
    const overlay = document.createElement('div');
    overlay.className = 'modal-overlay';
    overlay.dataset.appModal = 'true';
    overlay.setAttribute('role', 'dialog');
    overlay.setAttribute('aria-modal', 'true');
    overlay.setAttribute('aria-label', options.title || 'Dialog');
    
    const box = document.createElement('div');
    box.className = 'modal-box';
    box.tabIndex = -1;

    const previouslyFocused = document.activeElement;
    const previousBodyOverflow = document.body.style.overflow;
    
    let html = '';
    
    if (options.title) {
        html += `<div class="modal-title">${options.title}</div>`;
    }
    
    if (options.message) {
        html += `<div class="modal-message">${options.message}</div>`;
    }
    
    if (options.info) {
        html += '<div class="modal-info">';
        options.info.forEach(item => {
        html += `
            <div class="modal-info-item">
            <span class="modal-info-label">${item.label}:</span>
            <span>${item.value}</span>
            </div>
        `;
        });
        html += '</div>';
    }
    
    html += '<div class="modal-buttons">';
    
    if (options.buttons) {
        options.buttons.forEach((btn, index) => {
        const btnClass = btn.primary ? 'modal-btn-primary' : 'modal-btn-secondary';
        html += `<button class="modal-btn ${btnClass}" data-result="${btn.value}">${btn.text}</button>`;
        });
    } else {
        html += '<button class="modal-btn modal-btn-primary" data-result="ok">OK</button>';
    }
    
    html += '</div>';
    
    box.innerHTML = html;
    overlay.appendChild(box);
    document.body.appendChild(overlay);
    document.body.style.overflow = 'hidden';

    let closed = false;
    let keyHandlerAttached = false;

    const closeModal = result => {
        if (closed) return;
        closed = true;
        clearTimeout(keyHandlerTimer);
        if (keyHandlerAttached) {
            document.removeEventListener('keydown', modalKeyHandler, true);
        }
        overlay.remove();
        document.body.style.overflow = previousBodyOverflow;
        if (previouslyFocused instanceof HTMLElement && previouslyFocused.isConnected) {
            previouslyFocused.focus();
        }
        resolve(result);
    };

    const modalKeyHandler = event => {
        const buttons = box.querySelectorAll('.modal-btn');

        if (event.key === 'Tab') {
            const focusable = Array.from(box.querySelectorAll(
                'button:not([disabled]), a[href], input:not([disabled]), select:not([disabled]), textarea:not([disabled]), [tabindex]:not([tabindex="-1"])'
            ));

            if (!focusable.length) {
                event.preventDefault();
                box.focus();
                return;
            }

            const first = focusable[0];
            const last = focusable[focusable.length - 1];
            if (event.shiftKey && document.activeElement === first) {
                event.preventDefault();
                last.focus();
            } else if (!event.shiftKey && document.activeElement === last) {
                event.preventDefault();
                first.focus();
            }
        } else if (event.key === 'Enter' && buttons.length === 1) {
            event.preventDefault();
            event.stopPropagation();
            closeModal(buttons[0].getAttribute('data-result'));
        } else if (event.key === 'Escape' && options.closeOnOverlay !== false) {
            event.preventDefault();
            event.stopPropagation();
            closeModal(null);
        }
    };

    // Attach after the event that opened the modal has finished propagating.
    const keyHandlerTimer = setTimeout(() => {
        if (closed) return;
        document.addEventListener('keydown', modalKeyHandler, true);
        keyHandlerAttached = true;
        const firstButton = box.querySelector('.modal-btn');
        if (firstButton) firstButton.focus();
        else box.focus();
    }, 0);
    
    box.querySelectorAll('.modal-btn').forEach(btn => {
        btn.addEventListener('click', () => {
        closeModal(btn.getAttribute('data-result'));
        });
    });
    
    overlay.addEventListener('click', (e) => {
        if (e.target === overlay && options.closeOnOverlay !== false) {
        closeModal(null);
        }
    });
    });
}

function customAlert(title, message) {
    return showModal({
    title: title,
    message: message,
    buttons: [{ text: 'OK', value: 'ok', primary: true }]
    });
}

function customConfirm(title, message, yesText = 'Yes', noText = 'No') {
    return showModal({
    title: title,
    message: message,
    buttons: [
        { text: yesText, value: true, primary: true },
        { text: noText, value: false, primary: false }
    ]
    });
}

function openImageLightbox(url, name, sourcePage = '', creditHtml = '') {
    const previouslyFocused = document.activeElement;
    const previousBodyOverflow = document.body.style.overflow;
    const overlay = document.createElement('div');
    overlay.setAttribute('role', 'dialog');
    overlay.setAttribute('aria-modal', 'true');
    overlay.setAttribute('aria-label', `${name} image viewer`);
    overlay.tabIndex = -1;
    overlay.style.cssText = `
    position:fixed; top:0; left:0; right:0; bottom:0;
    background:rgba(0,0,0,0.92); z-index:99999;
    display:flex; flex-direction:column;
    align-items:center; justify-content:center;
    cursor:zoom-out; animation:fadeIn 0.2s ease;
    padding:20px;
    `;
    
    const fallbackCredit = `
        ${sourcePage ? `Image source: <a href="${sourcePage}" target="_blank"
            style="color:var(--color-muted); text-decoration:none;">Wikimedia Commons</a>` : 'Image source: Wikimedia Commons'}
    `;

    overlay.innerHTML = `
    <img src="${url}" alt="${name}"
        style="max-width:90vw; max-height:min(80vh, calc(100dvh - 150px)); border-radius:8px;
                border:2px solid var(--border-subtle); box-shadow:0 8px 40px rgba(0,0,0,0.8);" />
    <div style="margin-top:16px; font-family:Georgia,serif; font-style:italic;
                color:var(--color-accent); font-size:1.1em; letter-spacing:1px;">${name}</div>
    <div style="margin-top:8px; font-size:0.82em; color:var(--border-subtle); letter-spacing:1px;">
        ${creditHtml || fallbackCredit}
    </div>
    <div style="margin-top:16px; color:var(--border-subtle); font-size:0.8em; letter-spacing:2px;">
        CLICK ANYWHERE TO CLOSE
    </div>
    `;
    
    overlay.querySelectorAll('a').forEach(link => {
        link.addEventListener('click', event => event.stopPropagation());
    });

    const closeLightbox = () => {
        document.removeEventListener('keydown', lightboxKeyHandler, true);
        overlay.remove();
        document.body.style.overflow = previousBodyOverflow;
        if (previouslyFocused instanceof HTMLElement && previouslyFocused.isConnected) {
            previouslyFocused.focus();
        }
    };

    const lightboxKeyHandler = event => {
        if (event.key !== 'Escape') return;
        event.preventDefault();
        event.stopPropagation();
        closeLightbox();
    };

    overlay.onclick = closeLightbox;
    document.body.appendChild(overlay);
    document.body.style.overflow = 'hidden';
    document.addEventListener('keydown', lightboxKeyHandler, true);
    overlay.focus();
}

let cladeInfoRequestId = 0;

async function showCladeInfo(cladeName, options = {}) {
    const infoDiv = document.getElementById('clade-info');
    if (!infoDiv || !cladeName) return;

    const requestId = ++cladeInfoRequestId;
    const heading = options.heading || 'Clade';
    const includeRevealedPath = options.includeRevealedPath === true;
    infoDiv.innerHTML = `
    <div class="clade-info">
        <div class="loading">Loading information about ${escapeChallengeHtml(cladeName)}…</div>
    </div>
    `;

    const wikiInfo = await fetchWikipediaInfo(cladeName);
    if (requestId !== cladeInfoRequestId || !infoDiv.isConnected) return;

    const revealedPathHtml = includeRevealedPath
        ? `
            <div class="phylo-path"><h4>Revealed path to the target:</h4>
                ${Array.from(revealedClades).map(clade => `
                    <div class="phylo-step">
                        <span class="phylo-step-name">${escapeChallengeHtml(clade)}</span>
                    </div>
                `).join('')}
            </div>
        `
        : '';
    
    if (!wikiInfo) {
    infoDiv.innerHTML = `
        <div class="clade-info">
        <h3>${escapeChallengeHtml(heading)}: ${escapeChallengeHtml(cladeName)}</h3>
        <p style="color:#999;font-style:italic;">No encyclopedia entry found.</p>
        ${revealedPathHtml}
        </div>
    `;
    return;
    }

    let html = `<div class="clade-info"><h3>${escapeChallengeHtml(heading)}: ${escapeChallengeHtml(cladeName)}</h3><div class="clade-content">`;
    
    if (wikiInfo.image) {
    html += `<img src="${wikiInfo.image}" alt="${wikiInfo.title}" class="clade-image" />`;
    }
    
    html += `
    <div class="clade-text">
        ${wikiInfo.description 
        ? `<p>${wikiInfo.description}</p>` 
        : '<p style="color:#999;font-style:italic;">Description unavailable.</p>'
        }
        <a href="${wikiInfo.url}" target="_blank" class="clade-link">View Encyclopedia Entry</a>
    </div>
    `;
    
    html += `</div>${revealedPathHtml}</div>`;
    infoDiv.innerHTML = html;
}

async function updateCladeInfo() {
    const infoDiv = document.getElementById('clade-info');
    if (!infoDiv) return;

    const bestGuess = guesses.reduce((best, candidate) =>
        Number(candidate?.proximity?.matches || 0) > Number(best?.proximity?.matches || 0)
            ? candidate
            : best,
    null);
    const bestGuessDepth = Number(bestGuess?.proximity?.matches || 0);
    const bestGuessClade = bestGuess?.proximity?.lastCommonClade || null;

    const deepestHint = hintHistory.reduce((best, hint) => {
        if (!hint?.cladeName) return best;
        return Number(hint.depth || 0) > Number(best?.depth || 0) ? hint : best;
    }, null);
    const deepestHintDepth = Number(deepestHint?.depth || 0);

    const useHintClade = Boolean(deepestHint?.cladeName)
        && deepestHintDepth >= bestGuessDepth;
    const selectedClade = useHintClade ? deepestHint.cladeName : bestGuessClade;

    if (!selectedClade) {
        cladeInfoRequestId++;
        infoDiv.innerHTML = '';
        return;
    }

    await showCladeInfo(selectedClade, {
        heading: useHintClade ? 'Deepest Revealed Clade' : 'Most Recent Common Ancestor',
        includeRevealedPath: true
    });
}

function updateGuessHistory() {
    const historyDiv = document.getElementById('guess-history');

    if (!historyDiv) return;
    
    if (guesses.length === 0 && hintHistory.length === 0) {
    historyDiv.innerHTML = '';
    return;
    }
    
    let html = '<div class="guess-history"><h3>Exploration History</h3>';
    
    if (hintHistory.length > 0) {
    hintHistory.slice().reverse().forEach(hint => {
        const isCladeHint = Boolean(hint.cladeName);
        const hintName = isCladeHint
            ? `[HINT: ${hint.cladeName}]`
            : '[NAME HINT]';
        const hintDetail = isCladeHint
            ? `Revealed clade at depth ${hint.depth}`
            : hint.message || 'A clue about the target name was revealed.';
        html += `
        <div class="guess-item guess-item-hint">
            <span class="guess-name">${hintName}</span>
            <span class="guess-match">${hintDetail}</span>
        </div>
        `;
    });
    }
    
    guesses.slice().reverse().forEach(guess => {
    const divInfo = guess.proximity.lastCommonClade 
        ? ` → Last common: ${guess.proximity.lastCommonClade}` 
        : '';
    
    html += `
        <div class="guess-item">
        <span class="guess-name">${guess.dino.nome}${divInfo}</span>
        <span class="guess-match">${guess.proximity.matches}/${currentTargetDepth} nodes (${guess.proximity.percentage}%)</span>
        </div>
    `;
    });
    
    html += '</div>';
    historyDiv.innerHTML = html;
}