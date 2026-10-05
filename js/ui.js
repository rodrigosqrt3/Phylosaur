// ═══════════════════════════════════════════════
// UI AND MODALS
// ═══════════════════════════════════════════════
function setHeaderControls(screen) {
    if (screen !== 'challenge' && typeof stopChallengeStatusPolling === 'function') {
      stopChallengeStatusPolling();
    }
    if (screen !== 'museum' && typeof releaseMuseumViewResources === 'function') {
      releaseMuseumViewResources();
    } else if (screen !== 'museum' && typeof stopMuseumCardMediaLoading === 'function') {
      stopMuseumCardMediaLoading();
    }
    const controls = document.getElementById('header-controls');
    if (!controls) return;

    const statsBtn = currentUser
      ? `<button class="btn-hint btn-header" onclick="showStatsDashboard()">${t('nav.stats')}</button>`
      : '';

    const analyticsBtn = isAnalyticsAdmin
      ? `<button class="btn-hint btn-header" onclick="showAnalyticsDashboard()">${t('nav.analytics')}</button>`
      : '';

    const safeCurrentUser = escapeHtml(currentUser);
    const logoutBtn = currentUser
      ? `<button class="btn-hint btn-header btn-account" onclick="logout()" title="${escapeHtml(t('nav.signOut', { name: currentUser }))}">${safeCurrentUser}</button>`
      : '';

    const backBtn = `<button class="btn-hint btn-header btn-with-icon" onclick="navigateToAppRoute('/')"><i class="ui-icon ui-icon-arrow-left" aria-hidden="true"></i><span>${t('nav.levels')}</span></button>`;

const map = {
      'login':        '',
      'difficulty': `<button class="btn-hint btn-header" onclick="showMuseum()">${t('nav.museum')}</button>` + analyticsBtn + (currentUser ? statsBtn + logoutBtn : `<button class="btn-hint btn-header" onclick="showLoginModal()">${t('nav.signIn')}</button>`),
      'game': backBtn + (currentUser ? statsBtn : `<button class="btn-hint btn-header" onclick="showLoginModal()">${t('nav.signIn')}</button>`),
      'stats':        `<button class="btn-hint btn-header btn-with-icon" onclick="navigateBackOrHome('/')"><i class="ui-icon ui-icon-arrow-left" aria-hidden="true"></i><span>${t('nav.back')}</span></button>`,
      'museum':       `<button class="btn-hint btn-header btn-with-icon" onclick="navigateBackOrHome('/')"><i class="ui-icon ui-icon-arrow-left" aria-hidden="true"></i><span>${t('nav.back')}</span></button>`,
      'about':        `<button class="btn-hint btn-header btn-with-icon" onclick="navigateBackOrHome('/')"><i class="ui-icon ui-icon-arrow-left" aria-hidden="true"></i><span>${t('nav.back')}</span></button>`,
      'practice-menu':`<button class="btn-hint btn-header btn-with-icon" onclick="navigateToAppRoute('/')"><i class="ui-icon ui-icon-arrow-left" aria-hidden="true"></i><span>${t('nav.levels')}</span></button>`,
      'practice':     `<button class="btn-hint btn-header btn-with-icon" onclick="navigateToAppRoute('/')"><i class="ui-icon ui-icon-arrow-left" aria-hidden="true"></i><span>${t('nav.levels')}</span></button>`,
      'friends':      `<button class="btn-hint btn-header btn-with-icon" onclick="navigateBackOrHome('/')"><i class="ui-icon ui-icon-arrow-left" aria-hidden="true"></i><span>${t('nav.back')}</span></button>`,
      'challenge':    `<button class="btn-hint btn-header btn-with-icon" onclick="navigateBackOrHome('/friends')"><i class="ui-icon ui-icon-arrow-left" aria-hidden="true"></i><span>${t('nav.friends')}</span></button>`,
      'analytics':    `<button class="btn-hint btn-header btn-with-icon" onclick="navigateBackOrHome('/')"><i class="ui-icon ui-icon-arrow-left" aria-hidden="true"></i><span>${t('nav.back')}</span></button>`,
    };

    // Every screen reserves the same complete menu, even when it only shows
    // Back or has no actions. Account/permission updates cannot resize the shell.
    controls.classList.toggle('stable-header-controls', true);
    const sizingMenu = `
        <div class="header-controls-reserve" aria-hidden="true" inert>
          <button class="btn-hint btn-header" type="button" tabindex="-1" disabled>${t('nav.museum')}</button>
          <button class="btn-hint btn-header" type="button" tabindex="-1" disabled>${t('nav.analytics')}</button>
          <button class="btn-hint btn-header" type="button" tabindex="-1" disabled>${t('nav.stats')}</button>
          <button class="btn-hint btn-header header-account-reserve" type="button" tabindex="-1" disabled>${t('common.player')}</button>
        </div>`;
    controls.innerHTML = sizingMenu + `<div class="header-controls-visible">${map[screen] || ''}</div>`;
}

function applyTheme(theme, { persist = true } = {}) {
    currentTheme = theme === 'light' ? 'light' : 'dark';
    document.documentElement.dataset.theme = currentTheme;
    document.body.classList.toggle('light-mode', currentTheme === 'light');
    if (persist) {
        try {
            localStorage.setItem(PHYLOSAUR_STORAGE_KEYS.theme, currentTheme);
        } catch (_error) {}
    }
    updateThemeToggleState();
}

function toggleTheme() {
    applyTheme(currentTheme === 'dark' ? 'light' : 'dark');
}

function updateThemeToggleState() {
    const toggle = document.getElementById('theme-toggle');
    if (!toggle) return;

    const isLight = currentTheme === 'light';
    toggle.setAttribute('aria-pressed', String(isLight));
    toggle.setAttribute('aria-label', isLight ? t('theme.toDark') : t('theme.toLight'));
}

function escapeAppStateText(value) {
    return escapeHtml(value);
}

function renderAppState(message, { type = 'loading', detail = '', compact = false } = {}) {
    const safeType = ['loading', 'error', 'empty'].includes(type) ? type : 'loading';
    const role = safeType === 'error' ? 'alert' : 'status';
    return `<div class="app-state app-state-${safeType}${compact ? ' app-state-compact' : ''}"
                 role="${role}" aria-live="${safeType === 'error' ? 'assertive' : 'polite'}">
        <span class="app-state-icon" aria-hidden="true"></span>
        <div class="app-state-message">${escapeAppStateText(message)}</div>
        ${detail ? `<div class="app-state-detail">${escapeAppStateText(detail)}</div>` : ''}
    </div>`;
}

function focusAppScreenHeading(selector = '.screen-title') {
    queueMicrotask(() => {
        if (document.querySelector('.modal-overlay, .tutorial-overlay, [aria-modal="true"]')) return;
        const appContent = document.getElementById('app-content');
        const heading = appContent?.querySelector(selector);
        if (!heading || !heading.isConnected) return;

        heading.setAttribute('tabindex', '-1');
        try {
            heading.focus({ preventScroll: true });
        } catch (_error) {
            heading.focus();
        }

        const label = heading.textContent?.replace(/\s+/g, ' ').trim();
        if (label) document.title = `${label} - Phylosaur`;
    });
}

function isTopAppOverlay(overlay) {
    const overlays = document.querySelectorAll('[aria-modal="true"]');
    return overlay.isConnected && overlays[overlays.length - 1] === overlay;
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
    
    let html = '<div class="modal-scroll-region">';
    
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

    html += '</div>';
    
    html += '<div class="modal-buttons">';
    
    if (options.buttons) {
        options.buttons.forEach((btn, index) => {
        const btnClass = btn.primary ? 'modal-btn-primary' : 'modal-btn-secondary';
        html += `<button class="modal-btn ${btnClass}" data-result="${btn.value}">${btn.text}</button>`;
        });
    } else {
        html += `<button class="modal-btn modal-btn-primary" data-result="ok">${t('common.ok')}</button>`;
    }
    
    html += '</div>';
    
    box.innerHTML = html;
    overlay.appendChild(box);
    document.body.appendChild(overlay);
    document.body.style.overflow = 'hidden';

    let closed = false;
    let keyHandlerAttached = false;

    const closeModal = (result, { restoreFocus = true } = {}) => {
        if (closed) return;
        closed = true;
        clearTimeout(keyHandlerTimer);
        if (keyHandlerAttached) {
            document.removeEventListener('keydown', modalKeyHandler, true);
        }
        overlay.remove();
        document.body.style.overflow = previousBodyOverflow;
        if (restoreFocus && previouslyFocused instanceof HTMLElement && previouslyFocused.isConnected) {
            previouslyFocused.focus();
        }
        resolve(result);
    };

    const modalKeyHandler = event => {
        if (!isTopAppOverlay(overlay)) return;
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
            event.stopImmediatePropagation();
            closeModal(buttons[0].getAttribute('data-result'));
        } else if (event.key === 'Escape' && options.closeOnOverlay !== false) {
            event.preventDefault();
            event.stopImmediatePropagation();
            closeModal(null);
        }
    };

    // Attach after the event that opened the modal has finished propagating.
    const keyHandlerTimer = setTimeout(() => {
        if (closed) return;
        document.addEventListener('keydown', modalKeyHandler, true);
        keyHandlerAttached = true;
        if (!isTopAppOverlay(overlay)) return;
        const firstButton = box.querySelector('.modal-btn');
        if (firstButton) firstButton.focus();
        else box.focus();
    }, 0);
    overlay.dismissAppOverlay = ({ restoreFocus = true } = {}) => closeModal(null, { restoreFocus });
    
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
    const previousViewer = document.getElementById('image-lightbox');
    // Do not replace a covered viewer underneath a newer account/confirmation dialog.
    if (previousViewer && !isTopAppOverlay(previousViewer)) return;
    const previouslyFocused = previousViewer?.lightboxPreviouslyFocused || document.activeElement;
    previousViewer?.dismissAppOverlay({ restoreFocus: false });
    const previousBodyOverflow = document.body.style.overflow;
    const overlay = document.createElement('div');
    overlay.id = 'image-lightbox';
    overlay.className = 'image-lightbox';
    overlay.lightboxPreviouslyFocused = previouslyFocused;
    overlay.setAttribute('role', 'dialog');
    overlay.setAttribute('aria-modal', 'true');
    overlay.setAttribute('aria-label', t('media.imageViewer', { name }));
    overlay.tabIndex = -1;
    overlay.style.cssText = `
    position:fixed; top:0; left:0; right:0; bottom:0;
    background:rgba(0,0,0,0.92); z-index:10000;
    display:flex; flex-direction:column;
    align-items:center; justify-content:center;
    cursor:zoom-out; animation:fadeIn 0.2s ease;
    padding:20px;
    `;
    
    const fallbackCredit = `
        ${t('media.imageSource')} ${sourcePage ? `<a href="${escapeHtml(sourcePage)}" target="_blank" rel="noopener noreferrer"
            style="color:var(--color-muted); text-decoration:none;">Wikimedia Commons</a>` : 'Wikimedia Commons'}
    `;

    overlay.innerHTML = `
    <button class="image-lightbox-close museum-image-viewer-close" type="button"
        aria-label="${escapeHtml(t('museum.closeImage'))}">×</button>
    <img src="${escapeHtml(url)}" alt="${escapeHtml(name)}"
        style="max-width:90vw; max-height:min(80vh, calc(100dvh - 150px)); border-radius:8px;
                border:2px solid var(--border-subtle); box-shadow:0 8px 40px rgba(0,0,0,0.8);" />
    <div style="margin-top:16px; font-family:Georgia,serif; font-style:italic;
                color:var(--color-accent); font-size:1.1em; letter-spacing:1px;">${escapeHtml(name)}</div>
    <div style="margin-top:8px; font-size:0.82em; color:var(--border-subtle); letter-spacing:1px;">
        ${creditHtml || fallbackCredit}
    </div>
    <div style="margin-top:16px; color:var(--border-subtle); font-size:0.8em; letter-spacing:2px;">
        ${t('media.closeAnywhere')}
    </div>
    `;
    
    overlay.querySelectorAll('a').forEach(link => {
        link.addEventListener('click', event => event.stopPropagation());
    });

    let closed = false;
    const closeLightbox = ({ restoreFocus = true } = {}) => {
        if (closed) return;
        closed = true;
        const wasTop = isTopAppOverlay(overlay);
        const hadFocus = overlay.contains(document.activeElement);
        document.removeEventListener('keydown', lightboxKeyHandler, true);
        overlay.remove();
        document.body.style.overflow = previousBodyOverflow;
        if (restoreFocus && wasTop && hadFocus
            && previouslyFocused instanceof HTMLElement && previouslyFocused.isConnected) {
            previouslyFocused.focus({ preventScroll: true });
        }
    };

    const lightboxKeyHandler = event => {
        if (!isTopAppOverlay(overlay)) return;
        if (event.key === 'Escape') {
            event.preventDefault();
            event.stopImmediatePropagation();
            closeLightbox();
        } else if (event.key === 'Tab') {
            const controls = Array.from(overlay.querySelectorAll(
                'button:not([disabled]):not([hidden]), a[href]:not([hidden]), [tabindex="0"]:not([hidden])'
            ));
            const first = controls[0];
            const last = controls.at(-1);
            if (!first) {
                event.preventDefault();
                overlay.focus({ preventScroll: true });
            } else if (!controls.includes(document.activeElement)
                || (event.shiftKey && document.activeElement === first)
                || (!event.shiftKey && document.activeElement === last)) {
                event.preventDefault();
                (event.shiftKey ? last : first).focus({ preventScroll: true });
            }
        }
    };

    overlay.dismissAppOverlay = closeLightbox;
    overlay.addEventListener('click', () => {
        if (isTopAppOverlay(overlay)) closeLightbox();
    });
    document.body.appendChild(overlay);
    document.body.style.overflow = 'hidden';
    document.addEventListener('keydown', lightboxKeyHandler, true);
    overlay.querySelector('.image-lightbox-close').focus({ preventScroll: true });
}

let cladeInfoRequestId = 0;

async function showCladeInfo(cladeName, options = {}) {
    const infoDiv = document.getElementById('clade-info');
    if (!infoDiv || !cladeName) return;

    const requestId = ++cladeInfoRequestId;
    const heading = options.heading || t('tree.clade');
    const includeRevealedPath = options.includeRevealedPath === true;
    infoDiv.innerHTML = `
    <div class="clade-info">
        <div class="loading">${t('tree.loadingInfo', { clade: escapeChallengeHtml(cladeName) })}</div>
    </div>
    `;

    const wikiInfo = await fetchWikipediaInfo(cladeName);
    if (requestId !== cladeInfoRequestId || !infoDiv.isConnected) return;

    const revealedPathHtml = includeRevealedPath
        ? `
            <div class="phylo-path"><h4>${t('tree.revealedPath')}</h4>
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
        <p class="clade-empty-copy">${t('tree.noEncyclopedia')}</p>
        ${revealedPathHtml}
        </div>
    `;
    return;
    }

    let html = `<div class="clade-info"><h3>${escapeChallengeHtml(heading)}: ${escapeChallengeHtml(cladeName)}</h3><div class="clade-content">`;
    
    if (wikiInfo.image) {
    html += `<img src="${escapeChallengeHtml(wikiInfo.image)}" alt="${escapeChallengeHtml(wikiInfo.title)}" class="clade-image" />`;
    }
    
    html += `
    <div class="clade-text">
        ${wikiInfo.description 
        ? `<p>${escapeChallengeHtml(wikiInfo.description)}</p>`
        : `<p class="clade-empty-copy">${t('tree.descriptionUnavailable')}</p>`
        }
        ${wikiInfo.isLanguageFallback
          ? `<p class="encyclopedia-language-note">${t('encyclopedia.englishFallback')}</p>` : ''}
        <a href="${escapeChallengeHtml(wikiInfo.url)}" target="_blank" rel="noopener" class="clade-link">${t('tree.encyclopedia')}</a>
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
        heading: useHintClade ? t('tree.deepestRevealed') : t('tree.recentAncestor'),
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
    
    let html = `<div class="guess-history"><h3>${t('history.title')}</h3>`;
    
    if (hintHistory.length > 0) {
    hintHistory.slice().reverse().forEach(hint => {
        const isCladeHint = Boolean(hint.cladeName);
        const hintName = isCladeHint
            ? t('history.cladeHint', { clade: escapeHtml(hint.cladeName) })
            : t('history.nameHint');
        const hintDetail = isCladeHint
            ? t('history.revealedDepth', { depth: hint.depth })
            : escapeHtml(hint.message || t('game.nameHintFallback'));
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
        ? ` → ${t('history.lastCommon', { clade: escapeHtml(guess.proximity.lastCommonClade) })}`
        : '';
    
    html += `
        <div class="guess-item">
        <span class="guess-name">${escapeHtml(guess.dino.nome)}${divInfo}</span>
        <span class="guess-match">${t('history.nodes', {
            matches: guess.proximity.matches,
            depth: currentTargetDepth,
            percent: guess.proximity.percentage
        })}</span>
        </div>
    `;
    });
    
    html += '</div>';
    historyDiv.innerHTML = html;
}